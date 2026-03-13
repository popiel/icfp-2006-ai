package com.wolfkeep

import java.io.{InputStream, OutputStream, IOException}

class UniversalMachine(
  private val initialProgram: Array[Int],
  input: InputStream,
  output: OutputStream
) {
  private val registers = new Array[Int](8)
  private var arrays: Map[Int, Array[Int]] = Map()
  private var finger = 0
  private var running = true
  private var nextArrayId = 1

  arrays = Map(0 -> initialProgram.clone())

  def run(): Unit = {
    while (running) {
      val instruction = arrays(0)(finger)
      finger += 1

      implicit def r_to_i(r: Reg) = registers(r.idx)
      case class Reg(val idx: Int) {
        def := (value: Int) { registers(idx) = value }
      }

      val op = (instruction >>> 28) & 0xF
      def A = Reg((instruction >>>  6) & 7)
      def B = Reg((instruction >>>  3) & 7)
      def C = Reg((instruction >>>  0) & 7)
      def D = Reg((instruction >>> 25) & 7)
      def V = instruction & 0x01FFFFFF

      op match {
        case 0 => if (C.toInt != 0) A := B
        case 1 => A := arrays(B)(C)
        case 2 => arrays(A)(B) = C
        case 3 => A := B + C
        case 4 => A := ((B.toLong & 0xffffffffL) * (C.toLong & 0xffffffffL)).toInt
        case 5 => A := ((B.toLong & 0xffffffffL) / (C.toLong & 0xffffffffL)).toInt
        case 6 => A := ~(B & C)
        case 7 => running = false
        case 8 => {
          var newId = 1
          while (arrays contains newId) newId += 1
          arrays += newId -> new Array[Int](C)
          B := newId
        }
        case 9 => if (C.toInt == 0) fail("cannot abandon array 0") else arrays -= C
        case 10 => output(instruction)
        case 11 => input(instruction)
        case 12 => {
          if (B.toInt != 0) arrays += (0 -> arrays(B).clone())
          finger = C
        }
        case 13 => D := V
        case _ => fail(s"Unknown opcode: $op")
      }
    }
  }

  private def regA(instruction: Int): Int = ((instruction >> 6) & 0x7).toInt
  private def regB(instruction: Int): Int = ((instruction >> 3) & 0x7).toInt
  private def regC(instruction: Int): Int = (instruction & 0x7).toInt

  private def output(instruction: Int): Unit = {
    val c = regC(instruction)
    val value = registers(c)
    if (value > 255) {
      fail(s"Output: value $value exceeds 255")
    }
    try {
      output.write(value.toInt)
      output.flush()
    } catch {
      case e: IOException => fail(s"Output failed: ${e.getMessage}")
    }
  }

  private def input(instruction: Int): Unit = {
    val c = regC(instruction)
    try {
      val read = input.read()
      if (read == -1) {
        registers(c) = 0xFFFFFFFF
      } else {
        registers(c) = read & 0xFF
      }
    } catch {
      case e: IOException => fail(s"Input failed: ${e.getMessage}")
    }
  }

  private def fail(message: String): Unit = {
    val instruction = if (finger > 0 && (finger - 1) < arrays(0).length) arrays(0)(finger - 1) else -1
    val op = ((instruction >>> 28) & 0xF).toInt
    val a = ((instruction >>> 6) & 0x7).toInt
    val b = ((instruction >>> 3) & 0x7).toInt
    val c = (instruction & 0x7).toInt

    System.err.println(s"UM Error at finger position ${finger - 1}")
    System.err.println(s"Instruction: op=$op, a=$a, b=$b, c=$c")
    System.err.println(s"Registers: ${registers.mkString("[", ", ", "]")}")
    System.err.println(s"Message: $message")

    throw new RuntimeException(s"UM failed at position ${finger - 1}: $message")
  }
}

object UniversalMachine {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: UniversalMachine <program file>")
      sys.exit(1)
    }

    val filename = args(0)
    val program = readProgram(filename)
    val um = new UniversalMachine(program, System.in, System.out)
    um.run()
  }

  def readProgram(filename: String): Array[Int] = {
    val fis = new java.io.FileInputStream(filename)
    val bytes = fis.readAllBytes()
    fis.close()
    if (bytes.length % 4 != 0) {
      throw new IllegalArgumentException(s"Program file length ${bytes.length} is not a multiple of 4")
    }
    val words = new Array[Int](bytes.length / 4)
    for (i <- words.indices) {
      val b0 = bytes(i * 4) & 0xFF
      val b1 = bytes(i * 4 + 1) & 0xFF
      val b2 = bytes(i * 4 + 2) & 0xFF
      val b3 = bytes(i * 4 + 3) & 0xFF
      words(i) = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
    }
    words
  }
}
