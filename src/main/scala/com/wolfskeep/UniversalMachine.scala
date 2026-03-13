package com.wolfkeep

import java.io.{InputStream, OutputStream, IOException}

class UniversalMachine(
  private val initialProgram: Array[Int],
  input: InputStream,
  outputStream: OutputStream
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

      val op = (instruction >>> 28) & 0xF
      val a = (instruction >>>  6) & 7
      val b = (instruction >>>  3) & 7
      val c = instruction & 7
      val d = (instruction >>> 25) & 7
      val v = instruction & 0x01FFFFFF

      op match {
        case 0 => if (registers(c) != 0) registers(a) = registers(b)
        case 1 => registers(a) = arrays(registers(b))(registers(c))
        case 2 => arrays(registers(a))(registers(b)) = registers(c)
        case 3 => registers(a) = registers(b) + registers(c)
        case 4 => registers(a) = ((registers(b).toLong & 0xffffffffL) * (registers(c).toLong & 0xffffffffL)).toInt
        case 5 => registers(a) = ((registers(b).toLong & 0xffffffffL) / (registers(c).toLong & 0xffffffffL)).toInt
        case 6 => registers(a) = ~(registers(b) & registers(c))
        case 7 => running = false
        case 8 => {
          var newId = 1
          while (arrays contains newId) newId += 1
          arrays += newId -> new Array[Int](registers(c))
          registers(b) = newId
        }
        case 9 => if (registers(c) == 0) fail("cannot abandon array 0") else arrays -= registers(c)
        case 10 => {
          val value = registers(c)
          if (value > 255) {
            fail(s"Output: value $value exceeds 255")
          }
          try {
            outputStream.write(value.toInt)
            outputStream.flush()
          } catch {
            case e: IOException => fail(s"Output failed: ${e.getMessage}")
          }
        }
        case 11 => {
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
        case 12 => {
          if (registers(b) != 0) arrays += (0 -> arrays(registers(b)).clone())
          finger = registers(c)
        }
        case 13 => registers(d) = v
        case _ => fail(s"Unknown opcode: $op")
      }
    }
  }

  private def fail(message: String): Unit = {
    val prevInstruction = if (finger > 0 && (finger - 1) < arrays(0).length) arrays(0)(finger - 1) else -1
    val op = ((prevInstruction >>> 28) & 0xF).toInt
    val regA = ((prevInstruction >>> 6) & 0x7).toInt
    val regB = ((prevInstruction >>> 3) & 0x7).toInt
    val regC = (prevInstruction & 0x7).toInt

    System.err.println(s"UM Error at finger position ${finger - 1}")
    System.err.println(s"Instruction: op=$op, a=$regA, b=$regB, c=$regC")
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
