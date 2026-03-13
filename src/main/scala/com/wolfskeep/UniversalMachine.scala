package com.wolfkeep

import java.io.{InputStream, OutputStream}

class UniversalMachine(
  private val initialProgram: Array[Int],
  input: InputStream,
  outputStream: OutputStream
) {
  def run(): Unit = {
    val registers = new Array[Int](8)
    var arrays: Map[Int, Array[Int]] = Map(0 -> initialProgram.clone())
    var nextArrayId = 1
    var availableArrayIds: List[Int] = Nil
    var finger = 0
    while (true) {
      val instruction = arrays(0)(finger)
      finger += 1

      val op = (instruction >>> 28) & 0xF
      if (op == 0) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        if (registers(c) != 0) registers(a) = registers(b)
      } else if (op == 1) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        registers(a) = arrays(registers(b))(registers(c))
      } else if (op == 2) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        arrays(registers(a))(registers(b)) = registers(c)
      } else if (op == 3) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        registers(a) = registers(b) + registers(c)
      } else if (op == 4) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        registers(a) = ((registers(b).toLong & 0xffffffffL) * (registers(c).toLong & 0xffffffffL)).toInt
      } else if (op == 5) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        registers(a) = ((registers(b).toLong & 0xffffffffL) / (registers(c).toLong & 0xffffffffL)).toInt
      } else if (op == 6) {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        registers(a) = ~(registers(b) & registers(c))
      } else if (op == 7) {
        return
      } else if (op == 8) {
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        val newId = if (availableArrayIds != Nil) {
          val id = availableArrayIds.head
          availableArrayIds = availableArrayIds.tail
          id
        } else {
          val id = nextArrayId
          nextArrayId += 1
          id
        }
        arrays += newId -> new Array[Int](registers(c))
        registers(b) = newId
      } else if (op == 9) {
        val c = instruction & 7
        if (registers(c) == 0) {
          throw new IllegalArgumentException("cannot abandon array 0")
        } else {
          availableArrayIds = registers(c) :: availableArrayIds
          arrays -= registers(c)
        }
      } else if (op == 10) {
        val c = instruction & 7
        val value = registers(c)
        if (value > 255) {
          throw new IllegalArgumentException(s"Output: value $value exceeds 255")
        }
        outputStream.write(value.toInt)
        outputStream.flush()
      } else if (op == 11) {
        val c = instruction & 7
        val read = input.read()
        if (read == -1) {
          registers(c) = 0xFFFFFFFF
        } else {
          registers(c) = read & 0xFF
        }
      } else if (op == 12) {
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        if (registers(b) != 0) arrays += (0 -> arrays(registers(b)).clone())
        finger = registers(c)
      } else if (op == 13) {
        val d = (instruction >>> 25) & 7
        val v = instruction & 0x01FFFFFF
        registers(d) = v
      } else {
        throw new IllegalArgumentException(s"Unknown opcode: $op")
      }
    }
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
