package com.wolfkeep

import java.io.{InputStream, OutputStream, IOException}

class UniversalMachine(
  private val initialProgram: Array[Long],
  input: InputStream,
  output: OutputStream
) {
  private val registers = new Array[Long](8)
  private var arrays: Map[Int, Array[Long]] = Map()
  private var finger = 0
  private var running = true
  private var nextArrayId = 1

  arrays = Map(0 -> initialProgram.clone())

  def run(): Unit = {
    while (running && finger >= 0 && finger < arrays(0).length) {
      val instruction = arrays(0)(finger)
      finger += 1
      execute(instruction)
    }
  }

  private def execute(instruction: Long): Unit = {
    val op = ((instruction >>> 28) & 0xF).toInt

    op match {
      case 0 => conditionalMove(instruction)
      case 1 => arrayIndex(instruction)
      case 2 => arrayAmendment(instruction)
      case 3 => addition(instruction)
      case 4 => multiplication(instruction)
      case 5 => division(instruction)
      case 6 => notAnd(instruction)
      case 7 => halt()
      case 8 => allocation(instruction)
      case 9 => abandonment(instruction)
      case 10 => output(instruction)
      case 11 => input(instruction)
      case 12 => loadProgram(instruction)
      case 13 => orthography(instruction)
      case _ => fail(s"Unknown opcode: $op")
    }
  }

  private def regA(instruction: Long): Int = ((instruction >> 6) & 0x7).toInt
  private def regB(instruction: Long): Int = ((instruction >> 3) & 0x7).toInt
  private def regC(instruction: Long): Int = (instruction & 0x7).toInt

  private def conditionalMove(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    if (registers(c) != 0) {
      registers(a) = registers(b)
    }
  }

  private def arrayIndex(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    val arrayId = registers(b).toInt
    val offset = registers(c).toInt
    if (!arrays.contains(arrayId)) {
      fail(s"Array index: array $arrayId not active")
    }
    val arr = arrays(arrayId)
    if (offset < 0 || offset >= arr.length) {
      fail(s"Array index: offset $offset out of bounds for array of length ${arr.length}")
    }
    registers(a) = arr(offset)
  }

  private def arrayAmendment(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    val arrayId = registers(a).toInt
    val offset = registers(b).toInt
    if (!arrays.contains(arrayId)) {
      fail(s"Array amendment: array $arrayId not active")
    }
    val arr = arrays(arrayId)
    if (offset < 0 || offset >= arr.length) {
      fail(s"Array amendment: offset $offset out of bounds for array of length ${arr.length}")
    }
    arrays = arrays.updated(arrayId, arr.updated(offset, registers(c)))
  }

  private def addition(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    registers(a) = (registers(b) + registers(c)) & 0xFFFFFFFFL
  }

  private def multiplication(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    registers(a) = (registers(b) * registers(c)) & 0xFFFFFFFFL
  }

  private def division(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    if (registers(c) == 0) {
      fail("Division by zero")
    }
    registers(a) = registers(b) / registers(c)
  }

  private def notAnd(instruction: Long): Unit = {
    val a = regA(instruction)
    val b = regB(instruction)
    val c = regC(instruction)
    val bVal = (registers(b) & 0xFFFFFFFFL).toInt
    val cVal = (registers(c) & 0xFFFFFFFFL).toInt
    registers(a) = (~(bVal & cVal)) & 0xFFFFFFFFL
  }

  private def halt(): Unit = {
    running = false
  }

  private def allocation(instruction: Long): Unit = {
    val b = regB(instruction)
    val c = regC(instruction)
    val size = registers(c).toInt
    if (size < 0) {
      fail(s"Allocation: negative size $size")
    }
    val newArray = new Array[Long](size)
    var newId = 1
    while (arrays.contains(newId) || newId == 0) {
      newId += 1
    }
    arrays = arrays.updated(newId, newArray)
    registers(b) = newId
  }

  private def abandonment(instruction: Long): Unit = {
    val c = regC(instruction)
    val arrayId = registers(c).toInt
    if (arrayId == 0) {
      fail("Abandonment: cannot abandon array 0")
    }
    if (!arrays.contains(arrayId)) {
      fail(s"Abandonment: array $arrayId not active")
    }
    arrays = arrays - arrayId
  }

  private def output(instruction: Long): Unit = {
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

  private def input(instruction: Long): Unit = {
    val c = regC(instruction)
    try {
      val read = input.read()
      if (read == -1) {
        registers(c) = 0xFFFFFFFFL
      } else {
        registers(c) = read.toLong & 0xFF
      }
    } catch {
      case e: IOException => fail(s"Input failed: ${e.getMessage}")
    }
  }

  private def loadProgram(instruction: Long): Unit = {
    val b = regB(instruction)
    val c = regC(instruction)
    val arrayId = registers(b).toInt
    if (arrayId != 0) {
      if (!arrays.contains(arrayId)) {
        fail(s"Load program: array $arrayId not active")
      }
      val sourceArray = arrays(arrayId)
      val newProgram = sourceArray.clone()
      arrays = arrays.updated(0, newProgram)
    }
    finger = registers(c).toInt
    if (finger < 0 || finger >= arrays(0).length) {
      fail(s"Load program: finger position $finger out of bounds for array of length ${arrays(0).length}")
    }
  }

  private def orthography(instruction: Long): Unit = {
    val a = ((instruction >>> 25) & 0x7).toInt
    val value = instruction & 0x1FFFFFF
    registers(a) = value
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

  def readProgram(filename: String): Array[Long] = {
    val fis = new java.io.FileInputStream(filename)
    val bytes = fis.readAllBytes()
    fis.close()
    if (bytes.length % 4 != 0) {
      throw new IllegalArgumentException(s"Program file length ${bytes.length} is not a multiple of 4")
    }
    val words = new Array[Long](bytes.length / 4)
    for (i <- words.indices) {
      val b0 = (bytes(i * 4) & 0xFF).toLong
      val b1 = (bytes(i * 4 + 1) & 0xFF).toLong
      val b2 = (bytes(i * 4 + 2) & 0xFF).toLong
      val b3 = (bytes(i * 4 + 3) & 0xFF).toLong
      words(i) = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
    }
    words
  }
}
