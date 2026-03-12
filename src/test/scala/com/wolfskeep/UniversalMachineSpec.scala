package com.wolfkeep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.funsuite.AnyFunSuite

class UniversalMachineSpec extends AnyFunSuite {

  private def makeInstruction(op: Int, a: Int, b: Int, c: Int): Long = {
    ((op & 0xF) << 28) | ((a & 0x7) << 6) | ((b & 0x7) << 3) | (c & 0x7)
  }

  private def makeOrthography(a: Int, value: Long): Long = {
    ((13L & 0xFL) << 28) | ((a & 0x7L) << 25) | (value & 0xFFFFFFFFL)
  }

  test("Conditional Move - copies register when C != 0") {
    val program = Array(
      makeOrthography(0, 42),
      makeOrthography(1, 100),
      makeOrthography(2, 1),
      makeInstruction(0, 0, 1, 2),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 100)
  }

  test("Conditional Move - does not copy when C == 0") {
    val program = Array(
      makeOrthography(0, 42),
      makeOrthography(1, 100),
      makeOrthography(2, 0),
      makeInstruction(0, 0, 1, 2),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 42)
  }

  test("Array Index - reads from array") {
    val program = Array(
      makeOrthography(1, 1),
      makeOrthography(2, 1),
      makeInstruction(8, 0, 1, 2),
      makeOrthography(0, 42),
      makeOrthography(2, 0),
      makeInstruction(2, 1, 2, 0),
      makeInstruction(1, 3, 1, 2),
      makeInstruction(10, 0, 0, 3),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 42)
  }

  test("Array Amendment - writes to array") {
    val program = Array(
      makeOrthography(1, 1),
      makeOrthography(2, 1),
      makeInstruction(8, 0, 1, 2),
      makeOrthography(3, 99),
      makeOrthography(2, 0),
      makeInstruction(2, 1, 2, 3),
      makeInstruction(1, 4, 1, 2),
      makeInstruction(10, 0, 0, 4),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 99)
  }

  test("Addition - adds registers modulo 2^32") {
    val program = Array(
      makeOrthography(0, 100),
      makeOrthography(1, 55),
      makeInstruction(3, 2, 0, 1),
      makeInstruction(10, 0, 0, 2),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert((out.toByteArray()(0) & 0xFF).toInt == 155)
  }

  test("Not-And - bitwise NAND") {
    val program = Array(
      makeOrthography(1, 1),
      makeInstruction(8, 0, 3, 1),
      makeOrthography(0, 0xF0),
      makeOrthography(1, 0x0F),
      makeInstruction(6, 2, 0, 1),
      makeOrthography(4, 0),
      makeInstruction(2, 3, 4, 2),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
  }

  test("Multiplication - multiplies registers modulo 2^32") {
    val program = Array(
      makeOrthography(0, 2),
      makeOrthography(1, 3),
      makeInstruction(4, 2, 0, 1),
      makeInstruction(10, 0, 0, 2),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 6)
  }

  test("Division - divides registers") {
    val program = Array(
      makeOrthography(0, 10),
      makeOrthography(1, 3),
      makeInstruction(5, 2, 0, 1),
      makeInstruction(10, 0, 0, 2),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 3)
  }

  test("Halt - stops execution") {
    val program = Array(
      makeOrthography(0, 65),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0),
      makeInstruction(10, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray().length == 1)
    assert(out.toByteArray()(0).toInt == 65)
  }

  test("Allocation - creates array and returns ID") {
    val program = Array(
      makeOrthography(1, 10),
      makeInstruction(8, 0, 0, 1),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    val outputBytes = out.toByteArray()
    assert(outputBytes.length == 1)
    assert(outputBytes(0).toInt == 1)
  }

  test("Allocation - reuses freed array IDs") {
    val program = Array(
      makeOrthography(1, 10),
      makeInstruction(8, 0, 2, 1),
      makeInstruction(9, 0, 0, 2),
      makeOrthography(3, 10),
      makeInstruction(8, 0, 4, 3),
      makeInstruction(10, 0, 0, 4),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    val outputBytes = out.toByteArray()
    assert(outputBytes.length == 1)
    assert(outputBytes(0).toInt == 1)
  }

  test("Abandonment - deallocates array") {
    val program = Array(
      makeOrthography(1, 1),
      makeInstruction(8, 0, 2, 1),
      makeInstruction(9, 0, 0, 2),
      makeInstruction(1, 0, 2, 2),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    val thrown = intercept[RuntimeException] {
      um.run()
    }
    assert(thrown.getMessage.contains("not active"))
  }

  test("Output - outputs byte value") {
    val program = Array(
      makeOrthography(0, 72),
      makeInstruction(10, 0, 0, 0),
      makeOrthography(0, 101),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray().sameElements(Array(72, 101)))
  }

  test("Input - reads from input stream") {
    val program = Array(
      makeInstruction(11, 0, 0, 0),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val input = Array[Byte](65)
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(input), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 65)
  }

  test("Input - returns 0xFF on EOF") {
    val program = Array(
      makeInstruction(11, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
  }

  test("Load Program - replaces program array") {
    val program = Array(
      makeOrthography(1, 2),
      makeInstruction(8, 0, 2, 1),
      makeOrthography(3, 65),
      makeOrthography(0, 0),
      makeInstruction(2, 2, 0, 3),
      makeInstruction(12, 0, 2, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    val bytes = out.toByteArray()
    assert(bytes.length == 0)
  }

  test("Orthography - loads immediate value") {
    val program = Array(
      makeOrthography(0, 0x41),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 0x41)
  }

  test("Array Amendment - modifies array 0 (program array)") {
    val program = Array(
      makeOrthography(0, 0),
      makeOrthography(1, 65),
      makeInstruction(2, 0, 0, 1),
      makeOrthography(2, 0),
      makeInstruction(1, 3, 0, 2),
      makeInstruction(10, 0, 0, 3),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    um.run()
    assert(out.toByteArray()(0).toInt == 65)
  }

  test("Division by zero - fails") {
    val program = Array(
      makeOrthography(1, 0),
      makeInstruction(5, 0, 0, 1),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    val thrown = intercept[RuntimeException] {
      um.run()
    }
    assert(thrown.getMessage.contains("Division by zero"))
  }

  test("Output - fails on value > 255") {
    val program = Array(
      makeOrthography(0, 300),
      makeInstruction(10, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    val thrown = intercept[RuntimeException] {
      um.run()
    }
    assert(thrown.getMessage.contains("exceeds 255"))
  }

  test("Abandonment - fails on array 0") {
    val program = Array(
      makeInstruction(9, 0, 0, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    val thrown = intercept[RuntimeException] {
      um.run()
    }
    assert(thrown.getMessage.contains("cannot abandon array 0"))
  }

  test("Load Program - fails on inactive array") {
    val program = Array(
      makeOrthography(1, 99),
      makeInstruction(12, 0, 1, 0),
      makeInstruction(7, 0, 0, 0)
    )
    val out = new ByteArrayOutputStream()
    val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
    val thrown = intercept[RuntimeException] {
      um.run()
    }
    assert(thrown.getMessage.contains("not active"))
  }
}
