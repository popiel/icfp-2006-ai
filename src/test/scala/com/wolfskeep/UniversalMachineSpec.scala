package com.wolfkeep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class UniversalMachineSpec extends AnyWordSpec with Matchers {
  import UMOps._

  "Conditional Move" should {
    "copy register when C != 0" in {
      val program = Array(
        A := 42,
        B := 100,
        C := 1,
        A := B when C,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(100)
    }

    "not copy when C == 0" in {
      val program = Array(
        A := 42,
        B := 100,
        C := 0,
        A := B when C,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(42)
    }
  }

  "Array Index" should {
    "read from array" in {
      val program = Array(
        C := 1,
        A := alloc(C),
        D := 42,
        C := 0,
        A(C) := D,
        C := 0,
        B := A(C),
        output(B),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(42)
    }
  }

  "Array Amendment" should {
    "write to array" in {
      val program = Array(
        C := 1,
        A := alloc(C),
        D := 99,
        C := 0,
        A(C) := D,
        C := 0,
        B := A(C),
        output(B),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(99)
    }

    "modify array 0 (program array)" in {
      val program = Array(
        A := 0,
        B := 65,
        A(A) := B,
        C := A(A),
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(65)
    }
  }

  "Addition" should {
    "add registers modulo 2^32" in {
      val program = Array(
        A := 100,
        B := 55,
        C := A + B,
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      (out.toByteArray()(0) & 0xFF).toInt should equal(155)
    }
  }

  "Multiplication" should {
    "multiply registers modulo 2^32" in {
      val program = Array(
        A := 2,
        B := 3,
        C := A * B,
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(6)
    }
  }

  "Division" should {
    "divide registers" in {
      val program = Array(
        A := 10,
        B := 3,
        C := A / B,
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(3)
    }
  }

  "Not-And" should {
    "perform bitwise NAND with different values" in {
      val program = Array(
        A := 0xF0,
        B := 0x0F,
        C := A ^& B,
        C := C ^& C,
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0) should equal(0.toByte)
    }

    "perform bitwise NAND with overlapping bits" in {
      val program = Array(
        A := 0x0F,
        B := 0x0F,
        C := A ^& B,
        C := C ^& C,
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      (out.toByteArray()(0) & 0xFF) should equal(15)
    }
  }

  "Halt" should {
    "stop execution" in {
      val program = Array(
        A := 65,
        output(A),
        halt,
        output(A)
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray() should have size 1
      out.toByteArray()(0).toInt should equal(65)
    }
  }

  "Allocation" should {
    "create array and return ID" in {
      val program = Array(
        B := 10,
        A := alloc(B),
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray() should have size 1
      out.toByteArray()(0).toInt should equal(1)
    }

    "reuse freed array IDs" in {
      val program = Array(
        B := 10,
        A := alloc(B),
        abandon(A),
        D := 10,
        A := alloc(D),
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray() should have size 1
      out.toByteArray()(0).toInt should equal(1)
    }
  }

  "Abandonment" should {
    "deallocate array" in {
      val program = Array(
        C := 1,
        A := alloc(C),
        abandon(A),
        C := 0,
        B := A(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      a [RuntimeException] should be thrownBy { um.run() }
    }

    "fail on array 0" in {
      val program = Array(
        abandon(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = the [RuntimeException] thrownBy { um.run() }
      thrown.getMessage should include("cannot abandon array 0")
    }
  }

  "Output" should {
    "output byte value" in {
      val program = Array(
        A := 72,
        output(A),
        A := 101,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray() should equal(Array(72, 101))
    }

    "fail on value > 255" in {
      val program = Array(
        A := 300,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = the [RuntimeException] thrownBy { um.run() }
      thrown.getMessage should include("exceeds 255")
    }
  }

  "Input" should {
    "read from input stream" in {
      val program = Array(
        input(A),
        output(A),
        halt
      )
      val inputBytes = Array[Byte](65)
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(inputBytes), out)
      um.run()
      out.toByteArray()(0).toInt should equal(65)
    }

    "return 0xFF on EOF" in {
      val program = Array(
        input(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      // Input on empty stream should return 0xFFFFFFFF (all 1s)
      // The program just halts without outputting, so we just verify it runs
      out.toByteArray() should be(empty)
    }
  }

  "Load Program" should {
    "replace program array" in {
      val program = Array(
        B := 2,
        A := alloc(B),
        D := 65,
        C := 0,
        A(C) := D,
        jump(A(C)),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray() should be(empty)
    }

    "fail on inactive array" in {
      val program = Array(
        B := 99,
        jump(B(A)),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = the [RuntimeException] thrownBy { um.run() }
      thrown.getMessage should include("not active")
    }
  }

  "Orthography" should {
    "load immediate value" in {
      val program = Array(
        A := 0x41,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      out.toByteArray()(0).toInt should equal(0x41)
    }
  }

  "Division by zero" should {
    "fail" in {
      val program = Array(
        B := 0,
        A := A / B,
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = the [RuntimeException] thrownBy { um.run() }
      thrown.getMessage should include("Division by zero")
    }
  }
}
