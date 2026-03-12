package com.wolfkeep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.wordspec.AnyWordSpec

class UniversalMachineSpec extends AnyWordSpec {
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
      assert(out.toByteArray()(0).toInt == 100)
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
      assert(out.toByteArray()(0).toInt == 42)
    }
  }

  "Array Index" should {
    "read from array" in {
      val program = Array(
        C := 1,
        A := alloc(C),
        D := 42,
        C := 0,
        A(C) = D,
        C := 0,
        B := A(C),
        output(B),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      assert(out.toByteArray()(0).toInt == 42)
    }
  }

  "Array Amendment" should {
    "write to array" in {
      val program = Array(
        C := 1,
        A := alloc(C),
        D := 99,
        C := 0,
        A(C) = D,
        C := 0,
        B := A(C),
        output(B),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      assert(out.toByteArray()(0).toInt == 99)
    }

    "modify array 0 (program array)" in {
      val program = Array(
        A := 0,
        B := 65,
        A(A) = B,
        C := A(A),
        output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      assert(out.toByteArray()(0).toInt == 65)
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
      assert((out.toByteArray()(0) & 0xFF).toInt == 155)
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
      assert(out.toByteArray()(0).toInt == 6)
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
      assert(out.toByteArray()(0).toInt == 3)
    }
  }

  "Not-And" should {
    "perform bitwise NAND" in {
      val program = Array(
        A := 0xFF,
        B := 0xFF,
        C := A ^& B,
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
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
      assert(out.toByteArray().length == 1)
      assert(out.toByteArray()(0).toInt == 65)
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
      val outputBytes = out.toByteArray()
      assert(outputBytes.length == 1)
      assert(outputBytes(0).toInt == 1)
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
      val outputBytes = out.toByteArray()
      assert(outputBytes.length == 1)
      assert(outputBytes(0).toInt == 1)
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
      val thrown = intercept[RuntimeException] {
        um.run()
      }
      assert(thrown.getMessage.contains("not active"))
    }

    "fail on array 0" in {
      val program = Array(
        abandon(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = intercept[RuntimeException] {
        um.run()
      }
      assert(thrown.getMessage.contains("cannot abandon array 0"))
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
      assert(out.toByteArray().sameElements(Array(72, 101)))
    }

    "fail on value > 255" in {
      val program = Array(
        A := 300,
        output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = intercept[RuntimeException] {
        um.run()
      }
      assert(thrown.getMessage.contains("exceeds 255"))
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
      assert(out.toByteArray()(0).toInt == 65)
    }

    "return 0xFF on EOF" in {
      val program = Array(
        input(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
    }
  }

  "Load Program" should {
    "replace program array" in {
      val program = Array(
        B := 2,
        A := alloc(B),
        D := 65,
        C := 0,
        A(C) = D,
        load(A, C),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      um.run()
      val bytes = out.toByteArray()
      assert(bytes.length == 0)
    }

    "fail on inactive array" in {
      val program = Array(
        B := 99,
        load(B, A),
        halt
      )
      val out = new ByteArrayOutputStream()
      val um = new UniversalMachine(program, new ByteArrayInputStream(Array()), out)
      val thrown = intercept[RuntimeException] {
        um.run()
      }
      assert(thrown.getMessage.contains("not active"))
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
      assert(out.toByteArray()(0).toInt == 0x41)
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
      val thrown = intercept[RuntimeException] {
        um.run()
      }
      assert(thrown.getMessage.contains("Division by zero"))
    }
  }
}
