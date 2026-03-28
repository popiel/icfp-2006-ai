package com.wolfskeep

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class InstructionSpec extends AnyWordSpec with Matchers {
  import Instruction._

  "PossibleValuesSet.isZero" should {
    "return true for Set(0)" in {
      PossibleValuesSet(Set(0)).isZero shouldBe true
    }
    "return false for Set(1)" in {
      PossibleValuesSet(Set(1)).isZero shouldBe false
    }
    "return false for Set(0, 1)" in {
      PossibleValuesSet(Set(0, 1)).isZero shouldBe false
    }
    "return false for empty set" in {
      PossibleValuesSet(Set.empty).isZero shouldBe false
    }
  }

  "PossibleValuesSet.notZero" should {
    "return true for Set(1)" in {
      PossibleValuesSet(Set(1)).notZero shouldBe true
    }
    "return true for Set(1, 2, 3)" in {
      PossibleValuesSet(Set(1, 2, 3)).notZero shouldBe true
    }
    "return false for Set(0)" in {
      PossibleValuesSet(Set(0)).notZero shouldBe false
    }
    "return false for Set(0, 1)" in {
      PossibleValuesSet(Set(0, 1)).notZero shouldBe false
    }
    "return true for empty set" in {
      PossibleValuesSet(Set.empty).notZero shouldBe true
    }
  }

  "PossibleValuesRange.isZero" should {
    "return true for Range(0 to 0)" in {
      PossibleValuesRange(0 to 0).isZero shouldBe true
    }
    "return false for Range(1 to 5)" in {
      PossibleValuesRange(1 to 5).isZero shouldBe false
    }
    "return false for Range(-5 to 5)" in {
      PossibleValuesRange(-5 to 5).isZero shouldBe false
    }
  }

  "PossibleValuesRange.notZero" should {
    "return true for Range(1 to 5)" in {
      PossibleValuesRange(1 to 5).notZero shouldBe true
    }
    "return true for Range(-5 to -1)" in {
      PossibleValuesRange(-5 to -1).notZero shouldBe true
    }
    "return false for Range(0 to 5)" in {
      PossibleValuesRange(0 to 5).notZero shouldBe false
    }
    "return false for Range(-1 to 1)" in {
      PossibleValuesRange(-1 to 1).notZero shouldBe false
    }
  }

  "PossibleValues.maybeZero" should {
    "return true when not isZero and not notZero" in {
      PossibleValuesSet(Set(0, 1)).maybeZero shouldBe true
      PossibleValuesRange(0 to 5).maybeZero shouldBe true
    }
    "return false when isZero" in {
      PossibleValuesSet(Set(0)).maybeZero shouldBe false
      PossibleValuesRange(0 to 0).maybeZero shouldBe false
    }
    "return false when notZero" in {
      PossibleValuesSet(Set(1, 2)).maybeZero shouldBe false
      PossibleValuesRange(1 to 5).maybeZero shouldBe false
    }
  }

  "RichOptionPossibleValues.isZero" should {
    "return true for Some(PossibleValuesSet(Set(0)))" in {
      Some(PossibleValuesSet(Set(0))).isZero shouldBe true
    }
    "return false for Some(PossibleValuesSet(Set(1)))" in {
      Some(PossibleValuesSet(Set(1))).isZero shouldBe false
    }
    "return false for None" in {
      None.isZero shouldBe false
    }
  }

  "RichOptionPossibleValues.notZero" should {
    "return true for Some(PossibleValuesSet(Set(1)))" in {
      Some(PossibleValuesSet(Set(1))).notZero shouldBe true
    }
    "return true for Some(PossibleValuesRange(1 to 5))" in {
      Some(PossibleValuesRange(1 to 5)).notZero shouldBe true
    }
    "return false for Some(PossibleValuesSet(Set(0)))" in {
      Some(PossibleValuesSet(Set(0))).notZero shouldBe false
    }
    "return false for None" in {
      None.notZero shouldBe false
    }
  }

  "RichOptionPossibleValues.maybeZero" should {
    "return true for Some(PossibleValuesSet(Set(0, 1)))" in {
      Some(PossibleValuesSet(Set(0, 1))).maybeZero shouldBe true
    }
    "return false for Some(PossibleValuesSet(Set(0)))" in {
      Some(PossibleValuesSet(Set(0))).maybeZero shouldBe false
    }
    "return false for Some(PossibleValuesSet(Set(1, 2)))" in {
      Some(PossibleValuesSet(Set(1, 2))).maybeZero shouldBe false
    }
  }

  "RichOptionPossibleValues.union" should {
    "return None when either operand is None" in {
      None.union(Some(PossibleValuesSet(Set(1)))) shouldBe None
      Some(PossibleValuesSet(Set(1))).union(None) shouldBe None
      None.union(None) shouldBe None
    }

    "merge two sets" in {
      Some(PossibleValuesSet(Set(1, 2))).union(Some(PossibleValuesSet(Set(2, 3)))) shouldBe
        Some(PossibleValuesSet(Set(1, 2, 3)))
    }

    "merge overlapping ranges" in {
      Some(PossibleValuesRange(0 to 10)).union(Some(PossibleValuesRange(5 to 15))) shouldBe
        Some(PossibleValuesRange(0 to 15))
    }

    "merge adjacent ranges" in {
      Some(PossibleValuesRange(0 to 5)).union(Some(PossibleValuesRange(6 to 10))) shouldBe
        Some(PossibleValuesRange(0 to 10))
    }

    "merge ranges that touch at boundary" in {
      Some(PossibleValuesRange(0 to 5)).union(Some(PossibleValuesRange(5 to 10))) shouldBe
        Some(PossibleValuesRange(0 to 10))
    }

    "return None for disjoint ranges" in {
      Some(PossibleValuesRange(0 to 5)).union(Some(PossibleValuesRange(10 to 15))) shouldBe None
    }

    "return range when all set elements within range" in {
      Some(PossibleValuesSet(Set(2, 5))).union(Some(PossibleValuesRange(0 to 10))) shouldBe
        Some(PossibleValuesRange(0 to 10))
    }

    "expand range to set when range is small" in {
      Some(PossibleValuesSet(Set(20))).union(Some(PossibleValuesRange(0 to 5))) shouldBe
        Some(PossibleValuesSet((0 to 5).toSet + 20))
    }

    "expand range when set elements extend within limits" in {
      Some(PossibleValuesSet(Set(0, 200))).union(Some(PossibleValuesRange(100 to 300))) shouldBe
        Some(PossibleValuesRange(0 to 300))
    }

    "return None when expansion is too large" in {
      Some(PossibleValuesSet(Set(0, 10000))).union(Some(PossibleValuesRange(100 to 200))) shouldBe None
    }

    "handle empty set" in {
      Some(PossibleValuesSet(Set.empty)).union(Some(PossibleValuesRange(1 to 10))) shouldBe
        Some(PossibleValuesRange(1 to 10))
    }
  }

  "RichOptionPossibleValues addition (+)" should {
    "add two sets" in {
      Some(PossibleValuesSet(Set(1, 2))) + Some(PossibleValuesSet(Set(3, 4))) shouldBe
        Some(PossibleValuesSet(Set(4, 5, 6)))
    }

    "add two ranges" in {
      Some(PossibleValuesRange(0 to 10)) + Some(PossibleValuesRange(5 to 10)) shouldBe
        Some(PossibleValuesRange(5 to 20))
    }

    "return None for Set + Range" in {
      Some(PossibleValuesSet(Set(1))) + Some(PossibleValuesRange(1 to 5)) shouldBe None
    }

    "return None for Range + Set" in {
      Some(PossibleValuesRange(1 to 5)) + Some(PossibleValuesSet(Set(1))) shouldBe None
    }

    "return None when either is None" in {
      None + Some(PossibleValuesSet(Set(1))) shouldBe None
      Some(PossibleValuesSet(Set(1))) + None shouldBe None
    }

    "handle unsigned overflow in addition" in {
      Some(PossibleValuesSet(Set(-1))) + Some(PossibleValuesSet(Set(1))) shouldBe
        Some(PossibleValuesSet(Set(0)))
    }
  }

  "RichOptionPossibleValues multiplication (*)" should {
    "multiply two sets" in {
      Some(PossibleValuesSet(Set(2, 3))) * Some(PossibleValuesSet(Set(4, 5))) shouldBe
        Some(PossibleValuesSet(Set(8, 10, 12, 15)))
    }

    "multiply two ranges" in {
      Some(PossibleValuesRange(1 to 5)) * Some(PossibleValuesRange(2 to 3)) shouldBe
        Some(PossibleValuesRange(2 to 15))
    }

    "return None for Set * Range" in {
      Some(PossibleValuesSet(Set(2))) * Some(PossibleValuesRange(1 to 5)) shouldBe None
    }

    "return None when either is None" in {
      None * Some(PossibleValuesSet(Set(2))) shouldBe None
    }

    "handle large unsigned multiplication" in {
      Some(PossibleValuesSet(Set(65536))) * Some(PossibleValuesSet(Set(65536))) shouldBe
        Some(PossibleValuesSet(Set(0)))
    }
  }

  "RichOptionPossibleValues division (/)" should {
    "divide two sets" in {
      Some(PossibleValuesSet(Set(10, 20))) / Some(PossibleValuesSet(Set(2, 5))) shouldBe
        Some(PossibleValuesSet(Set(2, 4, 5, 10)))
    }

    "return None when divisor contains zero" in {
      Some(PossibleValuesSet(Set(10))) / Some(PossibleValuesSet(Set(0, 2))) shouldBe None
    }

    "return None when divisor range contains zero" in {
      Some(PossibleValuesRange(10 to 100)) / Some(PossibleValuesRange(0 to 5)) shouldBe None
    }

    "divide two ranges" in {
      Some(PossibleValuesRange(10 to 100)) / Some(PossibleValuesRange(2 to 5)) shouldBe
        Some(PossibleValuesRange(2 to 50))
    }

    "return None for Set / Range" in {
      Some(PossibleValuesSet(Set(10))) / Some(PossibleValuesRange(2 to 5)) shouldBe None
    }

    "return None when either is None" in {
      None / Some(PossibleValuesSet(Set(2))) shouldBe None
    }
  }

  "RichOptionPossibleValues NAND (^&)" should {
    "nand two sets" in {
      Some(PossibleValuesSet(Set(0xF0, 0x0F))) ^& Some(PossibleValuesSet(Set(0xFF, 0x0F))) shouldBe
        Some(PossibleValuesSet(Set(~0, ~0xF0, ~0x0F, ~0x0F)))
    }

    "return None for Range ^& Set" in {
      Some(PossibleValuesRange(0 to 5)) ^& Some(PossibleValuesSet(Set(1))) shouldBe None
    }

    "return None when either is None" in {
      None ^& Some(PossibleValuesSet(Set(1))) shouldBe None
    }
  }

"Abandonment" should {
    "be an Effect" in {
      val c = Orthography(1)
      Abandonment(c) shouldBe an[Effect]
    }
  }

  "Output" should {
    "be an Effect" in {
      val c = Orthography(1)
      Output(c) shouldBe an[Effect]
    }
  }

  "LoadProgram" should {
    "be an Effect" in {
      val b = Orthography(0)
      val c = Orthography(0)
      LoadProgram(b, c, Map.empty) shouldBe an[Effect]
    }
  }

  "ArrayAmendment" should {
    "be an Effect" in {
      val a = Orthography(0)
      val b = Orthography(0)
      val c = Orthography(0)
      ArrayAmendment(a, b, c) shouldBe an[Effect]
    }
  }

  "ConditionalMove" should {
    "return a.knownValues when c is zero" in {
      val c = Orthography(0)
      val b = Orthography(42)
      val a = Orthography(99)
      ConditionalMove(a, b, c).knownValues shouldBe Some(PossibleValuesSet(Set(99)))
    }

    "return b.knownValues when c excludes zero" in {
      val c = Orthography(5)
      val b = Orthography(42)
      val a = Orthography(99)
      ConditionalMove(a, b, c).knownValues shouldBe Some(PossibleValuesSet(Set(42)))
    }

    "return union when c may be zero" in {
      val b = Orthography(42)
      val c = Input
      val a = Orthography(99)
      ConditionalMove(a, b, c).knownValues shouldBe Some(PossibleValuesSet(Set(42, 99)))
    }
  }

  "ArrayIndex" should {
    "always return None for knownValues" in {
      val b = Orthography(1)
      val c = Orthography(2)
      ArrayIndex(b, c).knownValues shouldBe None
    }
  }

  "Addition" should {
    "delegate to + operator" in {
      val b = Orthography(10)
      val c = Orthography(1)
      Addition(b, c).knownValues shouldBe Some(PossibleValuesSet(Set(11)))
    }

    "return None when either operand is None" in {
      val b = ArrayIndex(Orthography(1), Orthography(2))
      val c = Orthography(1)
      Addition(b, c).knownValues shouldBe None
    }
  }

  "Multiplication" should {
    "delegate to * operator" in {
      val b = Orthography(2)
      val c = Orthography(4)
      Multiplication(b, c).knownValues shouldBe Some(PossibleValuesSet(Set(8)))
    }
  }

  "Division" should {
    "delegate to / operator" in {
      val b = Orthography(10)
      val c = Orthography(2)
      Division(b, c).knownValues shouldBe Some(PossibleValuesSet(Set(5)))
    }
  }

  "Nand" should {
    "delegate to ^& operator" in {
      val b = Orthography(0xF0)
      val c = Orthography(0xFF)
      Nand(b, c).knownValues shouldBe Some(PossibleValuesSet(Set(~0xF0)))
    }
  }

  "Allocation" should {
    "return Range(1 to Int.MaxValue)" in {
      val size = Orthography(10)
      Allocation(size).knownValues shouldBe Some(PossibleValuesRange(1 to Int.MaxValue))
    }
  }

  "Input" should {
    "return Range(-1 to 255)" in {
      Input.knownValues shouldBe Some(PossibleValuesRange(-1 to 255))
    }
  }

  "Orthography" should {
    "return Set with the immediate value" in {
      Orthography(42).knownValues shouldBe Some(PossibleValuesSet(Set(42)))
    }
    "handle max immediate value" in {
      Orthography(0x01FFFFFF).knownValues shouldBe Some(PossibleValuesSet(Set(0x01FFFFFF)))
    }
    "handle zero immediate value" in {
      Orthography(0).knownValues shouldBe Some(PossibleValuesSet(Set(0)))
    }
  }

  "Halt" should {
    "be an Effect" in {
      Halt shouldBe an[Effect]
    }
  }
}