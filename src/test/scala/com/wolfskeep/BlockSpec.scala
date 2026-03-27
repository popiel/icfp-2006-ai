package com.wolfskeep

import scala.collection.immutable.BitSet
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class BlockSpec extends AnyWordSpec with Matchers {
  "Block.+" should {
    "combine adjacent blocks" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map.empty)
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map.empty)
      val combined = block1 + block2
      combined.start should equal(0)
      combined.end should equal(10)
    }

    "throw IllegalArgumentException when blocks are not adjacent" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map.empty)
      val block2 = Block(7, 10, BitSet.empty, Map.empty, Map.empty)
      val thrown = the[IllegalArgumentException] thrownBy { block1 + block2 }
      thrown.getMessage should include("Blocks not adjacent")
    }

    "throw IllegalArgumentException when blocks overlap" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map.empty)
      val block2 = Block(5, 10, BitSet.empty, Map.empty, Map.empty)
      a[IllegalArgumentException] should be thrownBy { block1 + block2 }
    }

    "combine read bitsets without written overlap" in {
      val block1 = Block(0, 5, BitSet(1, 2), Map(3 -> 0), Map.empty)
      val block2 = Block(6, 10, BitSet(4, 5), Map.empty, Map.empty)
      val combined = block1 + block2
      combined.read should equal(BitSet(1, 2, 4, 5))
    }

    "subtract this.written from that.read in combined read" in {
      val block1 = Block(0, 5, BitSet(1), Map(2 -> 0, 3 -> 1), Map.empty)
      val block2 = Block(6, 10, BitSet(2, 3, 4), Map.empty, Map.empty)
      val combined = block1 + block2
      combined.read should equal(BitSet(1, 4))
    }

    "combine written maps" in {
      val block1 = Block(0, 5, BitSet.empty, Map(1 -> 0, 2 -> 1), Map.empty)
      val block2 = Block(6, 10, BitSet.empty, Map(2 -> 6, 3 -> 7), Map.empty)
      val combined = block1 + block2
      combined.written should equal(Map(1 -> 0, 2 -> 6, 3 -> 7))
    }

    "combine knownValues with empty maps" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map.empty)
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map.empty)
      val combined = block1 + block2
      combined.knownValues should equal(Map.empty)
    }

    "combine knownValues with non-overlapping keys" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map(0 -> Some(PossibleValuesSet(Set(42)))))
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map(1 -> Some(PossibleValuesSet(Set(99)))))
      val combined = block1 + block2
      combined.knownValues should equal(Map(
        0 -> Some(PossibleValuesSet(Set(42))),
        1 -> Some(PossibleValuesSet(Set(99)))
      ))
    }

    "override this.knownValues with that.knownValues for overlapping keys" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map(
        0 -> Some(PossibleValuesSet(Set(42))),
        1 -> Some(PossibleValuesSet(Set(10, 20)))
      ))
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map(
        0 -> Some(PossibleValuesSet(Set(99))),
        2 -> Some(PossibleValuesRange(0 to 5))
      ))
      val combined = block1 + block2
      combined.knownValues should equal(Map(
        0 -> Some(PossibleValuesSet(Set(99))),
        1 -> Some(PossibleValuesSet(Set(10, 20))),
        2 -> Some(PossibleValuesRange(0 to 5))
      ))
    }

    "override with None when that.knownValues has None for key" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map(0 -> Some(PossibleValuesSet(Set(42)))))
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map(0 -> None))
      val combined = block1 + block2
      combined.knownValues should equal(Map(0 -> None))
    }

    "preserve this.knownValues when that.knownValues has no entry for key" in {
      val block1 = Block(0, 5, BitSet.empty, Map.empty, Map(
        0 -> Some(PossibleValuesSet(Set(42))),
        1 -> Some(PossibleValuesRange(0 to 10))
      ))
      val block2 = Block(6, 10, BitSet.empty, Map.empty, Map(1 -> Some(PossibleValuesSet(Set(99)))))
      val combined = block1 + block2
      combined.knownValues should equal(Map(
        0 -> Some(PossibleValuesSet(Set(42))),
        1 -> Some(PossibleValuesSet(Set(99)))
      ))
    }

    "handle complex combination of all fields" in {
      val block1 = Block(0, 5, BitSet(1, 2), Map(3 -> 0), Map(0 -> Some(PossibleValuesSet(Set(1, 2)))))
      val block2 = Block(6, 10, BitSet(3, 4), Map(5 -> 6), Map(0 -> Some(PossibleValuesSet(Set(99))), 1 -> None))
      val combined = block1 + block2
      combined.start should equal(0)
      combined.end should equal(10)
      combined.read should equal(BitSet(1, 2, 4))
      combined.written should equal(Map(3 -> 0, 5 -> 6))
      combined.knownValues should equal(Map(
        0 -> Some(PossibleValuesSet(Set(99))),
        1 -> None
      ))
    }
  }
}