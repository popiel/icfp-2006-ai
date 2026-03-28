package com.wolfskeep

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class BlockSpec extends AnyWordSpec with Matchers {
  "Block" should {
    "be created with start, end, and effects" in {
      val block = Block(0, 5, List.empty)
      block.start should equal(0)
      block.end should equal(5)
      block.effects should equal(List.empty)
    }

    "contain effects" in {
      import Instruction._
      val block = Block(0, 5, List(Halt, Output(Instruction.Orthography(42))))
      block.effects should have size 2
    }
  }
}