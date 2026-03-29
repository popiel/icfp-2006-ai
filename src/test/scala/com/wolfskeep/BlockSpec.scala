package com.wolfskeep

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Instruction._

class BlockSpec extends AnyWordSpec with Matchers {
  def initialRegisters: Array[Computation] = 
    (0 to 7).map(i => RegisterAccess(i): Computation).toArray

  "Block" should {
    "be created with entry and spans" in {
      val span = Span(0, 5, initialRegisters, List.empty)
      val block = Block(0, List(span))
      block.start should equal(0)
      block.end should equal(5)
      block.spans should have size 1
    }

    "contain effects" in {
      val span = Span(0, 5, initialRegisters, List(Halt, Output(Orthography(42))))
      val block = Block(0, List(span))
      block.spans.head.effects should have size 2
    }
  }
}