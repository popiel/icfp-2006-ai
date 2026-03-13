package com.wolfskeep

sealed trait Reg {
  val index: Int
}

case object A extends Reg { val index = 0 }
case object B extends Reg { val index = 1 }
case object C extends Reg { val index = 2 }
case object D extends Reg { val index = 3 }
case object E extends Reg { val index = 4 }
case object F extends Reg { val index = 5 }
case object G extends Reg { val index = 6 }
case object H extends Reg { val index = 7 }

object UMOps {
  private def makeInstruction(op: Int, a: Int, b: Int, c: Int): Int = {
    ((op & 0xF) << 28) | ((a & 0x7) << 6) | ((b & 0x7) << 3) | (c & 0x7)
  }

  private def makeOrthography(a: Int, value: Int): Int = {
    (13 << 28) | ((a & 0x7) << 25) | (value & 0xFFFFFFFF)
  }

  // Expression types
  case class ArithExpr(lhs: Reg, rhs: Reg, op: Int)
  case class CMovExpr(b: Reg, c: Reg)
  case class ArrayIdx(arr: Reg, idx: Reg) {
    def :=(value: Reg) = makeInstruction(2, arr.index, idx.index, value.index)
  }

  // Unified implicit class for Reg := operations
  implicit class RegOps(r: Reg) {
    def :=(value: Int) = makeOrthography(r.index, value)
    def :=(expr: ArithExpr) = makeInstruction(expr.op, r.index, expr.lhs.index, expr.rhs.index)
    def :=(expr: CMovExpr) = makeInstruction(0, r.index, expr.b.index, expr.c.index)
    def :=(expr: ArrayIdx) = makeInstruction(1, r.index, expr.arr.index, expr.idx.index)
    def :=(expr: AllocResult) = makeInstruction(8, 0, r.index, expr.size.index)
    
    // Array index: A(B) returns ArrayIdx
    def apply(idx: Reg) = ArrayIdx(r, idx)
  }

  // Arithmetic: A := B + C, A := B * C, A := B / C, A := B ^& C
  implicit class RegArithOps(lhs: Reg) {
    def +(rhs: Reg) = ArithExpr(lhs, rhs, 3)
    def *(rhs: Reg) = ArithExpr(lhs, rhs, 4)
    def /(rhs: Reg) = ArithExpr(lhs, rhs, 5)
    def ^&(rhs: Reg) = ArithExpr(lhs, rhs, 6)
  }

  // Conditional Move: A := B when C
  implicit class RegCMovOps(lhs: Reg) {
    def when(c: Reg) = CMovExpr(lhs, c)
  }

  // Allocation: A := alloc(B)
  class AllocResult(val size: Reg)
  def alloc(c: Reg): AllocResult = new AllocResult(c)

  // Other instructions
  def halt = makeInstruction(7, 0, 0, 0)

  def abandon(c: Reg) = makeInstruction(9, 0, 0, c.index)

  def input(c: Reg) = makeInstruction(11, 0, 0, c.index)

  def output(c: Reg) = makeInstruction(10, 0, 0, c.index)

  def jump(expr: ArrayIdx) = makeInstruction(12, 0, expr.arr.index, expr.idx.index)
}
