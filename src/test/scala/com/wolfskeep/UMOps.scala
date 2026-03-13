package com.wolfkeep

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
  private def makeInstruction(op: Int, a: Int, b: Int, c: Int): Long = {
    ((op & 0xF) << 28) | ((a & 0x7) << 6) | ((b & 0x7) << 3) | (c & 0x7)
  }

  private def makeOrthography(a: Int, value: Long): Long = {
    ((13L & 0xFL) << 28) | ((a & 0x7L) << 25) | (value & 0xFFFFFFFFL)
  }

  // Expression types
  case class ArithExpr(lhs: Reg, rhs: Reg, op: Int)
  case class CMovExpr(b: Reg, c: Reg)
  case class ArrayIdx(arr: Reg, idx: Reg) {
    def :=(value: Reg): Long = makeInstruction(2, arr.index, idx.index, value.index)
  }

  // Unified implicit class for Reg := operations
  implicit class RegOps(r: Reg) {
    def :=(value: Long): Long = makeOrthography(r.index, value)
    def :=(expr: ArithExpr): Long = makeInstruction(expr.op, r.index, expr.lhs.index, expr.rhs.index)
    def :=(expr: CMovExpr): Long = makeInstruction(0, r.index, expr.b.index, expr.c.index)
    def :=(expr: ArrayIdx): Long = makeInstruction(1, r.index, expr.arr.index, expr.idx.index)
    def :=(expr: AllocResult): Long = makeInstruction(8, 0, r.index, expr.size.index)
    
    // Array index: A(B) returns ArrayIdx
    def apply(idx: Reg): ArrayIdx = ArrayIdx(r, idx)
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
  def halt: Long = makeInstruction(7, 0, 0, 0)

  def abandon(c: Reg): Long = makeInstruction(9, 0, 0, c.index)

  def input(c: Reg): Long = makeInstruction(11, 0, 0, c.index)

  def output(c: Reg): Long = makeInstruction(10, 0, 0, c.index)

  def jump(expr: ArrayIdx): Long = makeInstruction(12, 0, expr.arr.index, expr.idx.index)
}
