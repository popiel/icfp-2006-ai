package com.wolfskeep

import scala.collection.immutable.SortedMap
import Instruction._

sealed trait PossibleValues {
  def isZero: Boolean
  def notZero: Boolean
  def maybeZero = !isZero && !notZero
}
case class PossibleValuesSet(values: Set[Int]) extends PossibleValues {
  def isZero = values == Set(0)
  def notZero = !values.contains(0)
}
case class PossibleValuesRange(range: Range) extends PossibleValues {
  def isZero = range == (0 to 0)
  def notZero = !range.contains(0)
}

sealed trait Instruction
sealed trait Computation extends Instruction {
  def knownValues: Option[PossibleValues]
}
sealed trait Effect extends Instruction

object Instruction {
  implicit class RichOptionPossibleValues(v: Option[PossibleValues]) {
    def isZero = v match { case None => false; case Some(p) => p.isZero }
    def notZero = v match { case None => false; case Some(p) => p.notZero }
    def maybeZero = !isZero && !notZero

    def union(b: Option[PossibleValues]) = (v, b) match {
      case (None, _) | (_, None) => None
      case (Some(PossibleValuesSet(s1)), Some(PossibleValuesSet(s2))) =>
        Some(PossibleValuesSet(s1 ++ s2))
      case (Some(PossibleValuesRange(r1)), Some(PossibleValuesRange(r2))) =>
        if (r1.end + 1 >= r2.start && r2.end + 1 >= r1.start) {
          Some(PossibleValuesRange(math.min(r1.start, r2.start) to math.max(r1.end, r2.end)))
        } else {
          None
        }
      case (Some(PossibleValuesSet(s)), Some(PossibleValuesRange(r))) => unionSetRange(s, r)
      case (Some(PossibleValuesRange(r)), Some(PossibleValuesSet(s))) => unionSetRange(s, r)
    }

    private def unionSetRange(s: Set[Int], r: Range): Option[PossibleValues] = {
      if (s.isEmpty) return Some(PossibleValuesRange(r))
      if (s.forall(x => x >= r.start && x <= r.end)) {
        return Some(PossibleValuesRange(r))
      }
      val rangeSize = r.length
      if (rangeSize < 100) {
        return Some(PossibleValuesSet(s ++ r))
      }
      val sMin = s.min
      val sMax = s.max
      val newStart = math.min(r.start, sMin)
      val newEnd = math.max(r.end, sMax)
      val newSize = newEnd - newStart + 1
      if (newSize <= 2 * rangeSize || newSize - rangeSize <= 100) {
        Some(PossibleValuesRange(newStart to newEnd))
      } else {
        None
      }
    }

    private def toUnsigned(i: Int): Long = i & 0xFFFFFFFFL

    private def fromUnsigned(l: Long): Int = l.toInt

    private def addSets(bSet: Set[Int], cSet: Set[Int]): Option[PossibleValues] = {
      val results = for { b <- bSet; c <- cSet } yield {
        val sum = toUnsigned(b) + toUnsigned(c)
        Some(fromUnsigned(sum & 0xFFFFFFFFL))
      }
      Some(PossibleValuesSet(results.flatten.toSet))
    }

    private def addRanges(bRange: Range, cRange: Range): Option[PossibleValues] = {
      val newStart = toUnsigned(bRange.start) + toUnsigned(cRange.start)
      val newEnd = toUnsigned(bRange.end) + toUnsigned(cRange.end)
      val endDiff = newEnd - newStart
      if (newEnd >= 0x100000000L || endDiff > Int.MaxValue)
        None
      else if (newStart >= 0x100000000L)
        Some(PossibleValuesRange(fromUnsigned(newStart) to fromUnsigned(newEnd)))
      else
        Some(PossibleValuesRange(newStart.toInt to newEnd.toInt))
    }

    private def multiplySets(bSet: Set[Int], cSet: Set[Int]): Option[PossibleValues] = {
      val results = for { b <- bSet; c <- cSet } yield {
        val prod = toUnsigned(b) * toUnsigned(c)
        Some(fromUnsigned(prod))
      }
      Some(PossibleValuesSet(results.flatten.toSet))
    }

    private def multiplyRanges(bRange: Range, cRange: Range): Option[PossibleValues] = {
      val corners = List(
        toUnsigned(bRange.start) * toUnsigned(cRange.start),
        toUnsigned(bRange.start) * toUnsigned(cRange.end),
        toUnsigned(bRange.end) * toUnsigned(cRange.start),
        toUnsigned(bRange.end) * toUnsigned(cRange.end)
      )
      if (corners.exists(_ >= 0x100000000L)) None
      else {
        val minProd = corners.min.toInt
        val maxProd = corners.max.toInt
        Some(PossibleValuesRange(minProd to maxProd))
      }
    }

    private def divideSets(bSet: Set[Int], cSet: Set[Int]): Option[PossibleValues] = {
      if (cSet.exists(c => toUnsigned(c) == 0)) return None
      val results = for {
        b <- bSet
        c <- cSet
        if toUnsigned(c) != 0
      } yield fromUnsigned(toUnsigned(b) / toUnsigned(c))
      if (results.isEmpty) None
      else Some(PossibleValuesSet(results.toSet))
    }

    private def divideRanges(bRange: Range, cRange: Range): Option[PossibleValues] = {
      if (toUnsigned(cRange.start) == 0 || (cRange.start < 0 && cRange.end >= 0)) return None
      val corners = List(
        toUnsigned(bRange.start) / toUnsigned(cRange.start),
        toUnsigned(bRange.start) / toUnsigned(cRange.end),
        toUnsigned(bRange.end) / toUnsigned(cRange.start),
        toUnsigned(bRange.end) / toUnsigned(cRange.end)
      )
      val minQ = corners.min.toInt
      val maxQ = corners.max.toInt
      Some(PossibleValuesRange(minQ to maxQ))
    }

    private def nandSets(bSet: Set[Int], cSet: Set[Int]): Option[PossibleValues] = {
      val results = for { b <- bSet; c <- cSet } yield ~(b & c)
      Some(PossibleValuesSet(results))
    }

    private def addValues(bVal: Option[PossibleValues], cVal: Option[PossibleValues]): Option[PossibleValues] = (bVal, cVal) match {
      case (Some(PossibleValuesSet(bSet)), Some(PossibleValuesSet(cSet))) => addSets(bSet, cSet)
      case (Some(PossibleValuesRange(bRange)), Some(PossibleValuesRange(cRange))) => addRanges(bRange, cRange)
      case _ => None
    }

    private def multiplyValues(bVal: Option[PossibleValues], cVal: Option[PossibleValues]): Option[PossibleValues] = (bVal, cVal) match {
      case (Some(PossibleValuesSet(bSet)), Some(PossibleValuesSet(cSet))) => multiplySets(bSet, cSet)
      case (Some(PossibleValuesRange(bRange)), Some(PossibleValuesRange(cRange))) => multiplyRanges(bRange, cRange)
      case _ => None
    }

    private def divideValues(bVal: Option[PossibleValues], cVal: Option[PossibleValues]): Option[PossibleValues] = (bVal, cVal) match {
      case (Some(PossibleValuesSet(bSet)), Some(PossibleValuesSet(cSet))) => divideSets(bSet, cSet)
      case (Some(PossibleValuesRange(bRange)), Some(PossibleValuesRange(cRange))) => divideRanges(bRange, cRange)
      case _ => None
    }

    private def nandValues(bVal: Option[PossibleValues], cVal: Option[PossibleValues]): Option[PossibleValues] = (bVal, cVal) match {
      case (Some(PossibleValuesSet(bSet)), Some(PossibleValuesSet(cSet))) => nandSets(bSet, cSet)
      case _ => None
    }

    def +(that: Option[PossibleValues]) = addValues(v, that)
    def *(that: Option[PossibleValues]) = multiplyValues(v, that)
    def /(that: Option[PossibleValues]) = divideValues(v, that)
    def ^&(that: Option[PossibleValues]) = nandValues(v, that)
  }

case class ConditionalMove(a: Computation, b: Computation, c: Computation) extends Computation {
    lazy val knownValues = if (c.knownValues.isZero) a.knownValues else if (c.knownValues.notZero) b.knownValues else a.knownValues.union(b.knownValues)
  }
  case class ArrayIndex(b: Computation, c: Computation) extends Computation with Effect {
    def knownValues = None
  }
  case class ArrayAmendment(a: Computation, b: Computation, c: Computation) extends Effect
  case class Addition(b: Computation, c: Computation) extends Computation {
    lazy val knownValues = b.knownValues + c.knownValues
  }
  case class Multiplication(b: Computation, c: Computation) extends Computation {
    lazy val knownValues = b.knownValues * c.knownValues
  }
  case class Division(b: Computation, c: Computation) extends Computation {
    lazy val knownValues = b.knownValues / c.knownValues
  }
  case class Nand(b: Computation, c: Computation) extends Computation {
    lazy val knownValues = b.knownValues ^& c.knownValues
  }
  case object Halt extends Effect
  case class Allocation(size: Computation) extends Computation with Effect {
    val knownValues = Some(PossibleValuesRange(1 to Int.MaxValue))
  }
  case class Abandonment(c: Computation) extends Effect
  case class Output(c: Computation) extends Effect
  case object Input extends Computation with Effect {
    val knownValues = Some(PossibleValuesRange(-1 to 255))
  }
  case class LoadProgram(
    b: Computation,
    c: Computation,
    written: Map[Int, Computation]
  ) extends Effect
  case class Orthography(v: Int) extends Computation {
    def knownValues = Some(PossibleValuesSet(Set(v)))
  }
  case class RegisterAccess(r: Int) extends Computation {
    def knownValues = None
  }
}

case class Block(
  start: Int,
  end: Int,
  effects: List[Effect]
)

trait CompiledBlock {
  def run(registers: Array[Int]): Int
}

class Analyzer(val prog: Array[Int]) {
  val blocks: SortedMap[Int, Block] = SortedMap.empty
  val compiled: Map[Int, CompiledBlock] = Map.empty

  def from(finger: Int, registers: Array[Computation]): Block = {
    require(registers.length == 8)
    require(registers.forall(_ != null))
    
    var pos = finger
    var regs = registers.clone()
    var effects = List.empty[Effect]
    var touched = Set.empty[Int]
    
    while (pos < prog.length) {
      val instruction = prog(pos)
      val op = (instruction >>> 28) & 0xF
      
      if (op == 13) {
        val a = (instruction >>> 25) & 7
        val value = instruction & 0x01FFFFFF
        regs(a) = Orthography(value)
        touched = touched + a
        pos += 1
      } else {
        val a = (instruction >>> 6) & 7
        val b = (instruction >>> 3) & 7
        val c = instruction & 7
        
        op match {
          case 0 =>
            regs(a) = ConditionalMove(regs(a), regs(b), regs(c))
            touched = touched + a
            pos += 1
          case 1 =>
            effects = effects :+ ArrayIndex(regs(b), regs(c))
            regs(a) = RegisterAccess(a)
            touched = touched + a
            pos += 1
          case 2 =>
            effects = effects :+ ArrayAmendment(regs(a), regs(b), regs(c))
            pos += 1
          case 3 =>
            regs(a) = Addition(regs(b), regs(c))
            touched = touched + a
            pos += 1
          case 4 =>
            regs(a) = Multiplication(regs(b), regs(c))
            touched = touched + a
            pos += 1
          case 5 =>
            regs(a) = Division(regs(b), regs(c))
            touched = touched + a
            pos += 1
          case 6 =>
            regs(a) = Nand(regs(b), regs(c))
            touched = touched + a
            pos += 1
          case 7 =>
            return Block(finger, pos, effects :+ Halt)
          case 8 =>
            val comp = Allocation(regs(c))
            regs(b) = comp
            touched = touched + b
            effects = effects :+ comp
            pos += 1
          case 9 =>
            effects = effects :+ Abandonment(regs(c))
            pos += 1
          case 10 =>
            effects = effects :+ Output(regs(c))
            pos += 1
          case 11 =>
            regs(c) = Input
            touched = touched + c
            effects = effects :+ Input
            pos += 1
          case 12 =>
            val written = touched.map(r => r -> regs(r)).toMap
            return Block(finger, pos, effects :+ LoadProgram(regs(b), regs(c), written))
        }
      }
    }
    Block(finger, prog.length - 1, effects)
  }
}
