package com.wolfskeep

import scala.collection.mutable.{Queue, Set => MSet}

sealed trait RegisterState {
  def knownValues: Option[Set[Int]]
}
case class Known(value: Int) extends RegisterState {
  def knownValues: Option[Set[Int]] = Some(Set(value))
}
case class KnownValues(values: Set[Int]) extends RegisterState {
  def knownValues: Option[Set[Int]] = Some(values)
}
case object Unknown extends RegisterState {
  def knownValues: Option[Set[Int]] = None
}

object UMDecoder {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: UMDecoder <program file>")
      sys.exit(1)
    }
    val program = UniversalMachine.readProgram(args(0))
    val decoder = new UMDecoder(program)
    decoder.run()
  }
}

class UMDecoder(program: Array[Int]) {
  private val visited = MSet[Int]()
  private val worklist = Queue[(Int, Array[RegisterState])]()

  def run(): Unit = {
    val initialRegs = Array.fill[RegisterState](8)(Known(0))
    worklist.enqueue((0, initialRegs))

    while (worklist.nonEmpty) {
      val (entry, regs) = worklist.dequeue()
      if (!visited.contains(entry)) {
        scanSpan(entry, regs.clone())
      }
    }
  }

  private def scanSpan(entry: Int, regs: Array[RegisterState]): Unit = {
    var finger = entry
    val spanStart = entry
    var spanEnd = entry
    var halted = false

    while (finger < program.length && !visited.contains(finger) && !halted) {
      val instr = program(finger)
      val op = (instr >>> 28) & 0xF

      if (op == 7) {
        visited.add(finger)
        spanEnd = finger + 1
        halted = true
      } else if (op == 12) {
        val b = (instr >>> 3) & 7
        val c = instr & 7
        val bIsZero = regs(b).knownValues.exists(_ == Set(0))
        val cValues = regs(c).knownValues

        visited.add(finger)
        spanEnd = finger + 1

        if (bIsZero && cValues.isDefined) {
          for (jumpTarget <- cValues.get) {
            if (jumpTarget >= 0 && jumpTarget < program.length && !visited.contains(jumpTarget)) {
              worklist.enqueue((jumpTarget, regs.clone()))
            }
          }
        }
        halted = true
      } else {
        visited.add(finger)
        spanEnd = finger + 1
        updateRegisters(instr, op, regs)
        finger += 1
      }
    }

    var currentRegs = regs.clone()
    println(s"SPAN $spanStart to $spanEnd:")
    for (i <- spanStart until spanEnd) {
      println(decodeInstruction(program(i), currentRegs))
      if (i < spanEnd - 1) {
        updateRegisters(program(i), (program(i) >>> 28) & 0xF, currentRegs)
      }
    }
    println()
  }

  private def updateRegisters(instr: Int, op: Int, regs: Array[RegisterState]): Unit = {
    if (op == 13) {
      val d = (instr >>> 25) & 7
      val v = instr & 0x01FFFFFF
      val signedV = if (v >= 0x01000000) v - 0x02000000 else v
      regs(d) = Known(signedV)
    } else if (op == 0) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      val cValues = regs(c).knownValues
      val aValues = regs(a).knownValues
      val bValues = regs(b).knownValues
      
      if (cValues.isDefined && cValues.get == Set(0)) {
        // c is known to be 0, so a unchanged
      } else if (cValues.isDefined && !cValues.contains(0)) {
        // c is known to be non-zero, so a = b
        if (bValues.isDefined) {
          regs(a) = if (bValues.get.size == 1) Known(bValues.get.head) else KnownValues(bValues.get)
        } else {
          regs(a) = Unknown
        }
      } else {
        // c is unknown or has mixed 0/non-zero values
        // a can be either unchanged or equal to b
        val possibleValues = scala.collection.mutable.Set[Int]()
        if (aValues.isDefined) possibleValues ++= aValues.get
        if (bValues.isDefined) possibleValues ++= bValues.get
        
        if (possibleValues.isEmpty) {
          regs(a) = Unknown
        } else if (possibleValues.size == 1) {
          regs(a) = Known(possibleValues.head)
        } else {
          regs(a) = KnownValues(possibleValues.toSet)
        }
      }
    } else if (op >= 1 && op <= 6) {
      val a = (instr >>> 6) & 7
      if (op != 2) {
        regs(a) = Unknown
      }
    } else if (op == 8) {
      val b = (instr >>> 3) & 7
      regs(b) = Unknown
    } else if (op == 11) {
      val c = instr & 7
      regs(c) = Unknown
    }
  }

  private def decodeInstruction(instr: Int, regs: Array[RegisterState]): String = {
    val op = (instr >>> 28) & 0xF
    
    if (op == 13) {
      val d = (instr >>> 25) & 7
      val v = instr & 0x01FFFFFF
      val signedV = if (v >= 0x01000000) v - 0x02000000 else v
      s"${regName(d)} := $signedV"
    } else if (op == 0) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)} when ${regName(c)}"
    } else if (op == 1) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)}(${regName(c)})"
    } else if (op == 2) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)}(${regName(b)}) := ${regName(c)}"
    } else if (op == 3) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)} + ${regName(c)}"
    } else if (op == 4) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)} * ${regName(c)}"
    } else if (op == 5) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)} / ${regName(c)}"
    } else if (op == 6) {
      val a = (instr >>> 6) & 7
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(a)} := ${regName(b)} ^& ${regName(c)}"
    } else if (op == 7) {
      "halt"
    } else if (op == 8) {
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"${regName(b)} := alloc(${regName(c)})"
    } else if (op == 9) {
      val c = instr & 7
      s"abandon ${regName(c)}"
    } else if (op == 10) {
      val c = instr & 7
      val cVal = regs(c).knownValues.flatMap(_.headOption)// Get single value if known
      cVal match {
        case Some(v) =>
          val numStr = f"$v%3d"
          if (v >= 33 && v <= 126) {
            val ch = v.toChar
            s"output ${regName(c)} $numStr '$ch'"
          } else {
            s"output ${regName(c)} $numStr"
          }
        case None =>
          s"output ${regName(c)}"
      }
    } else if (op == 11) {
      val c = instr & 7
      s"input ${regName(c)}"
    } else if (op == 12) {
      val b = (instr >>> 3) & 7
      val c = instr & 7
      s"loadprogram ${regName(b)}(${regName(c)})"
    } else {
      s"unknown opcode $op"
    }
  }

  private def regName(i: Int): String = i match {
    case 0 => "A"
    case 1 => "B"
    case 2 => "C"
    case 3 => "D"
    case 4 => "E"
    case 5 => "F"
    case 6 => "G"
    case 7 => "H"
  }
}