package com.wolfskeep

import scala.collection.mutable.{Queue, Set => MSet}

sealed trait RegisterState
case class Known(value: Int) extends RegisterState
case object Unknown extends RegisterState

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
        val bKnown = regs(b) match {
          case Known(v) => v == 0
          case Unknown => false
        }
        val cKnown = regs(c) match {
          case Known(v) => Some(v)
          case Unknown => None
        }

        visited.add(finger)
        spanEnd = finger + 1

        if (bKnown && cKnown.isDefined) {
          val jumpTarget = cKnown.get
          if (jumpTarget >= 0 && jumpTarget < program.length && !visited.contains(jumpTarget)) {
            worklist.enqueue((jumpTarget, regs.clone()))
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

    println(s"SPAN $spanStart to $spanEnd:")
    for (i <- spanStart until spanEnd) {
      println(decodeInstruction(program(i), regs))
    }
    println()
  }

  private def updateRegisters(instr: Int, op: Int, regs: Array[RegisterState]): Unit = {
    if (op == 13) {
      val d = (instr >>> 25) & 7
      val v = instr & 0x01FFFFFF
      // Convert to signed 32-bit
      val signedV = if (v >= 0x01000000) v - 0x02000000 else v
      regs(d) = Known(signedV)
    } else if (op == 0) {
      val a = (instr >>> 6) & 7
      val c = instr & 7
      regs(c) match {
        case Known(0) => // a unchanged
        case _ => regs(a) = Unknown
      }
    } else if (op >= 1 && op <= 6) {
      val a = (instr >>> 6) & 7
      if (op != 2) { // ArrayUpdate doesn't write to register
        regs(a) = Unknown
      }
    } else if (op == 8) {
      val b = (instr >>> 3) & 7
      regs(b) = Unknown
    }
    // ops 9, 10, 11 don't write to registers
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
      s"output ${regName(c)}"
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