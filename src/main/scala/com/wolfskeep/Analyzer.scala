package com.wolfskeep

import scala.collection.immutable.SortedMap
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}
import org.objectweb.asm.Opcodes._
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
  case class ArrayIndex(a: Int, b: Computation, c: Computation) extends Computation with Effect {
    def knownValues = None
  }
  case class ArrayAmendment(a: Computation, b: Computation, c: Computation, finger: Int) extends Effect
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
  case class Allocation(b: Int, size: Computation) extends Computation with Effect {
    val knownValues = Some(PossibleValuesRange(1 to Int.MaxValue))
  }
  case class Abandonment(c: Computation) extends Effect
  case class Output(c: Computation) extends Effect
  case class Input(c: Int) extends Computation with Effect {
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

object Block {
  private var seqNum = 0
  def nextSeqNum(): Int = synchronized {
    val n = seqNum
    seqNum += 1
    n
  }
}

case class Block(
  start: Int,
  end: Int,
  effects: List[Effect]
) {
  private var compiledBlock: CompiledBlock = _
  
  def compile(): CompiledBlock = synchronized {
    if (compiledBlock != null) return compiledBlock
    
    import org.objectweb.asm.{ClassWriter, MethodVisitor, Label}
    import org.objectweb.asm.Opcodes._
    
    val seqNum = Block.nextSeqNum()
    val className = s"CompiledBlock_${start}_${end}_$seqNum"
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    
    cw.visit(V1_8, ACC_PUBLIC, className, null, "java/lang/Object", Array("com/wolfskeep/CompiledBlock"))
    
    // Add fields for start and end
    cw.visitField(ACC_PUBLIC, "start", "I", null, null).visitEnd()
    cw.visitField(ACC_PUBLIC, "end", "I", null, null).visitEnd()
    
    // Constructor
    var mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitLdcInsn(start)
    mv.visitFieldInsn(PUTFIELD, className, "start", "I")
    mv.visitVarInsn(ALOAD, 0)
    mv.visitLdcInsn(end)
    mv.visitFieldInsn(PUTFIELD, className, "end", "I")
    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    
    // start() getter method
    mv = cw.visitMethod(ACC_PUBLIC, "start", "()I", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, className, "start", "I")
    mv.visitInsn(IRETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    
    // end() getter method
    mv = cw.visitMethod(ACC_PUBLIC, "end", "()I", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, className, "end", "I")
    mv.visitInsn(IRETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    
    // run method
    mv = cw.visitMethod(ACC_PUBLIC, "run", "([I)I", null, null)
    mv.visitCode()
    
    // Generate bytecode for each effect
    for (effect <- effects) {
      generateEffect(mv, effect, className)
    }
    
    // If we reach here without returning, return -1 (shouldn't happen for valid blocks)
    mv.visitInsn(ICONST_M1)
    mv.visitInsn(IRETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    
    cw.visitEnd()
    
    val bytes = cw.toByteArray()
    val parentLoader = Thread.currentThread().getContextClassLoader
    val loader = new ClassLoader(parentLoader) {
      override def findClass(name: String): Class[_] = {
        if (name == className) {
          defineClass(null, bytes, 0, bytes.length)
        } else {
          super.findClass(name)
        }
      }
    }
    val clazz = loader.loadClass(className)
    compiledBlock = clazz.newInstance().asInstanceOf[CompiledBlock]
    compiledBlock
  }
  
  private def generateEffect(mv: MethodVisitor, effect: Effect, className: String): Unit = {
    import org.objectweb.asm.Label
    import org.objectweb.asm.Opcodes._
    
    effect match {
      case Halt =>
        mv.visitInsn(ICONST_M1)
        mv.visitInsn(IRETURN)
      
      case LoadProgram(b, c, written) =>
        // Store written values to registers
        for ((reg, comp) <- written) {
          generateComputation(mv, comp)
          mv.visitVarInsn(ALOAD, 1)
          mv.visitIntInsn(BIPUSH, reg)
          mv.visitInsn(SWAP)
          mv.visitInsn(IASTORE)
        }
        // Call MachineState.loadProgram(b_value)
        generateComputation(mv, b)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "loadProgram", "(I)V", false)
        // Return c value
        generateComputation(mv, c)
        mv.visitInsn(IRETURN)
      
      case ArrayIndex(a, b, c) =>
        generateComputation(mv, b)
        mv.visitFieldInsn(GETSTATIC, "com/wolfskeep/MachineState", "arrays", "[[I")
        mv.visitInsn(SWAP)
        mv.visitInsn(AALOAD)
        generateComputation(mv, c)
        mv.visitInsn(IALOAD)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, a)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
      
      case ArrayAmendment(a, b, c, finger) =>
        generateComputation(mv, a)
        
        if (a.knownValues.exists(_.isZero)) {
          generateComputation(mv, b)
          generateComputation(mv, c)
          mv.visitLdcInsn(finger)
          mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "amendArray", "(IIII)V", false)
        } else if (a.knownValues.exists(_.notZero)) {
          mv.visitFieldInsn(GETSTATIC, "com/wolfskeep/MachineState", "arrays", "[[I")
          mv.visitInsn(SWAP)
          mv.visitInsn(AALOAD)
          generateComputation(mv, b)
          generateComputation(mv, c)
          mv.visitInsn(IASTORE)
        } else {
          mv.visitInsn(DUP)
          val notZero = new Label()
          val done = new Label()
          mv.visitJumpInsn(IFNE, notZero)
          generateComputation(mv, b)
          generateComputation(mv, c)
          mv.visitLdcInsn(finger)
          mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "amendArray", "(IIII)V", false)
          mv.visitJumpInsn(GOTO, done)
          
          mv.visitLabel(notZero)
          mv.visitFieldInsn(GETSTATIC, "com/wolfskeep/MachineState", "arrays", "[[I")
          mv.visitInsn(SWAP)
          mv.visitInsn(AALOAD)
          generateComputation(mv, b)
          generateComputation(mv, c)
          mv.visitInsn(IASTORE)
          
          mv.visitLabel(done)
        }
      
      case Allocation(b, size) =>
        // Call MachineState.allocateArray(size) and store result in register b
        generateComputation(mv, size)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "allocateArray", "(I)I", false)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, b)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
      
      case Abandonment(c) =>
        generateComputation(mv, c)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "abandonArray", "(I)V", false)
      
      case Output(c) =>
        generateComputation(mv, c)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "writeOutput", "(I)V", false)
      
      case Input(c) =>
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "readInput", "()I", false)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, c)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
    }
  }
  
  private def generateComputation(mv: MethodVisitor, comp: Computation): Unit = {
    import org.objectweb.asm.Label
    import org.objectweb.asm.Opcodes._
    
    comp match {
      case Orthography(v) =>
        mv.visitLdcInsn(v)
      
      case RegisterAccess(r) =>
        // Load registers[r]
        mv.visitVarInsn(ALOAD, 1)
        mv.visitIntInsn(BIPUSH, r)
        mv.visitInsn(IALOAD)
      
      case Addition(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IADD)
      
      case Multiplication(b, c) =>
        // Unsigned multiplication
        generateComputation(mv, b)
        mv.visitInsn(I2L)
        mv.visitLdcInsn(0xFFFFFFFFL)
        mv.visitInsn(LAND)
        generateComputation(mv, c)
        mv.visitInsn(I2L)
        mv.visitLdcInsn(0xFFFFFFFFL)
        mv.visitInsn(LAND)
        mv.visitInsn(LMUL)
        mv.visitInsn(L2I)
      
      case Division(b, c) =>
        // Unsigned division
        generateComputation(mv, b)
        mv.visitInsn(I2L)
        mv.visitLdcInsn(0xFFFFFFFFL)
        mv.visitInsn(LAND)
        generateComputation(mv, c)
        mv.visitInsn(I2L)
        mv.visitLdcInsn(0xFFFFFFFFL)
        mv.visitInsn(LAND)
        mv.visitInsn(LDIV)
        mv.visitInsn(L2I)
      
      case Nand(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IAND)
        mv.visitInsn(ICONST_M1)
        mv.visitInsn(IXOR)
      
      case ConditionalMove(a, b, c) =>
        // Optimize based on knownValues
        if (c.knownValues.exists(_.isZero)) {
          generateComputation(mv, a)
        } else if (c.knownValues.exists(_.notZero)) {
          generateComputation(mv, b)
        } else {
          val useA = new Label()
          val done = new Label()
          generateComputation(mv, c)
          mv.visitJumpInsn(IFEQ, useA)
          generateComputation(mv, b)
          mv.visitJumpInsn(GOTO, done)
          mv.visitLabel(useA)
          generateComputation(mv, a)
          mv.visitLabel(done)
        }
      
      case ArrayIndex(a, b, c) =>
        generateComputation(mv, b)
        mv.visitFieldInsn(GETSTATIC, "com/wolfskeep/MachineState", "arrays", "[[I")
        mv.visitInsn(SWAP)
        mv.visitInsn(AALOAD)
        generateComputation(mv, c)
        mv.visitInsn(IALOAD)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, a)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
      
      case Allocation(b, size) =>
        generateComputation(mv, size)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "allocateArray", "(I)I", false)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, b)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
      
      case Input(c) =>
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "readInput", "()I", false)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, c)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
    }
  }
}

trait CompiledBlock {
  def start: Int
  def end: Int
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
            effects = effects :+ ArrayIndex(a, regs(b), regs(c))
            regs(a) = RegisterAccess(a)
            touched = touched + a
            pos += 1
          case 2 =>
            effects = effects :+ ArrayAmendment(regs(a), regs(b), regs(c), pos + 1)
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
            effects = effects :+ Allocation(b, regs(c))
            regs(b) = RegisterAccess(b)
            touched = touched + b
            pos += 1
          case 9 =>
            effects = effects :+ Abandonment(regs(c))
            pos += 1
          case 10 =>
            effects = effects :+ Output(regs(c))
            pos += 1
          case 11 =>
            effects = effects :+ Input(c)
            regs(c) = RegisterAccess(c)
            touched = touched + c
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
