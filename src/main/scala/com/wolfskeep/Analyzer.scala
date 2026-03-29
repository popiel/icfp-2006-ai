package com.wolfskeep

import scala.collection.immutable.SortedMap
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes, Label}
import org.objectweb.asm.Opcodes._
import Instruction._

sealed trait PossibleValues {
  def isZero: Boolean
  def notZero: Boolean
  def maybeZero = !isZero && !notZero
  def toFiniteList: Option[List[Int]]
}
case class PossibleValuesSet(values: Set[Int]) extends PossibleValues {
  def isZero = values == Set(0)
  def notZero = !values.contains(0)
  def toFiniteList: Option[List[Int]] = if (values.size < 10) Some(values.toList) else None
}
case class PossibleValuesRange(range: Range) extends PossibleValues {
  def isZero = range == (0 to 0)
  def notZero = !range.contains(0)
  def toFiniteList: Option[List[Int]] = if (range.length < 10) Some(range.toList) else None
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
    registersAfter: Array[Computation]
  ) extends Effect
  case class Jump(c: Computation, registersAfter: Array[Computation]) extends Effect
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

case class Span(
  start: Int,
  end: Int,
  initialRegisters: Array[Computation],
  effects: List[Effect]
) {
  def isUsableFor(regs: Array[Computation]): Boolean = {
    (0 until 8).forall { i =>
      val spanComp = initialRegisters(i)
      val jumpComp = regs(i)
      spanComp == jumpComp || spanComp.knownValues.isEmpty
    }
  }
  
  def knownValuesAt(i: Int): Option[PossibleValues] = initialRegisters(i).knownValues
}

case class Block(
  entry: Int,
  spans: List[Span]
) {
  require(spans.nonEmpty, "Block must have at least one span")
  
  private var compiledBlock: CompiledBlock = _
  
  def start: Int = spans.map(_.start).min
  def end: Int = spans.map(_.end).max
  
  def compile(): CompiledBlock = synchronized {
    if (compiledBlock != null) return compiledBlock
    compiledBlock = doCompile()
    compiledBlock
  }
  
  private def doCompile(): CompiledBlock = {
    import org.objectweb.asm.{ClassWriter, MethodVisitor, Label}
    import org.objectweb.asm.Opcodes._
    
    val seqNum = Block.nextSeqNum()
    val className = s"CompiledBlock_${entry}_${end}_$seqNum"
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    
    cw.visit(V1_8, ACC_PUBLIC, className, null, "java/lang/Object", Array("com/wolfskeep/CompiledBlock"))
    
    // Add fields for start and end
    cw.visitField(ACC_PUBLIC, "start", "I", null, null).visitEnd()
    cw.visitField(ACC_PUBLIC, "end", "I", null, null).visitEnd()
    
    // Constructor
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
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
    val mvStart = cw.visitMethod(ACC_PUBLIC, "start", "()I", null, null)
    mvStart.visitCode()
    mvStart.visitVarInsn(ALOAD, 0)
    mvStart.visitFieldInsn(GETFIELD, className, "start", "I")
    mvStart.visitInsn(IRETURN)
    mvStart.visitMaxs(0, 0)
    mvStart.visitEnd()
    
    // end() getter method
    val mvEnd = cw.visitMethod(ACC_PUBLIC, "end", "()I", null, null)
    mvEnd.visitCode()
    mvEnd.visitVarInsn(ALOAD, 0)
    mvEnd.visitFieldInsn(GETFIELD, className, "end", "I")
    mvEnd.visitInsn(IRETURN)
    mvEnd.visitMaxs(0, 0)
    mvEnd.visitEnd()
    
    // Find entry span (start == entry and knownValues are all None/RegisterAccess)
    val entrySpan = spans.find(_.start == entry).getOrElse(spans.head)
    
    // Create labels for each span
    val spanLabels = spans.map(span => span -> new Label()).toMap
    
    // Generate run method
    val mvRun = cw.visitMethod(ACC_PUBLIC, "run", "([I)I", null, null)
    mvRun.visitCode()
    
    // Jump to entry span
    mvRun.visitJumpInsn(GOTO, spanLabels(entrySpan))
    
    // Generate bytecode for each span
    spans.foreach { span =>
      val label = spanLabels(span)
      mvRun.visitLabel(label)
      generateSpanBytecode(mvRun, span, spanLabels, className)
    }
    
    // Default: return -1 (should not reach here)
    mvRun.visitInsn(ICONST_M1)
    mvRun.visitInsn(IRETURN)
    mvRun.visitMaxs(0, 0)
    mvRun.visitEnd()
    
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
    clazz.newInstance().asInstanceOf[CompiledBlock]
  }
  
  private def generateSpanBytecode(
    mv: MethodVisitor, 
    span: Span, 
    spanLabels: Map[Span, Label],
    className: String
  ): Unit = {
    import org.objectweb.asm.Opcodes._
    
    // Generate effects for this span
    span.effects.foreach { effect =>
      generateEffect(mv, effect, className, spanLabels)
    }
  }
  
  private def generateEffect(
    mv: MethodVisitor, 
    effect: Effect, 
    className: String,
    spanLabels: Map[Span, Label]
  ): Unit = {
    import org.objectweb.asm.Opcodes._
    
    effect match {
      case Halt =>
        mv.visitInsn(ICONST_M1)
        mv.visitInsn(IRETURN)
      
      case Jump(c, regsAfter) =>
        c.knownValues.flatMap(_.toFiniteList) match {
          case None =>
            throw new IllegalStateException(s"Jump with too many or unknown targets at compile time")
          case Some(targets) if targets.size >= 10 =>
            throw new IllegalStateException(s"Jump with ${targets.size} targets at compile time (should be LoadProgram)")
          case Some(targets) =>
            if (targets.size == 1) {
              // Single target: unconditional jump
              val targetValue = targets.head
              val targetSpan = findSpanForTarget(targetValue, regsAfter)
                .getOrElse(throw new IllegalStateException(s"No span found for target $targetValue"))
              syncRegisters(mv, regsAfter, targetSpan, className)
              mv.visitJumpInsn(GOTO, spanLabels(targetSpan))
            } else {
              // Multiple targets: compare and jump
              targets.foreach { targetValue =>
                val targetSpan = findSpanForTarget(targetValue, regsAfter)
                  .getOrElse(throw new IllegalStateException(s"No span found for target $targetValue"))
                generateComputation(mv, c)
                mv.visitLdcInsn(targetValue)
                val skipLabel = new Label()
                mv.visitJumpInsn(IF_ICMPNE, skipLabel)
                // Sync registers before jumping
                syncRegisters(mv, regsAfter, targetSpan, className)
                mv.visitJumpInsn(GOTO, spanLabels(targetSpan))
                mv.visitLabel(skipLabel)
              }
              // No matching target - should not happen at runtime, return error
              mv.visitInsn(ICONST_M1)
              mv.visitInsn(IRETURN)
            }
        }
      
      case LoadProgram(b, c, regsAfter) =>
        // Store written values to registers
        for (i <- 0 until 8) {
          val comp = regsAfter(i)
          if (!comp.isInstanceOf[RegisterAccess] || comp.asInstanceOf[RegisterAccess].r != i) {
            generateComputation(mv, comp)
            mv.visitVarInsn(ALOAD, 1)
            mv.visitInsn(SWAP)
            mv.visitIntInsn(BIPUSH, i)
            mv.visitInsn(SWAP)
            mv.visitInsn(IASTORE)
          }
        }
        generateComputation(mv, b)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "loadProgram", "(I)V", false)
        generateComputation(mv, c)
        mv.visitInsn(IRETURN)
      
      // ... other effects remain the same
      case ArrayIndex(a, b, c) =>
        generateComputation(mv, b)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "arrays", "()[[I", false)
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
          mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "arrays", "()[[I", false)
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
          mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "arrays", "()[[I", false)
          mv.visitInsn(SWAP)
          mv.visitInsn(AALOAD)
          generateComputation(mv, b)
          generateComputation(mv, c)
          mv.visitInsn(IASTORE)
          
          mv.visitLabel(done)
        }
      
      case Allocation(b, size) =>
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
  
  private def findSpanForTarget(target: Int, regsAfter: Array[Computation]): Option[Span] = {
    spans.find { span =>
      span.start == target && span.isUsableFor(regsAfter)
    }
  }
  
  private def syncRegisters(
    mv: MethodVisitor, 
    regsAfter: Array[Computation], 
    targetSpan: Span,
    className: String
  ): Unit = {
    import org.objectweb.asm.Opcodes._
    
    // For each register, if targetSpan has RegisterAccess for that register,
    // we need to write the value from regsAfter
    for (i <- 0 until 8) {
      val targetComp = targetSpan.initialRegisters(i)
      if (targetComp.knownValues.isEmpty) {
        // Target expects RegisterAccess, we need to write the value
        generateComputation(mv, regsAfter(i))
        mv.visitVarInsn(ALOAD, 1)
        mv.visitInsn(SWAP)
        mv.visitIntInsn(BIPUSH, i)
        mv.visitInsn(SWAP)
        mv.visitInsn(IASTORE)
      }
    }
  }
  
  private def generateComputation(mv: MethodVisitor, comp: Computation): Unit = {
    import org.objectweb.asm.Opcodes._
    
    comp match {
      case Orthography(v) =>
        mv.visitLdcInsn(v)
      
      case RegisterAccess(r) =>
        mv.visitVarInsn(ALOAD, 1)
        mv.visitIntInsn(BIPUSH, r)
        mv.visitInsn(IALOAD)
      
      case Addition(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IADD)
      
      case Multiplication(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IMUL)
      
      case Division(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IDIV)
      
      case Nand(b, c) =>
        generateComputation(mv, b)
        generateComputation(mv, c)
        mv.visitInsn(IAND)
        mv.visitInsn(ICONST_M1)
        mv.visitInsn(IXOR)
      
      case ConditionalMove(a, b, c) =>
        if (c.knownValues.isZero) {
          // Condition is always false, just return a
          generateComputation(mv, a)
        } else if (c.knownValues.notZero) {
          // Condition is always true, just return b
          generateComputation(mv, b)
        } else {
          // Unknown condition, need runtime check
          generateComputation(mv, c)
          val skipMove = new Label()
          mv.visitJumpInsn(IFEQ, skipMove)
          generateComputation(mv, b)
          mv.visitVarInsn(ALOAD, 1)
          mv.visitInsn(SWAP)
          mv.visitIntInsn(BIPUSH, a match {
            case RegisterAccess(r) => r
            case _ => throw new IllegalStateException(s"ConditionalMove target must be RegisterAccess, got $a")
          })
          mv.visitInsn(SWAP)
          mv.visitInsn(IASTORE)
          mv.visitLabel(skipMove)
        }
      
      case ArrayIndex(a, b, c) =>
        generateComputation(mv, b)
        mv.visitMethodInsn(INVOKESTATIC, "com/wolfskeep/MachineState", "arrays", "()[[I", false)
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
        // Input as Computation reads from the register (the effect already stored the input value)
        mv.visitVarInsn(ALOAD, 1)
        mv.visitIntInsn(BIPUSH, c)
        mv.visitInsn(IALOAD)
    }
  }
}

trait CompiledBlock {
  def start: Int
  def end: Int
  def run(registers: Array[Int]): Int
}

class Analyzer(val prog: Array[Int]) {
  def from(entry: Int, initialRegisters: Array[Computation]): Block = {
    import scala.collection.mutable
    
    val spans = mutable.ListBuffer[Span]()
    val worklist = mutable.Queue[(Int, Array[Computation])]()
    worklist.enqueue((entry, initialRegisters))
    
    while (worklist.nonEmpty) {
      val (startPos, regs) = worklist.dequeue()
      
      // Check if a usable span already exists
      spans.find(_.isUsableFor(regs)) match {
        case Some(_) => // Span already covers this, skip
        case None =>
          // Analyze new span
          val (endPos, effects, regsAfter) = analyzeSpan(startPos, regs)
          val newSpan = Span(startPos, endPos, regs, effects)
          spans += newSpan
          
          // Queue follow-up scans for Jump targets
          if (effects.nonEmpty) {
            effects.last match {
              case Jump(c, regsAfterArr) =>
                c.knownValues.flatMap(_.toFiniteList) match {
                  case None => // Shouldn't happen - Jump with too many targets
                  case Some(targets) =>
                    targets.foreach { target =>
                      worklist.enqueue((target, regsAfterArr))
                    }
                }
              case LoadProgram(_, _, _) => // No follow-up
              case Halt => // No follow-up
              case _ => // Other effects - shouldn't be at end
            }
          }
      }
    }
    
    Block(entry, spans.toList)
  }
  
  private def analyzeSpan(finger: Int, registers: Array[Computation]): (Int, List[Effect], Array[Computation]) = {
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
            val regsAfter = Array.tabulate(8)(i => touched match {
              case t if t.contains(i) => regs(i)
              case _ => RegisterAccess(i)
            })
            return (pos, effects :+ Halt, regsAfter)
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
            regs(c) = Input(c)
            touched = touched + c
            pos += 1
          case 12 =>
            val regsAfter = regs.clone()
            if (regs(b).knownValues.exists(_.isZero)) {
              // b is known to be zero - just jump, don't load program
              return (pos, effects :+ Jump(regs(c), regsAfter), regsAfter)
            } else {
              // b is non-zero or unknown - must use LoadProgram
              return (pos, effects :+ LoadProgram(regs(b), regs(c), regsAfter), regsAfter)
            }
          case _ =>
            // Illegal opcode - treat as halt
            val regsAfter = Array.tabulate(8)(i => 
              if (touched.contains(i)) regs(i) else RegisterAccess(i)
            )
            return (pos, effects :+ Halt, regsAfter)
        }
      }
    }
    (prog.length - 1, effects, Array.tabulate(8)(i => RegisterAccess(i)))
  }
}
