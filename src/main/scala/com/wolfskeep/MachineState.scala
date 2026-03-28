package com.wolfskeep

import java.io.{InputStream, OutputStream}
import scala.collection.mutable.{Map => MMap, Stack}
import Instruction._

object MachineState {
  var arrays: Array[Array[Int]] = _
  var availableArrayIds: Stack[Int] = new Stack[Int]()
  var input: InputStream = _
  var output: OutputStream = _
  var nextArrayId: Int = 1
  var compiledBlocks: MMap[Int, CompiledBlock] = MMap.empty
  var currentBlock: CompiledBlock = _
  var analyzer: Analyzer = _
  
  def initialize(prog: Array[Int], in: InputStream, out: OutputStream): Unit = {
    arrays = new Array[Array[Int]](1024)
    arrays(0) = prog
    availableArrayIds.clear()
    input = in
    output = out
    nextArrayId = 1
    compiledBlocks.clear()
    currentBlock = null
    analyzer = new Analyzer(prog)
  }
  
  def allocateArray(size: Int): Int = synchronized {
    val id = if (availableArrayIds.isEmpty) {
      val newId = nextArrayId
      nextArrayId += 1
      if (newId >= arrays.length) {
        val newArrays = new Array[Array[Int]](arrays.length * 2)
        System.arraycopy(arrays, 0, newArrays, 0, arrays.length)
        arrays = newArrays
      }
      newId
    } else {
      availableArrayIds.pop()
    }
    arrays(id) = new Array[Int](size)
    id
  }
  
  def abandonArray(id: Int): Unit = synchronized {
    if (id == 0) {
      throw new IllegalArgumentException("Cannot abandon array 0")
    }
    availableArrayIds.push(id)
    arrays(id) = null
  }
  
  def readInput(): Int = synchronized {
    val read = input.read()
    if (read == -1) {
      0xFFFFFFFF
    } else {
      read & 0xFF
    }
  }
  
  def writeOutput(value: Int): Unit = synchronized {
    if (value > 255) {
      throw new IllegalArgumentException(s"Output value $value exceeds 255")
    }
    output.write(value)
    output.flush()
  }
  
  def amendArray(arrayId: Int, index: Int, value: Int, finger: Int): Unit = synchronized {
    arrays(arrayId)(index) = value
    if (arrayId == 0) {
      val toRemove = compiledBlocks.filter { case (_, block) =>
        index >= block.start && index <= block.end
      }.keys.toList
      for (key <- toRemove) {
        compiledBlocks.remove(key)
      }
      if (currentBlock != null && index >= currentBlock.start && index <= currentBlock.end) {
        throw new SelfModifyingCodeException(finger)
      }
    }
  }
  
  def loadProgram(srcArrayId: Int): Unit = synchronized {
    if (srcArrayId != 0) {
      if (arrays(srcArrayId) == null) {
        throw new NoSuchElementException(s"Load program: array $srcArrayId not active")
      }
      arrays(0) = arrays(srcArrayId).clone()
      compiledBlocks.clear()
      analyzer = new Analyzer(arrays(0))
    }
  }
  
  def run(finger: Int, registers: Array[Int]): Int = synchronized {
    val block = compiledBlocks.getOrElseUpdate(finger, {
      val initialComps = (0 to 7).map(i => RegisterAccess(i): Computation).toArray
      analyzer.from(finger, initialComps).compile()
    })
    currentBlock = block
    try {
      block.run(registers)
    } finally {
      currentBlock = null
    }
  }
}
