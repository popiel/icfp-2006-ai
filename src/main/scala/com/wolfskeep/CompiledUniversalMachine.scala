package com.wolfskeep

import java.io.{InputStream, OutputStream}

class CompiledUniversalMachine(
  program: Array[Int],
  input: InputStream,
  output: OutputStream
) {
  def run(): Unit = {
    MachineState.initialize(program, input, output)
    val registers = new Array[Int](8)
    var finger = 0
    while (true) {
      val result = try {
        MachineState.run(finger, registers)
      } catch {
        case _: SelfModifyingCodeException =>
          MachineState.compiledBlocks.remove(finger)
          MachineState.run(finger, registers)
      }
      if (result == -1) return
      finger = result
    }
  }
}

object CompiledUniversalMachine {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: CompiledUniversalMachine <program file>")
      sys.exit(1)
    }
    val program = UniversalMachine.readProgram(args(0))
    new CompiledUniversalMachine(program, System.in, System.out).run()
  }
}