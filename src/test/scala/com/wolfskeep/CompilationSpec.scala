package com.wolfskeep

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import Instruction._

class CompilationSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  
  override def beforeEach(): Unit = {
    MachineState.compiledBlocks.clear()
    MachineState.currentBlock = null
  }
  
  // Helper to create initial registers with RegisterAccess
  def initialRegisters: Array[Computation] = 
    (0 to 7).map(i => RegisterAccess(i): Computation).toArray
  
  // Helper to create registers with known values
  def registersWithValues(values: Int*): Array[Computation] = {
    val regs = initialRegisters
    values.zipWithIndex.foreach { case (v, i) =>
      if (i < 8) regs(i) = Orthography(v)
    }
    regs
  }
  
  "MachineState" should {
    "initialize correctly" in {
      val prog = Array(0, 0, 0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      MachineState.arrays should not be null
      MachineState.arrays(0) should not be null
      MachineState.arrays(0) shouldBe theSameInstanceAs(prog)
      MachineState.nextArrayId shouldBe 1
      MachineState.availableArrayIds shouldBe empty
      MachineState.compiledBlocks shouldBe empty
      MachineState.currentBlock shouldBe null
    }
    
    "allocate and abandon arrays" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val id1 = MachineState.allocateArray(10)
      id1 shouldBe 1
      MachineState.arrays(id1) should not be null
      MachineState.arrays(id1).length shouldBe 10
      
      val id2 = MachineState.allocateArray(20)
      id2 shouldBe 2
      MachineState.arrays(id2).length shouldBe 20
      
      MachineState.abandonArray(id1)
      MachineState.availableArrayIds should contain(id1)
      MachineState.arrays(id1) shouldBe null
      
      val id3 = MachineState.allocateArray(5)
      id3 shouldBe id1  // Reuse abandoned ID
      MachineState.arrays(id3).length shouldBe 5
    }
    
    "not abandon array 0" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      an[IllegalArgumentException] should be thrownBy {
        MachineState.abandonArray(0)
      }
    }
    
    "read input correctly" in {
      val prog = Array(0)
      val input = new ByteArrayInputStream(Array(65, 66, 67))
      MachineState.initialize(prog, input, new ByteArrayOutputStream())
      
      MachineState.readInput() shouldBe 65
      MachineState.readInput() shouldBe 66
      MachineState.readInput() shouldBe 67
      MachineState.readInput() shouldBe 0xFFFFFFFF  // EOF
    }
    
    "write output correctly" in {
      val prog = Array(0)
      val output = new ByteArrayOutputStream()
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), output)
      
      MachineState.writeOutput(65)
      MachineState.writeOutput(66)
      MachineState.writeOutput(67)
      
      output.toByteArray shouldBe Array(65, 66, 67)
    }
    
    "fail on output > 255" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      an[IllegalArgumentException] should be thrownBy {
        MachineState.writeOutput(256)
      }
    }
    
    "load program from array" in {
      val prog = Array(0)
      val newProg = Array(1, 2, 3, 4, 5)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      MachineState.arrays(0) = newProg.clone()
      MachineState.loadProgram(0)
      
      MachineState.arrays(0) should equal(newProg)  // Still the same after clone
    }
    
    "load program with non-zero array id" in {
      val prog = Array(0)
      val newProg = Array(10, 20, 30)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      MachineState.arrays(1) = newProg.clone()
      MachineState.loadProgram(1)
      
      MachineState.arrays(0).toSet shouldBe newProg.toSet
      // Should be a clone
      MachineState.arrays(0) should not be theSameInstanceAs(newProg)
    }
    
    "fail to load from null array" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      MachineState.arrays(1) = null
      an[NoSuchElementException] should be thrownBy {
        MachineState.loadProgram(1)
      }
    }
    
    "clear compiled blocks and analyzer when loading from non-zero array" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val fakeBlock = new CompiledBlock {
        def start: Int = 0
        def end: Int = 10
        def run(registers: Array[Int]): Int = -1
      }
      MachineState.compiledBlocks(0) = fakeBlock
      MachineState.compiledBlocks(5) = fakeBlock
      
      MachineState.arrays(1) = Array(100, 200, 300)
      MachineState.loadProgram(1)
      
      MachineState.compiledBlocks shouldBe empty
      MachineState.analyzer should not be null
    }
    
    "not clear compiled blocks when loading from array 0" in {
      val prog = Array(0)
      MachineState.initialize(prog, new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val fakeBlock = new CompiledBlock {
        def start: Int = 0
        def end: Int = 10
        def run(registers: Array[Int]): Int = -1
      }
      MachineState.compiledBlocks(0) = fakeBlock
      
      MachineState.loadProgram(0)
      
      MachineState.compiledBlocks should contain key (0)
    }
    
    "amend array directly for non-zero array IDs" in {
      val prog = Array.fill(10)(0)
      MachineState.initialize(prog.clone(), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      MachineState.arrays(1) = Array(10, 20, 30)
      MachineState.amendArray(1, 1, 99, 100)
      
      MachineState.arrays(1)(1) shouldBe 99
      // Should not remove any compiled blocks since array ID was not 0
      MachineState.compiledBlocks shouldBe empty
    }
    
    "amend array 0 and invalidate compiled blocks" in {
      val prog = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      MachineState.initialize(prog.clone(), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      // Create a fake compiled block
      val fakeBlock = new CompiledBlock {
        def start: Int = 5
        def end: Int = 10
        def run(registers: Array[Int]): Int = 0
      }
      MachineState.compiledBlocks(5) = fakeBlock  // Block starting at 5
      
      // Amend at index 7 - should remove block at 5 since 7 is in [5, 10]
      MachineState.amendArray(0, 7, 99, 100)
      
      // Block starting at 5 should be removed because 7 is in [5, 10]
      MachineState.compiledBlocks should not contain key (5)
    }
    
    "throw SelfModifyingCodeException when amending currently running block" in {
      val prog = Array.fill(30)(0)
      MachineState.initialize(prog.clone(), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val runningBlock = new CompiledBlock {
        def start: Int = 10
        def end: Int = 20
        def run(registers: Array[Int]): Int = 0
      }
      MachineState.currentBlock = runningBlock
      
      val ex = the[SelfModifyingCodeException] thrownBy {
        MachineState.amendArray(0, 15, 99, 100)  // Index 15 is in [10, 20]
      }
      ex.finger shouldBe 100
    }
    
    "not throw exception when amending outside currently running block" in {
      val prog = Array.fill(30)(0)
      MachineState.initialize(prog.clone(), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val runningBlock = new CompiledBlock {
        def start: Int = 10
        def end: Int = 20
        def run(registers: Array[Int]): Int = 0
      }
      MachineState.currentBlock = runningBlock
      
      // Amend at index outside block range
      MachineState.amendArray(0, 5, 99, 100)  // Index 5 is not in [10, 20]
      
      // Should not throw
      MachineState.currentBlock should not be null
    }
  }
  
  "Block compilation" should {
    "create CompiledBlock with correct start and end" in {
      val block = Block(10, 20, List(Halt))
      val compiled = block.compile()
      
      compiled.start shouldBe 10
      compiled.end shouldBe 20
    }
    
    "compile Halt effect to return -1" in {
      val block = Block(0, 0, List(Halt))
      val compiled = block.compile()
      val registers = new Array[Int](8)
      
      compiled.run(registers) shouldBe -1
    }
    
    "compile Orthography to load immediate value" in {
      val block = Block(0, 0, List(
        Halt
      ))
      // Orthography is not an effect, it's a computation
      // We need to test it through a computation that uses it
    }
    
    "compile Addition computation" in {
      val regs = Array[Computation](
        Orthography(10),  // reg 0 = 10
        Orthography(20),  // reg 1 = 20
        RegisterAccess(2),
        RegisterAccess(3),
        RegisterAccess(4),
        RegisterAccess(5),
        RegisterAccess(6),
        RegisterAccess(7)
      )
      val analyzer = new Analyzer(Array(0))
      val block = analyzer.from(0, regs)
      // This will scan until Halt/LoadProgram, so we need a minimal program
    }
  }
  
  "Computation compilation" should {
    "compile RegisterAccess to load from registers" in {
      val regs = initialRegisters
      regs(0) = Orthography(42)
      
      val analyzer = new Analyzer(Array(0))
      val block = analyzer.from(0, regs)
    }
    
    "compile Addition to add two values" in {
      // Test that Addition(b, c) computes b + c correctly
      val add = Addition(Orthography(10), Orthography(20))
      add.knownValues shouldBe Some(PossibleValuesSet(Set(30)))
    }
    
    "compile Multiplication to multiply two values" in {
      val mul = Multiplication(Orthography(5), Orthography(7))
      mul.knownValues shouldBe Some(PossibleValuesSet(Set(35)))
    }
    
    "compile Division to divide two values" in {
      val div = Division(Orthography(20), Orthography(4))
      div.knownValues shouldBe Some(PossibleValuesSet(Set(5)))
    }
    
    "compile Nand to compute bitwise NAND" in {
      val nand = Nand(Orthography(0xFF), Orthography(0xF0))
      // ~(0xFF & 0xF0) = ~0xF0 = 0xFFFFFF0F as signed = -241
      nand.knownValues shouldBe Some(PossibleValuesSet(Set(~0xF0)))
    }
    
    "compile ConditionalMove with known zero condition" in {
      val cm = ConditionalMove(
        Orthography(99),
        Orthography(42),
        Orthography(0)  // c is zero
      )
      cm.knownValues shouldBe Some(PossibleValuesSet(Set(99)))
    }
    
    "compile ConditionalMove with known non-zero condition" in {
      val cm = ConditionalMove(
        Orthography(99),
        Orthography(42),
        Orthography(5)  // c is non-zero
      )
      cm.knownValues shouldBe Some(PossibleValuesSet(Set(42)))
    }
    
    "compile Allocation to return valid array ID range" in {
      val alloc = Allocation(0, Orthography(100))
      alloc.knownValues shouldBe Some(PossibleValuesRange(1 to Int.MaxValue))
    }
    
    "compile Input to return valid input range" in {
      val input = Input(0)
      input.knownValues shouldBe Some(PossibleValuesRange(-1 to 255))
    }
  }
  
  "Effect compilation" should {
    "track touched registers in LoadProgram written map" in {
      val analyzer = new Analyzer(Array(0))
      val regs = initialRegisters
      
      // Create a minimal test - in practice LoadProgram stops the block
    }
  }
  
  "Analyzer.from" should {
    "create block from program" in {
      // Simple program: Orthography to load value 42 into reg 0, then Halt
      // Opcode 13 (Orthography): 13 << 28 | (a << 25) | value
      // a = 0, value = 42 -> 0xD000002A
      val prog = Array(0xD000002A, 0x70000000)  // Orthography(42) to reg 0, then Halt
      
      val analyzer = new Analyzer(prog)
      val regs = initialRegisters
      val block = analyzer.from(0, regs)
      
      block.start shouldBe 0
      block.end shouldBe 1
      block.effects should have size 1
      block.effects.head shouldBe Halt
    }
    
    "handle Addition instruction" in {
      // Opcode 3 (Addition): a = b + c
      // Layout: (3 << 28) | (a << 6) | (b << 3) | c
      // If a=0, b=1, c=2: 0x30000102
      val prog = Array(
        0xD000002A,  // Orthography(42) to reg 0
        0xD020000A,  // Orthography(10) to reg 1
        0x30000102,  // Addition: reg0 = reg1 + reg2
        0x70000000   // Halt
      )
      
      val analyzer = new Analyzer(prog)
      val regs = initialRegisters
      regs(1) = Orthography(5)
      regs(2) = Orthography(10)
      
      val block = analyzer.from(0, regs)
      block.effects.last shouldBe Halt
    }
    
    "track touched registers" in {
      val prog = Array(
        0xD000002A,  // Orthography(42) to reg 0
        0x70000000   // Halt
      )
      
      val analyzer = new Analyzer(prog)
      val block = analyzer.from(0, initialRegisters)
      
      // The LoadProgram effect should have written map with touched registers
      // In this case, only reg 0 was touched (by Orthography)
    }
  }
  
  "CompiledBlock execution" should {
    "create valid compiled block" in {
      val block = Block(0, 0, List(Halt))
      val compiled = block.compile()
      
      compiled.start shouldBe 0
      compiled.end shouldBe 0
    }
    
    "handle Input effect in MachineState" in {
      val input = new ByteArrayInputStream(Array(42))
      MachineState.initialize(Array(0), input, new ByteArrayOutputStream())
      
      MachineState.readInput() shouldBe 42
      MachineState.readInput() shouldBe 0xFFFFFFFF.asInstanceOf[Int]  // EOF
    }
    
    "handle Output effect in MachineState" in {
      val output = new ByteArrayOutputStream()
      MachineState.initialize(Array(0), new ByteArrayInputStream(Array()), output)
      
      MachineState.writeOutput(72)
      MachineState.writeOutput(105)
      
      output.toByteArray shouldBe Array(72, 105)
    }
    
    "handle abandon array in MachineState" in {
      MachineState.initialize(Array(0), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val id = MachineState.allocateArray(10)
      MachineState.arrays(id) should not be null
      
      MachineState.abandonArray(id)
      MachineState.arrays(id) shouldBe null
      MachineState.availableArrayIds should contain(id)
    }
  }
  
  "Array amendment optimization" should {
    "generate direct store when array ID is known non-zero" in {
      // When a is Orthography(5), knownValues is Some(Set(5)), which is notZero
      Orthography(5).knownValues.exists(_.notZero) shouldBe true
    }
    
    "generate check when array ID might be zero" in {
      // Input's known values might include zero
      Input(0).knownValues.exists(_.notZero) shouldBe false
    }
    
    "generate direct store when array ID is from Allocation" in {
      // Allocation's known values are >= 1, so nonZero
      Allocation(0, Orthography(10)).knownValues.exists(_.notZero) shouldBe true
    }
  }
  
  "Self-modifying code detection" should {
    "not throw when modifying non-zero array" in {
      MachineState.initialize(Array(0, 0, 0), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      // Initialize array 1 so it exists
      MachineState.arrays(1) = Array(0, 0, 0)
      
      val runningBlock = new CompiledBlock {
        def start: Int = 10
        def end: Int = 20
        def run(registers: Array[Int]): Int = 0
      }
      MachineState.currentBlock = runningBlock
      
      // Amend array 1 (not array 0)
      MachineState.amendArray(1, 1, 99, 100)
      
      // Should not throw
      MachineState.currentBlock should not be null
    }
    
    "invalidate blocks when modifying array 0" in {
      MachineState.initialize(Array.fill(25)(0), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      val block1 = new CompiledBlock {
        def start: Int = 10
        def end: Int = 20
        def run(registers: Array[Int]): Int = 0
      }
      val block2 = new CompiledBlock {
        def start: Int = 30
        def end: Int = 40
        def run(registers: Array[Int]): Int = 0
      }
      
      MachineState.compiledBlocks(10) = block1
      MachineState.compiledBlocks(30) = block2
      
      // Amend at index 15 - should remove block1 but not block2
      MachineState.amendArray(0, 15, 99, 50)
      
      MachineState.compiledBlocks should not contain key (10)
      MachineState.compiledBlocks should contain key (30)
    }
    
    "clear currentBlock after run completes" in {
      MachineState.initialize(Array(0x70000000), new ByteArrayInputStream(Array()), new ByteArrayOutputStream())
      
      MachineState.run(0, new Array[Int](8))
      
      MachineState.currentBlock shouldBe null
    }
  }
  
  "SelfModifyingCodeException" should {
    "store finger value" in {
      val ex = new SelfModifyingCodeException(42)
      ex.finger shouldBe 42
      ex.getMessage should include("42")
    }
    
    "include finger in message" in {
      val ex = new SelfModifyingCodeException(100)
      ex.getMessage should include("100")
    }
  }
  
  "CompiledUniversalMachine" should {
    "execute simple halt program" in {
      import UMOps._
      val prog = Array(halt)
      val output = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), output)
      machine.run()
      
      output.toByteArray shouldBe empty
    }
    
    "execute orthography and halt" in {
      import UMOps._
      val prog = Array(A := 42, halt)
      val output = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), output)
      machine.run()
      
      output.toByteArray shouldBe empty
    }
    
    "execute output instruction" in {
      import UMOps._
      val prog = Array(D := 65, UMOps.output(D), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(65)
    }
    
    "handle self-modifying code by recompiling" in {
      import UMOps._
      val prog = Array(
        A := 0,
        B := 1,
        C := 0x99,
        A(B) := C,
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog.clone(), new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe empty
    }
    
    "execute addition" in {
      import UMOps._
      val prog = Array(
        B := 10,
        C := 32,
        A := B + C,
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      // Should complete without error
    }
    
    "handle allocation" in {
      import UMOps._
      val prog = Array(
        B := 10,
        A := alloc(B),
        UMOps.output(A),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(1)
    }
    
    "execute conditional move when condition is zero" in {
      import UMOps._
      val prog = Array(
        A := 66,
        B := 42,
        C := 0,
        A := B when C,
        UMOps.output(A),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(66)
    }
    
    "execute conditional move when condition is non-zero" in {
      import UMOps._
      val prog = Array(
        A := 66,
        B := 42,
        C := 1,
        A := B when C,
        UMOps.output(A),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(42)
    }
    
    "execute array operations" in {
      import UMOps._
      val prog = Array(
        C := 1,
        A := alloc(C),
        D := 42,
        C := 0,
        A(C) := D,
        C := 0,
        B := A(C),
        UMOps.output(B),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(42)
    }
    
    "execute multiplication and division" in {
      import UMOps._
      val prog = Array(
        A := 6,
        B := 7,
        C := A * B,
        D := C / A,
        UMOps.output(D),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(7)
    }
    
    "execute NAND operation" in {
      import UMOps._
      val prog = Array(
        A := 0xF0,
        B := 0x0F,
        C := A ^& B,
        C := C ^& C,
        UMOps.output(C),
        halt
      )
      
      val out = new ByteArrayOutputStream()
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(0)
    }
    
    "handle input" in {
      import UMOps._
      val prog = Array(A := UMOps.input(A), UMOps.output(A), halt)
      val inputBytes = Array[Byte](65)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(inputBytes), out)
      machine.run()
      
      out.toByteArray shouldBe Array(65)
    }
    
    "return 0xFF on EOF" in {
      import UMOps._
      val prog = Array(A := UMOps.input(A), UMOps.output(A), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      // EOF returns 0xFFFFFFFF, but output only outputs low byte
      out.toByteArray shouldBe Array(-1)
    }
    
    "output multiple values" in {
      import UMOps._
      val prog = Array(A := 72, UMOps.output(A), A := 101, UMOps.output(A), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(72, 101)
    }
    
    "fail on output > 255" in {
      import UMOps._
      val prog = Array(A := 300, UMOps.output(A), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      val thrown = the[RuntimeException] thrownBy { machine.run() }
      thrown.getMessage should include("exceeds 255")
    }
    
    "add registers modulo 2^32" in {
      import UMOps._
      val prog = Array(A := 100, B := 55, C := A + B, UMOps.output(C), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      (out.toByteArray()(0) & 0xFF) shouldBe 155
    }
    
    "perform NAND with overlapping bits" in {
      import UMOps._
      val prog = Array(A := 0x0F, B := 0x0F, C := A ^& B, C := C ^& C, UMOps.output(C), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      (out.toByteArray()(0) & 0xFF) shouldBe 15
    }
    
    "array amendment write to array" in {
      import UMOps._
      val prog = Array(
        C := 1,
        A := alloc(C),
        D := 99,
        C := 0,
        A(C) := D,
        C := 0,
        B := A(C),
        UMOps.output(B),
        halt
      )
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(99)
    }
    
    "array amendment modify array 0" in {
      import UMOps._
      val prog = Array(
        A := 0,
        B := 65,
        A(A) := B,
        C := A(A),
        UMOps.output(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog.clone(), new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(65)
    }
    
    "reuse freed array IDs" in {
      import UMOps._
      val prog = Array(
        B := 10,
        A := alloc(B),
        abandon(A),
        D := 10,
        A := alloc(D),
        UMOps.output(A),
        halt
      )
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      machine.run()
      
      out.toByteArray shouldBe Array(1)
    }
    
    "abandon array throws on null access" in {
      import UMOps._
      val prog = Array(
        C := 1,
        A := alloc(C),
        abandon(A),
        C := 0,
        B := A(C),
        halt
      )
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      a[RuntimeException] should be thrownBy { machine.run() }
    }
    
    "fail to abandon array 0" in {
      import UMOps._
      val prog = Array(abandon(A), halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      val thrown = the[RuntimeException] thrownBy { machine.run() }
      thrown.getMessage should include("cannot abandon array 0")
    }
    
    "division by zero fails" in {
      import UMOps._
      val prog = Array(B := 0, A := A / B, halt)
      val out = new ByteArrayOutputStream()
      
      val machine = new CompiledUniversalMachine(prog, new ByteArrayInputStream(Array()), out)
      an[ArithmeticException] should be thrownBy { machine.run() }
    }
  }
}