program HopperBASIC
{
    #define CPU_65C02S
    #define ROM_8K
    
    #define HOPPER_BASIC
    
    //#define SMALLCODE // optimize for size: no jump tables for switches
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Tools"
    uses "Tokenizer"
    uses "FunctionManager"
    uses "BytecodeCompiler"
    uses "BytecodeExecutor"
    uses "Interpreter"
    
    const string Welcome = "\nHopper BASIC v1.0\n";
    const string MemoryMsg = "Memory: ";
    const string KBytes = " bytes available\n";
    const string TestMsg = "\nTesting bytecode system...\n";
    const string TestResult = "Test complete!\n";
    
    printAvailableMemory()
    {
        // Use the runtime's Memory.Available() function
        Memory.Available();  // Pushes available memory (UInt) to stack
        
        // Pop the typed value - Memory.Available() returns UInt type
        Stacks.PopTop();  // This properly handles the type and puts value in TOP
        
        // Print the decimal value
        Tools.PrintDecimalWord();
    }
    
    initializeSystem()
    {
        // Initialize the complete runtime system
        
        // Set up program size as minimal (just our BASIC interpreter)
        STZ ZP.PROGSIZE  // empty for now (not Hopper program)
        
        // Initialize memory heap using runtime function
        Memory.InitializeHeapSize();
        
        // Initialize stacks
        Stacks.Initialize();
        
        // Clear flags and set up basic state
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS  // Program loaded flag
    }
    
    // Quick test of our bytecode system
    testBytecodeSystem()
    {
        LDA #(TestMsg % 256)
        STA ZP.IDXL
        LDA #(TestMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Initialize our subsystems
        FunctionManager.Initialize();
        
        // Simulate "PRINT 42" by directly using our APIs
        FunctionManager.startREPLCompilation();
        
        // Emit: OpLoadConst, 42, 0
        LDA #BytecodeCompiler.Opcodes.OpLoadConst
        STA ZP.NEXTL
        STZ ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.emitByte();
        
        LDA #42  // Test value
        STA ZP.TOPL
        STZ ZP.TOPH
        LDA #Types.UInt
        Stacks.PushTop();
        FunctionManager.emitWord();
        
        // Emit: OpPrintInt
        LDA #BytecodeCompiler.Opcodes.OpPrintInt
        STA ZP.NEXTL
        STZ ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.emitByte();
        
        // Emit: OpPrintNL
        LDA #BytecodeCompiler.Opcodes.OpPrintNL
        STA ZP.NEXTL
        STZ ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.emitByte();
        
        // Emit: OpHalt
        LDA #BytecodeCompiler.Opcodes.OpHalt
        STA ZP.NEXTL
        STZ ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        FunctionManager.emitByte();
        
        FunctionManager.finishREPLCompilation();
        
        // Execute the bytecode
        BytecodeExecutor.executeREPLFunction();
        
        // Clean up
        FunctionManager.cleanupREPLFunction();
        
        LDA #(TestResult % 256)
        STA ZP.IDXL
        LDA #(TestResult / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
    
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        INC ZP.SerialBreakFlag
    }
    
    Hopper()
    {
        SEI  // Disable interrupts during initialization
        
        // Initialize serial first
        Serial.Initialize();
        
        // Initialize the complete system using runtime functions
        initializeSystem();
        
        CLI  // Re-enable interrupts
        
        // Print welcome message
        LDA #(Welcome % 256)
        STA ZP.IDXL
        LDA #(Welcome / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Print memory information
        LDA #(MemoryMsg % 256)
        STA ZP.IDXL
        LDA #(MemoryMsg / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Print available memory using runtime function
        printAvailableMemory();
        
        LDA #(KBytes % 256)
        STA ZP.IDXL
        LDA #(KBytes / 256)
        STA ZP.IDXH
        Tools.PrintString();
        
        // Test our bytecode system before starting REPL
        testBytecodeSystem();
        
        // Start the BASIC interpreter
        Interpreter.Run();
    }
}
