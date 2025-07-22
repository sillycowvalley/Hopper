unit FunctionManager
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    
    // Function memory map:
    //   0000 heap allocator size
    //   xxxx functionID (0 = never assigned)
    //   xxxxxxxxxxxxxxxxxxxxxxxxxxxx name (14 bytes: null-terminated, 13 chars max)
    //   xxxx pNext (next function pointer)
    //   xxxx codeSize (bytecode size)
    //   xxxx... bytecode follows
    
    const uint fhID = 0;           // 2 bytes: function ID
    const uint fhName = 2;         // 14 bytes: function name
    const uint fhNext = 16;        // 2 bytes: next function pointer  
    const uint fhCodeSize = 18;    // 2 bytes: bytecode size
    const uint fhHeaderSize = 20;  // Total header size
    
    // Special function IDs
    const uint ReplFunctionID = 0xFFFF;
    const uint MainFunctionID = 0x0001;
    
    // ZeroPage variables for function management - using proper ZP constants
    const byte funcListHead     = ZP.FuncListHead;    // Head of function linked list
    const byte funcListHeadHi   = ZP.FuncListHeadHi;  // High byte
    const byte funcCount        = ZP.FuncCount;       // Number of functions (0-255)
    const byte currentFunc      = ZP.CurrentFunc;     // Current function being compiled
    const byte currentFuncHi    = ZP.CurrentFuncHi;   // High byte
    
    // Compilation state - using proper ZP constants
    const byte compileState     = ZP.CompileState;    // 0=none, 1=compiling, 2=complete
    const byte tempBlock        = ZP.FSOURCEADDRESSL; // Temporary compilation block (16-bit)
    const byte tempBlockHi      = ZP.FSOURCEADDRESSH; // High byte
    const byte writePos         = ZP.WritePosLo;      // Current write position in temp block
    const byte writePosHi       = ZP.WritePosHi;      // High byte
    
    // Need additional ZP slots for bytecode size - using available HOPPER_BASIC area
    const byte bytecodeSize     = ZP.LCOUNTL;         // Size of compiled bytecode
    const byte bytecodeSizeHi   = ZP.LCOUNTH;         // High byte
    
    Initialize()
    {
        // Clear function list
        STZ funcListHead
        STZ funcListHeadHi
        STZ funcCount
        STZ compileState
        STZ currentFunc
        STZ currentFuncHi
    }
    
    // Allocate a temporary block for compilation (512 bytes should be plenty)
    StartREPLCompilation()
    {
        LDA #0
        STA ZP.ACCL
        LDA #2  // 512 bytes (2 pages)
        STA ZP.ACCH
        
        Memory.Allocate();  // Returns address in IDX
        
        // Store temp block address
        LDA ZP.IDXL
        STA tempBlock
        LDA ZP.IDXH
        STA tempBlockHi
        
        // Initialize write position
        STZ writePos
        STZ writePosHi
        STZ bytecodeSize
        STZ bytecodeSizeHi
        
        LDA #1
        STA compileState  // Now compiling
    }
    
    // Emit a single byte to the compilation buffer
    EmitByte()
    {
        // Calculate address: tempBlock + writePos
        CLC
        LDA tempBlock
        ADC writePos
        STA ZP.IDXL
        LDA tempBlockHi
        ADC writePosHi
        STA ZP.IDXH
        
        // Get the byte to emit (should be on stack)  
        Stacks.PopA();  // Returns byte in A, doesn't touch TOP
        
        // Store it
        LDY #0
        STA [ZP.IDX], Y
        
        // Increment write position
        INC writePos
        if (Z)
        {
            INC writePosHi
        }
        
        // Increment bytecode size
        INC bytecodeSize
        if (Z)
        {
            INC bytecodeSizeHi
        }
    }
    
    // Emit a 16-bit word (little-endian)
    EmitWord()
    {
        // Word to emit should be in TOP
        Stacks.PopTop();
        
        // Emit low byte first
        LDA ZP.TOPL
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        EmitByte();
        
        // Emit high byte
        LDA ZP.TOPH
        STA ZP.NEXTL
        LDA #0
        STA ZP.NEXTH
        LDA #Types.Byte
        Stacks.PushNext();
        EmitByte();
    }
    
    // Create the REPL function and copy bytecode from temp buffer
    FinishREPLCompilation()
    {
        // Calculate total size needed: header + bytecode
        CLC
        LDA #fhHeaderSize
        ADC bytecodeSize
        STA ZP.ACCL
        LDA #0
        ADC bytecodeSizeHi
        STA ZP.ACCH
        
        // Allocate final function block
        Memory.Allocate();  // Returns address in IDX
        
        // Store as current function
        LDA ZP.IDXL
        STA currentFunc
        LDA ZP.IDXH
        STA currentFuncHi
        
        // Set up function header
        LDY #fhID
        LDA #(ReplFunctionID & 0xFF)
        STA [ZP.IDX], Y
        INY
        LDA #(ReplFunctionID >> 8)
        STA [ZP.IDX], Y
        
        // Set function name to "REPL"
        LDY #fhName
        LDA #'R'
        STA [ZP.IDX], Y
        INY
        LDA #'E'
        STA [ZP.IDX], Y
        INY
        LDA #'P'
        STA [ZP.IDX], Y
        INY
        LDA #'L'
        STA [ZP.IDX], Y
        INY
        LDA #0  // Null terminator
        STA [ZP.IDX], Y
        
        // Clear next pointer (REPL function is temporary)
        LDY #fhNext
        LDA #0
        STA [ZP.IDX], Y
        INY
        STA [ZP.IDX], Y
        
        // Set code size
        LDY #fhCodeSize
        LDA bytecodeSize
        STA [ZP.IDX], Y
        INY
        LDA bytecodeSizeHi
        STA [ZP.IDX], Y
        
        // Copy bytecode from temp buffer to final location
        // Source is already in FSOURCEADDRESS (tempBlock)
        
        // Destination: currentFunc + header size
        CLC
        LDA currentFunc
        ADC #fhHeaderSize
        STA ZP.FDESTINATIONADDRESSL
        LDA currentFuncHi
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // Copy size already in LCOUNT (bytecodeSize);
        
        // CopyBytes() munts FSOURCEADDRESS, set IDX for Memory.Free() below
        LDA ZP.FSOURCEADDRESSL
        STA ZP.IDXL
        LDA ZP.FSOURCEADDRESSH
        STA ZP.IDXH
        
        // Perform the copy
        Utilities.CopyBytes();
        
        // Free the temp buffer (address is still in FSOURCEADDRESS)
        Memory.Free();
        
        // Mark compilation complete
        LDA #2
        STA compileState
    }
    
    // Clean up the REPL function after execution
    CleanupREPLFunction()
    {
        LDA compileState
        CMP #2
        if (NZ) { return; }  // Not compiled
        
        // Free the REPL function
        LDA currentFunc
        STA ZP.IDXL
        LDA currentFuncHi
        STA ZP.IDXH
        Memory.Free();
        
        // Reset state
        STZ compileState
        STZ currentFunc
        STZ currentFuncHi
    }
    
    // Get pointer to bytecode for execution
    GetREPLBytecode()
    {
        // Returns bytecode address in IDX
        CLC
        LDA currentFunc
        ADC #fhHeaderSize
        STA ZP.IDXL
        LDA currentFuncHi
        ADC #0
        STA ZP.IDXH
    }
}
