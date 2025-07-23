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
    
    Initialize()
    {
        // Clear function list
        STZ ZP.FuncListHead
        STZ ZP.FuncListHeadHi
        STZ ZP.FuncCount
        STZ ZP.CompileState
        STZ ZP.CurrentFunc
        STZ ZP.CurrentFuncHi
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
        STA ZP.TempBlockLo
        LDA ZP.IDXH
        STA ZP.TempBlockHi
        
        // Initialize write position
        STZ ZP.WritePosLo
        STZ ZP.WritePosHi
        STZ ZP.BytecodeSizeLo
        STZ ZP.BytecodeSizeHi
        
        LDA #1
        STA ZP.CompileState  // Now compiling
    }
    
    // Emit a single byte to the compilation buffer
    EmitByte()
    {
        // Calculate address: tempBlock + writePos
        CLC
        LDA ZP.TempBlockLo
        ADC ZP.WritePosLo
        STA ZP.IDXL
        LDA ZP.TempBlockHi
        ADC ZP.WritePosHi
        STA ZP.IDXH
        
        // Get the byte to emit (should be on stack)  
        Stacks.PopA();  // Returns byte in A, doesn't touch TOP
               
        // Store it
        LDY #0
        STA [ZP.IDX], Y
        
        // Increment write position
        INC ZP.WritePosLo
        if (Z)
        {
            INC ZP.WritePosHi
        }
        
        // Increment bytecode size
        INC ZP.BytecodeSizeLo
        if (Z)
        {
            INC ZP.BytecodeSizeHi
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
        ADC ZP.BytecodeSizeLo
        STA ZP.ACCL
        LDA #0
        ADC ZP.BytecodeSizeHi
        STA ZP.ACCH
        
        // Allocate final function block
        Memory.Allocate();  // Returns address in IDX
        
        // Store as current function
        LDA ZP.IDXL
        STA ZP.CurrentFunc
        LDA ZP.IDXH
        STA ZP.CurrentFuncHi
        
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
        LDA ZP.BytecodeSizeLo
        STA [ZP.IDX], Y
        INY
        LDA ZP.BytecodeSizeHi
        STA [ZP.IDX], Y
        
        // Copy bytecode from temp buffer to final location
        // Set up for CopyBytes: FSOURCEADDRESS = tempBlock
        LDA ZP.TempBlockLo
        STA ZP.FSOURCEADDRESSL
        LDA ZP.TempBlockHi
        STA ZP.FSOURCEADDRESSH
        
        // FDESTINATIONADDRESS = currentFunc + header size
        CLC
        LDA ZP.CurrentFunc
        ADC #fhHeaderSize
        STA ZP.FDESTINATIONADDRESSL
        LDA ZP.CurrentFuncHi
        ADC #0
        STA ZP.FDESTINATIONADDRESSH
        
        // LCOUNT = bytecode size
        LDA ZP.BytecodeSizeLo
        STA ZP.FLENGTHL
        LDA ZP.BytecodeSizeHi
        STA ZP.FLENGTHH
        
        // Perform the copy
        Tools.CopyBytes();
        
        // Free the temp buffer
        LDA ZP.TempBlockLo
        STA ZP.IDXL
        LDA ZP.TempBlockHi
        STA ZP.IDXH
        Memory.Free();
        
        // Mark compilation complete
        LDA #2
        STA ZP.CompileState
    }
    
    // Clean up the REPL function after execution
    CleanupREPLFunction()
    {
        LDA ZP.CompileState
        CMP #2
        if (NZ) { return; }  // Not compiled
        
        // Free the REPL function
        LDA ZP.CurrentFunc
        STA ZP.IDXL
        LDA ZP.CurrentFuncHi
        STA ZP.IDXH
        Memory.Free();
        
        // Reset state
        STZ ZP.CompileState
        STZ ZP.CurrentFunc
        STZ ZP.CurrentFuncHi
    }
    
    // Get pointer to bytecode for execution
    GetREPLBytecode()
    {
        // Returns bytecode address in IDX
        CLC
        LDA ZP.CurrentFunc
        ADC #fhHeaderSize
        STA ZP.IDXL
        LDA ZP.CurrentFuncHi
        ADC #0
        STA ZP.IDXH
    }
}
