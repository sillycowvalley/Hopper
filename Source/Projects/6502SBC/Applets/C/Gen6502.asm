unit Gen6502
{
    uses "AST"
    uses "Library"
    
    uses "VCode"
    
    friend CodeGen, Library, VCode;
    
    // Code generation state
    const byte g65Slots = 0x90;
    
    const byte codeBuffer     = g65Slots+0;   // Pointer to allocated buffer
    const byte codeBufferL    = g65Slots+0;
    const byte codeBufferH    = g65Slots+1;
    const byte codeOffset     = g65Slots+2;   // RELATIVE offset in buffer (0-based)
    const byte codeOffsetL    = g65Slots+2;
    const byte codeOffsetH    = g65Slots+3;
    const byte codeSize       = g65Slots+4;   // Current allocated size
    const byte codeSizeL      = g65Slots+4;   
    const byte codeSizeH      = g65Slots+5;   
    const byte stringBuffer   = g65Slots+6;  // String literals
    const byte stringBufferL  = g65Slots+6;  // String literals
    const byte stringBufferH  = g65Slots+7;  // String literals
    const byte stringOffset   = g65Slots+8;  // RELATIVE offset in string buffer
    const byte stringOffsetL  = g65Slots+8;
    const byte stringOffsetH  = g65Slots+9;
    
    const string msgSaved = "Saved ";
    const string msgBytes = " bytes\n";
    
    const byte runtimeZeroPageSlots = 0x60;
    const byte runtimeBP            = runtimeZeroPageSlots+0; // Base pointer for stack frame
    
    const byte runtimeStack0        = runtimeZeroPageSlots+1;
    const byte runtimeStack0L       = runtimeZeroPageSlots+1;
    const byte runtimeStack0H       = runtimeZeroPageSlots+2;
    
    const byte runtimeStack1        = runtimeZeroPageSlots+3;
    const byte runtimeStack1L       = runtimeZeroPageSlots+3;
    const byte runtimeStack1H       = runtimeZeroPageSlots+4;
    
    const byte runtimeStack2        = runtimeZeroPageSlots+5;
    const byte runtimeStack2L       = runtimeZeroPageSlots+5;
    const byte runtimeStack2H       = runtimeZeroPageSlots+6;
    
    const byte runtimeStack3        = runtimeZeroPageSlots+7;
    const byte runtimeStack3L       = runtimeZeroPageSlots+7;
    const byte runtimeStack3H       = runtimeZeroPageSlots+8;
    
    // 65C02S opcodes
    enum OpCode
    {
        ORA_ZP  = 0x05,
        CLC     = 0x18,
        INC_A   = 0x1A,
        JSR     = 0x20,
        SEC     = 0x38, 
        PHA     = 0x48,
        JMP_ABS = 0x4C,
        PHY     = 0x5A,
        RTS     = 0x60,
        STZ_ZP  = 0x64,
        ADC_ZP  = 0x65,
        PLA     = 0x68,
        ADC_IMM = 0x69,
        JMP_IND = 0x6C,
        PLY     = 0x7A, 
        BRA     = 0x80,
        STA_ZP  = 0x85,
        STX_ZP  = 0x86, 
        TXA     = 0x8A,
        STA_IND_Y = 0x91,
        TYA     = 0x98,
        TXS     = 0x9A,
        LDY_IMM = 0xA0,
        LDX_IMM = 0xA2,
        LDA_ZP  = 0xA5, 
        LDX_ZP  = 0xA6,
        TAY     = 0xA8,
        LDA_IMM = 0xA9,
        LDA_IND_Y = 0xB1,
        LDA_ABS_Y = 0xB9,
        TSX     = 0xBA,
        CPY_IMM = 0xC0,
        CMP_IMM = 0xC9,
        INY     = 0xC8,
        DEX     = 0xCA,
        BNE     = 0xD0,
        PHX     = 0xDA, 
        INC_ZP  = 0xE6, 
        INX     = 0xE8,
        SBC_IMM = 0xE9, 
        NOP     = 0xEA, 
        BEQ     = 0xF0,
        PLX     = 0xFA,
    } 
    
    
    // Initialize code generation buffers and state
    // Allocates initial 4KB code buffer
    // Output: C set on success, clear on failure
    Initialize()
    {
        // Start with 4KB
        LDA #0x00
        STA ZP.ACCL
        LDA #0x10  // 4KB = 0x1000
        STA ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        STA codeBufferL
        LDA ZP.IDXH
        STA codeBufferH
        
        STZ codeOffsetL   // Start at offset 0
        STZ codeOffsetH
        
        LDA #0x10
        STA codeSizeH     // 4KB size
        STZ codeSizeL
        
        VCode.Initialize();
    }
    
    // Clean up and free allocated code generation buffers
    Dispose()
    {
        // Free code buffer
        LDA codeBufferL
        ORA codeBufferH
        if (NZ)
        {
            LDA codeBufferL
            STA ZP.IDXL
            LDA codeBufferH
            STA ZP.IDXH
            Memory.Free();
            
            STZ codeBufferL
            STZ codeBufferH
        }
        VCode.Dispose();
    }
    
    // Emit a single byte to the code buffer
    // Input: A = byte to emit
    // Output: C set on success, clear on failure
    // Note: Automatically grows buffer if needed
    EmitByte()  // A = byte to emit
    {
        VCode.IsEmpty();
        if (NC)
        {
            PHA
            VCode.Flush();
            PLA
        }
        emitByte();
    }
    
    emitByte()
    {    
        
        PHA
        
        TAX
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        loop
        {
            // Check if we need to grow buffer
            LDA codeOffsetH
            CMP codeSizeH
            if (Z)
            {
                LDA codeOffsetL
                CMP codeSizeL
                if (Z)
                {
                    PHX
                    growBuffer();  // Double the size
                    PLX
                    if (NC) { break; }
                }
            }
            
            // Add base to offset to get absolute address
            CLC
            LDA codeBufferL
            ADC codeOffsetL
            STA ZP.ACCL
            LDA codeBufferH
            ADC codeOffsetH
            STA ZP.ACCH
            
            // Store byte
            TXA
            STA [ZP.ACC]
            
            // Increment offset (relative!)
            INC codeOffsetL
            if (Z){ INC codeOffsetH}
            
            SEC
            break;
        } // single exit
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLA
        
    }
    
    // Double the size of the code buffer when it fills up
    // Internal helper for EmitByte
    // Output: C set on success, clear on failure
    growBuffer()
    {
        PHY
        
        LDA ZP.TEMP
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // new size
        ASL codeSizeL
        ROL codeSizeH
        
        // Allocate new buffer (double size)
        LDA codeSizeL
        STA ZP.ACCL
        LDA codeSizeH
        STA ZP.ACCH
        
        Memory.Allocate();  // New buffer in IDX
        if (NC) 
        { 
            // Failed to allocate
            Errors.OutOfMemory();
            PLY
            return; 
        }  
        
        // Copy old buffer to new
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Setup for copy: IDY = old (source), IDX = new (dest)
        LDA codeBufferL
        STA ZP.IDYL
        LDA codeBufferH
        STA ZP.IDYH
        
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        loop
        {
            LDA [ZP.IDY]
            STA [ZP.IDX]
            Shared.IncIDY();
            Shared.IncIDX();
            Shared.DecACC();
            LDA ZP.ACCL
            ORA ZP.ACCH
            if (Z) { break; }
        }
        
        // Free old buffer
        LDA codeBufferL
        STA ZP.IDXL
        LDA codeBufferH
        STA ZP.IDXH
        Memory.Free();
        
        // Update pointer to new buffer        
        PLA
        STA codeBufferH
        PLA
        STA codeBufferL
        SEC
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.TEMP
        
        PLY
    }
    
    // Patch the initial JMP instruction to point to main
    // Note: Called after main's address is known
    patchEntryJump()
    {
        // Calculate absolute address
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        AddEntryPoint();
        
        // Write to offset 1-2
        LDY #1
        LDA ZP.ACCL
        STA [codeBuffer], Y
        INY
        LDA ZP.ACCH
        STA [codeBuffer], Y
    }
    
    // Generate code to allocate the runtime stack
    // Output: C set on success, clear on failure
    // Note: Allocates 1K parallel stack, sets up stack pointers
    CreateStack()
    {
        // Patch JMP to main
        patchEntryJump();
    
        // Allocate 1022 bytes (0x0400) for the parallel stack (1022 + 2 allocator size bytes = 1024)
        LDA #0xFE
        STA ZP.ACCL
        LDA #0x03
        STA ZP.ACCH
        
        // Call Memory.Allocate via BIOS dispatch
        LDA # OpCode.LDX_IMM
        EmitByte(); if (NC) { return; }
        LDA # BIOSInterface.SysCall.MemAllocate
        EmitByte(); if (NC) { return; }
        Library.EmitDispatchCall();
        
        // Assume since this is the first allocation, Memory.Allocate returns page-aligned for 1K allocation
        // After 2-byte header, base address will be xx00
        
        // zero the LSB's since we are page aligned
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2L
        EmitByte(); if (NC) { return; }
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3L
        EmitByte(); if (NC) { return; }
        
        // now the MSB's
        LDA # OpCode.LDA_ZP
        EmitByte(); if (NC) { return; }
        LDA # ZP.IDXH
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack0H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack1H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack2H
        EmitByte(); if (NC) { return; }
        LDA # OpCode.INC_A
        EmitByte(); if (NC) { return; }
        LDA # OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #runtimeStack3H
        EmitByte(); if (NC) { return; }
    }
    CreateCLIArguments()
    {
        // Push exe pointer (always LineBuffer start)
        LDA #(Address.LineBuffer % 256)
        LDX #(Address.LineBuffer / 256)
        VCode.PushWORD(); // A = LSB, X = MSB
        
        // Now find the argument by walking LineBuffer
        // LDY #0
        LDA #OpCode.LDY_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // loop: LDA LineBuffer,Y
        LDA # OpCode.LDA_ABS_Y
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer % 256)
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer / 256)
        EmitByte(); if (NC) { return; }
        
        // BEQ no_arg (if null terminator)
        LDA #OpCode.BEQ
        EmitByte(); if (NC) { return; }
        LDA #3  // Forward branch to no_arg section
        EmitByte(); if (NC) { return; }
        
        // INY
        LDA #OpCode.INY
        EmitByte(); if (NC) { return; }
        
        // BNE loop
        LDA #OpCode.BNE
        EmitByte(); if (NC) { return; }
        LDA #0xF8  // Backward branch to loop start
        EmitByte(); if (NC) { return; }
        
// found_space: INY to skip the space
        LDA #OpCode.INY
        EmitByte(); if (NC) { return; }
        
        // LDA LineBuffer,Y - check if there's an arg
        LDA #OpCode.LDA_ABS_Y
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer % 256)
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer / 256)
        EmitByte(); if (NC) { return; }
        
        // BEQ no_arg - if null, no argument
        LDA #OpCode.BEQ
        EmitByte(); if (NC) { return; }
        LDA #18  // Forward branch to no_arg
        EmitByte(); if (NC) { return; }
        
        // TYA
        LDA #OpCode.TYA
        EmitByte(); if (NC) { return; }
        
        // CLC
        LDA #OpCode.CLC
        EmitByte(); if (NC) { return; }
        
        // ADC #low(LineBuffer)
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer % 256)
        EmitByte(); if (NC) { return; }
        
        // STA NEXT0
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        // LDA #high(LineBuffer)
        LDA #OpCode.LDA_IMM
        EmitByte(); if (NC) { return; }
        LDA #(Address.LineBuffer / 256)
        EmitByte(); if (NC) { return; }
        
        // ADC #0 (for carry)
        LDA #OpCode.ADC_IMM
        EmitByte(); if (NC) { return; }
        LDA #0
        EmitByte(); if (NC) { return; }
        
        // STA NEXT1
        LDA #OpCode.STA_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        // STZ NEXT2, NEXT3
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        // BRA done
        LDA #OpCode.BRA
        EmitByte(); if (NC) { return; }
        LDA #8  // Skip no_arg section
        EmitByte(); if (NC) { return; }
        
        // no_arg: push NULL
        // STZ NEXT0-3
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT0
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT1
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT2
        EmitByte(); if (NC) { return; }
        
        LDA #OpCode.STZ_ZP
        EmitByte(); if (NC) { return; }
        LDA #ZP.NEXT3
        EmitByte(); if (NC) { return; }
        
        // done: Push arg pointer (or NULL)
        VCode.PushNEXT(); if (NC) { return; }
        
        // fake RTS slots
        VCode.Reserve(); if (NC) { return; }
        VCode.Reserve();
    }
    
    // Recursively walk AST and emit all string literals
    // Input: IDX = AST node to start from
    // Output: C set on success, clear on failure
    // Note: Updates iOffset field of StringLit nodes with code offset
    EmitStrings()  // Input: IDX = node
    {
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { SEC return; }  // Null node
        
        // Check if this is a StringLit
        LDY # AST.iNodeType
        LDA [ZP.IDX], Y
        CMP # AST.NodeType.StringLit
        if (Z)
        {
            // Store current offset in the node
            LDY # AST.iOffset
            LDA codeOffsetL
            STA [ZP.IDX], Y
            INY
            LDA codeOffsetH
            STA [ZP.IDX], Y
            
            // Get string pointer
            LDY # AST.iData
            LDA [ZP.IDX], Y
            STA ZP.STRL
            INY
            LDA [ZP.IDX], Y
            STA ZP.STRH
            
            // Emit the string
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                EmitByte();
                if (NC) { return; }
                if (Z) { break; }  // Including null
                INY
            }
        }
        
        // Save current node
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Recurse on first child
        LDY # AST.iChild
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        EmitStrings();
        
        // Restore and recurse on sibling
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        if (NC) { return; } 
        
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        EmitStrings();
    }
    

    // Save generated code to an executable file
    // Input: ZP.STR = output filename
    // Output: C set on success, clear on failure
    // Note: Deletes existing file, marks output as executable
    Save()
    {
        LDA # FileType.Any // all files
        File.Exists();
        if (C)
        {
            File.Delete();
        }
        
        // Open file for writing
        File.StartSave();
        if (NC)
        {
            LDA #Error.FileSaveError
            Errors.Show();
            return;
        }
        
        // Set source to our code buffer
        LDA codeBufferL
        STA File.SectorSourceL
        LDA codeBufferH
        STA File.SectorSourceH
        
        // Set transfer length to amount of code generated
        LDA codeOffsetL
        STA File.TransferLengthL
        LDA codeOffsetH
        STA File.TransferLengthH
        
        // Write the code buffer
        File.AppendStream();
        if (NC)
        {
            LDA # Error.FileSaveError
            Errors.Show();
            return;
        }
        
        // Mark as executable and close
        LDA #0x80  // Executable flag
        File.EndSave();
        if (NC)
        {
            LDA # Error.FileSaveError
            Errors.Show();
            return;
        }
        
        LDA #(msgSaved % 256)
        STA ZP.STRL
        LDA #(msgSaved / 256)
        STA ZP.STRH
        Print.String();
        
        // Print byte count
        LDA codeOffsetL
        STA ZP.ACCL
        LDA codeOffsetH
        STA ZP.ACCH
        Shared.MoveAccToTop();
        Long.Print();
        
        LDA #(msgBytes % 256)
        STA ZP.STRL
        LDA #(msgBytes / 256)
        STA ZP.STRH
        Print.String();
        
#if defined(DEBUG)        
        // Add diagnostic hex dump
        Print.NewLine();
        dumpCodeBuffer();
        Print.NewLine();
#endif        
        
        SEC
    }
    
    // Diagnostic hex dump of generated code
    // Shows 16 bytes per row with addresses starting at 0x0800
    // Note: Debug function only, included when DEBUG defined
    dumpCodeBuffer()
    {
        PHY
        PHX
        
        // Start at beginning of code buffer
        LDA codeBufferL
        STA ZP.IDYL
        LDA codeBufferH
        STA ZP.IDYH
        
        // Track virtual address (starts at 0x0800)
        LDA #0x00
        STA ZP.ACCL
        LDA #0x08
        STA ZP.ACCH
        
        // Track bytes remaining
        LDA codeOffsetL
        STA ZP.IDXL
        LDA codeOffsetH
        STA ZP.IDXH
        
        loop
        {
            // Check if we have bytes left
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; }
            
            // Print address
            LDA ZP.ACCH
            Print.Hex();
            LDA ZP.ACCL
            Print.Hex();
            LDA #':'
            Print.Char();
            Print.Space();
            
            // Print up to 32 bytes on this row
            LDX #32
            loop
            {
                // Check if we have bytes left
                LDA ZP.IDXL
                ORA ZP.IDXH
                if (Z) 
                { 
                    // Pad with spaces if less than 16 bytes on last row
                    loop
                    {
                        PHX
                        Print.Space();
                        Print.Space();
                        Print.Space();
                        PLX
                        DEX
                        if (Z) { break; }
                    }
                    break; 
                }
                
                PHX
                // Print byte
                LDA [ZP.IDY]
                Print.Hex();
                Print.Space();
                PLX
                
                // Advance buffer pointer
                INC ZP.IDYL
                if (Z) { INC ZP.IDYH }
                
                // Decrement bytes remaining
                LDA ZP.IDXL
                if (Z)
                {
                    DEC ZP.IDXH
                }
                DEC ZP.IDXL
                
                // Decrement column counter
                DEX
                if (Z) { break; }
            }
            
            Print.NewLine();
            
            // Advance virtual address by 16
            CLC
            LDA ZP.ACCL
            ADC #32
            STA ZP.ACCL
            LDA ZP.ACCH
            ADC #0
            STA ZP.ACCH
        }
        
        PLX
        PLY
    }   
    
    
    // Generate function prologue
    //    LDA #runtimeBP
    //    PHA
    //    TSX            // Get current stack pointer
    //    STX runtimeBP  // Save as base pointer
    Prologue()
    {
        loop
        {
            // push BP
            LDA # OpCode.LDA_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            LDA #OpCode.PHA
            EmitByte(); if (NC) { break; }
            
            // BP = SP
            LDA # OpCode.TSX
            EmitByte(); if (NC) { break; }
            LDA # OpCode.STX_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            
            SEC
            break;
        }
    }
    
    // Generate function epilogue
    // Restore stack pointer
    //
    //    LDX #runtimeBP
    //    TXS
    //    PLA
    //    STA #runtimeBP
    // 
    Epilogue()
    {
        loop
        {
            // SP = BP     
            LDA # OpCode.LDX_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            LDA #OpCode.TXS
            EmitByte(); if (NC) { break; }
            
            // pop BP
            LDA # OpCode.PLA
            EmitByte(); if (NC) { break; }
            LDA # OpCode.STA_ZP
            EmitByte(); if (NC) { break; }
            LDA # runtimeBP
            EmitByte(); if (NC) { break; }
            
            if (BBS0, CodeGen.functionFlags) // Bit 0 - we're in "main"
            {
                if (BBS1, CodeGen.functionFlags) // Bit 1 - "main" had arguments
                {
                    VCode.Discard(); if (NC) { break; } // fake return
                    VCode.Discard(); if (NC) { break; } // fake return
                    VCode.Discard(); if (NC) { break; } // arg*
                    VCode.Discard(); if (NC) { break; } // exe*
                }
            }
            
            LDA # OpCode.RTS
            EmitByte(); if (NC) { break; }
            
            SEC
            break;
        }
    }
    
    // Generate code for an integer literal
    // Input: IDX = IntLit or LongLit node
    // Output: Code emitted to load value into ZP.NEXT0-3 at runtime
    //         C set on success, clear on failure
    LongNEXT()  // Input: IDX = literal node
    {
        // Get pointer to 32-bit value in heap
        LDY #AST.iData
        LDA [ZP.IDX], Y
        STA ZP.IDYL
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDYH
        
        // Read the 4 bytes from heap into temp storage
        LDY #0
        LDA [ZP.IDY], Y
        STA ZP.NEXT0
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT1
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT2
        INY
        LDA [ZP.IDY], Y
        STA ZP.NEXT3
        
        LDA ZP.NEXT1
        ORA ZP.NEXT2
        ORA ZP.NEXT3
        if (Z)
        {
            LDA ZP.NEXT0
            VCode.PushCHAR();
            SEC
            return;
        }
        
        LDA ZP.NEXT2
        ORA ZP.NEXT3
        if (Z)
        {
            LDA ZP.NEXT0
            LDX ZP.NEXT1
            VCode.PushWORD();
            SEC
            return;
        }
        
        // Now emit code to load these values at runtime
        
        // Emit code for NEXT0
        LDA ZP.NEXT0
        if (Z)  // Optimize: use STZ for zero
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT0
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT0
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT1
        LDA ZP.NEXT1
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT1
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT1
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT2
        LDA ZP.NEXT2
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT2
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT2
            EmitByte(); if (NC) { return; }
        }
        
        // Emit code for NEXT3
        LDA ZP.NEXT3
        if (Z)
        {
            LDA #OpCode.STZ_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { return; }
        }
        else
        {
            LDA #OpCode.LDA_IMM
            EmitByte(); if (NC) { return; }
            LDA ZP.NEXT3
            EmitByte(); if (NC) { return; }
            LDA #OpCode.STA_ZP
            EmitByte(); if (NC) { return; }
            LDA #ZP.NEXT3
            EmitByte(); if (NC) { return; }
        }
        PushNEXT();
        SEC
    }  
    
}
