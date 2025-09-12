unit CodeGen
{
    // Code generation state
    const byte cgSlots = 0x90;
    
    const byte codeBuffer    = cgSlots+0;   // Pointer to allocated buffer
    const byte codeBufferL   = cgSlots+0;
    const byte codeBufferH   = cgSlots+1;
    const byte codeOffset    = cgSlots+2;   // RELATIVE offset in buffer (0-based)
    const byte codeOffsetL   = cgSlots+2;
    const byte codeOffsetH   = cgSlots+3;
    const byte codeSize      = cgSlots+4;   // Current allocated size
    const byte codeSizeL     = cgSlots+4;   
    const byte codeSizeH     = cgSlots+5;   
    const byte stringBuffer  = cgSlots+6;  // String literals
    const byte stringBufferL = cgSlots+6;  // String literals
    const byte stringBufferH = cgSlots+7;  // String literals
    const byte stringOffset  = cgSlots+8;  // RELATIVE offset in string buffer
    const byte stringOffsetL = cgSlots+8;
    const byte stringOffsetH = cgSlots+9;
    
    // Success message
    const string msgSaved = "Saved ";
    const string msgBytes = " bytes\n";
    
    // 6502 opcodes
    enum OpCode
    {
        JMP_ABS = 0x4C,
        RTS     = 0x60,
        LDA_IMM = 0xA9,
        STA_ZP  = 0x85,
        LDX_IMM = 0xA2,
        JMP_IND = 0x6C,
    }    
    
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
    }
    
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
    }
    
    EmitByte()  // A = byte to emit
    {
        PHA
        
PHA Print.Hex(); Print.Space(); PLA

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
                    growBuffer();  // Double the size
                    if (NC) { break; }
                }
            }
            
            // Add base to offset to get absolute address
            CLC
            LDA codeBufferL
            ADC codeOffsetL
            STA ZP.IDXL
            LDA codeBufferH
            ADC codeOffsetH
            STA ZP.IDXH
            
            // Store byte
            PLA  // Get byte back
            PHA
            STA [ZP.IDX]
            
            // Increment offset (relative!)
            INC codeOffsetL
            if (Z){ INC codeOffsetH}
            
            SEC
            break;
        } // single exit
        
        PLA
        
    }
    
    growBuffer()
    {
        PHY
        
        // new size
        ASL codeSizeL
        ROL codeSizeH
        
        // Allocate new buffer (double size)
        LDA codeSizeL
        STA ZP.ACCL
        LDA codeSizeH
        STA ZP.ACCH
        
        Memory.Allocate();  // New buffer in IDX
        if (NC) { PLY return; }  // Failed to allocate : TODO : print the error here
        
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
        PLY
    }
    
    // Recursively walk AST and emit string literals
    emitStrings()  // Input: IDX = node
    {
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z) { return; }  // Null node
        
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
        emitStrings();
        
        // Restore and recurse on sibling
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        LDY #AST.iNext
        LDA [ZP.IDX], Y
        TAX
        INY
        LDA [ZP.IDX], Y
        STA ZP.IDXH
        STX ZP.IDXL
        emitStrings();
    }
    
    // Generate code for main function
    emitMain()
    {
        // Patch the JMP at offset 1-2 to point to main
        CLC
        LDA codeOffsetL
        ADC #0x00
        STA ZP.ACCL
        LDA codeOffsetH
        ADC #0x08    // Base 0x0800
        STA ZP.ACCH
        
        // Write low byte of main address
        LDY #1
        LDA ZP.ACCL
        STA [codeBuffer], Y
        
        // Write high byte at offset 2
        INY
        LDA ZP.ACCH
        STA [codeBuffer], Y
        
        // For now, just emit RTS at current location
        // TODO: Walk AST to find main function node
        // TODO: Store offset in function node
        // TODO: Generate printf call
        
        LDA # OpCode.RTS
        EmitByte();
        
        SEC
    }
    
    Compile()
    {
Print.NewLine();  
      
        // 1. Reserve 3 bytes for JMP to main
        LDA # OpCode.JMP_ABS
        EmitByte();  // JMP absolute
        if (NC) { return; }
        LDA #0
        EmitByte();  // Placeholder low
        if (NC) { return; }
        LDA #0
        EmitByte();  // Placeholder high
        if (NC) { return; }
        
        // 2. Walk AST and emit all string literals
        AST.GetRoot();  // -> IDX
        emitStrings();
        if (NC) { return; }
        
        // 3. Generate main function
        emitMain();
        if (NC) { return; }
        
        // TODO 
        
        SEC
    }

    // Save generated code to file
    // Input: ZP.STR = output filename (e.g., "HELLOX")
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
            Errors.ShowError();
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
            Errors.ShowError();
            return;
        }
        
        // Mark as executable and close
        LDA #0x80  // Executable flag
        File.EndSave();
        if (NC)
        {
            LDA # Error.FileSaveError
            Errors.ShowError();
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
        
        SEC
    }
}

