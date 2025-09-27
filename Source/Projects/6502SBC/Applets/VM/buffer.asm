unit Buffer
{
    const byte fileHeaderSize = 6;
    
    // Single base for easy relocation
    const byte bufferSlots = 0x60; // 0x60..0x6F
    
    // Code buffer management (matching C compiler)
    const uint codeBuffer    = bufferSlots+0;  // Pointer to code buffer
    const byte codeBufferL   = bufferSlots+0;
    const byte codeBufferH   = bufferSlots+1;
    
    const uint codeOffset    = bufferSlots+2;  // Current size of code
    const byte codeOffsetL   = bufferSlots+2;
    const byte codeOffsetH   = bufferSlots+3;
    
    const uint codeCapacity  = bufferSlots+4; // Buffer capacity
    const byte codeCapacityL = bufferSlots+4;
    const byte codeCapacityH = bufferSlots+5;
    
    const byte codeByte       = bufferSlots+6;
    
    const uint nextFunctionID = bufferSlots+7;
    
    const uint dataSize       = bufferSlots+8;
    const byte dataSizeL      = bufferSlots+8;
    const byte dataSizeH      = bufferSlots+9;
    
    const byte headerBlock    = bufferSlots+10;
    const byte headerBlockL   = bufferSlots+10;
    const byte headerBlockH   = bufferSlots+11;
    
    const byte indexBlock     = headerBlock;
    const byte indexBlockL    = headerBlockL;
    const byte indexBlockH    = headerBlockH;
    
    // Pages:
    // 0 - function offsets
    // 1 - function sizes // reserved for globals
    // 2 - start of constant data, then functions
    // ...
    
    GetNextFunctionNumber() // 0..254 in multiples of 2
    {
        LDA nextFunctionID
        STA ZP.TOP0
        STZ ZP.TOP1
        
        INC nextFunctionID
        INC nextFunctionID
    }
    
    // function ID in TOP
    CaptureFunctionStart()
    {
        // address of function table
        LDY ZP.TOP0
        
        LDA codeBufferH
        STA ZP.IDXH
        STZ ZP.IDXL
        
        LDA codeOffsetL
        STA [ZP.IDX], Y
        INY
        LDA codeOffsetH
        STA [ZP.IDX], Y
        
//Print.NewLine(); LDA ZP.TOP0 Print.Hex(); Print.Space(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex();Print.Space(); LDA codeOffsetH Print.Hex(); LDA codeOffsetL Print.Hex();
//Print.NewLine(); 
        SEC
    }
    // function ID in TOP
    CaptureFunctionEnd()
    {
        // start address
        LDY nextFunctionID
        DEY
        DEY
        
        LDA codeBufferH
        STA ZP.IDXH
        STZ ZP.IDXL
        
        LDA [ZP.IDX], Y
        STA ZP.NEXT0
        INY
        LDA [ZP.IDX], Y
        STA ZP.NEXT1
        
        // size = codeOffset - start
        CLC
        LDA codeOffsetL
        SBC ZP.NEXT0
        STA ZP.NEXT0
        LDA codeOffsetH
        SBC ZP.NEXT1
        STA ZP.NEXT1
        
        // address of function size table
        INC ZP.IDXH
        
        DEY
        LDA ZP.NEXT0
        STA [ZP.IDX], Y
        INY
        LDA ZP.NEXT1
        STA [ZP.IDX], Y
        
//Print.NewLine(); DEY TYA Print.Hex(); Print.Space(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex(); Print.Space();LDA ZP.NEXT1 Print.Hex(); LDA ZP.NEXT0 Print.Hex();
//Print.NewLine();         
        SEC
    }
    
    GetDataOffset()
    {
        SEC
        LDA codeOffsetL
        SBC # 0
        STA ZP.TOP0
        LDA codeOffsetH
        SBC # 2 // after global and function offset and function size pages
        STA ZP.TOP1
    }
    GetCodeOffset() // used by labels
    {
        SEC
        LDA codeOffsetL
        SBC # 0
        STA ZP.TOP0
        LDA codeOffsetH
        SBC # 2 // after global and function pages
        STA ZP.TOP1
    }
    UpdateDataSize()
    {
        SEC
        LDA codeOffsetL
        SBC #0
        STA dataSizeL
        LDA codeOffsetH
        SBC #2 // function, function size and global pages
        STA dataSizeH
    }
    
    // Create initial 2K buffer
    // C if success, NC if not
    Initialize()
    {
        // Clear our slots
        STZ codeOffsetL
        STZ codeOffsetH
        
        STZ dataSizeL
        STZ dataSizeH
        
        // assume there is always at least 1 (.MAIN)
        LDA #4 // multiples of 2, .MAIN is 2
        STA nextFunctionID
        
        // Allocate 2K initial buffer
        LDA #0x00   // 2048 = 0x0800
        STA ZP.ACCL
        LDA #0x08
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            // Failed to allocate
            STZ codeBufferL
            STZ codeBufferH
            CLC
            return;
        }
        
        // Store buffer pointer from ZP.IDX
        LDA ZP.IDXL
        STA codeBufferL
        LDA ZP.IDXH
        STA codeBufferH
        
        // Store capacity
        LDA #0x00
        STA codeCapacityL
        LDA #0x08
        STA codeCapacityH
        
        LDA # fileHeaderSize
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            // Failed to allocate
            STZ headerBlockH
            STZ headerBlockL
            CLC
            return;
        }
        LDA ZP.IDXH
        STA headerBlockH
        LDA ZP.IDXL
        STA headerBlockL
        
        SEC  // Success
    }
    Dispose()
    {
        LDA headerBlockH
        ORA headerBlockL
        if (NZ)
        {
            LDA headerBlockH
            STA ZP.IDXH
            LDA headerBlockL
            STA ZP.IDXL
            Memory.Free();
        }
        LDA codeBufferH
        ORA codeBufferL
        if (NZ)
        {
            LDA codeBufferH
            STA ZP.IDXH
            LDA codeBufferL
            STA ZP.IDXL
            Memory.Free();
        }
    }
    
    // Expand buffer and copy existing.
    // C if success, NC if not
    Grow() 
    {
        // new size
        ASL codeCapacityL
        ROL codeCapacityH
        
        // Allocate new buffer (double size)
        LDA codeCapacityL
        STA ZP.ACCL
        LDA codeCapacityH
        STA ZP.ACCH
        
        // Allocate new buffer
        Memory.Allocate();
        if (NC)
        {
            // Out of memory
            CLC
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
        
        // old offset is number of bytes used in old buffer
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
    }
    
    Reserve() // same as Emit but without writing
    {
        PHY
        
        loop
        {
            // Check if we need to grow
            LDA codeOffsetH
            CMP codeCapacityH
            if (Z)
            {
                LDA codeOffsetL
                CMP codeCapacityL
            }
            if (C) // codeOffset >= codeCapacity
            {
                // Low bytes not equal, we have space
                Grow();
                if (NC) { break; }
            }
            // Now we have space
            
            // Increment codeSize
            INC codeOffsetL if (Z) { INC codeOffsetH }
            
            SEC
            
            break;
        } // single exit
        
        PLY
    }
    
    // A -> buffer, expand if needed
    // C if success, NC if not
    Emit() 
    {
        PHY
        
        STA codeByte  // Save byte to emit
        
        loop
        {
            // Check if we need to grow
            LDA codeOffsetH
            CMP codeCapacityH
            if (Z)
            {
                LDA codeOffsetL
                CMP codeCapacityL
            }
            if (C) // codeOffset >= codeCapacity
            {
                // Low bytes not equal, we have space
                Grow();
                if (NC) { break; }
            }
            // Now we have space
            
            // Calculate address: codeBuffer + codeSize
            CLC
            LDA codeOffsetL
            ADC codeBufferL
            STA ZP.IDXL
            LDA codeOffsetH
            ADC codeBufferH
            STA ZP.IDXH
            
            
            // pretend the Memory size word isn't there
            SEC
            LDA ZP.IDXL
            SBC #2
            STA ZP.IDXL
            LDA ZP.IDXH
            SBC #0
            STA ZP.IDXH
            
            LDA codeByte
            STA [ZP.IDX]
            
            // Increment codeSize
            INC codeOffsetL if (Z) { INC codeOffsetH }
            
            SEC
            
            break;
        } // single exit
        
        PLY
    }
    
    // Dump 256-byte page in hex/ASCII format
    // Input: ZP.IDXH = start address of page
    DumpPage()
    {
        PHY
        
        STZ ZP.ACCL  // Offset counter
        STZ ZP.IDXL
        
        loop
        {
            // Print offset (4 hex digits)
            LDA ZP.IDXH
            Print.Hex();
            LDA ZP.ACCL
            Print.Hex();
            LDA #':'
            Print.Char();
            Print.Space();
            
            // Print 16 bytes in hex
            LDY #0
            loop
            {
                LDA [ZP.IDX], Y
                Print.Hex();
                Print.Space();
                
                // Extra space after 8 bytes
                INY
                CPY #8
                if (Z)
                {
                    Print.Space();
                }
                CPY #16
                if (Z) { break; }
            }
            
            // Print ASCII representation
            Print.Space();
            
            LDY #0
            loop
            {
                LDA [ZP.IDX], Y
                // Check if printable (32-126)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #128
                    if (C)  // >= 128
                    {
                        LDA #'.'    
                    }
                }
                else
                {
                    LDA #'.'
                }
                Print.Char();
                
                INY
                CPY #16
                if (Z) { break; }
            }
            
            Print.NewLine();
            
            // Advance to next line
            CLC
            LDA ZP.IDXL
            ADC #16
            STA ZP.IDXL
            if (C)
            {
                INC ZP.IDXH
            }
            
            // Update offset
            CLC
            LDA ZP.ACCL
            ADC #16
            STA ZP.ACCL
            if (Z) { break; }  // Wrapped after 256 bytes
        }
        PLY
    }
    
    // filename in STR
    Save()
    {
        loop
        {
            LDA # FileType.Any // all files
            File.Exists();
            if (C)
            {
                File.Delete();
            }
            
            // Open file for writing
            File.StartSave();  if (NC) { break; }
            
            // update and emit the header
            
            // file header
            LDY #0
            LDA #'V' STA [headerBlock], Y INY
            LDA #'M' STA [headerBlock], Y INY
            LDA #'B' STA [headerBlock], Y INY
            // number of functions is nextFunctionID/2 - 1
            LDA nextFunctionID
            LSR A
            DEC
            STA [headerBlock], Y INY
            LDA dataSizeL
            STA [headerBlock], Y INY
            LDA dataSizeH
            STA [headerBlock], Y 
            
            // Set source to our code buffer
            LDA headerBlockL
            STA File.SectorSourceL
            LDA headerBlockH
            STA File.SectorSourceH
            
            LDA # fileHeaderSize
            STA File.TransferLengthL
            LDA #0
            STA File.TransferLengthH

            // Write the code buffer
            File.AppendStream();  if (NC) { break; }

            LDA #2
            STA ZP.IDXL
            STA ZP.IDYL
            LDA codeBufferH
            STA ZP.IDXH
            STA ZP.IDYH
            INC ZP.IDYH
                        
            LDA nextFunctionID
            LSR A // /= 2
            TAX
            DEX
            loop
            {
                LDY #0
                LDA [ZP.IDX]
                STA [indexBlock], Y
                INY
                IncIDX();
                LDA [ZP.IDX]
                DEC
                DEC
                STA [indexBlock], Y
                INY
                LDA [ZP.IDY]
                STA [indexBlock], Y
                INY
                IncIDY();
                LDA [ZP.IDY]
                STA [indexBlock], Y
                INY
            
                LDA indexBlockL
                STA File.SectorSourceL
                LDA indexBlockH
                STA File.SectorSourceH
            
                LDA #4
                STA File.TransferLengthL
                STZ File.TransferLengthH
            
                PHX
                File.AppendStream();  if (NC) { PLX break; }
                PLX
                DEX
                if (Z) { break; }
            }
            
            // Set source to our code buffer
            STZ File.SectorSourceL
            LDA codeBufferH
            INC // 256 byte function table, 256 global bytes
            INC
            STA File.SectorSourceH
            
            // Set transfer length to amount of code generated
            SEC
            LDA codeOffsetL
            SBC # 0
            STA File.TransferLengthL
            LDA codeOffsetH
            SBC # 2 // 256 byte function table, 256 global bytes
            STA File.TransferLengthH
            
            LDA File.TransferLengthL
            ORA File.TransferLengthH
            if (NZ)
            {
                File.AppendStream();  if (NC) { break; }
            }
            
            LDA #0x00  // not executable flag
            File.EndSave(); if (NC) { break; }
            
#ifdef DEBUG
            LDA codeBufferH
            STA ZP.IDXH
            DumpPage();
            DumpPage();
            DumpPage();
            DumpPage();
#endif            

            SEC
            break;
        } // single exit
    }
}
