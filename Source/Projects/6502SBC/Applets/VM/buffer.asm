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
        LDA codeOffsetL
        STA [codeBuffer], Y
        INY
        LDA codeOffsetH
        STA [codeBuffer], Y
    }
    
    GetDataOffset()
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
        SBC #2 // function and global page
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
            LDA codeByte
            STA [ZP.IDX]
            
            // Increment codeSize
            INC codeOffsetL if (Z) { INC codeOffsetH }
            
            SEC
            
            break;
        } // single exit
        
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
            // number of functions is nextFunctionID/2
            LDA nextFunctionID
            LSR A
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

Print.NewLine(); 
LDA SectorSourceH Print.Hex();   LDA SectorSourceL Print.Hex(); Print.Space();
LDA TransferLengthH Print.Hex(); LDA TransferLengthL Print.Hex(); Print.Space();
                                   
            // Write the code buffer
            File.AppendStream();  if (NC) { break; }
            
            // only emit the used part of the function table (0..256 bytes)
            LDA codeBufferL
            STA File.SectorSourceL
            LDA codeBufferH
            STA File.SectorSourceH
            
            LDA nextFunctionID // nextFunctionID = functionCount x 2
            STA File.TransferLengthL
            STZ File.TransferLengthH

Print.NewLine(); 
LDA SectorSourceH Print.Hex();   LDA SectorSourceL Print.Hex(); Print.Space();
LDA TransferLengthH Print.Hex(); LDA TransferLengthL Print.Hex(); Print.Space();
LDY #0 LDA [codeBuffer], Y Print.Hex(); INY LDA [codeBuffer], Y Print.Hex();
                        
            LDA File.TransferLengthL
            ORA File.TransferLengthH
            if (NZ)
            {
                File.AppendStream();  if (NC) { break; }
            }
            
            // Set source to our code buffer
            CLC
            LDA codeBufferL
            ADC # 0
            STA File.SectorSourceL
            LDA codeBufferH
            ADC # 2 // 256 byte function table, 256 global bytes
            STA File.SectorSourceH
            
            // Set transfer length to amount of code generated
            SEC
            LDA codeOffsetL
            SBC # 0
            STA File.TransferLengthL
            LDA codeOffsetH
            SBC # 2 // 256 byte function table, 256 global bytes
            STA File.TransferLengthH
            
Print.NewLine(); 
LDA SectorSourceH Print.Hex();   LDA SectorSourceL Print.Hex(); Print.Space();
LDA TransferLengthH Print.Hex(); LDA TransferLengthL Print.Hex(); Print.Space();            
            
            LDA File.TransferLengthL
            ORA File.TransferLengthH
            if (NZ)
            {
                File.AppendStream();  if (NC) { break; }
            }
            
            LDA #0x00  // not executable flag
            File.EndSave(); if (NC) { break; }

            SEC
            break;
        } // single exit
    }
}
