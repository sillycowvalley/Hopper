unit Buffer
{
    // file header size
    const byte headerSize = 6; 
    
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
    
    const uint functionCount  = bufferSlots+7;
    
    const uint globalSize     = bufferSlots+8;
    const byte globalSizeL    = bufferSlots+8;
    const byte globalSizeH    = bufferSlots+9;
    
    // Create initial 2K buffer
    // C if success, NC if not
    Initialize()
    {
        // Clear our slots
        STZ codeOffsetL
        STZ codeOffsetH
        
        STZ globalSizeL
        STZ globalSizeH
        
        // assume there is always at least 1 (.MAIN)
        LDA #1
        STA functionCount
        
        // Allocate 2K initial buffer
        LDA #0x00   // 2048 = 0x0800
        STA ZP.ACCL
        LDA #0x08
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            // Failed to allocate
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
        
        SEC  // Success
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
            LDA functionCount
            LDY #3
            STA [codeBuffer], Y
            INY
            LDA globalSizeL
            STA [codeBuffer], Y
            INY
            LDA globalSizeH
            STA [codeBuffer], Y
            
            // Set source to our code buffer
            LDA codeBufferL
            STA File.SectorSourceL
            LDA codeBufferH
            STA File.SectorSourceH
            
            LDA # headerSize
            STA File.TransferLengthL
            LDA #0
            STA File.TransferLengthH

Print.NewLine(); 
LDA SectorSourceH Print.Hex();   LDA SectorSourceL Print.Hex(); Print.Space();
LDA TransferLengthH Print.Hex(); LDA TransferLengthL Print.Hex(); Print.Space();
                                   
            // Write the code buffer
            File.AppendStream();  if (NC) { break; }
            
            // only emit the used part of the function table (0..256 bytes)
            CLC
            LDA codeBufferL
            ADC # headerSize
            STA File.SectorSourceL
            LDA codeBufferH
            ADC #0
            STA File.SectorSourceH
            
            // functionCount x 2
            CLC
            LDA functionCount
            ADC functionCount
            STA File.TransferLengthL
            ADC #0
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
            
            // only emit the used part of the globals (0..256 bytes)
            CLC
            LDA codeBufferL
            ADC # headerSize
            STA File.SectorSourceL
            LDA codeBufferH
            ADC # 1 // 256 byte function table
            STA File.SectorSourceH
                        
            LDA globalSizeL
            STA File.TransferLengthL
            LDA globalSizeH
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
            
            // Set source to our code buffer
            CLC
            LDA codeBufferL
            ADC # headerSize
            STA File.SectorSourceL
            LDA codeBufferH
            ADC # 2 // 256 byte function table, 256 global bytes
            STA File.SectorSourceH
            
            // Set transfer length to amount of code generated
            SEC
            LDA codeOffsetL
            SBC # headerSize
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
