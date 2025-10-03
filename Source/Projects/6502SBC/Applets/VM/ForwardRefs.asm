unit ForwardRefs
{
    uses "Buffer"
    
    const byte forwardSlots = 0xB0; // 0xB0..0xBF
    
    const byte refsHead = forwardSlots+0;
    const byte refsHeadL = forwardSlots+0; 
    const byte refsHeadH = forwardSlots+1;
    
    const byte refsCurrent = forwardSlots+2;
    const byte refsCurrentL = forwardSlots+2;
    const byte refsCurrentH = forwardSlots+3;
    
    // Node structure:
    // +0: next pointer (16-bit)
    // +2: patch address in code buffer (16-bit) 
    // +4: resolved flag (1 byte) - 0xFF if resolved, 0x00 if unresolved
    // +5: label name string (null-terminated, inline)
    
    const byte iNext = 0;
    const byte iPatchAddr = 2;
    const byte iResolved = 4;
    const byte iLabelName = 5;
    
    const string errUnresolvedLabel = "Unresolved label: ";
    
    Initialize()
    {
        STZ refsHeadL
        STZ refsHeadH
        SEC
    }
    
    Dispose()
    {
        loop
        {
            LDA refsHeadL
            ORA refsHeadH
            if (Z) { break; }  // Empty list
            
            // Point to current node
            LDA refsHeadL
            STA ZP.IDXL
            LDA refsHeadH
            STA ZP.IDXH
            
            // Check if resolved
            LDY # iResolved
            LDA [ZP.IDX], Y
            if (Z)  // Unresolved!
            {
                // Report error
                LDA #(errUnresolvedLabel % 256)
                STA ZP.STRL
                LDA #(errUnresolvedLabel / 256)
                STA ZP.STRH
                Print.String();
                
                // Print label name
                CLC
                LDA ZP.IDXL
                ADC # iLabelName
                STA ZP.STRL
                LDA ZP.IDXH
                ADC #0
                STA ZP.STRH
                Print.String();
                Print.NewLine();
                
                CLC  // Signal error
                return;
            }
            
            // Save next pointer before freeing
            LDY # iNext
            LDA [ZP.IDX], Y  // Next low
            STA refsCurrentL
            INY
            LDA [ZP.IDX], Y  // Next high
            STA refsCurrentH
            
            // Free current node
            Memory.Free();
            
            // Move to next
            LDA refsCurrentL
            STA refsHeadL
            LDA refsCurrentH
            STA refsHeadH
        }
        Initialize(); // zero everything
    }
    
    // Add forward reference
    // labelName in STR, patch address in TOP
    Add()
    {
        String.Length();
        INC  // Include null terminator
        CLC
        ADC # 5  // Header size (next + patch address + resolved flag)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        Memory.Allocate();
        if (NC) { return; }
        
        // Fill node
        LDY # iNext
        LDA refsHeadL
        STA [ZP.IDX], Y
        INY
        LDA refsHeadH
        STA [ZP.IDX], Y
        
        // Patch address
        LDY # iPatchAddr
        LDA ZP.TOP0
        STA [ZP.IDX], Y
        INY
        LDA ZP.TOP1
        STA [ZP.IDX], Y
        
        // Mark as unresolved
        LDY # iResolved
        LDA #0
        STA [ZP.IDX], Y
        
        // Copy string inline
        LDA ZP.STRL
        STA ZP.IDYL
        LDA ZP.STRH
        STA ZP.IDYH
        
        LDY # iLabelName
        loop
        {
            LDA [ZP.IDY]
            STA [ZP.IDX], Y
            if (Z) { break; }
            IncIDY();
            INY
        }
        
        // Update head
        LDA ZP.IDXL
        STA refsHeadL
        LDA ZP.IDXH
        STA refsHeadH
        
        SEC
    }
    
    // Patch all forward references to labelName
    // labelName pointer in STR, label address in TOP
    Patch()
    {
        // Walk list and patch matching labels
        LDA refsHeadL
        STA refsCurrentL
        LDA refsHeadH
        STA refsCurrentH
        
        loop
        {
            LDA refsCurrentL
            ORA refsCurrentH
            if (Z) { break; }  // End of list
            
            // Point to current node
            LDA refsCurrentL
            STA ZP.IDXL
            LDA refsCurrentH
            STA ZP.IDXH
            
            // Get label name and compare
            CLC
            LDA ZP.IDXL
            ADC # iLabelName
            STA ZP.NEXT0
            LDA ZP.IDXH
            ADC #0
            STA ZP.NEXT1
            String.Compare(); // STR == NEXT -> C or NC
            
            // Inside ForwardRefs.Patch() where we found a match:
            if (C)  // Found match
            {
                // Mark as resolved
                LDY # iResolved
                LDA #0xFF
                STA [ZP.IDX], Y
                
                // Get patch address from node
                LDY # iPatchAddr
                LDA [ZP.IDX], Y
                STA ZP.NEXT0  // Patch location (code offset)
                INY
                LDA [ZP.IDX], Y
                STA ZP.NEXT1
                
                // Calculate branch offset:
                // offset = label_address - (patch_address + 1)
                // TOP = label address, NEXT = patch address
                INC ZP.NEXT0 if (Z) { INC ZP.NEXT1 } // +1 for instruction after branch
                
                SEC
                LDA ZP.TOP0
                SBC ZP.NEXT0
                STA ZP.NEXT0  // Calculated offset
                LDA ZP.TOP1
                SBC ZP.NEXT1
                STA ZP.NEXT1
                
                // Patch the buffer at the stored location
                // Convert code offset back to buffer address
                LDY # iPatchAddr
                LDA [ZP.IDX], Y
                CLC
                ADC #0  // Low byte of 2-page offset
                STA ZP.IDYL
                INY
                LDA [ZP.IDX], Y
                ADC #2  // Add back the 2 pages
                STA ZP.IDYH
                
                // Now add to buffer base and subtract Memory header
                CLC
                LDA ZP.IDYL
                ADC Buffer.codeBufferL
                STA ZP.IDYL
                LDA ZP.IDYH
                ADC Buffer.codeBufferH
                STA ZP.IDYH
                
                // Account for Memory.Allocate size header
                SEC
                LDA ZP.IDYL
                SBC #2
                STA ZP.IDYL
                LDA ZP.IDYH
                SBC #0
                STA ZP.IDYH
                
                // Write the calculated offset
                LDA ZP.NEXT0  // Should be the branch offset
                STA [ZP.IDY]
            }
            
            // Move to next
            LDY # iNext
            LDA [ZP.IDX], Y
            STA refsCurrentL
            INY
            LDA [ZP.IDX], Y
            STA refsCurrentH
        }
        SEC
    }
}
