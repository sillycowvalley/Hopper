unit Labels
{
    uses "../System/String"
    
    const byte labelsSlots = 0xA0; // 0xA0..0xAF
    
    // Labels list structure (linked list)
    const uint labelsHead    = labelsSlots+0;  // Head of linked list
    const byte labelsHeadL   = labelsSlots+0;
    const byte labelsHeadH   = labelsSlots+1;
    
    // Temp workspace for traversal
    const uint labelsCurrent  = labelsSlots+2;  // Current node pointer
    const byte labelsCurrentL = labelsSlots+2;
    const byte labelsCurrentH = labelsSlots+3;
    
    const string separator  = " = ";
    const string hexPrefix  = "0x";
    
    // Node structure:
    // +0: next pointer (16-bit)
    // +2: value (16-bit)
    // +4: name string (null-terminated, inline)
    
    Initialize()
    {
        STZ labelsHeadL
        STZ labelsHeadH
        SEC
    }
    
    Dispose()
    {
        loop
        {
            LDA labelsHeadL
            ORA labelsHeadH
            if (Z) { break; }  // Empty list
            
            // Save next pointer before freeing
            LDA labelsHeadL
            STA ZP.IDXL
            LDA labelsHeadH
            STA ZP.IDXH
            LDY #0
            LDA [ZP.IDX], Y  // Next low
            STA labelsCurrentL
            INY
            LDA [ZP.IDX], Y  // Next high
            STA labelsCurrentH
            
            // Free current node
            Memory.Free();
            
            // Move to next
            LDA labelsCurrentL
            STA labelsHeadL
            LDA labelsCurrentH
            STA labelsHeadH
        }
        STZ labelsHeadL
        STZ labelsHeadH
        SEC
    }
    
    // name in STR, value in TOP0..1
    // C if ok, NC if failure
    Add()
    {
        LDA ZP.TOP0
        STA ZP.ACCL
        LDA ZP.TOP1
        STA ZP.ACCH
        
        loop
        {
            // Check if already exists
            FindLabel();
            if (C)  // Already exists
            {
                CLC
                break;
            }
            
            // Restore value
            LDA ZP.ACCH
            STA ZP.TOP1
            LDA ZP.ACCL
            STA ZP.TOP0
            
            // STR -> A
            String.Length();
            
            // Calculate node size: 5 + strlen + 1
            INC  // Include null
            CLC
            ADC #5  // Header size
            STA ZP.ACCL
            LDA #0
            ADC #0
            STA ZP.ACCH
            
            // Allocate node
            Memory.Allocate();
            if (NC)  // Out of memory
            {
                CLC
                break;
            }
            
            // Fill node
            // Next pointer = current head
            LDY #0
            LDA labelsHeadL
            STA [ZP.IDX], Y
            INY
            LDA labelsHeadH
            STA [ZP.IDX], Y
            
            // Value
            INY
            LDA ZP.TOP0
            STA [ZP.IDX], Y
            INY
            LDA ZP.TOP1
            STA [ZP.IDX], Y
            
            LDA ZP.STRL
            STA ZP.IDYL
            LDA ZP.STRH
            STA ZP.IDYH
            
            // Copy string inline
            INY  // Y=4
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
            STA labelsHeadL
            LDA ZP.IDXH
            STA labelsHeadH
            
            SEC
            break;
        } // single exit
    }
    
    // name in STR
    // C if found, NC if not
    // value in TOP0..1
    FindLabel()
    {
        // Start at head
        LDA labelsHeadL
        STA labelsCurrentL
        LDA labelsHeadH
        STA labelsCurrentH
        
        loop
        {
            // Check end of list
            LDA labelsCurrentL
            ORA labelsCurrentH
            if (Z)  // Not found
            {
                CLC
                break;
            }
            
            // Point to current node
            LDA labelsCurrentL
            STA ZP.IDXL
            LDA labelsCurrentH
            STA ZP.IDXH
            
            // Compare inline string at offset 4
            CLC
            LDA ZP.IDXL
            ADC #4
            STA ZP.NEXT0
            LDA ZP.IDXH
            ADC #0
            STA ZP.NEXT1
            String.Compare(); // STR == NEXT -> C or NC
            
            if (C) 
            { 
                // Get value
                LDY #2
                LDA [ZP.IDX], Y
                STA ZP.TOP0
                INY
                LDA [ZP.IDX], Y
                STA ZP.TOP1
                
                break;  // Found with value loaded
            }
            
            // Move to next
            LDY #0
            LDA [ZP.IDX], Y
            STA labelsCurrentL
            INY
            LDA [ZP.IDX], Y
            STA labelsCurrentH
        }
    }
    
    // Debug function to dump symbol table
    Dump()
    {
        Print.NewLine();
        
        // Start at head
        LDA labelsHeadL
        STA labelsCurrentL
        LDA labelsHeadH
        STA labelsCurrentH
        
        loop
        {
            // Check end of list
            LDA labelsCurrentL
            ORA labelsCurrentH
            if (Z) { break; }  // End of list
            
            // Point to current node
            LDA labelsCurrentL
            STA ZP.IDXL
            LDA labelsCurrentH
            STA ZP.IDXH
            
            // Print name (at offset 4)
            CLC
            LDA ZP.IDXL
            ADC #4
            STA ZP.STRL
            LDA ZP.IDXH
            ADC #0
            STA ZP.STRH
            Print.String();
            
            // Print separator
            LDA #(separator % 256)
            STA ZP.STRL
            LDA #(separator / 256)
            STA ZP.STRH
            Print.String();
            
            // Print value as hex
            LDA #(hexPrefix % 256)
            STA ZP.STRL
            LDA #(hexPrefix / 256)
            STA ZP.STRH
            Print.String();
            
            LDY #3
            LDA [ZP.IDX], Y  // Value high
            Print.Hex();
            
            LDY #2
            LDA [ZP.IDX], Y  // Value low
            Print.Hex();
            
            Print.NewLine();
            
            // Move to next
            LDY #0
            LDA [ZP.IDX], Y
            STA labelsCurrentL
            INY
            LDA [ZP.IDX], Y
            STA labelsCurrentH
        }
    }
}
