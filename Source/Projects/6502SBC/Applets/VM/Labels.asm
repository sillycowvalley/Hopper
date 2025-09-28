unit Labels
{
    uses "../System/String"
    
    const byte labelsSlots = 0xA0; // 0xA0..0xAF
    
    // Labels list structure (linked list)
    const byte labelsHead    = labelsSlots+0;  // Head of linked list
    const byte labelsHeadL   = labelsSlots+0;
    const byte labelsHeadH   = labelsSlots+1;
    
    // Temp workspace for traversal
    const byte labelsCurrent  = labelsSlots+2;  // Current node pointer
    const byte labelsCurrentL = labelsSlots+2;
    const byte labelsCurrentH = labelsSlots+3;
    
    const byte labelID        = labelsSlots+4;
    
    const string separator  = " = ";
    const string hexPrefix  = "0x";
    
    // Node structure:
    // +0: next pointer (16-bit)
    // +2: ID    (8-bit 1..255 per function)
    // +3: Resolved? 0 or 1
    // +4: value (16-bit offset in code buffer)
    // +6: name string (null-terminated, inline)
    
    
    const byte iNext     = 0;
    const byte iID       = 2;
    const byte iResolved = 3;
    const byte iValue    = 4;
    const byte iName     = 6;
    
    const byte nSize  = 6;
    
    Initialize()
    {
        STZ labelsHeadL
        STZ labelsHeadH
        STZ labelID
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
            LDY # iNext
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
        Initialize(); // zero everything
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
            
            // Calculate node size: nSize + strlen + 1
            INC  // Include null
            CLC
            ADC # nSize  // Header size
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
            LDY # iNext
            LDA labelsHeadL
            STA [ZP.IDX], Y
            INY
            LDA labelsHeadH
            STA [ZP.IDX], Y
            
            // Value
            LDY # iValue
            LDA ZP.TOP0
            STA [ZP.IDX], Y
            INY
            LDA ZP.TOP1
            STA [ZP.IDX], Y
            
            INC labelID
            
            LDY # iID
            LDA labelID
            STA [ZP.IDX], Y
            
            LDY # iResolved
            LDA #1 // resolved for now
            STA [ZP.IDX], Y
            
            LDA ZP.STRL
            STA ZP.IDYL
            LDA ZP.STRH
            STA ZP.IDYH
            
            // Copy string inline
            LDY # iName
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
    // value    in TOP0..1
    // resolved in ACCL
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
            
            // Compare inline string at offset iName
            CLC
            LDA ZP.IDXL
            ADC # iName
            STA ZP.NEXT0
            LDA ZP.IDXH
            ADC #0
            STA ZP.NEXT1
            String.Compare(); // STR == NEXT -> C or NC
            
            if (C) 
            { 
                // Get value
                LDY # iValue
                LDA [ZP.IDX], Y
                STA ZP.TOP0
                INY
                LDA [ZP.IDX], Y
                STA ZP.TOP1
                
                LDY # iResolved
                LDA [ZP.IDX], Y
                STA ZP.ACCL
                
                break;  // Found with value loaded
            }
            
            // Move to next
            LDY # iNext
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
            
            // Print name (at offset iName)
            CLC
            LDA ZP.IDXL
            ADC # iName
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
            
            LDY # (iValue+1)
            LDA [ZP.IDX], Y
            Print.Hex();
            
            LDY # iValue
            LDA [ZP.IDX], Y
            Print.Hex();
            
            Print.NewLine();
            
            // Move to next
            LDY # iNext
            LDA [ZP.IDX], Y
            STA labelsCurrentL
            INY
            LDA [ZP.IDX], Y
            STA labelsCurrentH
        }
    }
}
