unit Symbols
{
    uses "../System/String"
    
    const byte symbolsSlots = 0x80; // 0x80..0x8F
    
    // Symbol table structure (linked list)
    const uint symbolHead    = symbolsSlots+0;  // Head of linked list
    const byte symbolHeadL   = symbolsSlots+0;
    const byte symbolHeadH   = symbolsSlots+1;
    
    // Temp workspace for traversal
    const uint symbolCurrent = symbolsSlots+2;  // Current node pointer
    const byte symbolCurrentL = symbolsSlots+2;
    const byte symbolCurrentH = symbolsSlots+3;
    
    const byte symbolType     = symbolsSlots+4;
    
    const string typeNames0 = "CONST ";
    const string typeNames1 = "DATA  ";
    const string typeNames2 = "FUNC  ";
    const string separator  = " = ";
    const string hexPrefix  = "0x";
    
    // Node structure:
    // +0: next pointer (16-bit)
    // +2: value (16-bit)
    // +4: type (8-bit)
    // +5: name string (null-terminated, inline)
    
    // Symbol types
    flags SymbolType
    {
        Constant = 0b00100000,
        Data     = 0b01000000,
        Function = 0b01100000,
        
        Mask     = 0b11100000,
    }
    
    Initialize()
    {
        STZ symbolHeadL
        STZ symbolHeadH
        SEC
    }
    
    Dispose()
    {
        loop
        {
            LDA symbolHeadL
            ORA symbolHeadH
            if (Z) { break; }  // Empty list
            
            // Save next pointer before freeing
            LDA symbolHeadL
            STA ZP.IDXL
            LDA symbolHeadH
            STA ZP.IDXH
            LDY #0
            LDA [ZP.IDX], Y  // Next low
            STA symbolCurrentL
            INY
            LDA [ZP.IDX], Y  // Next high
            STA symbolCurrentH
            
            // Free current node
            Memory.Free();
            
            // Move to next
            LDA symbolCurrentL
            STA symbolHeadL
            LDA symbolCurrentH
            STA symbolHeadH
        }
        SEC
    }
    
    // name in STR, value in TOP0..1, type in A
    // C if ok, NC if failure
    Add()
    {
        STA symbolType
        
        LDA ZP.TOP0
        STA ZP.ACCL
        LDA ZP.TOP1
        STA ZP.ACCH
        
        loop
        {
            // Check if already exists
            FindSymbol();
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
            LDA symbolHeadL
            STA [ZP.IDX], Y
            INY
            LDA symbolHeadH
            STA [ZP.IDX], Y
            
            // Value
            INY
            LDA ZP.TOP0
            STA [ZP.IDX], Y
            INY
            LDA ZP.TOP1
            STA [ZP.IDX], Y
            
            // Type
            INY
            LDA symbolType
            STA [ZP.IDX], Y
            
            LDA ZP.STRL
            STA ZP.IDYL
            LDA ZP.STRH
            STA ZP.IDYH
            
            // Copy string inline
            INY  // Y=5
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
            STA symbolHeadL
            LDA ZP.IDXH
            STA symbolHeadH
            
            SEC
            break;
        } // single exit
    }
    
    // name in STR
    // C if found, NC if not
    // value in TOP0..1, type in A
    FindSymbol()
    {
        // Start at head
        LDA symbolHeadL
        STA symbolCurrentL
        LDA symbolHeadH
        STA symbolCurrentH
        
        loop
        {
            // Check end of list
            LDA symbolCurrentL
            ORA symbolCurrentH
            if (Z)  // Not found
            {
                CLC
                break;
            }
            
            // Point to current node
            LDA symbolCurrentL
            STA ZP.IDXL
            LDA symbolCurrentH
            STA ZP.IDXH
            
            // Compare inline string at offset 5
            CLC
            LDA ZP.IDXL
            ADC #5
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
                
                // Get type
                INY
                LDA [ZP.IDX], Y
                SEC
                break;  // Found with value loaded
            }
            
            // Move to next
            LDY #0
            LDA [ZP.IDX], Y
            STA symbolCurrentL
            INY
            LDA [ZP.IDX], Y
            STA symbolCurrentH
        }

    }
    
    // Debug function to dump symbol table
    Dump()
    {
        Print.NewLine();
        
        // Start at head
        LDA symbolHeadL
        STA symbolCurrentL
        LDA symbolHeadH
        STA symbolCurrentH
        
        loop
        {
            // Check end of list
            LDA symbolCurrentL
            ORA symbolCurrentH
            if (Z) { break; }  // End of list
            
            // Point to current node
            LDA symbolCurrentL
            STA ZP.IDXL
            LDA symbolCurrentH
            STA ZP.IDXH
            
            // Print type
            LDY #4
            LDA [ZP.IDX], Y  // Get type
            AND # SymbolType.Mask
            switch (A)
            {
                case SymbolType.Constant:
                {
                    LDA #(typeNames0 % 256)
                    STA ZP.STRL
                    LDA #(typeNames0 / 256)
                    STA ZP.STRH
                }
                case SymbolType.Data:
                {
                    LDA #(typeNames1 % 256)
                    STA ZP.STRL
                    LDA #(typeNames1 / 256)
                    STA ZP.STRH
                }
                case SymbolType.Function:
                {
                    LDA #(typeNames2 % 256)
                    STA ZP.STRL
                    LDA #(typeNames2 / 256)
                    STA ZP.STRH
                }
            }
            Print.String();
            Print.Space();
            
            // Print name (at offset 5)
            CLC
            LDA ZP.IDXL
            ADC #5
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

            LDY #4
            LDA [ZP.IDX], Y  // Get type 
            AND # (NumberType.Byte | NumberType.Char)
            if (Z)
            {
                LDY #3
                LDA [ZP.IDX], Y  // Value high
                Print.Hex();
            }
            LDY #2
            LDA [ZP.IDX], Y  // Value low
            Print.Hex();
            
            Print.NewLine();
            
            // Move to next
            LDY #0
            LDA [ZP.IDX], Y
            STA symbolCurrentL
            INY
            LDA [ZP.IDX], Y
            STA symbolCurrentH
        }
    }
}
