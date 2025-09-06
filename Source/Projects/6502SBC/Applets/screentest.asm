program ScreenFill
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Serial"
    
    const string title = "40x25 Screen Fill Demo\n";
    const string complete = "\nScreen fill complete!\n";
    const string breakMsg = "\nBreak!\n";
    
    // Use actually FREE zero page slots (0x58-0x59 are unallocated)
    const byte rowCounter = 0x58;
    const byte colCounter = 0x59;
    
    Hopper()
    {
        // Print title
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize row counter (25 rows)
        LDA #25
        STA rowCounter
        
        loop  // Outer loop for rows
        {
            // Initialize column counter (40 columns)
            LDA #40
            STA colCounter
            
            loop  // Inner loop for columns
            {
                // Print 'X'
                LDA #'X'
                Print.Char();
                
                // Decrement column counter
                DEC colCounter
                if (Z) { break; }  // Exit inner loop when columns done
            }
            
            // Print newline after each row
            Print.NewLine();
            
            // Check for break key (Ctrl+C)
            Serial.IsBreak();
            if (C) 
            { 
                // User pressed break - exit early
                LDA #(breakMsg % 256)
                STA ZP.STRL
                LDA #(breakMsg / 256)
                STA ZP.STRH
                Print.String();
                break; 
            }
            
            // Decrement row counter
            DEC rowCounter
            if (Z) { break; }  // Exit outer loop when rows done
        }
        
        // Print completion message (only if we didn't break)
        Serial.IsBreak();
        if (NC)  // No break detected
        {
            LDA #(complete % 256)
            STA ZP.STRL
            LDA #(complete / 256)
            STA ZP.STRH
            Print.String();
        }
    }
}
