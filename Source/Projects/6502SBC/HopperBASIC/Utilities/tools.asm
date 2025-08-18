unit Tools // Tools.asm
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Production utilities only - debug functionality moved to Debug.asm
 
    // Print null-terminated string to serial output
    // Input: ZP.STR = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringSTR()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.STR], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 
        
    // Print null-terminated string to serial output
    // Input: ZP.ACC = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringACC()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        loop                // Print each character until null terminator
        {
            LDA [ZP.ACC], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 
    
    // Input: A - character to test
    // Output: C for yes, NC for no
    IsPrintable()
    {
        loop
        {
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (NC)  // <= 126
                {
                    SEC
                    break;
                }
            }
            CLC
            break;
        } // exit loop
    }
    // Print null-terminated string to serial output
    // Input: A - character to print
    // Output: 'A' for printable, 0xXX for unprintable to serial
    // Preserves: Everything
    PrintChar()
    {
        PHA
        loop
        {
            CMP #32
            if (C)  // >= 32
            {
                CMP #127
                if (NC)  // <= 126
                {
                    PHA LDA #'\'' Serial.WriteChar(); PLA
                    Serial.WriteChar();
                    LDA #'\'' Serial.WriteChar();
                    break;
                }
            }
            
            // Not printable
            PHA LDA #'0' Serial.WriteChar(); LDA #'x' Serial.WriteChar(); PLA
            Serial.HexOut();
            
            break;
        } // exit loop   
        PLA
    }
    
    // Print null-terminated string to serial output
    // Input: ZP.TOP = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringTOP()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.TOP], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 
    
    // Print null-terminated string to serial output
    // Input: ZP.NEXT = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringNEXT()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.NEXT], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    }
    
    // Print null-terminated string to serial output
    // Input: ZP.IDY = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringIDY()
    {
        PHA
        PHY
        
        LDY #0              // Initialize string index
        
        loop                // Print each character until null terminator
        {
            LDA [ZP.IDY], Y // Load character from string
            if (Z) { break; } // Exit if null terminator found
            
            Serial.WriteChar(); // Print the character
            INY             // Move to next character
        }
        
        PLY
        PLA
    } 

    // Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    {
        PHP  // Push processor status (including carry flag)
        PHA
        LDA #'\n' 
        Serial.WriteChar();
        PLA
        PLP  // Pull processor status (restore carry flag)
    }
    
    // Write character preserving carry flag
    // Input: A = character to output
    // Output: Character printed to serial
    // Preserves: Everything
    COut()
    {
        PHP  // Push processor status (including carry flag)
        Serial.WriteChar();
        PLP  // Pull processor status (restore carry flag)
    }
    
    
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOP = 16-bit number to print (0-65535)
    //        ZP.LTOP0-3 = 32 bit signed LONG (if ZP.TOPT == BASICType.LONG)
    //        ZP.TOPT = type (for signed/unsigned determination)
    // Output: Decimal number printed to serial
    // Preserves: Everything
    PrintDecimal()
    {
        PHA
        LDA ZP.TOPT
        PHA
        
        loop
        {
            LDA ZP.TOPT
            Long.IsLong();
            if (NC)
            {
                Long.ToLong(); // ZP.TOPL, ZP.TOPH, ZP.TOPT -> ZP.LTOP0-3, ZP.TOPT
                if (NC)
                {
                    break;
                }
            }
            Long.Print();
            break;
        } // single exit
        
        PLA
        STA ZP.TOPT
        PLA
    }
    
    // Copy bytes from source to destination
    // Input: ZP.FSOURCEADDRESS = source pointer
    //        ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to copy (16-bit)
    // Output: Data copied from source to destination
    // Preserves: Input parameters (saves/restores ZP.F* variables)
    CopyBytes()
    {
        PHA
        PHY
        
        // Save the input parameters that we'll modify during the copy
        LDA ZP.FSOURCEADDRESSL
        PHA
        LDA ZP.FSOURCEADDRESSH
        PHA
        LDA ZP.FDESTINATIONADDRESSL
        PHA
        LDA ZP.FDESTINATIONADDRESSH
        PHA
        LDA ZP.FLENGTHL
        PHA
        LDA ZP.FLENGTHH
        PHA
        
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to copy
            
            // Copy one byte: *FDESTINATIONADDRESS = *FSOURCEADDRESS
            LDY #0
            LDA [ZP.FSOURCEADDRESS], Y
            STA [ZP.FDESTINATIONADDRESS], Y
            
            // Increment FSOURCEADDRESS
            INC ZP.FSOURCEADDRESSL
            if (Z)
            {
                INC ZP.FSOURCEADDRESSH
            }
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
        
        // Restore the input parameters in reverse order
        PLA
        STA ZP.FLENGTHH
        PLA
        STA ZP.FLENGTHL
        PLA
        STA ZP.FDESTINATIONADDRESSH
        PLA
        STA ZP.FDESTINATIONADDRESSL
        PLA
        STA ZP.FSOURCEADDRESSH
        PLA
        STA ZP.FSOURCEADDRESSL
        
        PLY
        PLA
    }
    
    // Zero bytes at destination address
    // Input: ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to zero (16-bit)
    // Output: Memory zeroed at destination
    // Preserves: Input parameters (saves/restores ZP.F* variables)
    ZeroBytes()
    {
        PHA
        PHY
        
        // Save the input parameters that we'll modify during zeroing
        LDA ZP.FDESTINATIONADDRESSL
        PHA
        LDA ZP.FDESTINATIONADDRESSH
        PHA
        LDA ZP.FLENGTHL
        PHA
        LDA ZP.FLENGTHH
        PHA
        
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to zero
            
            // Write zero: *FDESTINATIONADDRESS = 0
            LDA #0
            LDY #0
            STA [ZP.FDESTINATIONADDRESS], Y
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
        
        // Restore the input parameters in reverse order
        PLA
        STA ZP.FLENGTHH
        PLA
        STA ZP.FLENGTHL
        PLA
        STA ZP.FDESTINATIONADDRESSH
        PLA
        STA ZP.FDESTINATIONADDRESSL
        
        PLY
        PLA
    }
    
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything (saves/restores ZP.TOP)
    StringLength()
    {
        PHX
        PHY
        
        // Use stack to preserve caller's ZP.TOP instead of corrupting it
        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH  
        PHA
        
        STX ZP.TOPL
        STY ZP.TOPH
        
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y
            if (Z) { break; }
            INY
#ifdef DEBUG
            if (Z) // wrapped around from 0xFF to 0x00
            {
                LDA # 0x04  Debug.Crash(); // runaway StringLength() calculation
            }
#endif
        }
        
        // Restore caller's ZP.TOP
        PLA
        STA ZP.TOPH
        PLA
        STA ZP.TOPL
        
        TYA  // Length in A
        
        PLY
        PLX
    }
    
    // Calculate string length and return as 16-bit value
    // Input: ZP.TOP = pointer to null-terminated string
    // Output: ZP.TOP = string length as 16-bit value (TOPL/TOPH)
    // Modifies: A, X, Y
    // Note: Returns 0 for null pointer
    StringLengthTOP()
    {
        PHA
        PHX
        PHY
        
        // Check for null pointer
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            // Null pointer - return length 0
            STZ ZP.TOPL
            STZ ZP.TOPH
            PLY
            PLX
            PLA
            return;
        }
        
        // Call existing StringLength with X,Y registers
        LDX ZP.TOPL
        LDY ZP.TOPH
        StringLength();  // Returns length in A, preserves X,Y
        
        // Store length as 16-bit value in ZP.TOP
        STA ZP.TOPL
        STZ ZP.TOPH  // Length fits in byte, so high byte is 0
        
        PLY
        PLX
        PLA
    }
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    StringCompare()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.TOP], Y
            CMP [ZP.NEXT], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.TOP], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            
            INY
        }
        
        PLY
        PLA
    } 
    
    // Compare two strings using throwaway registers
    // Input: ZP.STR = first string, ZP.STR2 = second string
    // Output: C set if match, NC if different
    // Preserves: Everything
    StringCompareSTR()
    {
        PHA
        PHY
        
        LDY #0
        loop
        {
            // First check pointer equality (optimization)
            LDA ZP.STRL
            CMP ZP.STR2L
            if (Z)
            {
                LDA ZP.STRH
                CMP ZP.STR2H
                if (Z)
                {
                    // Same pointer = equal strings
                    SEC // Set C for match
                    break;
                }
            }
            LDA [ZP.STR], Y
            CMP [ZP.STR2], Y
            if (NZ) 
            { 
                // Characters don't match
                CLC  // Set NC for mismatch
                break; 
            }
            
            // Characters matched - check if we hit end of string
            LDA [ZP.STR], Y
            if (Z) 
            { 
                // Both chars are null (since they matched in CMP above)
                // Strings are equal
                SEC  // Set C for match
                break; 
            }
            INY
        }
        PLY
        PLA
    } 
    // number of spaces to print in X
    Spaces()
    {
        PHA
        loop
        {
            LDA #' '
            Serial.WriteChar();
            DEX
            if (Z) // Set Z - counter reached 0
            { 
                break; 
            }
        }
        PLA
    }
}
