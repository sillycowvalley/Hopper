unit ComparisonInstructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // Private helper: Signed 16-bit comparison of NEXT vs TOP
    // Input: ZP.NEXT, ZP.TOP (16-bit signed values)
    // Output: ZP.ACC = comparison result:
    //         0 = NEXT < TOP
    //         1 = NEXT == TOP  
    //         2 = NEXT > TOP
    // Modifies: ZP.ACC, processor flags
    doSignedCompare()
    {
        PHA
        
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            // High bytes equal, compare low bytes unsigned
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDA #1      // NEXT == TOP
                STA ZP.ACC
            }
            else if (C)
            {
                LDA #2      // NEXT > TOP
                STA ZP.ACC
            }
            else
            {
                LDA #0      // NEXT < TOP
                STA ZP.ACC
            }
        }
        else
        {
            // High bytes different - signed comparison needed
            PHP             // Save comparison flags
            LDA ZP.NEXTH    // Get NEXT high byte
            EOR ZP.TOPH     // XOR with TOP high byte to check if signs differ
            if (MI)         // Signs differ
            {
                BIT ZP.NEXTH
                if (MI)     // NEXT is negative, TOP is positive
                {
                    LDA #0  // NEXT < TOP (negative < positive)
                }
                else        // NEXT is positive, TOP is negative
                {
                    LDA #2  // NEXT > TOP (positive > negative)
                }
                STA ZP.ACC
                PLP             // Clean up stack if we didn't use it
            }
            else
            {
                // Same signs, use the original comparison result
                PLP         // Restore comparison flags
                if (C)      // NEXT >= TOP (unsigned when same signs)
                {
                    LDA #2  // NEXT > TOP
                }
                else
                {
                    LDA #0  // NEXT < TOP
                }
                STA ZP.ACC
            }
        }
        PLA
    }
    
    // Private helper: Unsigned 16-bit comparison of NEXT vs TOP
    // Input: ZP.NEXT, ZP.TOP (16-bit unsigned values)
    // Output: ZP.ACC = comparison result:
    //         0 = NEXT < TOP
    //         1 = NEXT == TOP  
    //         2 = NEXT > TOP
    // Modifies: ZP.ACC, processor flags
    doUnsignedCompare()
    {
        PHA
        
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            // High bytes equal, compare low bytes
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDA #1      // NEXT == TOP
                STA ZP.ACC
            }
            else if (C)
            {
                LDA #2      // NEXT > TOP
                STA ZP.ACC
            }
            else
            {
                LDA #0      // NEXT < TOP
                STA ZP.ACC
            }
        }
        else
        {
            // High bytes different
            if (C)
            {
                LDA #2      // NEXT > TOP
                STA ZP.ACC
            }
            else
            {
                LDA #0      // NEXT < TOP
                STA ZP.ACC
            }
        }
        
        PLA
    }
    
    // Check if two types are compatible for ordering comparison operations
    // Input: ZP.NEXTT = left operand type, ZP.TOPT = right operand type
    //        ZP.NEXT = left value, ZP.TOP = right value (for WORD/INT range check)
    // Output: C set if compatible, NC set if TYPE MISMATCH
    //         ZP.NEXTT = result type (promoted type for the comparison)
    // Modifies: ZP.NEXTT (promoted type), processor flags
    // Note: Rejects BIT types, handles INT/WORD promotion with signed comparison logic
    CheckOrderingComparisonCompatibility()
    {
        PHA
        PHX
        
        loop
        {
            // Ordering comparisons reject BIT types
            LDA ZP.NEXTT
            CMP #BasicType.BIT
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            
            LDA ZP.TOPT
            CMP #BasicType.BIT
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            
            // Same type check
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (Z)
            {
                SEC  // Set C - compatible
                break;
            }
            
            // BYTE vs INT - always compatible, promotes to INT
            LDA ZP.NEXTT
            CMP #BasicType.BYTE
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    LDA #BasicType.INT
                    STA ZP.NEXTT
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // INT vs BYTE - always compatible, promotes to INT
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.BYTE
                if (Z)
                {
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // BYTE vs WORD - always compatible, promotes to WORD
            LDA ZP.NEXTT
            CMP #BasicType.BYTE
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    LDA #BasicType.WORD
                    STA ZP.NEXTT
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // WORD vs BYTE - always compatible, promotes to WORD
            LDA ZP.NEXTT
            CMP #BasicType.WORD
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.BYTE
                if (Z)
                {
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // WORD vs INT - comparison-specific promotion
            LDA ZP.NEXTT
            CMP #BasicType.WORD
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    // Check INT sign to determine promotion direction
                    BIT ZP.TOPH  // Check sign bit of INT value (ZP.TOP)
                    if (MI)      // Negative INT
                    {
                        // INT < 0: check if WORD >= 32768
                        LDA ZP.NEXTH  // Check WORD high byte
                        CMP #0x80     // Compare with 32768 high byte
                        if (C)        // WORD >= 32768
                        {
                            if (Z)    // Exactly 32768?
                            {
                                LDA ZP.NEXTL
                                if (Z)  // Exactly 32768
                                {
                                    CLC  // Set NC - incompatible
                                    break;
                                }
                            }
                            // WORD > 32767
                            CLC  // Set NC - incompatible
                            break;
                        }
                        // WORD <= 32767: promote to INT for signed comparison
                        LDA #BasicType.INT
                        STA ZP.NEXTT
                        SEC  // Set C - compatible
                        break;
                    }
                    else
                    {
                        // INT >= 0: promote to WORD for unsigned comparison
                        LDA #BasicType.WORD
                        STA ZP.NEXTT
                        SEC  // Set C - compatible
                        break;
                    }
                }
            }
            
            // INT vs WORD - comparison-specific promotion
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    // Check INT sign to determine promotion direction
                    BIT ZP.NEXTH  // Check sign bit of INT value (ZP.NEXT)
                    if (MI)       // Negative INT
                    {
                        // INT < 0: check if WORD >= 32768
                        LDA ZP.TOPH   // Check WORD high byte
                        CMP #0x80     // Compare with 32768 high byte
                        if (C)        // WORD >= 32768
                        {
                            if (Z)    // Exactly 32768?
                            {
                                LDA ZP.TOPL
                                if (Z)  // Exactly 32768
                                {
                                    CLC  // Set NC - incompatible
                                    break;
                                }
                            }
                            // WORD > 32767
                            CLC  // Set NC - incompatible
                            break;
                        }
                        // WORD <= 32767: promote to INT for signed comparison
                        LDA #BasicType.INT
                        STA ZP.NEXTT
                        SEC  // Set C - compatible
                        break;
                    }
                    else
                    {
                        // INT >= 0: promote to WORD for unsigned comparison
                        LDA #BasicType.WORD
                        STA ZP.NEXTT
                        SEC  // Set C - compatible
                        break;
                    }
                }
            }
            
            // No other compatibility rules matched
            CLC  // Set NC - incompatible
            break;
        }
        
        PLX
        PLA
    }
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Equal()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #0  // Equality comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            // Types are compatible, do the comparison
            LDX #0  // Assume not equal
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDA ZP.NEXTH
                CMP ZP.TOPH
                if (Z)
                {
                    LDX #1  // Equal
                }
            }
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    NotEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #0  // Equality comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            // Types are compatible, do the comparison
            LDX #1  // Assume not equal
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDA ZP.NEXTH
                CMP ZP.TOPH
                if (Z)
                {
                    LDX #0  // Actually equal
                }
            }
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessThan()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            CheckOrderingComparisonCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Check if result is NEXT < TOP (ZP.ACC == 0)
            LDX #0          // Assume false
            LDA ZP.ACC
            if (Z)          // Result was 0 (NEXT < TOP)
            {
                LDX #1      // True
            }
            
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterThan()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            CheckOrderingComparisonCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Check if result is NEXT > TOP (ZP.ACC == 2)
            LDX #0          // Assume false
            LDA ZP.ACC
            CMP #2
            if (Z)          // Result was 2 (NEXT > TOP)
            {
                LDX #1      // True
            }
            
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            CheckOrderingComparisonCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Check if result is NEXT <= TOP (ZP.ACC == 0 OR ZP.ACC == 1)
            LDX #0          // Assume false
            LDA ZP.ACC
            CMP #2
            if (NZ)         // Result was not 2 (so NEXT <= TOP)
            {
                LDX #1      // True
            }
            
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            CheckOrderingComparisonCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                
                Messages.StorePC(); // 6502 PC -> IDY
                
                break;
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Check if result is NEXT >= TOP (ZP.ACC == 1 OR ZP.ACC == 2)
            LDX #0          // Assume false
            LDA ZP.ACC
            if (NZ)         // Result was not 0 (so NEXT >= TOP)
            {
                LDX #1      // True
            }
            
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }
}
