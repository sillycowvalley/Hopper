unit ComparisonInstructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // Signed 16-bit comparison of NEXT vs TOP
    // Input: ZP.NEXT, ZP.TOP (16-bit signed values)
    // Output: ZP.ACC = comparison result (0=NEXT<TOP, 1=NEXT==TOP, 2=NEXT>TOP)
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
    
    // Unsigned 16-bit comparison of NEXT vs TOP
    // Input: ZP.NEXT, ZP.TOP (16-bit unsigned values)
    // Output: ZP.ACC = comparison result (0=NEXT<TOP, 1=NEXT==TOP, 2=NEXT>TOP)
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

    // Check if BIT types are allowed for comparison operation
    // Input: ZP.NEXTT and ZP.TOPT (operand types), A = permission level (1=not allowed, 2=allowed if both BIT)
    // Output: C set if allowed, C clear if not allowed
    checkBITTypes()
    {
        loop
        {
            SEC // assume ok (in case not BIT types)
            
            CMP # 1 // not allowed at all
            if (Z)
            {
                LDA ZP.NEXTT
                CMP # BasicType.BIT
                if (Z)
                {
                    CLC
                    break;    
                } 
                LDA ZP.TOPT
                CMP # BasicType.BIT
                if (Z)
                {   
                    CLC
                    break;
                }
                SEC
                break;
            }
            
            CMP # 2 // allowed if both BIT
            if (Z)
            {
                LDA ZP.NEXTT
                CMP # BasicType.BIT
                if (Z)
                {
                    LDA ZP.TOPT
                    CMP # BasicType.BIT    
                    if (NZ)
                    {
                        CLC   
                        break;
                    }
                }
                LDA ZP.TOPT
                CMP # BasicType.BIT
                if (Z)
                {
                    LDA ZP.NEXTT
                    CMP # BasicType.BIT    
                    if (NZ)
                    {
                        CLC   
                        break;
                    }
                }
                SEC
                break;
            }
            BRK // bad argument : should be #1 or #2
            break;
        } // single exit
    }
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack, C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    Equal()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDX #0          // Assume false
            
            // Check for STRING types first
            LDA ZP.NEXTT
            CMP #BasicType.STRING
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.STRING
                if (Z)
                {
                    // Both are STRING - compare them
                    Tools.StringCompare(); // Now includes pointer optimization
                    if (C)
                    {
                        LDX #1 // Strings match
                    }
                    // continue to PushX at end of loop
                }
                else
                {
                    // STRING vs non-STRING = type mismatch
                    CLC
                    break; // fall through to type mismatch error
                }
            }
            else
            {
                LDA ZP.TOPT
                CMP #BasicType.STRING
                if (Z)
                {
                    // non-STRING vs STRING = type mismatch
                    CLC
                    break; // fall through to type mismatch error
                }
            }
            
            LDA #2 // allowed
            checkBITTypes();
            if (NC)
            {
                break; // fall through to type mismatch error
            }            
            
            // MSB
            LDA ZP.TOPT
            CMP ZP.NEXTT
            if (NZ)
            {
                // Never BIT vs INT|WORD|BYTE since checkBITTypes assured that either both or neither are BIT
                // Never STRING vs INT|WORD|BYTE since STRING is dealt with above
                // So, only checking INT vs WORD|BYTE here
                LDA ZP.NEXTT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.NEXTH
                    if (MI)
                    {
                        LDX #0 // false (NEXT != TOP) since -ve INT cannot be same as any WORD|BYTE
                        break; // continue to PushX at end of loop
                    }
                }
                
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.TOPH
                    if (MI)
                    {
                        LDX #0 // false (NEXT != TOP) since -ve INT cannot be same as any WORD|BYTE
                        break; // continue to PushX at end of loop
                    }
                }
                // continue to PushX at end of loop
            }
            
            // LSB
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                // MSB
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    LDX #1 // true
                }
                // continue to PushX at end of loop
            }   
            Stacks.PushX();
            SEC
            break;
        } // single exit
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
    
    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack, C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    NotEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDX #1          // Assume true (not equal)
            
            // Check for STRING types first
            LDA ZP.NEXTT
            CMP #BasicType.STRING
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.STRING
                if (Z)
                {
                    // Both are STRING - compare them
                    Tools.StringCompare(); // Now includes pointer optimization
                    if (C)
                    {
                        LDX #0 // Strings match, so NOT EQUAL is false
                    }
                    // continue to PushX at end of loop
                }
                else
                {
                    // STRING vs non-STRING = type mismatch
                    CLC
                    break; // fall through to type mismatch error
                }
            }
            else
            {
                LDA ZP.TOPT
                CMP #BasicType.STRING
                if (Z)
                {
                    // non-STRING vs STRING = type mismatch
                    CLC
                    break; // fall through to type mismatch error
                }
            
            }
            LDA #2 // allowed
            checkBITTypes();
            if (NC)
            {
                break; // fall through to type mismatch error
            }
            
            // MSB
            LDA ZP.TOPT
            CMP ZP.NEXTT
            if (NZ)
            {
                // Never BIT vs INT|WORD|BYTE since checkBITTypes assured that either both or neither are BIT
                // Never STRING vs INT|WORD|BYTE since STRING is dealt with above
                // So, only checking INT vs WORD|BYTE here
                LDA ZP.NEXTT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.NEXTH
                    if (MI)
                    {
                        LDX #1 // true (NEXT != TOP) since -ve INT cannot be same as any WORD|BYTE
                        break; // continue to PushX at end of loop
                    }
                }
                
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.TOPH
                    if (MI)
                    {
                        LDX #1 // true (NEXT != TOP) since -ve INT cannot be same as any WORD|BYTE
                        break; // continue to PushX at end of loop
                    }
                }
            }
            
            // LSB
            LDA ZP.TOPL
            CMP ZP.NEXTL
            if (Z)
            {
                // MSB
                LDA ZP.TOPH
                CMP ZP.NEXTH
                if (Z)
                {
                    LDX #0 // false (NEXT == TOP)
                }
                // continue to PushX at end of loop
            }
            Stacks.PushX();
            SEC
            break;
        } // single exit
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
    
    // Check types for integer comparison operations
    // Input: ZP.NEXTT and ZP.TOPT (operand types)
    // Output: X = comparison strategy (1=unsigned, 2=signed, 3=signs comparison), Y = result for signs comparison
    checkINTTypes()
    {
        loop
        {
            LDX #1 // default to  unsigned compare
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    // both INT
                    LDX #2
                    break;
                }
                
                // INT vs WORD
                BIT ZP.NEXTH
                if (MI)
                {
                    // NEXT < 0 
                    BIT ZP.TOPH
                    if (MI)
                    {
                        // TOP > 32657
                        // 0 = NEXT < TOP
                        // 1 = NEXT == TOP  
                        // 2 = NEXT > TOP
                        LDX #3
                        LDY #0
                        break;
                    }
                    LDX #2 // signed
                }
            }
            LDA ZP.TOPT
            CMP #BasicType.INT
            if (Z)
            {
                // WORD vs INT
                BIT ZP.TOPH
                if (MI)
                {
                    // TOP < 0 
                    BIT ZP.NEXTH
                    if (MI)
                    {
                        // NEXT > 32657
                        // 0 = NEXT < TOP
                        // 1 = NEXT == TOP  
                        // 2 = NEXT > TOP
                        LDX #3
                        LDY #2
                        break;
                    }
                    LDX #2 // signed
                }
            }
            
            break;
        }
    }
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    LessThan()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDA #1 // not allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }
            checkINTTypes();
            switch (X)
            {
                case 1:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doUnsignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #0
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                case 2:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doSignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #0
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                case 3:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    LDX #0 // Assume false
                    CPY #0
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                default:
                {
                    CLC  // type mismatch
                    break;
                }
            }
            Stacks.PushX();
            SEC
            break;
        }
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    GreaterThan()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDA #1 // not allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }
            checkINTTypes();
            switch (X)
            {
                case 1:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doUnsignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #2
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                case 2:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doSignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #2
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                case 3:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    LDX #0 // Assume false
                    CPY #2
                    if (Z)
                    {
                        LDX #1 // true
                    }
                }
                default:
                {
                    CLC  // type mismatch
                    break;
                }
            }
            Stacks.PushX();
            SEC
            break;
        }
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    LessEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDA #1 // not allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }
            checkINTTypes();
            switch (X)
            {
                case 1:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doUnsignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #2
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                case 2:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doSignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #2
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                case 3:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    LDX #0 // Assume false
                    CPY #2
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                default:
                {
                    CLC  // type mismatch
                    break;
                }
            }
            Stacks.PushX();
            SEC
            break;
        }
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    GreaterEqual()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Stacks.PopTopNext();
            
            LDA #1 // not allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }
            checkINTTypes();
            switch (X)
            {
                case 1:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doUnsignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #0
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                case 2:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    doSignedCompare();
                    LDX #0     // Assume false
                    LDA ZP.ACCL
                    CMP #0
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                case 3:
                {
                    // 0 = NEXT < TOP
                    // 1 = NEXT == TOP  
                    // 2 = NEXT > TOP
                    LDX #0 // Assume false
                    CPY #0
                    if (NZ)
                    {
                        LDX #1 // true
                    }
                }
                default:
                {
                    CLC  // type mismatch
                    break;
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        }
        
        if (NC)
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            
            BIT ZP.EmulatorPCL // 6502 PC -> EmulatorPC
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
}
