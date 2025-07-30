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

    // Inputs: ZP.NEXTT and ZP.TOPT, A = 0 for not allowed at all, A = 1 for allowed if both are BIT
    // Result: C for BIT types allowed and both are BIT, NC for not allowed
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
            // Pop two operands
            Stacks.PopTopNext();
            
            LDX #0          // Assume false
            
            LDA #2 // allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }            
            LDA ZP.TOPT
            CMP ZP.NEXTT
            if (NZ)
            {
                // Never BIT vs INT|WORD|BYTE since checkBITTypes assured that either both or neither are BIT
                LDA ZP.NEXTT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.NEXTH
                    if (MI)
                    {
                        CLC // NEXT < 0
                        break;
                    }
                }
                
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.TOPH
                    if (MI)
                    {
                        CLC // TOP < 0
                        break;
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
                    LDX #1 // true
                }
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
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
            // Pop two operands
            Stacks.PopTopNext();
            
            LDA #2 // allowed
            checkBITTypes();
            if (NC)
            {
                break;
            }
            
            LDA ZP.TOPT
            CMP ZP.NEXTT
            if (NZ)
            {
                // Never BIT vs INT|WORD|BYTE since checkBITTypes assured that either both or neither are BIT
                LDA ZP.NEXTT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.NEXTH
                    if (MI)
                    {
                        CLC // NEXT < 0
                        break;
                    }
                }
                
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    BIT ZP.TOPH
                    if (MI)
                    {
                        CLC // TOP < 0
                        break;
                    }
                }
            }
            
            LDX #1          // Assume true
            
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
    
    // Inputs: ZP.NEXTT and ZP.TOPT
    // Result: X: 1 = unsigned compare, 2 = signed compare, 3 = 'signs' comparison, 4 = type mismatch, 
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
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
            
            Messages.StorePC(); // 6502 PC -> IDY
            
            CLC
        }
        
        PLY
        PLX
        PLA
    }
}
