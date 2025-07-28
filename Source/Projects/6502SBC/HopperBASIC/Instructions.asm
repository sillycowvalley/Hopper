unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // Check if RHS value is compatible with LHS type
    // Input: ZP.TOP = RHS value, ZP.TOPT = RHS type, ZP.NEXTT = LHS type
    // Output: C set if compatible, NC if incompatible
    // Modifies: Processor flags only
    CheckRHSTypeCompatibility()
    {
        PHA  // Preserve A register
        
        loop // Single exit point for all compatibility checks
        {
            LDA ZP.NEXTT
            CMP #BasicType.BIT
            if (NZ)
            {
                // For non-BIT types, use general type compatibility checking
                LDA #1  // Arithmetic operation mode
                CheckTypeCompatibility();
                // Carry flag already set by CheckTypeCompatibility
                break;
            }
            
            // Special case for BIT type - only allows values 0 or 1
            LDA ZP.TOPH
            if (NZ)  // High byte must be 0
            {
                CLC  // Incompatible
                break;
            }
            
            LDA ZP.TOPL
            CMP #0
            if (Z)
            {
                SEC  // Compatible (value is 0)
                break;
            }
            
            CMP #1
            if (Z)
            {
                SEC  // Compatible (value is 1)
                break;
            }
            
            CLC  // Incompatible (value not 0 or 1)
            break;
        } // end of single exit loop
        
        PLA  // Restore A register
    }
    
    // Check if two types are compatible for operations
    // Input: ZP.NEXTT = left operand type, ZP.TOPT = right operand type
    //        ZP.NEXT = left value, ZP.TOP = right value (for WORD/INT range check)
    //        ZP.ACCT = operation mode:
    //          0 = Equality comparison (=, <>) - BIT types allowed, result is BIT
    //          1 = Arithmetic (+, -, *, /, %) - BIT types rejected, result is promoted numeric type
    //          2 = Bitwise/logical (AND, OR) - BIT types allowed, result is operand type or promoted
    //          3 = Ordering comparison (<, >, <=, >=) - BIT types rejected, result is BIT
    // Output: C set if compatible, NC set if TYPE MISMATCH
    //         ZP.NEXTT = result type (updated based on operation mode and type promotion)
    // Modifies: processor flags
    CheckTypeCompatibility()
    {
        PHA
        PHX
        
        // Save original ZP.ACCT value
        LDX ZP.ACCT
        
        // Store operation mode in ZP.ACCT for internal use
        STA ZP.ACCT
        
        loop // Single exit point for all compatibility checks
        {
            // Mode-specific type restrictions
            CMP #1  // Arithmetic operations
            if (Z)
            {
                // Arithmetic: reject BIT types
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
            }
            else
            {
                LDA ZP.ACCT  // Get operation mode again
                CMP #3  // Ordering comparison operations
                if (Z)
                {
                    // Ordering comparison: reject BIT types
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
                }
                // Modes 0 (equality) and 2 (bitwise/logical) allow BIT types
            }
            
            // Check for ARRAY and STRING types - not supported in Phase 1
            LDA ZP.NEXTT
            CMP #BasicType.ARRAY
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            CMP #BasicType.STRING
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            
            LDA ZP.TOPT
            CMP #BasicType.ARRAY
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            CMP #BasicType.STRING
            if (Z)
            {
                CLC  // Set NC - type mismatch
                break;
            }
            
            // If both types are the same, check compatibility and set result type
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (Z)
            {
                // Set result type based on operation mode
                LDA ZP.ACCT  // Get operation mode
                switch (A)
                {
                    case 0:  // Equality comparison
                    case 3:  // Ordering comparison
                    {
                        LDA #BasicType.BIT
                        STA ZP.NEXTT  // Result type = BIT
                    }
                    case 1:  // Arithmetic operations
                    case 2:  // Bitwise/logical operations
                    {
                        // Result type = operand type (no change needed)
                    }
                }
                SEC  // Set C - compatible
                break;
            }
            
            // Different types - check specific compatibility rules and apply type promotion
            
            // BYTE vs INT - always compatible, promotes to INT
            LDA ZP.NEXTT
            CMP #BasicType.BYTE
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
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
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
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
                    STA ZP.NEXTT  // Promote to WORD for all operations except comparisons
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
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
                    // WORD is already the promoted type (no change to ZP.NEXTT needed)
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // WORD vs INT - compatible only if INT >= 0 (non-negative), promotes to WORD
            LDA ZP.NEXTT
            CMP #BasicType.WORD
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    // Check if INT (right operand) is non-negative
                    BIT ZP.TOPH  // Test sign bit of INT value
                    if (MI)      // High bit set - negative INT
                    {
                        CLC      // Set NC - type mismatch
                        break;
                    }
                    // WORD is already the promoted type (no change to ZP.NEXTT needed)
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC          // Set C - compatible
                    break;
                }
            }
            
            // INT vs WORD - compatible only if WORD <= 32767 (fits in signed range), promotes to INT
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    // Check if WORD (right operand) fits in signed INT range (<= 32767)
                    BIT ZP.TOPH  // Test high bit of WORD value
                    if (MI)      // High bit set - value >= 32768, too large for INT
                    {
                        CLC      // Set NC - type mismatch
                        break;
                    }
                    // WORD fits in INT range, INT is already the target type (no change to ZP.NEXTT needed)
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC          // Set C - compatible
                    break;
                }
            }
            
            // INT vs WORD - compatible only if INT >= 0 (non-negative), promotes to WORD
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    // Check if INT (left operand) is non-negative
                    BIT ZP.NEXTH  // Test high bit of INT value
                    if (MI)       // High bit set - negative INT
                    {
                        CLC       // Set NC - type mismatch
                        break;
                    }
                    LDA #BasicType.WORD
                    STA ZP.NEXTT  // Promote to WORD
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC           // Set C - compatible
                    break;
                }
            }
            
            // All other combinations are incompatible (STRING with numbers, etc.)
            CLC  // Set NC - type mismatch
            break;
        } // end of single exit loop
        
        // Restore ZP.ACCT
        STX ZP.ACCT
        PLX
        PLA
    }
    
    // Set result type for mixed-type operations
    // Input: A = operation mode (0=equality, 1=arithmetic, 2=bitwise, 3=ordering)
    //        ZP.NEXTT = promoted operand type
    // Output: ZP.NEXTT = final result type
    setResultTypeForMixedOperation()
    {
        PHA  // Preserve A register
        
        switch (A)
        {
            case 0:  // Equality comparison
            case 3:  // Ordering comparison  
            {
                LDA #BasicType.BIT
                STA ZP.NEXTT  // Result type = BIT
            }
            case 1:  // Arithmetic operations
            case 2:  // Bitwise/logical operations
            {
                // Result type = promoted operand type (ZP.NEXTT already set correctly)
            }
        }
        
        PLA  // Restore A register
    }
    
    // Shared subtraction logic helper
    // Input: ZP.NEXT = left operand, ZP.TOP = right operand (both popped from stack)
    //        ZP.NEXTT = left type, ZP.TOPT = right type
    // Output: Result pushed to stack
    // Munts: ZP.NEXT, ZP.NEXTT, stack
    // Error: Sets ZP.LastError if type mismatch
    subShared()
    {
        LDA #1  // Arithmetic operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.NEXTH
        
        LDA ZP.NEXTT       
        Stacks.PushNext();
    }
    
    // Addition operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Sum pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT
    // Error: Sets ZP.LastError if type mismatch
    Addition()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        CLC
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.NEXTH
        
        LDA ZP.NEXTT
        Stacks.PushNext();
    }
        
    // Subtraction operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Difference (left - right) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT
    // Error: Sets ZP.LastError if type mismatch
    Subtraction()
    {
        Stacks.PopTopNext();
        subShared();
    }
    
    // Handle sign extraction for signed operations
    // Input: ZP.NEXT = left operand, ZP.TOP = right operand
    // Output: ZP.FSIGN = count of negative operands (0, 1, or 2)
    //         ZP.NEXT and ZP.TOP converted to positive values
    doSigns()
    {
        PHA  // Preserve A
        PHX  // Preserve X
        
        LDX #0 
        LDA ZP.NEXTH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the negative values
            IntMath.NegateNext(); // NEXT = -NEXT
        }
        LDA ZP.TOPH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the negative values
            IntMath.NegateTop(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
        
        PLX  // Restore X
        PLA  // Restore A
    }
    
    // Multiplication operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Product pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.FSIGN
    // Error: Sets ZP.LastError if type mismatch
    Multiply()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - handle signed multiplication
            doSigns();
            IntMath.MulShared();
            LDA ZP.FSIGN     // load the sign count
            CMP #1
            if (Z)           // one negative (not zero or two)
            {
                IntMath.NegateTop(); // TOP = -TOP
            }
        }
        else
        {
            // WORD or BYTE - unsigned multiplication
            IntMath.MulShared();
            LDA ZP.TOPT
        }
        
        LDA ZP.NEXTT
        Stacks.PushTop();
    }
    
    // Division operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Quotient (left / right) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.FSIGN
    // Error: Sets ZP.LastError if type mismatch or division by zero
    Divide()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - handle signed division
            doSigns(); // munts X
            IntMath.UtilityDiv();
            
            LDA ZP.FSIGN     // load the sign count
            CMP #1
            if (Z)           // one negative (not zero or two)
            {
                IntMath.NegateNext(); // NEXT = -NEXT
            }
        }
        else
        {
            // BYTE or WORD - unsigned division
            // NEXT = NEXT / TOP
            IntMath.UtilityDiv();
        }
        LDA ZP.NEXTT
        Stacks.PushNext();
    }
    
    // Modulo operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Remainder (left % right) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.ACC, ZP.ACCT, ZP.TOPT, ZP.NEXTT, ZP.FSIGN
    // Error: Sets ZP.LastError if type mismatch or division by zero
    Modulo()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - handle signed modulo
            doSigns();
        }
        // ACC = NEXT % TOP
        IntMath.DivMod();
            
        LDA ZP.NEXTT
        STA ZP.ACCT
        Stacks.PushACC(); // munts Y, A
    }
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    Equal()
    {
        Stacks.PopTopNext();  // Gets both values and their types
        
        LDA #0  // Equality comparison operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
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
        Stacks.PushX();  // Push result (X) with BIT type
    }
    
    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    NotEqual()
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
            return;
        }
        
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
        Stacks.PushX(); // X as BIT type
    }
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right)
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    LessThan()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - signed comparison: NEXT < TOP?
            // Calculate TOP - NEXT and check if result > 0
            SEC
            LDA ZP.TOPL
            SBC ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.TOPH
            SBC ZP.NEXTH
            STA ZP.TOPH
            
            ASL           // sign bit into carry
            
            LDX #0  // Assume NEXT >= TOP
            loop
            {
                if (C) { break; }  // Negative result means NEXT >= TOP
                // Zero or positive result
                LDA ZP.TOPL
                ORA ZP.TOPH
                if (Z)
                {
                    break;  // Zero result means NEXT == TOP
                }
                LDX #1  // Positive result means NEXT < TOP
                break;
            }
        }
        else
        {
            // WORD and BYTE - unsigned comparison: NEXT < TOP?
            LDX #1 // Assume NEXT < TOP
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDA ZP.NEXTL
                CMP ZP.TOPL
            }
            if (C) // NEXT >= TOP?
            {
                LDX #0 // NEXT not < TOP
            }
        }
        Stacks.PushX(); // Result as BIT type
    }
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right)
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    GreaterThan()
    {
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - signed comparison: NEXT > TOP?
            // Calculate NEXT - TOP and check if result > 0
            SEC
            LDA ZP.NEXTL
            SBC ZP.TOPL
            STA ZP.TOPL
            LDA ZP.NEXTH
            SBC ZP.TOPH
            STA ZP.TOPH
            
            ASL           // sign bit into carry
            
            LDX #0  // Assume NEXT <= TOP
            loop
            {
                if (C) { break; }  // Negative result means NEXT <= TOP
                // Zero or positive result
                LDA ZP.TOPL
                ORA ZP.TOPH
                if (Z)
                {
                    break;  // Zero result means NEXT == TOP
                }
                LDX #1  // Positive result means NEXT > TOP
                break;
            }
        }
        else
        {
            // WORD and BYTE - unsigned comparison: testing !(NEXT <= TOP) which is (NEXT > TOP)
            LDX #0 // Assume NEXT <= TOP
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDA ZP.NEXTL
                CMP ZP.TOPL
            }
            if (NZ) // NEXT != TOP?
            {
                if (C) // NEXT < TOP?
                {
                    LDX #1   // NEXT > TOP
                }
            }
        }
        Stacks.PushX(); // Result as BIT type
    }
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right)
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    LessEqual()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - signed comparison: NEXT <= TOP?
            // Calculate TOP - NEXT and check if result >= 0
            SEC
            LDA ZP.TOPL
            SBC ZP.NEXTL
            STA ZP.TOPL
            LDA ZP.TOPH
            SBC ZP.NEXTH
            STA ZP.TOPH
            
            ASL           // sign bit into carry
            
            LDX #0  // Assume NEXT > TOP
            if (NC)
            {
                // Zero or positive result means NEXT <= TOP
                LDX #1
            }
        }
        else
        {
            // WORD and BYTE - unsigned comparison: NEXT <= TOP?
            LDX #1 // Assume NEXT <= TOP
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDA ZP.NEXTL
                CMP ZP.TOPL
            }
            if (NZ) // NEXT != TOP?
            {
                if (C) // NEXT < TOP?
                {
                    LDX #0  // NEXT > TOP
                }
            }
        }
        Stacks.PushX(); // Result as BIT type
    }
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right)
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, X
    // Error: Sets ZP.LastError if type mismatch
    GreaterEqual()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDA ZP.NEXTT
        CMP # BasicType.INT
        if (Z)
        {
            // INT - signed comparison: testing !(NEXT < TOP) which is (NEXT >= TOP)
            LDX #0 // Assume NEXT < TOP
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDA ZP.NEXTL
                CMP ZP.TOPL
            }
            if (C) // NEXT < TOP?
            {
                LDX #1   // NEXT >= TOP
            }
        }
        else
        {
            // WORD and BYTE - unsigned comparison: NEXT >= TOP?
            // Calculate NEXT - TOP and check if result >= 0
            SEC
            LDA ZP.NEXTL
            SBC ZP.TOPL
            STA ZP.TOPL
            LDA ZP.NEXTH
            SBC ZP.TOPH
            STA ZP.TOPH
            
            ASL           // sign bit into carry
            
            LDX #0  // Assume NEXT < TOP
            if (NC)
            {
                // Zero or positive result means NEXT >= TOP
                LDX #1
            }
        }
        Stacks.PushX(); // Result as BIT type
    }
    
    // Bitwise/logical AND operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise AND result pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT
    // Error: Sets ZP.LastError if type mismatch
    And()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #2  // Bitwise/logical operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // NEXT & TOP -> NEXT
        LDA ZP.NEXTL
        AND ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        AND ZP.TOPH
        STA ZP.NEXTH
        
        LDA ZP.NEXTT
        Stacks.PushNext();
    }
    
    // Bitwise/logical OR operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise OR result pushed to stack
    // Munts: Stack, ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT
    // Error: Sets ZP.LastError if type mismatch
    Or()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #2  // Bitwise/logical operation
        CheckTypeCompatibility();
        
        if (NC)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // NEXT | TOP -> NEXT
        LDA ZP.NEXTL
        ORA ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ORA ZP.TOPH
        STA ZP.NEXTH
        
        LDA ZP.NEXTT
        Stacks.PushNext();
    }
    
    // Logical NOT operation (pops one operand, pushes BIT result)
    // Input: Stack contains BIT operand on top
    // Output: Logical NOT result (BIT) pushed to stack
    // Munts: Stack, ZP.TOP, ZP.TOPT, X
    // Error: Sets ZP.LastError if operand is not BIT type
    LogicalNot()
    {
        Stacks.PopTop();
        
        // Check if operand is BIT type (only valid for logical NOT)
        LDA ZP.TOPT
        CMP #BasicType.BIT
        if (NZ)  // Not a BIT type
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        // Logical NOT: convert 0 to 1, and 1 to 0 (assumes BIT type is 0 or 1)
        LDA ZP.TOPL
        EOR # 0x01
        TAX
        
        Stacks.PushX();  // Push result (X) with BIT type
    }
}
