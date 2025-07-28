unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // 
    // Legitimate modifications by instruction methods:
    // - Stack interface: ZP.TOP, ZP.TOPT, ZP.NEXT, ZP.NEXTT, ZP.SP, value stack memory
    // - Error handling: ZP.LastErrorL, ZP.LastErrorH (when type mismatches occur)
    // - Type checking: ZP.NEXTT (updated to result type for operations)
    // - Arithmetic scratch: ZP.FSIGN (sign tracking), ZP.UWIDE0-UWIDE3 (via IntMath), ZP.ACC (remainder operations)
    // - Temporary: ZP.ACCT (type checking workspace, always restored)
    
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
    //          2 = Bitwise (&, |) - BIT types rejected, result is promoted numeric type
    //          3 = Ordering comparison (<, >, <=, >=) - BIT types rejected, result is BIT
    //          4 = Logical (AND, OR) - Only BIT types allowed, result is BIT
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
                CMP #2  // Bitwise operations
                if (Z)
                {
                    // Bitwise: reject BIT types (numeric only)
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
                    else
                    {
                        LDA ZP.ACCT  // Get operation mode again
                        CMP #4  // Logical operations
                        if (Z)
                        {
                            // Logical: only BIT types allowed
                            LDA ZP.NEXTT
                            CMP #BasicType.BIT
                            if (NZ)
                            {
                                CLC  // Set NC - type mismatch
                                break;
                            }
                            
                            LDA ZP.TOPT
                            CMP #BasicType.BIT
                            if (NZ)
                            {
                                CLC  // Set NC - type mismatch
                                break;
                            }
                        }
                        // Mode 0 (equality) allows BIT types
                    }
                }
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
                    case 2:  // Bitwise operations
                    case 4:  // Logical operations
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
    // Input: A = operation mode (0=equality, 1=arithmetic, 2=bitwise, 3=ordering, 4=logical)
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
            case 2:  // Bitwise operations
            case 4:  // Logical operations
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
    // Modifies: Stack interface (ZP.NEXT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    subShared()
    {
        PHA  // Preserve A register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Perform subtraction (left - right)
            SEC
            LDA ZP.NEXTL
            SBC ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.NEXTH
            SBC ZP.TOPH
            STA ZP.NEXTH
            
            // Push result to stack
            LDA ZP.NEXTT
            Stacks.PushNext(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLA  // Restore A register
    }
    
    // Addition operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Sum pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Addition()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Perform addition
            CLC
            LDA ZP.NEXTL
            ADC ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.NEXTH
            ADC ZP.TOPH
            STA ZP.NEXTH
            
            // Push result to stack
            LDA ZP.NEXTT
            Stacks.PushNext(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
        
    // Subtraction operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Difference (left - right) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Subtraction()
    {
        PHX  // Preserve X register
        
        // Pop two operands
        Stacks.PopTopNext(); // Pop operands, modifies X
        
        // Delegate to shared subtraction logic
        subShared();
        
        // Restore registers
        PLX  // Restore X register
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
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    //          Arithmetic scratch space (ZP.FSIGN, ZP.UWIDE0-UWIDE3 via IntMath)
    Multiply()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
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
            }
            
            // Push result to stack
            LDA ZP.NEXTT
            Stacks.PushTop(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }    
    
    // Division operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Quotient (left / right) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch or division by zero occurs)
    //          Arithmetic scratch space (ZP.FSIGN, ZP.ACC via IntMath)
    Divide()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - handle signed division
                doSigns();
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
            
            // Push result to stack
            LDA ZP.NEXTT
            Stacks.PushNext(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }    
    // Modulo operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Remainder (left % right) pushed to stack
    // Error: Sets ZP.LastError if type mismatch or division by zero
    Modulo()
    {
        PHA
        PHY
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
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
            Stacks.PushACC(); // Push remainder, modifies Y
            
            break; // Success exit
        }
        
        PLY
        PLA
    }
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Equal()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #0  // Equality comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
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
            Stacks.PushX(); // Push result (X) with BIT type, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    NotEqual()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #0  // Equality comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
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
            Stacks.PushX(); // X as BIT type, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessThan()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison: NEXT < TOP?
                LDX #0  // Assume NEXT >= TOP (not less than)
                LDA ZP.NEXTH
                CMP ZP.TOPH
                if (Z)
                {
                    // High bytes equal, compare low bytes unsigned
                    LDA ZP.NEXTL
                    CMP ZP.TOPL
                    if (C)  // NEXT < TOP
                    {
                        LDX #1  // NEXT < TOP
                    }
                }
                else
                {
                    // High bytes different - need signed comparison
                    EOR ZP.TOPH    // XOR to check if signs differ
                    if (MI)        // Signs differ
                    {
                        // If signs differ, check NEXT's sign
                        BIT ZP.NEXTH
                        if (MI)    // NEXT is negative, TOP is positive
                        {
                            LDX #1  // NEXT < TOP
                        }
                        // else NEXT is positive, TOP is negative: NEXT > TOP (X stays 0)
                    }
                    else
                    {
                        // Same signs, use carry from high byte comparison
                        if (C)     // NEXT >= TOP (unsigned when same signs)
                        {
                            // X stays 0 (NEXT >= TOP)
                        }
                        else
                        {
                            LDX #1  // NEXT < TOP
                        }
                    }
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
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterThan()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison: NEXT > TOP?
                LDX #0  // Assume NEXT <= TOP (not greater than)
                LDA ZP.NEXTH
                CMP ZP.TOPH
                if (Z)
                {
                    // High bytes equal, compare low bytes unsigned
                    LDA ZP.NEXTL
                    CMP ZP.TOPL
                    if (NZ)     // NEXT != TOP?
                    {
                        if (C)  // NEXT >= TOP, and we know NEXT != TOP, so NEXT > TOP
                        {
                            LDX #1  // NEXT > TOP
                        }
                    }
                }
                else
                {
                    // High bytes different - need signed comparison
                    EOR ZP.TOPH    // XOR to check if signs differ
                    if (MI)        // Signs differ
                    {
                        // If signs differ, check TOP's sign  
                        BIT ZP.TOPH
                        if (MI)    // TOP is negative, NEXT is positive
                        {
                            LDX #1  // NEXT > TOP
                        }
                        // else TOP is positive, NEXT is negative: NEXT < TOP (X stays 0)
                    }
                    else
                    {
                        // Same signs, use carry from high byte comparison
                        if (C)     // NEXT >= TOP (unsigned when same signs)
                        {
                            LDX #1  // NEXT > TOP (since high bytes differ)
                        }
                    }
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
                    if (C) // NEXT >= TOP?
                    {
                        LDX #1   // NEXT > TOP
                    }
                }
            }
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessEqual()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
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
                    if (C) // NEXT >= TOP?
                    {
                        LDX #0  // NEXT > TOP
                    }
                }
            }
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterEqual()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - signed comparison: testing !(NEXT < TOP) which is (NEXT >= TOP)
                LDX #1 // Assume NEXT >= TOP
                LDA ZP.NEXTH
                CMP ZP.TOPH
                if (Z)
                {
                    LDA ZP.NEXTL
                    CMP ZP.TOPL
                }
                if (NC) // NEXT < TOP?
                {
                    LDX #0   // NEXT < TOP, so not >= 
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
                
                LDX #1  // Assume NEXT >= TOP
                if (C)
                {
                    // Negative result means NEXT < TOP
                    LDX #0
                }
            }
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Bitwise AND operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise AND result (promoted numeric type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    BitwiseAnd()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #2  // Bitwise operation (numeric types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // NEXT & TOP -> NEXT
            LDA ZP.NEXTL
            AND ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.NEXTH
            AND ZP.TOPH
            STA ZP.NEXTH
            
            LDA ZP.NEXTT
            Stacks.PushNext(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Bitwise OR operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise OR result (promoted numeric type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    BitwiseOr()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #2  // Bitwise operation (numeric types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // NEXT | TOP -> NEXT
            LDA ZP.NEXTL
            ORA ZP.TOPL
            STA ZP.NEXTL
            LDA ZP.NEXTH
            ORA ZP.TOPH
            STA ZP.NEXTH
            
            LDA ZP.NEXTT
            Stacks.PushNext(); // Push result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Logical AND operation (pops two operands, pushes result)
    // Input: Stack contains two BIT operands (right operand on top)
    // Output: Logical AND result (BIT type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LogicalAnd()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #4  // Logical operation (BIT types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Logical AND: both operands must be non-zero for result to be 1
            LDX #0  // Assume result is 0 (false)
            
            // Check if left operand is non-zero
            LDA ZP.NEXTL
            ORA ZP.NEXTH
            if (Z) 
            { 
                // Left is 0, result is 0
                Stacks.PushX(); // Push BIT result, modifies Y
                break;
            }
            
            // Left is non-zero, check right operand
            LDA ZP.TOPL
            ORA ZP.TOPH
            if (Z) 
            { 
                // Right is 0, result is 0
                Stacks.PushX(); // Push BIT result, modifies Y
                break;
            }
            
            // Both are non-zero, result is 1
            LDX #1
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Logical OR operation (pops two operands, pushes result)
    // Input: Stack contains two BIT operands (right operand on top)
    // Output: Logical OR result (BIT type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LogicalOr()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            LDA #4  // Logical operation (BIT types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Logical OR: result is 1 if either operand is non-zero
            LDX #0  // Assume result is 0 (false)
            
            // Check if left operand is non-zero
            LDA ZP.NEXTL
            ORA ZP.NEXTH
            if (NZ) 
            { 
                // Left is non-zero, result is 1
                LDX #1
                Stacks.PushX(); // Push BIT result, modifies Y
                break;
            }
            
            // Left is 0, check right operand
            LDA ZP.TOPL
            ORA ZP.TOPH
            if (NZ) 
            { 
                // Right is non-zero, result is 1
                LDX #1
            }
            
            // Push result (X = 0 if both were 0, X = 1 if either was non-zero)
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Restore registers
        PLY  // Restore Y register
        PLX  // Restore X register
        PLA  // Restore A register
    }
    
    // Logical NOT operation (pops one operand, pushes BIT result)
    // Input: Stack contains BIT operand on top
    // Output: Logical NOT result (BIT) pushed to stack
    // Error: Sets ZP.LastError if operand is not BIT type
    LogicalNot()
    {
        PHA
        PHX
        PHY
        
        loop // Single exit point for cleanup
        {
            Stacks.PopTop(); // Pop operand, modifies X
            
            // Check if operand is BIT type (only valid for logical NOT)
            LDA ZP.TOPT
            CMP #BasicType.BIT
            if (NZ)
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Logical NOT: convert 0 to 1, and 1 to 0
            LDA ZP.TOPL
            EOR # 0x01
            TAX
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        }
        
        PLY
        PLX
        PLA
    }
}
