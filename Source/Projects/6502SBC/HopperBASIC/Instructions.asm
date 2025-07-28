unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // Instruction implementation for HopperBASIC operations
    // Preserves caller state except for documented outputs:
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
        PHA
        
        loop
        {
            LDA ZP.NEXTT
            CMP #BasicType.BIT
            if (NZ)
            {
                // For non-BIT types, use general type compatibility checking
                LDA #1  // Arithmetic operation mode
                CheckTypeCompatibility();
                break;
            }
            
            // Special case for BIT type - only allows values 0 or 1
            LDA ZP.TOPH
            if (NZ)
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
        }
        
        PLA
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
    //         ZP.NEXTT = operands type (updated based on operation mode and type promotion)
    // Modifies: processor flags
    CheckTypeCompatibility()
    {
        PHA
        PHX
        
        // Save original ZP.ACCT value
        LDX ZP.ACCT
        
        // Store operation mode in ZP.ACCT for internal use
        STA ZP.ACCT
        
        loop
        {
            // Mode-specific type restrictions
            LDA ZP.ACCT
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
                LDA ZP.ACCT
                CMP #2  // Bitwise operations
                if (Z)
                {
                    // Bitwise: allow all types including BIT
                }
                else
                {
                    LDA ZP.ACCT
                    CMP #3  // Ordering comparison
                    if (Z)
                    {
                        // Ordering: reject BIT types
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
                        LDA ZP.ACCT
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
                            
                            // Both are BIT - set result type and compatible flag
                            LDA #BasicType.BIT
                            STA ZP.NEXTT
                            SEC  // Set C - compatible
                            break;
                        }
                        else
                        {
                            // Equality: all types allowed, result is BIT
                        }
                    }
                }
            }
            
            // Same type check
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (Z)
            {
                // Same types are always compatible
                LDA ZP.ACCT
                SEC  // Set C - compatible
                break;
            }
            
            // Different types - apply specific compatibility rules and type promotion
            
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
                    
                    LDA ZP.ACCT
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
                    LDA ZP.ACCT
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
                    LDA ZP.ACCT
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
                    LDA ZP.ACCT
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // WORD vs INT - runtime compatibility check required
            LDA ZP.NEXTT
            CMP #BasicType.WORD
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.INT
                if (Z)
                {
                    // Check if this is a comparison operation
                    LDA ZP.ACCT
                    CMP #3  // Ordering comparison
                    if (Z)
                    {
                        // For comparisons: check INT sign to determine promotion direction
                        BIT ZP.TOPH  // Check sign bit of INT value (ZP.TOP)
                        if (MI)      // Negative INT
                        {
                            // INT < 0: promote WORD to INT if WORD = 32767
                            LDA ZP.NEXTH  // Check WORD high byte
                            CMP #0x80     // Compare with 32768 high byte
                            if (C)        // WORD = 32768
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
                            // WORD = 32767: promote to INT for signed comparison
                            LDA #BasicType.INT
                            STA ZP.NEXTT
                            SEC  // Set C - compatible
                            break;
                        }
                        else
                        {
                            // INT = 0: promote to WORD for unsigned comparison
                            LDA #BasicType.WORD
                            STA ZP.NEXTT
                            SEC  // Set C - compatible
                            break;
                        }
                    }
                    
                    // For non-comparison operations: use existing range-check logic
                    LDA ZP.NEXTH  // Check high byte of NEXT (WORD value)
                    CMP #0x80     // Compare with 32768 high byte
                    if (C)        // WORD = 32768
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
                    // WORD = 32767, compatible with INT
                    LDA ZP.ACCT
                    SEC  // Set C - compatible
                    break;
                }
            } // WORD vs INT
            
            // INT vs WORD - runtime compatibility check required
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    // Check if this is a comparison operation
                    LDA ZP.ACCT
                    CMP #3  // Ordering comparison
                    if (Z)
                    {
                        // For comparisons: check INT sign to determine promotion direction
                        BIT ZP.NEXTH  // Check sign bit of INT value (ZP.NEXT)
                        if (MI)       // Negative INT
                        {
                            // INT < 0: promote WORD to INT if WORD = 32767
                            LDA ZP.TOPH   // Check WORD high byte
                            CMP #0x80     // Compare with 32768 high byte
                            if (C)        // WORD = 32768
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
                            // WORD = 32767: promote to INT for signed comparison
                            LDA #BasicType.INT
                            STA ZP.NEXTT
                            SEC  // Set C - compatible
                            break;
                        }
                        else
                        {
                            // INT = 0: promote to WORD for unsigned comparison
                            LDA #BasicType.WORD
                            STA ZP.NEXTT
                            SEC  // Set C - compatible
                            break;
                        }
                    }
                    
                    // For non-comparison operations: use existing logic
                    BIT ZP.NEXTH  // Check sign bit of NEXT (INT value)
                    if (MI)       // Negative INT
                    {
                        CLC  // Set NC - incompatible
                        break;
                    }
                    LDA #BasicType.WORD
                    STA ZP.NEXTT
                    
                    // INT is non-negative, compatible with WORD
                    LDA ZP.ACCT
                    SEC  // Set C - compatible
                    break;
                }
            } // INT vs WORD
            
            // No other compatibility rules matched
            CLC  // Set NC - incompatible
            break;
        }
        
        // Restore original ZP.ACCT
        STX ZP.ACCT
        
        PLX
        PLA
    }
    
      
    // Shared subtraction logic helper
    // Input: ZP.NEXT = left operand, ZP.TOP = right operand (both popped from stack)
    //        ZP.NEXTT = left type, ZP.TOPT = right type
    // Output: Result pushed to stack
    // Modifies: Stack interface (ZP.NEXT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    subShared()
    {
        PHA
        PHY
        
        loop
        {
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
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
            Stacks.PushNext();
            
            break;
        }
        
        PLY
        PLA
    }
    
    // Addition operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Sum pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Addition()
    {
        PHA
        PHX
        PHY
        
        loop
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
                break;
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
            Stacks.PushNext();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
        
    // Subtraction operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Difference (left - right) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Subtraction()
    {
        PHX
        
        // Pop two operands
        Stacks.PopTopNext();
        
        // Delegate to shared subtraction logic
        subShared();
        
        PLX
    }
        
    // Handle sign extraction for signed operations
    // Input: ZP.NEXT = left operand, ZP.TOP = right operand
    // Output: ZP.FSIGN = count of negative operands (0, 1, or 2)
    //         ZP.NEXT and ZP.TOP converted to positive values
    doSigns()
    {
        PHA
        PHX
        
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
        
        PLX
        PLA
    }

    // UnaryMinus operation
    // Input: Stack contains one operand (the value to negate)
    // Output: Negated value pushed to stack with INT type
    // Modifies: Stack interface (ZP.TOP, ZP.TOPT, ZP.SP, stack memory)
    UnaryMinus()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop the operand to negate
            Stacks.PopTop();
            
            // Check if this is WORD 32768 (special case for -32768)
            LDA ZP.TOPT
            CMP #BasicType.WORD
            if (Z)
            {
                LDA ZP.TOPH
                CMP #0x80
                if (Z)
                {
                    LDA ZP.TOPL
                    if (Z)  // Exactly 32768
                    {
                        // Convert WORD 32768 to INT -32768 directly
                        LDA #BasicType.INT
                        STA ZP.TOPT
                        // Value 0x8000 is correct for -32768
                        
                        LDA #BasicType.INT
                        Stacks.PushTop();
                        break;
                    }
                }
            }
            
            // Handle normal cases
            LDA ZP.TOPT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - negate using two's complement
                SEC
                LDA #0
                SBC ZP.TOPL
                STA ZP.TOPL
                LDA #0
                SBC ZP.TOPH
                STA ZP.TOPH
                // Type remains INT
            }
            else
            {
                // WORD/BYTE - convert to two's complement (will become INT)
                SEC
                LDA #0
                SBC ZP.TOPL
                STA ZP.TOPL
                LDA #0
                SBC ZP.TOPH
                STA ZP.TOPH
                
                // Force result type to INT (all negative numbers are INT)
                LDA #BasicType.INT
                STA ZP.TOPT
            }
            
            LDA #BasicType.INT
            Stacks.PushTop();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Multiplication operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Product pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    //          Arithmetic scratch space (ZP.FSIGN, ZP.UWIDE0-UWIDE3 via IntMath)
    Multiply()
    {
        PHA
        PHX
        PHY
        
        loop
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
                break;
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
            Stacks.PushTop();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Division operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Quotient (left / right) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch or division by zero occurs)
    //          Arithmetic scratch space (ZP.FSIGN, ZP.ACC via IntMath)
    Divide()
    {
        PHA
        PHX
        PHY
        
        loop
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
                break;
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
            Stacks.PushNext();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }    
    
    // Modulo operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Remainder (left % right) pushed to stack
    // Error: Sets ZP.LastError if type mismatch or division by zero
    Modulo()
    {
        PHA
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #1  // Arithmetic operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
            }
            
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                // INT - handle signed modulo
                doSigns();
            }
            
            // ACC = NEXT % TOP
            IntMath.DivMod();
                
            LDA ZP.NEXTT
            STA ZP.ACCT
            Stacks.PushACC();
            
            break;
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
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
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
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
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
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
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
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
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
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
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
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Bitwise AND operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise AND result (promoted numeric type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    BitwiseAnd()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #2  // Bitwise operation (numeric types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
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
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Bitwise OR operation (pops two operands, pushes result)
    // Input: Stack contains two operands (right operand on top)
    // Output: Bitwise OR result (promoted numeric type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    BitwiseOr()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #2  // Bitwise operation (numeric types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
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
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Logical AND operation (pops two operands, pushes result)
    // Input: Stack contains two BIT operands (right operand on top)
    // Output: Logical AND result (BIT type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LogicalAnd()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #4  // Logical operation (BIT types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
            }
            
            // Logical AND: both operands must be non-zero for result to be 1
            LDX #0  // Assume result is 0 (false)
            
            // Check if left operand is non-zero
            LDA ZP.NEXTL
            ORA ZP.NEXTH
            if (Z) 
            { 
                // Left is 0, result is 0
                Stacks.PushX();
                break;
            }
            
            // Left is non-zero, check right operand
            LDA ZP.TOPL
            ORA ZP.TOPH
            if (Z) 
            { 
                // Right is 0, result is 0
                Stacks.PushX();
                break;
            }
            
            // Both are non-zero, result is 1
            LDX #1
            Stacks.PushX();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
    
    // Logical OR operation (pops two operands, pushes result)
    // Input: Stack contains two BIT operands (right operand on top)
    // Output: Logical OR result (BIT type) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LogicalOr()
    {
        PHA
        PHX
        PHY
        
        loop
        {
            Stacks.PopTopNext();
            
            LDA #4  // Logical operation (BIT types only)
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
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
                Stacks.PushX();
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
            Stacks.PushX();
            
            break;
        }
        
        PLY
        PLX
        PLA
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
        
        loop
        {
            Stacks.PopTop();
            
            // Check if operand is BIT type (only valid for logical NOT)
            LDA ZP.TOPT
            CMP #BasicType.BIT
            if (NZ)
            {
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break;
            }
            
            // Logical NOT: convert 0 to 1, and 1 to 0
            LDA ZP.TOPL
            EOR #0x01
            TAX
            
            Stacks.PushX();
            
            break;
        }
        
        PLY
        PLX
        PLA
    }
}
