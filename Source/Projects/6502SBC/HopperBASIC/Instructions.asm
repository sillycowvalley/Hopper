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
        
        // Debug: Show CheckTypeCompatibility entry
        LDA #'<'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
        // Debug: Show input parameters
        LDA #'M'
        Tools.COut();
        LDA #'O'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'E'
        Tools.COut();
        LDA #':'
        Tools.COut();
        Tools.HOut();  // A register contains operation mode
        LDA #'/'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA ZP.NEXTT
        Tools.HOut();
        LDA #'/'
        Tools.COut();
        LDA #'R'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA ZP.TOPT
        Tools.HOut();
        LDA #' '
        Tools.COut();
        
        // Save original ZP.ACCT value
        LDX ZP.ACCT
        
        // Store operation mode in ZP.ACCT for internal use
        STA ZP.ACCT
        
        loop // Single exit point for all compatibility checks
        {
            // Debug: Show operation mode analysis
            LDA #'A'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'L'
            Tools.COut();
            LDA #'Y'
            Tools.COut();
            LDA #'Z'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA ZP.ACCT
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            // Mode-specific type restrictions
            LDA ZP.ACCT
            CMP #1  // Arithmetic operations
            if (Z)
            {
                // Debug: Arithmetic mode
                LDA #'A'
                Tools.COut();
                LDA #'R'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'H'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // Arithmetic: reject BIT types
                LDA ZP.NEXTT
                CMP #BasicType.BIT
                if (Z)
                {
                    // Debug: Left operand is BIT - reject
                    LDA #'L'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #'F'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'B'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'!'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    CLC  // Set NC - type mismatch
                    break;
                }
                
                LDA ZP.TOPT
                CMP #BasicType.BIT
                if (Z)
                {
                    // Debug: Right operand is BIT - reject
                    LDA #'R'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'G'
                    Tools.COut();
                    LDA #'H'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'B'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'!'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
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
                    // Debug: Bitwise mode
                    LDA #'B'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'W'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'S'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    // Bitwise: allow all types including BIT
                    // (No type restrictions for bitwise operations)
                }
                else
                {
                    LDA ZP.ACCT
                    CMP #3  // Ordering comparison
                    if (Z)
                    {
                        // Debug: Ordering comparison mode
                        LDA #'O'
                        Tools.COut();
                        LDA #'R'
                        Tools.COut();
                        LDA #'D'
                        Tools.COut();
                        LDA #'E'
                        Tools.COut();
                        LDA #'R'
                        Tools.COut();
                        LDA #' '
                        Tools.COut();
                        
                        // Ordering: reject BIT types
                        LDA ZP.NEXTT
                        CMP #BasicType.BIT
                        if (Z)
                        {
                            // Debug: Left operand is BIT - reject
                            LDA #'L'
                            Tools.COut();
                            LDA #'E'
                            Tools.COut();
                            LDA #'F'
                            Tools.COut();
                            LDA #'T'
                            Tools.COut();
                            LDA #'B'
                            Tools.COut();
                            LDA #'I'
                            Tools.COut();
                            LDA #'T'
                            Tools.COut();
                            LDA #'!'
                            Tools.COut();
                            LDA #' '
                            Tools.COut();
                            
                            CLC  // Set NC - type mismatch
                            break;
                        }
                        
                        LDA ZP.TOPT
                        CMP #BasicType.BIT
                        if (Z)
                        {
                            // Debug: Right operand is BIT - reject
                            LDA #'R'
                            Tools.COut();
                            LDA #'I'
                            Tools.COut();
                            LDA #'G'
                            Tools.COut();
                            LDA #'H'
                            Tools.COut();
                            LDA #'T'
                            Tools.COut();
                            LDA #'B'
                            Tools.COut();
                            LDA #'I'
                            Tools.COut();
                            LDA #'T'
                            Tools.COut();
                            LDA #'!'
                            Tools.COut();
                            LDA #' '
                            Tools.COut();
                            
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
                            // Debug: Logical mode
                            LDA #'L'
                            Tools.COut();
                            LDA #'O'
                            Tools.COut();
                            LDA #'G'
                            Tools.COut();
                            LDA #'I'
                            Tools.COut();
                            LDA #'C'
                            Tools.COut();
                            LDA #'A'
                            Tools.COut();
                            LDA #'L'
                            Tools.COut();
                            LDA #' '
                            Tools.COut();
                            
                            // Logical: only BIT types allowed
                            LDA ZP.NEXTT
                            CMP #BasicType.BIT
                            if (NZ)
                            {
                                // Debug: Left operand is not BIT - reject
                                LDA #'L'
                                Tools.COut();
                                LDA #'E'
                                Tools.COut();
                                LDA #'F'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'N'
                                Tools.COut();
                                LDA #'O'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'B'
                                Tools.COut();
                                LDA #'I'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'!'
                                Tools.COut();
                                LDA #' '
                                Tools.COut();
                                
                                CLC  // Set NC - type mismatch
                                break;
                            }
                            
                            LDA ZP.TOPT
                            CMP #BasicType.BIT
                            if (NZ)
                            {
                                // Debug: Right operand is not BIT - reject
                                LDA #'R'
                                Tools.COut();
                                LDA #'I'
                                Tools.COut();
                                LDA #'G'
                                Tools.COut();
                                LDA #'H'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'N'
                                Tools.COut();
                                LDA #'O'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'B'
                                Tools.COut();
                                LDA #'I'
                                Tools.COut();
                                LDA #'T'
                                Tools.COut();
                                LDA #'!'
                                Tools.COut();
                                LDA #' '
                                Tools.COut();
                                
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
                            // Debug: Equality comparison mode (mode 0)
                            LDA #'E'
                            Tools.COut();
                            LDA #'Q'
                            Tools.COut();
                            LDA #'U'
                            Tools.COut();
                            LDA #'A'
                            Tools.COut();
                            LDA #'L'
                            Tools.COut();
                            LDA #' '
                            Tools.COut();
                            
                            // Equality: all types allowed, result is BIT
                            // (No type restrictions - fall through to compatibility checks)
                        }
                    }
                }
            }
            
            // Debug: Now check type compatibility
            LDA #'C'
            Tools.COut();
            LDA #'O'
            Tools.COut();
            LDA #'M'
            Tools.COut();
            LDA #'P'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
            // Same type check
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (Z)
            {
                // Debug: Same types
                LDA #'S'
                Tools.COut();
                LDA #'A'
                Tools.COut();
                LDA #'M'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'Y'
                Tools.COut();
                LDA #'P'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // Same types are always compatible
                LDA ZP.ACCT  // Get operation mode
                setResultTypeForSameType();
                SEC  // Set C - compatible
                break;
            }
            
            // Debug: Different types, check specific compatibility rules
            LDA #'D'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'Y'
            Tools.COut();
            LDA #'P'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
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
                    // Debug: BYTE vs INT
                    LDA #'B'
                    Tools.COut();
                    LDA #'Y'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'N'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
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
                    // Debug: INT vs BYTE
                    LDA #'I'
                    Tools.COut();
                    LDA #'N'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'B'
                    Tools.COut();
                    LDA #'Y'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
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
                    // Debug: BYTE vs WORD
                    LDA #'B'
                    Tools.COut();
                    LDA #'Y'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'W'
                    Tools.COut();
                    LDA #'O'
                    Tools.COut();
                    LDA #'R'
                    Tools.COut();
                    LDA #'D'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
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
                    // Debug: WORD vs BYTE
                    LDA #'W'
                    Tools.COut();
                    LDA #'O'
                    Tools.COut();
                    LDA #'R'
                    Tools.COut();
                    LDA #'D'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'B'
                    Tools.COut();
                    LDA #'Y'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'E'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    // WORD is already the promoted type (no change to ZP.NEXTT needed)
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // INT vs WORD - runtime compatibility check required
            LDA ZP.NEXTT
            CMP #BasicType.INT
            if (Z)
            {
                LDA ZP.TOPT
                CMP #BasicType.WORD
                if (Z)
                {
                    // Debug: INT vs WORD
                    LDA #'I'
                    Tools.COut();
                    LDA #'N'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'W'
                    Tools.COut();
                    LDA #'O'
                    Tools.COut();
                    LDA #'R'
                    Tools.COut();
                    LDA #'D'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    // INT vs WORD: check if INT is non-negative
                    BIT ZP.NEXTH  // Check sign bit of NEXT (INT value)
                    if (MI)       // Negative INT
                    {
                        // Debug: INT is negative
                        LDA #'N'
                        Tools.COut();
                        LDA #'E'
                        Tools.COut();
                        LDA #'G'
                        Tools.COut();
                        LDA #'!'
                        Tools.COut();
                        LDA #' '
                        Tools.COut();
                        
                        CLC  // Set NC - incompatible
                        break;
                    }
                    // INT is non-negative, compatible with WORD
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
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
                    // Debug: WORD vs INT
                    LDA #'W'
                    Tools.COut();
                    LDA #'O'
                    Tools.COut();
                    LDA #'R'
                    Tools.COut();
                    LDA #'D'
                    Tools.COut();
                    LDA #'+'
                    Tools.COut();
                    LDA #'I'
                    Tools.COut();
                    LDA #'N'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    // WORD vs INT: check if WORD fits in INT range (= 32767)
                    LDA ZP.NEXTH  // Check high byte of NEXT (WORD value)
                    CMP #0x80     // Compare with 32768 high byte
                    if (C)        // WORD = 32768
                    {
                        if (Z)    // Exactly 32768?
                        {
                            LDA ZP.NEXTL
                            if (Z)  // Exactly 32768
                            {
                                // Debug: WORD is exactly 32768
                                LDA #'3'
                                Tools.COut();
                                LDA #'2'
                                Tools.COut();
                                LDA #'7'
                                Tools.COut();
                                LDA #'6'
                                Tools.COut();
                                LDA #'8'
                                Tools.COut();
                                LDA #'!'
                                Tools.COut();
                                LDA #' '
                                Tools.COut();
                                
                                CLC  // Set NC - incompatible
                                break;
                            }
                        }
                        // WORD > 32767
                        
                        // Debug: WORD too large
                        LDA #'T'
                        Tools.COut();
                        LDA #'O'
                        Tools.COut();
                        LDA #'O'
                        Tools.COut();
                        LDA #'B'
                        Tools.COut();
                        LDA #'I'
                        Tools.COut();
                        LDA #'G'
                        Tools.COut();
                        LDA #'!'
                        Tools.COut();
                        LDA #' '
                        Tools.COut();
                        
                        CLC  // Set NC - incompatible
                        break;
                    }
                    // WORD = 32767, compatible with INT
                    LDA ZP.ACCT  // Get operation mode
                    setResultTypeForMixedOperation();
                    SEC  // Set C - compatible
                    break;
                }
            }
            
            // No other compatibility rules matched
            // Debug: Incompatible types
            LDA #'I'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'C'
            Tools.COut();
            LDA #'O'
            Tools.COut();
            LDA #'M'
            Tools.COut();
            LDA #'P'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'!'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
            CLC  // Set NC - incompatible
            break;
        } // end of single exit loop
        
        // Debug: Show result and restore original ZP.ACCT
        PHP  // Save result flags
        
        LDA #'R'
        Tools.COut();
        LDA #'E'
        Tools.COut();
        LDA #'S'
        Tools.COut();
        LDA #':'
        Tools.COut();
        PLP  // Restore flags to check them
        PHP  // Save them again
        if (C)
        {
            LDA #'O'
            Tools.COut();
            LDA #'K'
            Tools.COut();
        }
        else
        {
            LDA #'F'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'L'
            Tools.COut();
        }
        LDA #'/'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #'Y'
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA ZP.NEXTT
        Tools.HOut();
        LDA #' '
        Tools.COut();
        
        // Restore original ZP.ACCT
        STX ZP.ACCT
        
        // Debug: Show CheckTypeCompatibility exit
        LDA #'C'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'>'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
        PLP  // Restore final result flags
        PLX
        PLA
    }
    
    
    
    // Set result type for same-type operations
    // Input: A = operation mode (0=equality, 1=arithmetic, 2=bitwise, 3=ordering, 4=logical)
    //        ZP.NEXTT = operand type (both operands have same type)
    // Output: ZP.NEXTT = final result type
    setResultTypeForSameType()
    {
        PHA  // Preserve A register
        
        switch (A)
        {
            case 0:  // Equality comparison (=, <>)
            case 3:  // Ordering comparison (<, >, <=, >=)
            {
                LDA #BasicType.BIT
                STA ZP.NEXTT  // Result type = BIT (comparisons always return BIT)
            }
            case 1:  // Arithmetic operations (+, -, *, /, MOD)
            case 2:  // Bitwise operations (&, |)
            case 4:  // Logical operations (AND, OR) - only for BIT types
            {
                // Result type = same as operand type (ZP.NEXTT unchanged)
                // For arithmetic: INT + INT = INT, WORD + WORD = WORD, etc.
                // For bitwise: INT & INT = INT, WORD & WORD = WORD, etc.
                // For logical: BIT AND BIT = BIT, BIT OR BIT = BIT
            }
        }
        
        PLA  // Restore A register
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

// UnaryMinus operation with debug instrumentation
// Input: Stack contains one operand (the value to negate)
// Output: Negated value pushed to stack with INT type
// Modifies: Stack interface (ZP.TOP, ZP.TOPT, ZP.SP, stack memory)
UnaryMinus()
{
    PHA  // Preserve A register
    PHX  // Preserve X register
    PHY  // Preserve Y register
    
    // Debug: Show UnaryMinus start
    LDA #'U'
    Tools.COut();
    LDA #'M'
    Tools.COut();
    LDA #':'
    Tools.COut();
    
    loop // Single exit point for cleanup
    {
        // Pop the operand to negate
        Stacks.PopTop(); // Pop operand, modifies X
        
        // Debug: Show input type and value
        LDA #'I'
        Tools.COut();
        LDA #'N'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA ZP.TOPT  // Show input type
        Tools.HOut();
        LDA #'-'
        Tools.COut();
        LDA ZP.TOPH  // Show input value high
        Tools.HOut();
        LDA ZP.TOPL  // Show input value low  
        Tools.HOut();
        LDA #' '
        Tools.COut();
        
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
                    
                    // Debug: Show special case
                    LDA #'S'
                    Tools.COut();
                    LDA #'P'
                    Tools.COut();
                    LDA #' '
                    Tools.COut();
                    
                    // Debug: Show output before push
                    LDA #'O'
                    Tools.COut();
                    LDA #'U'
                    Tools.COut();
                    LDA #'T'
                    Tools.COut();
                    LDA #':'
                    Tools.COut();
                    LDA ZP.TOPT
                    Tools.HOut();
                    LDA #'-'
                    Tools.COut();
                    LDA ZP.TOPH
                    Tools.HOut();
                    LDA ZP.TOPL
                    Tools.HOut();
                    LDA #' '
                    Tools.COut();
                    
                    LDA #BasicType.INT
                    Stacks.PushTop(); // Push result, modifies Y
                    break; // Success exit
                }
            }
        }
        
        // Handle normal cases
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (Z)
        {
            // Debug: Show INT case
            LDA #'I'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
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
            // Debug: Show WORD/BYTE case
            LDA #'W'
            Tools.COut();
            LDA #'R'
            Tools.COut();
            LDA #'D'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
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
        
        // Debug: Show output before push
        LDA #'O'
        Tools.COut();
        LDA #'U'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #':'
        Tools.COut();
        LDA ZP.TOPT
        Tools.HOut();
        LDA #'-'
        Tools.COut();
        LDA ZP.TOPH
        Tools.HOut();
        LDA ZP.TOPL
        Tools.HOut();
        LDA #' '
        Tools.COut();
        
        LDA #BasicType.INT
        Stacks.PushTop(); // Push result, modifies Y
        
        break; // Success exit
    } // Single exit loop
    
    // Restore registers
    PLY  // Restore Y register
    PLX  // Restore X register
    PLA  // Restore A register
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
    
    
    // doSignedCompare() with debug instrumentation
// Replace the existing doSignedCompare() method with this version

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
    
    // Debug: Show input values
    LDA #'N'
    Tools.COut();
    LDA #':'
    Tools.COut();
    LDA ZP.NEXTH
    Tools.HOut();
    LDA ZP.NEXTL
    Tools.HOut();
    LDA #' '
    Tools.COut();
    LDA #'T'
    Tools.COut();
    LDA #':'
    Tools.COut();
    LDA ZP.TOPH
    Tools.HOut();
    LDA ZP.TOPL
    Tools.HOut();
    LDA #' '
    Tools.COut();
    
    LDA ZP.NEXTH
    CMP ZP.TOPH
    if (Z)
    {
        // Debug: Same high bytes
        LDA #'S'
        Tools.COut();
        LDA #'H'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
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
        // Debug: Different high bytes
        LDA #'D'
        Tools.COut();
        LDA #'H'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
        // High bytes different - signed comparison needed
        // Need to preserve the comparison result and check signs properly
        PHP             // Save comparison flags
        LDA ZP.NEXTH    // Get NEXT high byte
        EOR ZP.TOPH     // XOR with TOP high byte to check if signs differ
        if (MI)         // Signs differ
        {
            // Debug: Different signs
            LDA #'D'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
            BIT ZP.NEXTH
            if (MI)     // NEXT is negative, TOP is positive
            {
                // Debug: NEXT negative, TOP positive
                LDA #'N'
                Tools.COut();
                LDA #'-'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                LDA #0  // NEXT < TOP (negative < positive)
            }
            else        // NEXT is positive, TOP is negative
            {
                // Debug: NEXT positive, TOP negative  
                LDA #'N'
                Tools.COut();
                LDA #'+'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                LDA #2  // NEXT > TOP (positive > negative)
            }
            STA ZP.ACC
        }
        else
        {
            // Debug: Same signs
            LDA #'S'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
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
        PLP             // Clean up stack if we didn't use it
    }
    
    // Debug: Show final result
    LDA #'A'
    Tools.COut();
    LDA #':'
    Tools.COut();
    LDA ZP.ACC
    Tools.HOut();
    LDA #' '
    Tools.COut();
    
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
        PHA  // Preserve A register
        PHX  // Preserve X register
        PHY  // Preserve Y register
        
        // Debug: Show LessThan entry
        LDA #'<'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            // Debug: Show operands before type compatibility check
            LDA #'B'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTH
            Tools.HOut();
            LDA ZP.NEXTL
            Tools.HOut();
            LDA #'/'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.TOPT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.TOPH
            Tools.HOut();
            LDA ZP.TOPL
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                // Debug: Show type compatibility failure
                LDA #'T'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'F'
                Tools.COut();
                LDA #'A'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'L'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Debug: Show operands after type compatibility check
            LDA #'A'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTH
            Tools.HOut();
            LDA ZP.NEXTL
            Tools.HOut();
            LDA #'/'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.TOPT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.TOPH
            Tools.HOut();
            LDA ZP.TOPL
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            // Debug: Show the critical type decision point
            LDA #'D'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'C'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'O'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #'='
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA #BasicType.INT
            Tools.HOut();
            LDA #'?'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
            // Show the actual comparison result
            LDA ZP.NEXTT
            CMP #BasicType.INT
            PHP  // Save the comparison flags
            
            // Debug: Show comparison result
            LDA #'C'
            Tools.COut();
            LDA #'M'
            Tools.COut();
            LDA #'P'
            Tools.COut();
            LDA #':'
            Tools.COut();
            PLP  // Restore flags to check them
            PHP  // Save them again for the actual decision
            if (Z)
            {
                LDA #'E'
                Tools.COut();
                LDA #'Q'
                Tools.COut();
                LDA #'U'
                Tools.COut();
                LDA #'A'
                Tools.COut();
                LDA #'L'
                Tools.COut();
            }
            else
            {
                LDA #'N'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'Q'
                Tools.COut();
            }
            LDA #' '
            Tools.COut();
            
            PLP  // Restore flags for actual decision
            if (Z)
            {
                // Debug: Show we're choosing signed comparison
                LDA #'C'
                Tools.COut();
                LDA #'H'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #':'
                Tools.COut();
                LDA #'S'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'G'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'D'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // Debug: Show we're choosing unsigned and why
                LDA #'C'
                Tools.COut();
                LDA #'H'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #':'
                Tools.COut();
                LDA #'U'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'S'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'G'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'D'
                Tools.COut();
                LDA #' '
                Tools.COut();
                LDA #'('
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'X'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'='
                Tools.COut();
                LDA ZP.NEXTT  // Show what type caused unsigned choice
                Tools.HOut();
                LDA #')'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Debug: Show comparison result
            LDA #'R'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #'U'
            Tools.COut();
            LDA #'L'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA ZP.ACC
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            // Check if result is NEXT < TOP (ZP.ACC == 0)
            LDX #0          // Assume false
            LDA ZP.ACC
            if (Z)          // Result was 0 (NEXT < TOP)
            {
                LDX #1      // True
            }
            
            // Debug: Show final boolean result
            LDA #'F'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'L'
            Tools.COut();
            LDA #':'
            Tools.COut();
            TXA
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Debug: Show LessThan exit
        LDA #'L'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #'>'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
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
        
        // Debug: Show GreaterThan entry
        LDA #'<'
        Tools.COut();
        LDA #'G'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
        loop // Single exit point for cleanup
        {
            // Pop two operands
            Stacks.PopTopNext(); // Pop operands, modifies X
            
            // Debug: Show operands before type compatibility check
            LDA #'B'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTH
            Tools.HOut();
            LDA ZP.NEXTL
            Tools.HOut();
            LDA #'/'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.TOPT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.TOPH
            Tools.HOut();
            LDA ZP.TOPL
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            LDA #3  // Ordering comparison operation
            CheckTypeCompatibility();
            
            if (NC)  // Type mismatch
            {
                // Debug: Show type compatibility failure
                LDA #'T'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'F'
                Tools.COut();
                LDA #'A'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'L'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                LDA #(Messages.TypeMismatch % 256)
                STA ZP.LastErrorL
                LDA #(Messages.TypeMismatch / 256)
                STA ZP.LastErrorH
                break; // Error exit
            }
            
            // Debug: Show operands after type compatibility check
            LDA #'A'
            Tools.COut();
            LDA #'F'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTH
            Tools.HOut();
            LDA ZP.NEXTL
            Tools.HOut();
            LDA #'/'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA ZP.TOPT
            Tools.HOut();
            LDA #':'
            Tools.COut();
            LDA ZP.TOPH
            Tools.HOut();
            LDA ZP.TOPL
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            // Debug: Show the critical type decision point
            LDA #'D'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'C'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'O'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA ZP.NEXTT
            Tools.HOut();
            LDA #'='
            Tools.COut();
            LDA #'='
            Tools.COut();
            LDA #BasicType.INT
            Tools.HOut();
            LDA #'?'
            Tools.COut();
            LDA #' '
            Tools.COut();
            
            // Show the actual comparison result
            LDA ZP.NEXTT
            CMP #BasicType.INT
            PHP  // Save the comparison flags
            
            // Debug: Show comparison result
            LDA #'C'
            Tools.COut();
            LDA #'M'
            Tools.COut();
            LDA #'P'
            Tools.COut();
            LDA #':'
            Tools.COut();
            PLP  // Restore flags to check them
            PHP  // Save them again for the actual decision
            if (Z)
            {
                LDA #'E'
                Tools.COut();
                LDA #'Q'
                Tools.COut();
                LDA #'U'
                Tools.COut();
                LDA #'A'
                Tools.COut();
                LDA #'L'
                Tools.COut();
            }
            else
            {
                LDA #'N'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'Q'
                Tools.COut();
            }
            LDA #' '
            Tools.COut();
            
            PLP  // Restore flags for actual decision
            if (Z)
            {
                // Debug: Show we're choosing signed comparison
                LDA #'C'
                Tools.COut();
                LDA #'H'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #':'
                Tools.COut();
                LDA #'S'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'G'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'D'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // INT - signed comparison
                doSignedCompare();
            }
            else
            {
                // Debug: Show we're choosing unsigned and why
                LDA #'C'
                Tools.COut();
                LDA #'H'
                Tools.COut();
                LDA #'O'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'C'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #':'
                Tools.COut();
                LDA #'U'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'S'
                Tools.COut();
                LDA #'I'
                Tools.COut();
                LDA #'G'
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'D'
                Tools.COut();
                LDA #' '
                Tools.COut();
                LDA #'('
                Tools.COut();
                LDA #'N'
                Tools.COut();
                LDA #'E'
                Tools.COut();
                LDA #'X'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'T'
                Tools.COut();
                LDA #'='
                Tools.COut();
                LDA ZP.NEXTT  // Show what type caused unsigned choice
                Tools.HOut();
                LDA #')'
                Tools.COut();
                LDA #' '
                Tools.COut();
                
                // WORD and BYTE - unsigned comparison
                doUnsignedCompare();
            }
            
            // Debug: Show comparison result
            LDA #'R'
            Tools.COut();
            LDA #'E'
            Tools.COut();
            LDA #'S'
            Tools.COut();
            LDA #'U'
            Tools.COut();
            LDA #'L'
            Tools.COut();
            LDA #'T'
            Tools.COut();
            LDA #':'
            Tools.COut();
            LDA ZP.ACC
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            // Check if result is NEXT > TOP (ZP.ACC == 2)
            LDX #0          // Assume false
            LDA ZP.ACC
            CMP #2
            if (Z)          // Result was 2 (NEXT > TOP)
            {
                LDX #1      // True
            }
            
            // Debug: Show final boolean result
            LDA #'F'
            Tools.COut();
            LDA #'I'
            Tools.COut();
            LDA #'N'
            Tools.COut();
            LDA #'A'
            Tools.COut();
            LDA #'L'
            Tools.COut();
            LDA #':'
            Tools.COut();
            TXA
            Tools.HOut();
            LDA #' '
            Tools.COut();
            
            Stacks.PushX(); // Push BIT result, modifies Y
            
            break; // Success exit
        } // Single exit loop
        
        // Debug: Show GreaterThan exit
        LDA #'G'
        Tools.COut();
        LDA #'T'
        Tools.COut();
        LDA #'>'
        Tools.COut();
        LDA #' '
        Tools.COut();
        
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
