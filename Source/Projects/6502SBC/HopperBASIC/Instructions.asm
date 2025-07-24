unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "/Source/Runtime/6502/IntMath"
    uses "Messages"
    uses "BasicTypes"
    
    // Helper function to check if two types are compatible for operations
    // Input: ZP.NEXTT = left operand type, ZP.TOPT = right operand type
    //        ZP.NEXT = left value, ZP.TOP = right value (for WORD/INT range check)
    //        A = operation mode:
    //          0 = Equality comparison (=, <>) - BIT types allowed, result is BIT
    //          1 = Arithmetic (+, -, *, /, %) - BIT types rejected, result is promoted numeric type
    //          2 = Bitwise/logical (AND, OR) - BIT types allowed, result is operand type or promoted
    //          3 = Ordering comparison (<, >, <=, >=) - BIT types rejected, result is BIT
    // Output: Z set if compatible, NZ set if TYPE MISMATCH
    //         ZP.NEXTT = result type (updated based on operation mode and type promotion)
    // Uses: ZP.ACCL for temporary storage
    checkTypeCompatibility()
    {
        STA ZP.ACCL  // Save operation mode
        
        // Mode-specific type restrictions
        LDA ZP.ACCL
        CMP #1  // Arithmetic operations
        if (Z)
        {
            // Arithmetic: reject BIT types
            LDA ZP.NEXTT
            CMP #BasicType.BIT
            if (Z)
            {
                LDA #1  // Set NZ - type mismatch
                return;
            }
            
            LDA ZP.TOPT
            CMP #BasicType.BIT
            if (Z)
            {
                LDA #1  // Set NZ - type mismatch
                return;
            }
        }
        else
        {
            LDA ZP.ACCL
            CMP #3  // Ordering comparison operations
            if (Z)
            {
                // Ordering comparison: reject BIT types
                LDA ZP.NEXTT
                CMP #BasicType.BIT
                if (Z)
                {
                    LDA #1  // Set NZ - type mismatch
                    return;
                }
                
                LDA ZP.TOPT
                CMP #BasicType.BIT
                if (Z)
                {
                    LDA #1  // Set NZ - type mismatch
                    return;
                }
            }
            // Modes 0 (equality) and 2 (bitwise/logical) allow BIT types
        }
        
        // Check for ARRAY and STRING types - not supported in Phase 1
        LDA ZP.NEXTT
        CMP #BasicType.ARRAY
        if (Z)
        {
            LDA #1  // Set NZ - type mismatch
            return;
        }
        CMP #BasicType.STRING
        if (Z)
        {
            LDA #1  // Set NZ - type mismatch
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.ARRAY
        if (Z)
        {
            LDA #1  // Set NZ - type mismatch
            return;
        }
        CMP #BasicType.STRING
        if (Z)
        {
            LDA #1  // Set NZ - type mismatch
            return;
        }
        
        // If both types are the same, check compatibility and set result type
        LDA ZP.NEXTT
        CMP ZP.TOPT
        if (Z)
        {
            // Set result type based on operation mode
            LDA ZP.ACCL
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
            LDA #0  // Set Z - compatible
            return;
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
                setResultTypeForMixedOperation();
                LDA #0  // Set Z - compatible
                return;
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
                setResultTypeForMixedOperation();
                LDA #0  // Set Z - compatible
                return;
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
                setResultTypeForMixedOperation();
                LDA #0  // Set Z - compatible
                return;
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
                setResultTypeForMixedOperation();
                LDA #0  // Set Z - compatible
                return;
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
                BIT ZP.TOPH  // Test high bit of INT value
                if (MI)      // High bit set - negative INT
                {
                    LDA #1   // Set NZ - type mismatch
                    return;
                }
                // WORD is already the promoted type (no change to ZP.NEXTT needed)
                setResultTypeForMixedOperation();
                LDA #0       // Set Z - compatible
                return;
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
                    LDA #1    // Set NZ - type mismatch
                    return;
                }
                LDA #BasicType.WORD
                STA ZP.NEXTT  // Promote to WORD
                setResultTypeForMixedOperation();
                LDA #0        // Set Z - compatible
                return;
            }
        }
        
        // All other combinations are incompatible (STRING with numbers, etc.)
        LDA #1  // Set NZ - type mismatch
    }
    
    // Helper function to set result type for mixed-type operations
    // Input: ZP.ACCL = operation mode, ZP.NEXTT = promoted operand type
    // Output: ZP.NEXTT = final result type
    setResultTypeForMixedOperation()
    {
        LDA ZP.ACCL
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
    }
    
    // Helper for shared subtraction logic
    subShared()
    {
        LDA #1  // Arithmetic operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    Addition()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        Tools.DumpVariables();
        
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
        
    Subtraction()
    {
        Stacks.PopTopNext();
        subShared();
    }
    
    doSigns()
    {
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
    }
    
    // Multiplicative operators with stubs
    Multiply()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    Divide()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    Modulo()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #1  // Arithmetic operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    // Comparison operators
    Equal()
    {
        Stacks.PopTopNext();  // Gets both values and their types
        
        LDA #0  // Equality comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    NotEqual()
    {
        Stacks.PopTopNext();
        
        LDA #0  // Equality comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    LessThan()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    GreaterThan()
    {
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    LessEqual()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    GreaterEqual()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #3  // Ordering comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    // Logical operators
    And()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #2  // Bitwise/logical operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
    
    Or()
    {
        // Pop two operands
        Stacks.PopTopNext();
        
        LDA #2  // Bitwise/logical operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
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
