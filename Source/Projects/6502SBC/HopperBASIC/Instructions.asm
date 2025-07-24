unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "BasicTypes"
    
    // Helper function to check if two types are compatible for operations
    // Input: ZP.NEXTT = left operand type, ZP.TOPT = right operand type
    //        ZP.NEXT = left value, ZP.TOP = right value (for WORD/INT range check)
    //        A = operation type: 0=comparison (BIT allowed), 1=arithmetic (BIT not allowed)
    // Output: Z set if compatible, NZ set if TYPE MISMATCH
    //         ZP.NEXTT = result type (for arithmetic operations)
    // Uses: ZP.ACCL for temporary storage
    checkTypeCompatibility()
    {
        STA ZP.ACCL  // Save operation type (0=comparison, 1=arithmetic)
        
        // For arithmetic operations, reject BIT types
        LDA ZP.ACCL
        if (NZ)  // Arithmetic operation
        {
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
        
        // Check for ARRAY type on either side - always an error
        LDA ZP.NEXTT
        CMP #BasicType.ARRAY
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
        
        // If both types are the same, always OK
        LDA ZP.NEXTT
        CMP ZP.TOPT
        if (Z)
        {
            // For arithmetic, result type = operand type
            // For comparison, result type = BIT (set below)
            LDA ZP.ACCL
            if (Z)  // Comparison operation
            {
                LDA #BasicType.BIT
                STA ZP.NEXTT  // Result type = BIT
            }
            LDA #0  // Set Z - compatible
            return;
        }
        
        // Different types - check specific compatibility rules
        
        // BYTE vs INT - always OK
        LDA ZP.NEXTT
        CMP #BasicType.BYTE
        if (Z)
        {
            LDA ZP.TOPT
            CMP #BasicType.INT
            if (Z)
            {
                // For mixed types, determine result type
                LDA ZP.ACCL
                if (Z)  // Comparison operation
                {
                    LDA #BasicType.BIT
                    STA ZP.NEXTT
                }
                else    // Arithmetic operation - promote to INT
                {
                    LDA #BasicType.INT
                    STA ZP.NEXTT
                }
                LDA #0  // Set Z - compatible
                return;
            }
        }
        
        // INT vs BYTE - always OK  
        LDA ZP.NEXTT
        CMP #BasicType.INT
        if (Z)
        {
            LDA ZP.TOPT
            CMP #BasicType.BYTE
            if (Z)
            {
                // For mixed types, determine result type
                LDA ZP.ACCL
                if (Z)  // Comparison operation
                {
                    LDA #BasicType.BIT
                    STA ZP.NEXTT
                }
                else    // Arithmetic operation - promote to INT
                {
                    LDA #BasicType.INT
                    STA ZP.NEXTT
                }
                LDA #0  // Set Z - compatible
                return;
            }
        }
        
        // BYTE vs WORD - always OK
        LDA ZP.NEXTT
        CMP #BasicType.BYTE
        if (Z)
        {
            LDA ZP.TOPT
            CMP #BasicType.WORD
            if (Z)
            {
                // For mixed types, determine result type
                LDA ZP.ACCL
                if (Z)  // Comparison operation
                {
                    LDA #BasicType.BIT
                    STA ZP.NEXTT
                }
                else    // Arithmetic operation - promote to WORD
                {
                    LDA #BasicType.WORD
                    STA ZP.NEXTT
                }
                LDA #0  // Set Z - compatible
                return;
            }
        }
        
        // WORD vs BYTE - always OK
        LDA ZP.NEXTT
        CMP #BasicType.WORD
        if (Z)
        {
            LDA ZP.TOPT
            CMP #BasicType.BYTE
            if (Z)
            {
                LDA #0  // Set Z - compatible
                return;
            }
        }
        
        // WORD vs INT - OK if INT >= 0 (high bit clear)
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
                // For mixed types, determine result type
                LDA ZP.ACCL
                if (Z)  // Comparison operation
                {
                    LDA #BasicType.BIT
                    STA ZP.NEXTT
                }
                else    // Arithmetic operation - promote to WORD
                {
                    LDA #BasicType.WORD
                    STA ZP.NEXTT
                }
                LDA #0       // Set Z - compatible
                return;
            }
        }
        
        // INT vs WORD - OK if INT >= 0 (high bit clear)
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
                // For mixed types, determine result type
                LDA ZP.ACCL
                if (Z)  // Comparison operation
                {
                    LDA #BasicType.BIT
                    STA ZP.NEXTT
                }
                else    // Arithmetic operation - promote to WORD
                {
                    LDA #BasicType.WORD
                    STA ZP.NEXTT
                }
                LDA #0        // Set Z - compatible
                return;
            }
        }
        
        // All other combinations are incompatible
        LDA #1  // Set NZ - type mismatch
    }
    
    
    subShared()
    {
        LDA #1  // Arithmetic  operation
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
        
        LDA # 1  // Comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        DumpVariables();
        
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
    
    Equals()
    {
        Stacks.PopTopNext();  // Gets both values and their types
        
        LDA #0  // Comparison operation
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
        
        LDA #0  // Comparison operation
        checkTypeCompatibility();
        
        if (NZ)  // Type mismatch
        {
            LDA #(Messages.TypeMismatch % 256)
            STA ZP.LastErrorL
            LDA #(Messages.TypeMismatch / 256)
            STA ZP.LastErrorH
            return;
        }
        
        LDX # 1
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDX # 0
            }
        }
        Stacks.PushX(); // X, type is BasicType.BIT
    }
}
