unit Instructions // Instructions.asm
{
    
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
    const string checkRHS = "CheckRHS";
    CheckRHSTypeCompatibility()
    {
        PHA
        PHX
        PHY
#ifdef TRACE
        LDA #(checkRHS % 256) STA ZP.TraceMessageL LDA #(checkRHS / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
//Debug.NL(); NOut(); Space(); LDA #'<' COut(); LDA #'-' COut(); Space(); TOut();
        
        loop
        {
#ifdef DEBUG            
            LDA ZP.TOPT  // RHS type
            switch (A)
            {
                case BASICType.CHAR:
                case BASICType.BIT:
                case BASICType.STRING:
                case BASICType.LONG:
                {
                }
                default:
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    break;
                }
            }
#endif
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (Z)
            {
                // you can assign anything to anything if they are the same type
                SEC
                break;
            }
            if (BBS4, ZP.NEXTT)
            {
                // if LHS is VAR, you can assign anything to it
                SEC
                break;
            }
            
            Error.TypeMismatch(); BIT ZP.EmulatorPCL
            break;
        } // loop
        
#ifdef TRACE
        LDA #(checkRHS % 256) STA ZP.TraceMessageL LDA #(checkRHS / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        PLY
        PLX
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
            Long.PopTopNextStrict();
            if (NC) { break; }
//Debug.NL(); NLOut(); LDA #'+' COut(); TLOut(); LDA#'-' COut();LDA#'>' COut();
            
            // Perform addition
            CLC
            LDA ZP.NEXT0
            ADC ZP.TOP0
            STA ZP.NEXT0
            LDA ZP.NEXT1
            ADC ZP.TOP1
            STA ZP.NEXT1
            LDA ZP.NEXT2
            ADC ZP.TOP2
            STA ZP.NEXT2
            LDA ZP.NEXT3
            ADC ZP.TOP3
            STA ZP.NEXT3

//NLOut();
            // Push result to stack
            Long.PushNext();
            if (NC) { break; }
            SEC
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
        PHA
        PHX
        PHY
        
        loop
        {
            // Pop two operands
            Long.PopTopNextStrict();
            if (NC) { break; }
            
            // Perform subtraction (left - right)
            SEC
            LDA ZP.NEXT0
            SBC ZP.TOP0
            STA ZP.NEXT0
            LDA ZP.NEXT1
            SBC ZP.TOP1
            STA ZP.NEXT1
            LDA ZP.NEXT2
            SBC ZP.TOP2
            STA ZP.NEXT2
            LDA ZP.NEXT3
            SBC ZP.TOP3
            STA ZP.NEXT3
         
            // Push result to stack
            LDA ZP.NEXTT
            Long.PushNext();
            if (NC) { break; }
            
            SEC
            break;
        }
        PLY
        PLX
        PLA
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
            Long.PopTop();
            if (BBR3, ZP.TOPT)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            SEC
            LDA # 0
            SBC ZP.TOP0
            STA ZP.NEXT0
            LDA # 0
            SBC ZP.TOP1
            STA ZP.NEXT1
            LDA # 0
            SBC ZP.TOP2
            STA ZP.NEXT2
            LDA # 0
            SBC ZP.TOP3
            STA ZP.NEXT3
            
            LDA #BASICType.LONG
            STA ZP.NEXTT
            Long.PushNext();
            if (NC) { break; }
            SEC
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
            Long.PopTopNextStrict();
            if (NC) { break; }
            
            Long.Mul();
            if (NC) { break; }
            SEC
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
            Long.PopTopNextStrict();
            if (NC) { break; }
            
            // Check for division by zero
            LDA ZP.TOP0
            ORA ZP.TOP1
            ORA ZP.TOP2
            ORA ZP.TOP3
            if (Z)  // Divisor is zero
            {
                Error.DivisionByZero(); BIT ZP.EmulatorPCL
                break;
            }
            
            Long.Div();
            if (NC) { break; }
            SEC
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
            Long.PopTopNextStrict();
            if (NC) { break; }
            
            // Check for division by zero
            LDA ZP.TOP0
            ORA ZP.TOP1
            ORA ZP.TOP2
            ORA ZP.TOP3
            if (Z)  // Divisor is zero
            {
                Error.DivisionByZero(); BIT ZP.EmulatorPCL
                break;
            }
            
            Long.Mod();
            if (NC) { break; }
            SEC
            break;
        }
        
        PLY
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
            Long.PopTopNextStrict();
            if (NC) { break; }
            
            // NEXT & TOP -> NEXT
            LDA ZP.NEXT0
            AND ZP.TOP0
            STA ZP.NEXT0
            LDA ZP.NEXT1
            AND ZP.TOP1
            STA ZP.NEXT1
            LDA ZP.NEXT2
            AND ZP.TOP2
            STA ZP.NEXT2
            LDA ZP.NEXT3
            AND ZP.TOP3
            STA ZP.NEXT3
            
            LDA ZP.NEXTT
            Long.PushNext();
            if (NC) { break; }
            SEC
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
            Long.PopTopNextStrict();
            if (NC) { break; }

            // NEXT | TOP -> NEXT
            LDA ZP.NEXT0
            ORA ZP.TOP0
            STA ZP.NEXT0
            LDA ZP.NEXT1
            ORA ZP.TOP1
            STA ZP.NEXT1
            LDA ZP.NEXT2
            ORA ZP.TOP2
            STA ZP.NEXT2
            LDA ZP.NEXT2
            ORA ZP.TOP2
            STA ZP.NEXT2

            LDA ZP.NEXTT
            Long.PushNext();
            if (NC) { break; }
            SEC
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
            
            LDA ZP.TOPT
            ORA ZP.NEXTT
            CMP #BASICType.BIT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Logical AND: both operands must be non-zero for result to be 1
            LDX #0  // Assume result is 0 (false)
            
            // Check if left operand is non-zero
            LDA ZP.NEXT0
            AND ZP.TOP0
            if (NZ) 
            { 
                LDX #1
            }
            
            Stacks.PushX();
            SEC
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
            
            LDA ZP.TOPT
            ORA ZP.NEXTT
            CMP #BASICType.BIT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Logical OR: both operands must be non-zero for result to be 1
            LDX #0  // Assume result is 0 (false)
            
            // Check if left operand is non-zero
            LDA ZP.NEXT0
            ORA ZP.TOP0
            if (NZ) 
            { 
                LDX #1
            }
            
            Stacks.PushX();
            SEC
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
            CMP #BASICType.BIT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Logical NOT: convert 0 to 1, and 1 to 0
            LDA ZP.TOPL
            EOR #0x01
            TAX
            
            Stacks.PushX();
            SEC
            break;
        }
        
        PLY
        PLX
        PLA
    }
}
