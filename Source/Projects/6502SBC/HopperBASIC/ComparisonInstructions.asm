unit ComparisonInstructions // ComparisonInstructions.asm
{
    
    
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack, C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    Equal()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.EQ();
                SEC
                break;
            }
            LDX #0          // Assume false (not equal)
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC
                break;
            }
            switch (A)
            {
                case BASICType.STRING:
                {
                    // Both operands are STRING - compare string content
                    Tools.StringCompare(); // Includes pointer equality optimization
                    if (C)
                    {
                        LDX #1 // Strings are equal
                    }
                }
                case BASICType.CHAR:
                case BASICType.BIT:
                {
                    LDA ZP.TOP0
                    CMP ZP.NEXT0
                    if (Z)
                    {
                        LDX #1 // Values are equal
                    }   
                }
                default:
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    CLC
                    break;
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        } // single exit
    }

    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack, C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    NotEqual()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.NE();
                SEC
                break;
            }
            LDX #1          // Assume true (not equal)
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                CLC
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            switch (A)
            {
                case BASICType.STRING:
                {
                    // Both operands are STRING - compare string content
                    Tools.StringCompare(); // Includes pointer equality optimization
                    if (C)
                    {
                        LDX #0 // Strings are equal
                    }
                }
                case BASICType.CHAR:
                case BASICType.BIT:
                {
                    LDA ZP.TOP0
                    CMP ZP.NEXT0
                    if (Z)
                    {
                        LDX #0 // Values are equal
                    }   
                }
                default:
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    CLC
                    break;
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        } // single exit
    }
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    LessThan()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.LT();
                SEC
                break;
            }
            
            LDX #0 // false
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC
                break; // BIT type mismatch
            }
            switch (A)
            {
                case BASICType.CHAR:
                {
                    LDA ZP.NEXT0
                    CMP ZP.TOP0
                    if (Z)
                    {
                        LDX #0      // NEXT == TOP
                    }
                    else if (C)
                    {
                        LDX #0      // NEXT > TOP
                    }
                    else
                    {
                        LDX #1      // NEXT < TOP
                    }
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL break;
                    break; // BIT type mismatch
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        } // loop
    }
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    GreaterThan()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.GT();
                SEC
                break;
            }
            
            LDX #0 // false
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC
                break; // BIT type mismatch
            }
            switch (A)
            {
                case BASICType.CHAR:
                {
                    LDA ZP.NEXT0
                    CMP ZP.TOP0
                    if (Z)
                    {
                        LDX #0      // NEXT == TOP
                    }
                    else if (C)
                    {
                        LDX #1      // NEXT > TOP
                    }
                    else
                    {
                        LDX #0      // NEXT < TOP
                    }
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL break;
                    break; // BIT type mismatch
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        }
    }    
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    LessEqual()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.LE();
                SEC
                break;
            }
            
            LDX #0 // false
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC
                break; // BIT type mismatch
            }
            switch (A)
            {
                case BASICType.CHAR:
                {
                    LDA ZP.NEXT0
                    CMP ZP.TOP0
                    if (Z)
                    {
                        LDX #1      // NEXT == TOP
                    }
                    else if (C)
                    {
                        LDX #0      // NEXT > TOP
                    }
                    else
                    {
                        LDX #1      // NEXT < TOP
                    }
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL break;
                    break; // BIT type mismatch
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        }
    }
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right), C set if successful
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory), ZP.LastError on type mismatch
    GreaterEqual()
    {
        loop
        {
            // Pop two operands
            Long.PopTopNext();
            if (NC) { break; }
            if (BBS3, ZP.TOPT) // if either is LONG, both will be long
            {
                Long.GE();
                SEC
                break;
            }
            
            LDX #0 // false
            
            LDA ZP.NEXTT
            CMP ZP.TOPT
            if (NZ)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                CLC
                break; // BIT type mismatch
            }
            switch (A)
            {
                case BASICType.CHAR:
                {
                    LDA ZP.NEXT0
                    CMP ZP.TOP0
                    if (Z)
                    {
                        LDX #1      // NEXT == TOP
                    }
                    else if (C)
                    {
                        LDX #1      // NEXT > TOP
                    }
                    else
                    {
                        LDX #0      // NEXT < TOP
                    }
                }
                default:
                {
                    Error.InvalidOperator(); BIT ZP.EmulatorPCL break;
                    break; // BIT type mismatch
                }
            }
            
            Stacks.PushX();
            SEC
            break;
        }
    }
}
