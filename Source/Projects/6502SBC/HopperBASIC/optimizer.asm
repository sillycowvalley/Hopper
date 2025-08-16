unit Optimizer
{
    uses "./Definitions/OpCodes"

#ifdef PEEPHOLE
    
    flags PeepConstraint
    {
        None               = 0b00000000, // no contraints
        EqualOperands0and3 = 0b00000001, // operands for instructions 0 and 3 must be equal
        EqualOperands0and2 = 0b00000010, // operands for instructions 0 and 2 must be equal
        
    }
    enum NewOperands
    {
        None,
        Operand0,            // PEEPREPLACE PEEPOP0
        Operand0AndOperand2, // PEEPREPLACE PEEPOP0 PEEPOP2
        Operand0AndOperand3, // PEEPREPLACE PEEPOP0 PEEPOP3
    }
    
    const byte[] peepPatterns = {
    //  PEEP0,             PEEP1,        PEEP2,            PEEP3,             CONSTRAINTS,                       OPERANDS,                        EMIT            
    
    // PUSHGLOBAL <n>, PUSH1, ADD, POPGLOBAL <n> -> INCGLOBAL <n>
        OpCode.POPGLOBAL,  OpCode.ADD,   OpCode.PUSH1,     OpCode.PUSHGLOBAL, PeepConstraint.EqualOperands0and3, NewOperands.Operand0,            OpCode.INCGLOBAL, 
    // PUSHLOCAL <n>,  PUSH1, ADD, POPLOCAL  <n> -> INCLOCAL <n>        
        OpCode.POPLOCAL,   OpCode.ADD,   OpCode.PUSH1,     OpCode.PUSHLOCAL,  PeepConstraint.EqualOperands0and3, NewOperands.Operand0,            OpCode.INCLOCAL,  
    // PUSHLOCAL <n>,  PUSHLOCAL <m>, ADD, POPLOCAL  <n> -> ADDLOCALS <n> <m>
        OpCode.POPLOCAL,   OpCode.ADD,   OpCode.PUSHLOCAL, OpCode.PUSHLOCAL,  PeepConstraint.EqualOperands0and3, NewOperands.Operand0AndOperand2, OpCode.ADDLOCALS,
    // PUSHLOCAL <m>,  PUSHLOCAL <n>, ADD, POPLOCAL  <n> -> ADDLOCALS <n> <m>
        OpCode.POPLOCAL,   OpCode.ADD,   OpCode.PUSHLOCAL, OpCode.PUSHLOCAL,  PeepConstraint.EqualOperands0and2, NewOperands.Operand0AndOperand3, OpCode.ADDLOCALS,
        
        OpCode.INVALID  // End marker = 0
    };

    
    ClearPeeps()
    {
        STZ ZP.PEEP3
        STZ ZP.PEEP2
        STZ ZP.PEEP1
        STZ ZP.PEEP0
    }
    
    
    DumpPeeps()
    {
        // Debug Output:
        Debug.NL();
        
        LDA ZP.XPCH
        HOut();
        LDA ZP.XPCL
        HOut();
        
        Space();
        
        LDX ZP.PEEP3
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
            TXA
            AND #0xC0
            CMP #0x40
            if (Z)
            {
                Space(); LDA ZP.PEEPOP3 HOut();
            }
        }
        LDX ZP.PEEP2
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
            TXA
            AND #0xC0
            CMP #0x40
            if (Z)
            {
                Space(); LDA ZP.PEEPOP2 HOut();
            }
        }
        LDX ZP.PEEP1
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
            TXA
            AND #0xC0
            CMP #0x40
            if (Z)
            {
                Space(); LDA ZP.PEEPOP1 HOut();
            }
        }
        LDX ZP.PEEP0
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
            TXA
            AND #0xC0
            CMP #0x40
            if (Z)
            {
                Space(); LDA ZP.PEEPOP0 HOut();
            }
            TXA
            AND #0xC0
            CMP #0x80
            if (Z)
            {
                Space(); LDA ZP.PEEPOP0 HOut(); Space(); LDA ZP.PEEPOP1 HOut(); // not correct
            }
        }
        Space();
    }        
    
    // IDY - current instruction pointer
    // A = contains OpCode
    //
    // OpCode & 0xC0 - high bits indicate opcode width:
    //     0x00 - 1 byte
    //     0x40 - 2 bytes
    //     0x80 - 3 bytes
    //     0xC0 - 4 bytes
    stepBack()
    {
        PHA // preserve the opcode
        PHY
        LSR A LSR A LSR A LSR A LSR A LSR A // divide by 64
        INC
        TAY
        loop
        {
            DecIDY();
            DEY
            if (Z) { break; }
        }
        PLY
        PLA
    }
    stepBackXPC()
    {
        PHA // preserve the opcode
        PHY
        LSR A LSR A LSR A LSR A LSR A LSR A // divide by 64
        INC
        TAY
        loop
        {
            LDA ZP.XPCL
            if (Z)
            {
                DEC ZP.XPCH
            }
            DEC ZP.XPCL
            
            LDA OpCodeBufferContentLengthL
            if (Z)
            {
                DEC OpCodeBufferContentLengthH
            }
            DEC OpCodeBufferContentLengthL
            
            DEY
            if (Z) { break; }
        }
        PLY
        PLA
    }
    incXPC()
    {
        // Increment PC
        INC ZP.XPCL
        if (Z)
        {
            INC ZP.XPCH
        }
        INC OpCodeBufferContentLengthL
        if (Z)
        {
            INC OpCodeBufferContentLengthH
        }
     }
    
    // Compiler.compilerOpCode is the new PEEP0
    pushPeepOp()
    {
        LDA ZP.PEEP2
        STA ZP.PEEP3
        
        LDA ZP.PEEP1
        STA ZP.PEEP2
        
        LDA ZP.PEEP0
        STA ZP.PEEP1
        
        LDA Compiler.compilerOpCode
        STA ZP.PEEP0
    }
    
    // PEEP0 has been deleted so PEEP1 is the new PEEP0
    popPeepOp()
    {
        LDA ZP.PEEP1
        STA ZP.PEEP0
        LDA ZP.PEEP2
        STA ZP.PEEP1
        LDA ZP.PEEP3
        STA ZP.PEEP2
        LDA # OpCode.INVALID
        STA ZP.PEEP3
    }
    
    // Try to match current PEEP sequence against optimization patterns
    // Called after shifting in the new opcode
    // C if an optimization was made, NC if not
    tryOptimize()
    {
        // Point to 6 bytes before start of peepPatterns table
        LDA #((peepPatterns-7) % 256)
        STA ZP.IDXL
        LDA #((peepPatterns-7) / 256)
        STA ZP.IDXH
        
        loop
        {
            // Advance to next pattern (6 bytes)
            CLC
            LDA ZP.IDXL
            ADC #7
            STA ZP.IDXL
            if (C)
            {
                INC ZP.IDXH
            }
            
            // Check for end marker
            LDY #0
            LDA [ZP.IDX], Y
            if (Z) { CLC break; } // Found OpCode.INVALID - no more patterns
            
            // Compare PEEP0
            CMP ZP.PEEP0
            if (NZ) { continue; }
            
            // Compare PEEP1
            LDY #1
            LDA [ZP.IDX], Y
            CMP ZP.PEEP1
            if (NZ) { continue; }
            
            // Compare PEEP2
            INY
            LDA [ZP.IDX], Y
            CMP OpCode.INVALID
            if (NZ) // do we have a 3rd instruction in the pattern?
            {
                CMP ZP.PEEP2
                if (NZ) { continue; }
                
                // Compare PEEP3
                INY
                LDA [ZP.IDX], Y
                CMP OpCode.INVALID
                if (NZ) // do we have a 4th instruction in the pattern?
                {
                    CMP ZP.PEEP3
                    if (NZ) { continue; }
                }
            }
            
            // The opcodes match the pattern!
            
            // store the number of instructions in the pattern
            INY
            STY ZP.PEEPOPS
            
            // store operand constraints
            LDY #4
            LDA [ZP.IDX], Y 
            STA ZP.PEEPCONSTRAINTS
            
            // store replacement opcode's operands
            LDY #5
            LDA [ZP.IDX], Y
            STA ZP.PEEPOPERANDS
            
            // store replacement opcode
            LDY #6
            LDA [ZP.IDX], Y
            STA ZP.PEEPREPLACE 
            
            // Load operands for instructions that have single byte operands
            LDA ZP.XPCL
            STA ZP.IDYL
            LDA ZP.XPCH
            STA ZP.IDYH
            
            STZ ZP.PEEPOP0
            STZ ZP.PEEPOP1
            STZ ZP.PEEPOP2
            STZ ZP.PEEPOP3
            
            // PEEP0 - step back 1 to opcode
            LDA ZP.PEEP0
            stepBack();
            AND #0xC0
            CMP #0x40
            if (Z) // Single byte operand
            {
                LDY #1
                LDA [ZP.IDY], Y
                STA ZP.PEEPOP0
            }
            
            // PEEP1 - step back 1 to opcode
            LDA ZP.PEEP1
            stepBack();
            AND #0xC0
            CMP #0x40
            if (Z) // Single byte operand
            {
                LDY #1
                LDA [ZP.IDY], Y
                STA ZP.PEEPOP1
            }
            
            LDA ZP.PEEPOPS
            CMP #2
            if (NZ) // must be 3 or 4
            {
                // PEEP2 - step back 1 to opcode
                LDA ZP.PEEP2
                stepBack();
                AND #0xC0
                CMP #0x40
                if (Z) // Single byte operand
                {
                    LDY #1
                    LDA [ZP.IDY], Y
                    STA ZP.PEEPOP2
                }
            }
            LDA ZP.PEEPOPS
            CMP #4
            if (Z)
            {
                // PEEP3 - step back 1 to opcode
                LDA ZP.PEEP3
                stepBack();
                AND #0xC0
                CMP #0x40
                if (Z) // Single byte operand
                {
                    LDY #1
                    LDA [ZP.IDY], Y
                    STA ZP.PEEPOP3
                }
            }
            
            // Test constraints:
            LDA ZP.PEEPCONSTRAINTS
            AND #PeepConstraint.EqualOperands0and3
            if (NZ)
            {
                LDA ZP.PEEPOP0
                CMP ZP.PEEPOP3
                if (NZ)
                {
                    // operand 0 != operand 3
                    continue;
                }
            }
            
            LDA ZP.PEEPCONSTRAINTS
            AND #PeepConstraint.EqualOperands0and2
            if (NZ)
            {
                LDA ZP.PEEPOP0
                CMP ZP.PEEPOP2
                if (NZ)
                {
                    // operand 0 != operand 2
                    CLC
                    continue;
                }
            }
            
            // We have a winner!!
#ifdef DEBUG            
            DumpPeeps();
#endif
            
            // remove ZP.PEEPOPS instructions
            LDX ZP.PEEPOPS
            loop
            {
                LDA ZP.PEEP0
                stepBackXPC();
                popPeepOp();
                DEX
                if (Z) { break; }
            }
            
            // Emit ZP.PEEPREPLACE opcode
            
            // Write opcode
            LDA ZP.PEEPREPLACE
            STA Compiler.compilerOpCode
            STA [ZP.XPC]
            STA Compiler.compilerLastOpCode
            incXPC();
            
            pushPeepOp(); // Compiler.compilerOpCode is the new PEEP0
            
            
            // Write operands
            LDA ZP.PEEPOPERANDS
            switch (A)
            {
                case NewOperands.Operand0:
                {
                    LDA ZP.PEEPOP0
                    STA [ZP.XPC]
                    incXPC();
                }
                case NewOperands.Operand0AndOperand2:
                {
                    LDA ZP.PEEPOP0
                    STA [ZP.XPC]
                    incXPC();
               
                    LDA ZP.PEEPOP2
                    STA [ZP.XPC]
                    incXPC();
                }
                case NewOperands.Operand0AndOperand3:
                {
                    LDA ZP.PEEPOP0
                    STA [ZP.XPC]
                    incXPC();
                    
                    LDA ZP.PEEPOP3
                    STA [ZP.XPC]
                    incXPC();
                }
                
                default:
                {
                    Error.TODO(); BIT ZP.EmulatorPCL
                }
            }
            
#ifdef DEBUG            
            //DumpPeeps();
#endif
            
            SEC // success   
            break; // Exit after applying optimization
        }
    }
       
    // ZP.XPCH points beyond the last byte added to the opcode stream    
    // current opcode is in Compiler.compilerOpCode
    Peep()
    {
        PHA
        PHX
        PHY
        
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {
            pushPeepOp(); // Compiler.compilerOpCode is the new PEEP0
            
            tryOptimize();
            if (C)
            {
                continue; // try another pattern
            }
            
            break;
        } // single exit
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        PLY
        PLX
        PLA
    }

#endif
     
    
    // Check if variable is a simple integral constant that can be inlined
    // Input: ZP.IDX = variable node address
    // Output: C set if simple constant, ZP.TOP = value, ZP.TOPT = type
    //         NC if not eligible for inlining
    // Preserves: ZP.IDX, all tokenizer state
    const string isSimpleConstantTrace = "IsSimpleConst";
    IsSimpleIntegralConstant()
    {
    #ifdef TRACE
        LDA #(isSimpleConstantTrace % 256) STA ZP.TraceMessageL 
        LDA #(isSimpleConstantTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodEntry();
    #endif

        LDA ZP.TOPL
        PHA
        LDA ZP.TOPH
        PHA
                
        loop
        {
            // 1. Check if variable has CONSTANT flag
            Variables.GetType(); // Output: ZP.ACCT = packed type
            LDA ZP.ACCT
            AND #SymbolType.CONSTANT
            if (Z)
            {
                CLC // Not a constant
                break;
            }
            
            // 2. Get initialization token stream
            Variables.GetTokens(); // Output: ZP.ACC = token stream pointer
            LDA ZP.NEXTL
            ORA ZP.NEXTH
            if (Z)
            {
                CLC // No initialization tokens
                break;
            }
            
            // 3. Save ALL tokenizer state
            LDA ZP.TokenizerPosL
            PHA
            LDA ZP.TokenizerPosH  
            PHA
            LDA ZP.TokenBufferL
            PHA
            LDA ZP.TokenBufferH
            PHA
            LDA ZP.TokenBufferContentLengthL
            PHA
            LDA ZP.TokenBufferContentLengthH
            PHA
            LDA ZP.CurrentToken
            PHA
            
            loop // Nested single exit loop for parsing
            {
                // Point tokenizer to initialization stream
                LDA ZP.NEXTL
                STA ZP.TokenBufferL
                LDA ZP.NEXTH
                STA ZP.TokenBufferH
                STZ ZP.TokenizerPosL    // Start at beginning
                STZ ZP.TokenizerPosH 
                
                // Set arbitrary large buffer size - we only need ~10 bytes
                LDA #0xFF
                STA ZP.TokenBufferContentLengthL
                STZ ZP.TokenBufferContentLengthH  // 255 bytes - way more than needed
                
                
                // Get first token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { CLC break; }
                
                // Check if it's a literal
                LDA ZP.CurrentToken
                switch (A)
                {
                    case Token.EOL:
                    case Token.EOF:
                    {
                        CLC // tokenizer failed?
                        break;
                    }
                    case Token.NUMBER:
                    {
                        // Get the literal value
                        Tokenizer.GetTokenNumber(); // Output: ZP.TOP = value, ZP.TOPT = type
                        // Check next token is EOL
                        Tokenizer.NextToken(); 
                        LDA ZP.CurrentToken
                        CMP #Token.EOL
                        if (Z)
                        {
                            SEC // Success - simple number constant
                            break;
                        }
                        CLC // Not just a number
                        break;
                    }
                    case Token.TRUE:
                    {
                        // TRUE constant
                        LDA #1
                        STA ZP.TOPL
                        STZ ZP.TOPH
                        LDA #BASICType.BIT
                        STA ZP.TOPT
                        
                        // Check next token is EOF
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Token.EOL
                        if (Z)
                        {
                            SEC // Success - simple TRUE
                            break;
                        }
                        CLC // Not just TRUE
                        break;
                    }
                    case Token.FALSE:
                    {
                        // FALSE constant  
                        STZ ZP.TOPL
                        STZ ZP.TOPH
                        LDA #BASICType.BIT
                        STA ZP.TOPT
                        
                        // Check next token is EOF
                        Tokenizer.NextToken();
                        LDA ZP.CurrentToken
                        CMP #Token.EOL
                        if (Z)
                        {
                            SEC // Success - simple FALSE
                            break;
                        }
                        CLC // Not just FALSE
                        break;
                    }
                    default:
                    {
                        // Not a simple literal
                        CLC
                        break;
                    }
                } // switch
            } // stack balance
            
            // Restore ALL tokenizer state (in reverse order)
            PLA STA ZP.CurrentToken
            PLA STA ZP.TokenBufferContentLengthH
            PLA STA ZP.TokenBufferContentLengthL 
            PLA STA ZP.TokenBufferH
            PLA STA ZP.TokenBufferL
            PLA STA ZP.TokenizerPosH
            PLA STA ZP.TokenizerPosL
            
            // Carry flag is correctly set from inner loop
            break;
        } // single exit
        
        if (C)
        {
            PLA PLA
        }
        else
        {
            // restore global name
            PLA STA ZP.TOPH
            PLA STA ZP.TOPL
        }
        
    #ifdef TRACE
        LDA #(isSimpleConstantTrace % 256) STA ZP.TraceMessageL 
        LDA #(isSimpleConstantTrace / 256) STA ZP.TraceMessageH 
        Trace.MethodExit();
    #endif
    }
    
}
