unit Optimizer
{
    uses "./Definitions/OpCodes"
    
    ClearPeeps()
    {
        STZ ZP.PEEP3
        STZ ZP.PEEP2
        STZ ZP.PEEP1
        STZ ZP.PEEP0
    }
    
    // ZP.XPCH points beyond the last byte added to the opcode stream    
    // current opcode is in Compiler.compilerOpCode
    Peep()
    {
        LDA ZP.PEEP2
        STA ZP.PEEP3
        LDA ZP.PEEP1
        STA ZP.PEEP2
        LDA ZP.PEEP0
        STA ZP.PEEP1
        LDA Compiler.compilerOpCode
        STA ZP.PEEP0
        
#ifdef DEBUG
/*
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
        }
        LDX ZP.PEEP2
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
        }
        LDX ZP.PEEP1
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
        }
        LDX ZP.PEEP0
        if (NZ)
        {
            Space(); OpCodes.ToString(); PrintStringSTR(); 
        }
        */
#endif        
    }
    
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
