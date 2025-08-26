unit Statement // Statement.asm
{
    uses "Tokenizer"
    uses "Instructions"
    uses "FunctionDeclaration.asm"
    uses "Emit"
    uses "Compiler"
    
    friend FunctionDeclaration, Console, Compiler;
    
    // Private Statement layer storage - BasicStatementWorkspace (32 bytes)
    const uint stmtNamePtr     = Address.BasicStatementWorkspace;      // 2 bytes - identifier name pointer
    const uint stmtValue       = Address.BasicStatementWorkspace + 2;  // 2 bytes - initial/evaluated value
    const uint stmtTokensPtr   = Address.BasicStatementWorkspace + 4;  // 2 bytes - tokens pointer  
    const uint stmtTokPos      = Address.BasicStatementWorkspace + 8;  // 2 bytes - saved tokenizer position
    const uint stmtTokLen      = Address.BasicStatementWorkspace + 10; // 2 bytes - token stream length
    const uint stmtSymbol      = Address.BasicStatementWorkspace + 12; // 1 byte  - symbol information
    const uint stmtType        = Address.BasicStatementWorkspace + 13; // 1 byte  - type information
    const uint stmtIsConstant  = Address.BasicStatementWorkspace + 14; // 1 byte  - current expression is const
    const uint stmtObjectPtr   = Address.BasicStatementWorkspace + 15; // 2 bytes - object node pointer
    const uint stmtStringPtr   = Address.BasicStatementWorkspace + 17; // 2 bytes - string memory pointer
    
    const uint funcCaptureMode       = Address.BasicStatementWorkspace + 19; // 1 byte - console mode
    const uint funcCaptureStartPos   = Address.BasicStatementWorkspace + 20; // 2 bytes - start position in token buffer
    const uint funcOriginalLength    = Address.BasicStatementWorkspace + 22; // 2 bytes - original buffer length when capture started
    
    const uint replFunctionPtr       = Address.BasicStatementWorkspace + 24; // 2 bytes
    
    const uint declInitializer       = Address.BasicStatementWorkspace + 26; // 1 bytes
    
    flags CaptureMode
    {
        Off = 0,
        Func = 1,
        Begin = 2
    }
    
    SetCaptureMode()
    {
        STA funcCaptureMode
        /*
        IsCaptureModeOn();
        if (C)
        {
            UseBASICBuffers();
        }
        else
        {
            UseREPLBuffers();
        }
        */
    }
    IsCaptureModeOff()
    {
        LDA funcCaptureMode
        CMP # CaptureMode.Off
        if (Z)
        {
            SEC // C = CaptureMode.Off
        }   
        else
        {
            CLC // NC = not CaptureMode.Off
        }
    }
    IsCaptureModeOn()
    {
        LDA funcCaptureMode
        CMP # CaptureMode.Off
        if (Z)
        {
            CLC // NC = CaptureMode.Off
        }   
        else
        {
            SEC // C = not CaptureMode.Off
        }
    }
    IsCaptureModeBegin()
    {
        LDA funcCaptureMode
        CMP # CaptureMode.Begin
        if (Z)
        {
            SEC // C = CaptureMode.Begin
        }   
        else
        {
            CLC // NC = not CaptureMode.Begin
        }
    }
    IsCaptureModeFunc()
    {
        LDA funcCaptureMode
        CMP # CaptureMode.Func
        if (Z)
        {
            SEC // C = CaptureMode.Func
        }   
        else
        {
            CLC // NC = not CaptureMode.Func
        }
    }
    GetCaptureMode()
    {
        LDA funcCaptureMode
    }
    
    
    SetIsConstant()
    {
        STA stmtIsConstant
    }
    IsConstant()
    {
        LDA stmtIsConstant
        if (Z)
        {
            CLC // not constant (stmtISConstant == 0)
        }   
        else
        {
            SEC // constant (stmtISConstant == 1)
        }
    }
    
    
    // Resolve identifier to its type
    // Input: ZP.CurrentToken = IDENTIFIER token or keyword
    // Output: ZP.ACCT = IdentifierType, ZP.IDX = node address (if found), ZP.ACCL as BP offset if Local
    // Modifies: ZP.TOP, ZP.IDX, ZP.ACCT
    const string resolveIdentifierTrace = "ResolveId";
    ResolveIdentifier()
    {
        PHA
        PHX
        PHY
        
    #ifdef TRACE
        PHA LDA #(resolveIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(resolveIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
    #endif

        
                        
        loop // Single exit block for clean error handling
        {
            // 1. Check if it's a keyword
            Tokens.IsKeyword(); // Input: A = token value to check
            if (C)
            {
                LDA #IdentifierType.Keyword
                STA ZP.ACCT
                break; // success
            }
            
            // Get the identifier name for lookup
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            CheckError();
            if (NC) { break; }

            // 2. Check if it's a local or argument (if we're in a function)
            Locals.Resolve(); // Input: ZP.TOP = name, Output: C = found, ZP.IDX = node, ACCT = type
            if (C)
            {
                break; // success
            }
            
            // 3. Check if it's a global variable or constant
            Variables.Resolve(); // Input: ZP.TOP = name, Output: C = found, ACCT = type, ZP.IDX = node
            if (C)
            {
                // A contains IdentifierType.Global or IdentifierType.Constant
                break; // success
            }
            
            // 4. Check if it's a function
            Functions.Find(); // Input: ZP.TOP = name, Output: C = found, ZP.IDX = node, ZP.ACCL = signed one byte BP offset
            if (C)
            {
                LDA # IdentifierType.Function
                STA ZP.ACCT
                break; // success
            }
            
            // Not found anywhere
            LDA #IdentifierType.Undefined
            CLC
            break;
        } // end of single exit block
        
    #ifdef TRACE
        PHA LDA #(resolveIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(resolveIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
    #endif
        
        PLY
        PLX
        PLA
    }
    
    // Evaluate expression using JIT compilation
    // Input: ZP.CurrentToken = first token of expression
    // Output: Expression result pushed onto value stack
    //         ZP.CurrentToken = token after expression
    //         C set if successful, NC if error
    // Munts: Stack, ZP.CurrentToken, all ZP variables used by parsing
    // Error: Sets ZP.LastError if syntax error or type mismatch
    const string strEvaluateExpression = "EvalExpr";
    EvaluateExpression()
    {
#ifdef TRACE
        LDA #(strEvaluateExpression % 256) STA ZP.TraceMessageL LDA #(strEvaluateExpression / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            // Set literal base to TokenizerBuffer for REPL
            Compiler.SetLiteralBase();
            Compiler.CompileExpression();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            if (BBS0, ZP.CompilerFlags)
            {
                // no need to execute - we already have the result
                Long.PushTop();
                States.SetSuccess();
                break;
            }
            
            // Emit HALT for REPL
            Emit.Halt();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation (important for function calls from REPL)
            LDA ZP.OpCodeBufferContentLengthL
            PHA
            LDA ZP.OpCodeBufferContentLengthH
            PHA
            
            States.SetSuccess(); // clear
            
            // 2. Execute opcodes ? result on stack
            Executor.ExecuteOpCodes();
            
            // Restore opcode buffer length after execution (important for function calls from REPL)
            PLA
            STA ZP.OpCodeBufferContentLengthH
            PLA
            STA ZP.OpCodeBufferContentLengthL
            
            CheckError(); 
            if (NC)
            {
                States.SetFailure(); 
                break;
            } 
            States.SetSuccess(); // TODO : consider State.Return vs State.Success in terms of stack slot
            
            // Result is now on stack
            break;
        } // single exit
        
#ifdef TRACE
        LDA #(strEvaluateExpression % 256) STA ZP.TraceMessageL LDA #(strEvaluateExpression / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    // Add this to Statement.asm - follows same pattern as EvaluateExpression()

    // Execute statement using JIT compilation
    // Input: ZP.CurrentToken = first token of statement (or reset tokenizer position first)
    // Output: Statement compiled and executed
    //         ZP.CurrentToken = token after statement
    //         C set if successful, NC if error
    // Munts: Stack, ZP.CurrentToken, all ZP variables used by compilation/execution
    // Error: Sets ZP.LastError if compilation or execution error
    const string strExecuteStatement = "ExecStmt";
    ExecuteStatement()
    {
    #ifdef TRACE
        LDA #(strExecuteStatement % 256) STA ZP.TraceMessageL LDA #(strExecuteStatement / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        loop
        {

            // Set literal base to TokenizerBuffer for REPL (same as EvaluateExpression)
            Compiler.SetLiteralBase();
            
            // Initialize opcode buffer if this is the start of compilation  
            BufferManager.UseREPLOpCodeBuffer();
            Compiler.InitOpCodeBuffer();
            CheckError();
            if (NC) { States.SetFailure(); break; }

            // Compile the statement (not expression)
            Compiler.CompileStatement();
            CheckError();
            if (NC) { States.SetFailure(); break; }

            // Emit HALT for REPL
            Emit.Halt();
            CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation 
            LDA ZP.OpCodeBufferContentLengthL
            PHA
            LDA ZP.OpCodeBufferContentLengthH
            PHA
            
            States.SetSuccess(); // Clear state

            // Execute the compiled statement opcodes
            Executor.ExecuteOpCodes();
            CheckError();
            
            // Restore opcode buffer length
            PLA
            STA ZP.OpCodeBufferContentLengthH
            PLA
            STA ZP.OpCodeBufferContentLengthL
            
            CheckError(); 
            if (NC)
            {
                States.SetFailure(); 
                break;
            } 
            
            States.SetSuccess();
            break;
        }
    
    #ifdef TRACE
        LDA #(strExecuteStatement % 256) STA ZP.TraceMessageL LDA #(strExecuteStatement / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    
    // Execute a statement starting from current token position
    // Input: ZP.CurrentToken = first token of statement
    // Output: Statement executed, stack may contain results
    //         ZP.CurrentToken = token after statement
    // Munts: Stack, ZP.CurrentToken, all parsing and execution variables
    // Error: Sets ZP.LastError if statement execution fails
    const string executeTrace = "Execute";
    Execute()
    {
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL LDA #(executeTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        LDA ZP.CurrentToken
        
        switch (A)
        {
            case Token.REM:
            case Token.COMMENT:
            {
                // Comments are no-ops - just advance to next token
                Tokenizer.NextToken();
                SEC  // Success
            }
            case Token.FUNC:
            {
                FunctionDeclaration.ExecuteFunctionDeclaration();
            }
            case Token.BEGIN:
            {
                FunctionDeclaration.ExecuteBeginDeclaration();
            }
            
            case Token.PRINT:
            case Token.CLS:
            case Token.IDENTIFIER: // Could be assignment or function call
            {
                SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                Statement.ExecuteStatement(); // EXECUTION: IDENTIFIER, IDENTIFIER(), PRINT, CLS - GLOBAL LOAD SAVE
            }
            
            case Token.CONST:
            {
                executeConstantDeclaration();
            }
            
            // array types
            case Token.INT:
            case Token.WORD:
            case Token.BYTE:
            case Token.BIT:
            case Token.CHAR:
            
            // inferred variable type
            case Token.VAR: 
            {
                executeVariableDeclaration();
            }
            
            case Token.STRING:
            case Token.LONG:
            {
                Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
            }
            
            default:
            {
                // Unexpected token for statement
                Error.SyntaxError(); BIT ZP.EmulatorPCL
            }
        } // switch
        
        if (C) // Only if statement executed successfully
        {
            LDA ZP.CurrentToken
            CMP #Token.REM
            if (Z)
            {
                Tokenizer.NextToken(); // Skip REM and consume comment text
            }
            else
            {
                CMP #Token.COMMENT  
                if (Z)
                {
                    Tokenizer.NextToken(); // Skip COMMENT and consume comment text
                }
            }
        }
        
#ifdef TRACE
        LDA #(executeTrace % 256) STA ZP.TraceMessageL LDA #(executeTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
        
        IsTracing();
        if (C)
        {
            //DumpBuffers();
        }
#endif
    }
    
    
    // Input: ZP.CurrentToken = CONST
    const string executeConstDeclTrace = "ExecConst";
    executeConstantDeclaration()
    {
    #ifdef TRACE
        LDA #(executeConstDeclTrace % 256) STA ZP.TraceMessageL LDA #(executeConstDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
    #endif
        loop
        {
            Tokenizer.NextToken(); // consume 'CONST'
            if (NC) 
            {
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break; // error exit
            } 
            
            // Check that VAR is not used with CONST
            LDA ZP.CurrentToken
            CMP #Token.VAR
            if (Z)
            {
                // CONST VAR is invalid - constants cannot change type
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                break; // error exit
            }
            
            LDA #SymbolType.CONSTANT
            STA stmtSymbol
            processSingleSymbolDeclaration();
            break;
        } // single exit
        
    #ifdef TRACE
        LDA #(executeConstDeclTrace % 256) STA ZP.TraceMessageL LDA #(executeConstDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
    #endif
    }
    
    // Execute variable declaration statement
    // Input: ZP.CurrentToken = type token (INT, WORD, BIT)
    // Output: Variable declared and added to symbol table
    //         ZP.CurrentToken = token after declaration
    // Munts: Stack, ZP.CurrentToken, symbol tables, memory allocation, 
    //        all statement buffer locations, all parsing variables
    // Error: Sets ZP.LastError if syntax error, type mismatch, name conflict, or memory allocation fails
    const string executeVarDeclTrace = "ExecVarDecl";
    executeVariableDeclaration()
    {
#ifdef TRACE
        LDA #(executeVarDeclTrace % 256) STA ZP.TraceMessageL LDA #(executeVarDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        LDA #SymbolType.VARIABLE
        STA stmtSymbol
        
        loop
        {
            processSingleSymbolDeclaration(); // Consumes VAR, processes variable
            CheckError();
            if (NC) { break; }
            
            // Check for comma
            LDA ZP.CurrentToken
            CMP #Token.COMMA
            if (NZ) { break; }
            
            // Set up for next iteration: pretend the COMMA is a VAR ..
            LDX #Token.VAR
        }
        
#ifdef TRACE
        LDA #(executeVarDeclTrace % 256) STA ZP.TraceMessageL LDA #(executeVarDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    const string processSingleSymbolDeclTrace = "ProcSymDecl";
    processSingleSymbolDeclaration()
    {
#ifdef TRACE
        LDA #(processSingleSymbolDeclTrace % 256) STA ZP.TraceMessageL LDA #(processSingleSymbolDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop
        {
            // Initialize string pointer tracking
            STZ (stmtStringPtr + 0)
            STZ (stmtStringPtr + 1)

            // Current token can be:
            // - BYTE, BIT, CHAR, WORD, INT if it is an ARRAY
            // - VAR if it is a variable
            // - EQUAL if it is a constant
            STZ stmtType
            
            // Output: C set if token is a type keyword, NC if not a type keyword, A = BASICType
            BASICTypes.FromToken();
            if (C)
            {
                switch (A)
                {
                    case BASICType.CHAR:
                    case BASICType.BIT:
                    case BASICType.BYTE:
                    case BASICType.WORD:
                    case BASICType.INT:
                    {
                        // ARRAY type?
                        STA stmtType // LHS type
                        
                        LDA stmtSymbol
                        CMP #SymbolType.CONSTANT
                        if (Z)
                        {
                            Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                        }
                        
                        // advance to the IDENTIFIER
                        Tokenizer.NextToken();                      
                        CheckError();
                        if (NC) { break; } // error exit
                        
                    }
                    case BASICType.VAR:
                    {
                        // variable type
                        ORA # BASICType.LONG // good default until assignment says otherwise
                        STA stmtType // LHS type
                        
                        LDA stmtSymbol
                        CMP #SymbolType.CONSTANT
                        if (Z)
                        {
                            Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                        }
                        
                        // advance to the IDENTIFIER
                        Tokenizer.NextToken();                      
                        CheckError();
                        if (NC) { break; } // error exit
                    }
                    default:
                    {
                        // must be a constant, type defined by inference later
                        LDA # BASICType.VOID
                        STA stmtType
                        
                        LDA stmtSymbol
                        CMP #SymbolType.CONSTANT
                        if (NZ)
                        {
                            Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                        }
                        TAX
                        CMP #Token.EQUALS
                        if (NZ)
                        {
                            Error.ExpectedEqual(); BIT ZP.EmulatorPCL
                        }
                        
                    }
                }
            }
            
            LDX ZP.CurrentToken // IDENTIFIER token
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP # Token.IDENTIFIER
            if (NZ)
            {
                Tokens.IsKeyword();
                if (C)
                {
                    Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                }
                else
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                }
                CLC
                break; // error exit
            }
            
            // Get the identifier string
            Tokenizer.GetTokenString();  // Returns pointer in ZP.TOP
           
            loop
            {
                // Save name pointer
                LDA ZP.TOPL
                PHA
                LDA ZP.TOPH
                PHA
                
                // function by same name exists? name pointer in TOP
                LDX #ZP.FunctionsList
                Objects.Find(); // ZP.IDX = symbol node address
                if (C)  
                {
                    Error.FunctionExists(); BIT ZP.EmulatorPCL
                    CLC  // Error
                    break;
                }
                // if it already exists, check type compatibility before replacing: const for const, var for var
                STZ ZP.SymbolIteratorFilter // constant or variable
                Variables.Find(); // ZP.IDX = symbol node address
                if (C)
                {
                    // Get existing symbol type
                    Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                    CheckError();
                    if (NC) { break; }
                    
                    LDA ZP.ACCT
                    AND # SymbolType.MASK  // Extract existing symbol type
                    STA ZP.NEXTT           // Temporarily store existing type
                    
                    // Compare with new symbol type being declared
                    LDA stmtSymbol // New symbol type (VARIABLE or CONSTANT)
                    CMP ZP.NEXTT
                    if (NZ)
                    {
                        // Type mismatch - show appropriate error
                        LDA ZP.NEXTT
                        CMP # SymbolType.CONSTANT
                        if (Z)
                        {
                            Error.ConstantExists(); BIT ZP.EmulatorPCL
                        }
                        else
                        {
                            Error.VariableExists(); BIT ZP.EmulatorPCL
                        }
                        CLC
                        break;
                    }
                    
                    // Same type - allow replacement by removing existing
                    Variables.Remove(); // delete it (name ptr in TOP)
                    if (NC) { break; }
                }
                
                Tokenizer.NextToken();
                CheckError();
                if (NC) { break; } // error exit
                
                //STZ ZP.TOPT // set RHS type to 0 if there is no initializer
                STZ declInitializer // no initializer
                
                // Check for optional initialization
                LDX ZP.CurrentToken
                switch (X)
                {
                
                    case Token.LBRACKET:
                    {
                        // This is an array declaration
                        // Set ARRAY flag in type
                        LDA stmtType
                        ORA # BASICType.ARRAY
                        STA stmtType
                        
                        // Save tokenizer position before expression
                        LDA ZP.TokenizerPosL
                        PHA
                        LDA ZP.TokenizerPosH
                        PHA
                        
                        // Get next token (start of expression)
                        Tokenizer.NextToken();
                        CheckError();
                        if (C)
                        {
                            SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                            SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                            Statement.EvaluateExpression(); // EXECUTION: initialize ARRAY (size expression)- GLOBAL LOAD SAVE (our current variable does not exist yet)
                            CheckError();
                        }
                        
                        // Restore tokenizer position before expression
                        PLA
                        STA ZP.FSOURCEADDRESSH
                        PLA
                        STA ZP.FSOURCEADDRESSL
                        
                        if (NC) { break; } // error exit
                        
                        // subtract current tokenizer position
                        SEC
                        LDA ZP.TokenizerPosL
                        SBC ZP.FSOURCEADDRESSL
                        STA ZP.FLENGTHL  // Length low
                        LDA ZP.TokenizerPosH
                        SBC ZP.FSOURCEADDRESSH
                        STA ZP.FLENGTHH  // Length high
                        
                        // strip the RBRACKET
                        LDA ZP.FLENGTHL
                        if (Z)
                        {
                            DEC ZP.FLENGTHH
                        }
                        DEC ZP.FLENGTHL
                        
                        Tools.CreateTokenStream(); // Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, -> ZP.IDY
                        if (NC) { break; } // error exit
                        
                        // Set tokens pointer to the new stream
                        LDA ZP.IDYL
                        STA (stmtTokensPtr + 0)
                        LDA ZP.IDYH
                        STA (stmtTokensPtr + 1)
                     
                        // Pop the result into NEXT
                        Long.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT,  modifies X
                        
                        LDA ZP.NEXTT
                        CMP # BASICType.LONG
                        if (Z)
                        {
                            LDA ZP.NEXT2
                            ORA ZP.NEXT3
                            if (NZ)
                            {
                                Error.BadIndex(); BIT ZP.EmulatorPCL
                                CLC
                                break; 
                            }
                            // ok
                        }
                        else
                        {
                            Error.BadIndex(); BIT ZP.EmulatorPCL
                            CLC
                            break; 
                        }
                        
                        INC declInitializer
                        SEC
                        
                        // Check for RBRACKET
                        LDA ZP.CurrentToken
                        CMP #Token.RBRACKET
                        if (NZ)
                        {
                            Error.ExpectedRightBracket(); BIT ZP.EmulatorPCL
                            CLC
                            break;
                        }
                        
                        // Move past RBRACKET
                        Tokenizer.NextToken();
                        CheckError();
                        if (NC) { break; }
                    }
                    case Token.EQUALS:
                    {
                        LDA stmtType
                        AND # BASICType.TYPEMASK
                        switch (A)
                        {
                            case BASICType.CHAR:
                            case BASICType.BYTE:
                            case BASICType.INT:
                            case BASICType.WORD:
                            {
                                Error.IllegalIdentifier(); BIT ZP.EmulatorPCL // not an ARRAY anymore
                                break;
                            }
                        }
                        
                        // Save tokenizer position before expression
                        LDA ZP.TokenizerPosL
                        PHA
                        LDA ZP.TokenizerPosH
                        PHA
                                               
                        // Get next token (start of expression)
                        Tokenizer.NextToken();
                        CheckError();
                        if (C)
                        {
                            SMB4 ZP.FLAGS // Bit 4 - initialization mode: Load and Save globals to stack (ExecuteOpCodes)
                            SMB5 ZP.FLAGS // Bit 5 - initialization mode: do not create a RETURN slot for REPL calls (in compileFunctionCallOrVariable)
                            Statement.EvaluateExpression(); // EXECUTION: initialize global variable (RHS expression) - GLOBAL LOAD SAVE (our current variable does not exist yet)
                            CheckError();
                        }
                        
                        // Restore tokenizer position before expression
                        PLA
                        STA ZP.FSOURCEADDRESSH
                        PLA
                        STA ZP.FSOURCEADDRESSL
                        
                        if (NC) { break; } // error exit
                        
                        // subtract current tokenizer position
                        SEC
                        LDA ZP.TokenizerPosL
                        SBC ZP.FSOURCEADDRESSL
                        STA ZP.FLENGTHL  // Length low
                        LDA ZP.TokenizerPosH
                        SBC ZP.FSOURCEADDRESSH
                        STA ZP.FLENGTHH  // Length high
                        Tools.CreateTokenStream(); // Munts: A, ZP.IDY, ZP.ACC, ZP.FLENGTH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS, -> ZP.IDY
                        if (NC) { break; } // error exit
                        
                        // Set tokens pointer to the new stream
                        LDA ZP.IDYL
                        STA (stmtTokensPtr + 0)
                        LDA ZP.IDYH
                        STA (stmtTokensPtr + 1)

                        Long.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT,  modifies X

                        if (BBR0, ZP.CompilerFlags) // constant expression:  was constant expression, the folded value is on VM stack
                        {
                            LDA stmtSymbol
                            CMP #SymbolType.CONSTANT
                            if (Z)
                            {
                                LDA ZP.NEXTT
                                CMP #BASICType.STRING // constant expression of sorts
                                if (NZ)
                                {
                                    Error.ConstantExpressionExpected(); BIT ZP.EmulatorPCL
                                    break; // error exit
                                }
                            }
                        }
                        
                        
                        INC declInitializer
                        SEC
                    }
                    case Token.EOL:
                    case Token.COLON:
                    case Token.COMMA:
                    {
                        LDA stmtSymbol
                        CMP #SymbolType.CONSTANT
                        if (Z)
                        {
                            // constants must be initialized
                            Error.ConstantExpressionExpected(); BIT ZP.EmulatorPCL
                            CLC
                            break; // error exit
                        }
                        
                        LDA stmtType
                        AND # BASICType.TYPEMASK
                        switch (A)
                        {
                            case BASICType.CHAR:
                            case BASICType.BYTE:
                            case BASICType.INT:
                            case BASICType.WORD:
                            {
                                Error.IllegalIdentifier(); BIT ZP.EmulatorPCL // not an ARRAY anymore
                                break;
                            }
                        }
                        
                        // Pure VAR variable without initialization
                        STZ ZP.NEXT0
                        STZ ZP.NEXT1
                        STZ ZP.NEXT2
                        STZ ZP.NEXT3
                        
                        // Default VAR to LONG with value 0
                        LDA #BASICType.LONG
                        STA ZP.NEXTT
                        
                        // no expression tokens
                        STZ ZP.IDXH
                        STZ ZP.IDXL
                        
                        SEC  // Success
                    }
                    default:
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        CLC
                        break; // error exit
                    }
                } // switch
                
                SEC  // Success
                break;
            }
            // Now restore name IDENTIFIER pointer to TOP
            PLA
            STA ZP.TOP1
            PLA
            STA ZP.TOP0
            break;
        } // loop
        
        loop
        {
            if (NC)
            {
                break; // error exit
            }
                     
            LDA declInitializer // did we have "= <expression>"?
            if (NZ)
            {
                // ARRAY : stmtType is correct
                // VAR   : stmtType is correct
                
                LDA stmtSymbol
                AND # SymbolType.CONSTANT
                if (NZ)
                {
                    // CONSTANT: infer type from type of initializer expression
                    LDA ZP.NEXTT
                    STA stmtType
                }
                SEC
            }
            else
            {
                SEC // absent RHS is ok, default to INT -> 0, BIT -> FALSE, STRING -> empty
            }
            
            // For VAR variables, update type based on initialization
            LDA stmtType
            AND # BASICType.VAR
            if (NZ)  // VAR variable
            {
                // Set type to RHS type while preserving VAR bit
                LDA ZP.NEXTT  // RHS type from expression
                ORA #BASICType.VAR  // Keep VAR bit
                STA stmtType
            }
            
            
            // Pack symbolType|dataType: SymbolType in top 3 bits, dataType in bottom 5 bits
            LDA stmtType  // dataType (bottom 6 bits)
            ORA stmtSymbol // SymbolType (top 2 bits)
            STA ZP.ACCT
            
            LDA (stmtTokensPtr+0)
            STA ZP.IDYL
            LDA (stmtTokensPtr+1)
            STA ZP.IDYH
            
            // Call Variables.Declare
            // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
            //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
            Variables.Declare();
            CheckError();
            if (C)
            {
                // Success - ownership transferred to Variables table
                STZ (stmtTokensPtr+0)
                STZ (stmtTokensPtr+1)
            }
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            break;
        } // loop
        
        LDA (stmtTokensPtr+0)
        ORA (stmtTokensPtr+1)
        if (NZ)
        {
            LDA (stmtTokensPtr+0)
            STA ZP.IDXL
            LDA (stmtTokensPtr+1)
            STA ZP.IDXH
            Memory.Free(); // Input: ZP.IDX, Munts: A, ZP.IDX, ZP.M* -> C on success
        }
        LDA (stmtStringPtr+0)
        ORA (stmtStringPtr+1)
        if (NZ)
        {
            LDA (stmtStringPtr+0)
            STA ZP.IDXL
            LDA (stmtStringPtr+1)
            STA ZP.IDXH
            Memory.Free(); // Input: ZP.IDX, Munts: A, ZP.IDX, ZP.M* -> C on success
        }

#ifdef TRACE
        LDA #(processSingleSymbolDeclTrace % 256) STA ZP.TraceMessageL LDA #(processSingleSymbolDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
      
    
}
