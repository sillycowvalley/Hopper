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
    
    flags CaptureMode
    {
        Off = 0,
        Func = 1,
        Begin = 2
    }
    
    SetCaptureMode()
    {
        STA funcCaptureMode
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
    // Output: ZP.ACCT = IdentifierType, ZP.IDX = node address (if found)
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
            Error.CheckError();
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
            Functions.Find(); // Input: ZP.TOP = name, Output: C = found, ZP.IDX = node
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
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Emit HALT for REPL
            Emit.Halt();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation (important for function calls from REPL)
            LDA ZP.OpCodeBufferContentSizeL
            PHA
            LDA ZP.OpCodeBufferContentSizeH
            PHA
            
            States.SetSuccess(); // clear
            
            // 2. Execute opcodes ? result on stack
            Executor.ExecuteOpCodes();
            
            // Restore opcode buffer length after execution (important for function calls from REPL)
            PLA
            STA ZP.OpCodeBufferContentSizeH
            PLA
            STA ZP.OpCodeBufferContentSizeL
            
            Error.CheckError(); 
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
            Compiler.InitOpCodeBuffer();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile the statement (not expression)
            Compiler.CompileStatement();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Emit HALT for REPL
            Emit.Halt();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation 
            LDA ZP.OpCodeBufferContentSizeL
            PHA
            LDA ZP.OpCodeBufferContentSizeH
            PHA
            
            States.SetSuccess(); // Clear state
            
            // Execute the compiled statement opcodes
            Executor.ExecuteOpCodes();
            Error.CheckError();
            
            // Restore opcode buffer length
            PLA
            STA ZP.OpCodeBufferContentSizeH
            PLA
            STA ZP.OpCodeBufferContentSizeL
            
            Error.CheckError(); 
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
            case Token.IDENTIFIER: // Could be assignment or function call
            {
                ExecuteStatement();
            }
            
            case Token.CONST:
            {
                executeConstantDeclaration();
            }
            case Token.INT:
            case Token.WORD:
            case Token.BIT:
            case Token.BYTE:
            case Token.STRING:
            case Token.VAR: 
            {
                executeVariableDeclaration();
            }
            
            default:
            {
                // Unexpected token for statement
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                CLC  // Error
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
            
            // we want a constant expression
            LDA #1
            SetIsConstant();
            
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
        processSingleSymbolDeclaration();
        
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

            LDX ZP.CurrentToken
            
            // Output: C set if token is a type keyword, NC if not a type keyword, A = BASICType
            BASICTypes.FromToken();
            if (NC)
            {
                // expecting INT WORD BYTE BIT STRING
                Error.SyntaxError(); BIT ZP.EmulatorPCL
                CLC break;
            }
            
            // Check if this is a VAR type declaration
            CMP #BASICType.VAR
            if (Z)
            {
                // VAR type starts untyped but will adopt type from initialization
                // Keep as pure VAR for now, will be updated later
                STA stmtType
            }
            
            
            STA stmtType // LHS type
            
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; } // error exit
            
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
                // if it already exists, check type compatibility before replacing
                STZ ZP.SymbolIteratorFilter // constant or variable
                Variables.Find(); // ZP.IDX = symbol node address
                if (C)
                {
                    // Get existing symbol type
                    Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                    Error.CheckError();
                    if (NC) { break; }
                    
                    LDA ZP.ACCT
                    AND #0xF0  // Extract existing symbol type (high nibble)
                    STA ZP.NEXTT // Temporarily store existing type
                    
                    // Compare with new symbol type being declared
                    LDA stmtSymbol // New symbol type (VARIABLE or CONSTANT)
                    CMP ZP.NEXTT
                    if (NZ)
                    {
                        // Type mismatch - show appropriate error
                        LDA ZP.NEXTT
                        CMP #SymbolType.CONSTANT
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
                Error.CheckError();
                if (NC) { break; } // error exit
                
                STZ ZP.TOPT // set RHS type to 0 if there is no initializer
                
                // Check for optional initialization
                LDX ZP.CurrentToken
                switch (X)
                {
                    case Token.EQUALS:
                    {
                        // Save tokenizer position before expression
                        LDA ZP.TokenizerPosL
                        PHA
                        LDA ZP.TokenizerPosH
                        PHA
                        
                        // Get next token (start of expression)
                        Tokenizer.NextToken();
                        Error.CheckError();
                        if (C)
                        {
                            Statement.EvaluateExpression();
                            Error.CheckError();
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
                        CreateTokenStream();
                        if (NC) { break; } // error exit
                        
                        // Set tokens pointer to the new stream
                        LDA ZP.FDESTINATIONADDRESSL
                        STA (stmtTokensPtr + 0)
                        LDA ZP.FDESTINATIONADDRESSH
                        STA (stmtTokensPtr + 1)
                     
                        // Pop the result into NEXT
                        Stacks.PopNext();  // Result in ZP.NEXT, type in ZP.NEXTT,  modifies X
                        
                        LDA stmtSymbol
                        CMP #SymbolType.CONSTANT
                        if (Z)
                        {
                            IsConstant();
                            if (NC)
                            {
                                Error.ConstantExpressionExpected(); BIT ZP.EmulatorPCL
                                CLC
                                break; // error exit
                            }
                        }
                        SEC
                    }
                    case Token.EOL:
                    case Token.COLON:
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
                        CMP #BASICType.VAR  // Check if pure VAR (not VAR with underlying type)
                        if (Z)  // Pure VAR variable without initialization
                        {
                            // Default VAR to INT with value 0
                            LDA #BASICType.INT
                            ORA #BASICType.VAR
                            STA stmtType
                            STZ ZP.NEXTL
                            STZ ZP.NEXTH
                            LDA #BASICType.INT
                            STA ZP.NEXTT
                            SEC  // Success
                            break;
                        }
                        
                        LDA stmtType
                        CMP # BASICType.STRING
                        if (Z)
                        {
                            // STRING default: allocate copy of EmptyString
                            LDA #(Variables.EmptyString % 256)
                            STA ZP.TOPL
                            LDA #(Variables.EmptyString / 256)
                            STA ZP.TOPH
                            
                            Variables.AllocateAndCopyString(); // Input: ZP.TOP = source, Output: ZP.IDY = allocated copy
                            Error.CheckError();
                            if (NC) { break; } // allocation failed
                            
                            // Track allocated string for cleanup
                            LDA ZP.IDYL
                            STA (stmtStringPtr + 0)
                            LDA ZP.IDYH
                            STA (stmtStringPtr + 1)
                           
                            // Use allocated copy as the default value
                            LDA ZP.IDYL
                            STA ZP.NEXTL
                            LDA ZP.IDYH
                            STA ZP.NEXTH
                        }
                        else
                        {
                            // Other types default: 0 (INT->0, BIT->FALSE, WORD->0, BYTE->0)
                            STZ ZP.NEXTL
                            STZ ZP.NEXTH
                        }
                        // no expression tokens
                        STZ ZP.IDXH
                        STZ ZP.IDXL
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
            // Now restore name pointer to TOP
            PLA
            STA ZP.TOPH
            PLA
            STA ZP.TOPL
            break;
        } // loop
        
        loop
        {
            if (NC)
            {
                break; // error exit
            }
            LDA ZP.TOPL
            PHA
            LDA ZP.TOPH
            PHA
            LDA ZP.NEXTL
            PHA
            LDA ZP.NEXTH
            PHA
            
            LDA ZP.NEXTT // did we have "= <expression>"?
            if (NZ)
            {
                LDA ZP.NEXTL
                STA ZP.TOPL
                LDA ZP.NEXTH
                STA ZP.TOPH
                LDA ZP.NEXTT
                STA ZP.TOPT
                
                // Check if this is a VAR variable
                LDA stmtType
                AND #BASICType.VAR
                if (NZ)  // VAR variable - skip type checking
                {
                    SEC  // Always compatible
                }
                else  // Non-VAR variable - check type compatibility
                {
                    LDA stmtType
                    STA ZP.NEXTT // LHS type
                    
                    // RHS in TOP
                    // LHS type in NEXTT
                    CheckRHSTypeCompatibility();
                }
            }
            else
            {
                SEC // absent RHS is ok, default to INT -> 0, BIT -> FALSE, STRING -> empty
            }
            
            PLA
            STA ZP.NEXTH
            PLA 
            STA ZP.NEXTL
            PLA
            STA ZP.TOPH
            PLA 
            STA ZP.TOPL
            

            if (NC)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // For VAR variables, update type based on initialization
            LDA stmtType
            AND #BASICType.VAR
            if (NZ)  // VAR variable
            {
                // Set type to RHS type while preserving VAR bit
                LDA ZP.NEXTT  // RHS type from expression
                ORA #BASICType.VAR  // Keep VAR bit
                STA stmtType
            }
            
            
            // Pack symbolType|dataType: SymbolType in top 3 bits, dataType in bottom 5 bits
            LDA stmtType  // dataType (bottom 5 bits)
            ORA stmtSymbol // SymbolType (top 3 bits)
            STA ZP.ACCT
            
            LDA (stmtTokensPtr+0)
            STA ZP.IDYL
            LDA (stmtTokensPtr+1)
            STA ZP.IDYH

            // Call Variables.Declare
            // Input: ZP.TOP = name pointer, ZP.ACCT = symbolType|dataType (packed),
            //        ZP.NEXT = initial value (16-bit), ZP.IDY = tokens pointer (16-bit)
            Variables.Declare();
            Error.CheckError();
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
            Memory.Free();
        }
        LDA (stmtStringPtr+0)
        ORA (stmtStringPtr+1)
        if (NZ)
        {
            LDA (stmtStringPtr+0)
            STA ZP.IDXL
            LDA (stmtStringPtr+1)
            STA ZP.IDXH
            Memory.Free();
        }

#ifdef TRACE
        LDA #(processSingleSymbolDeclTrace % 256) STA ZP.TraceMessageL LDA #(processSingleSymbolDeclTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
      
    // Create token stream from tokenizer buffer slice
    // Input: ZP.FSOURCEADDRESS = start position in BasicTokenizerBuffer
    //        ZP.FLENGTH = length of token stream to copy
    // Output: ZP.FDESTINATIONADDRESS = pointer to allocated token stream copy
    // Munts: ZP.IDXL, ZP.IDXH, ZP.ACCL, ZP.ACCH, ZP.FSOURCEADDRESS, ZP.FDESTINATIONADDRESS
    // Error: Sets ZP.LastError if memory allocation fails
    const string createTokenStreamTrace = "CreateTokStr";
    CreateTokenStream()
    {
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
#ifdef TRACE
        LDA #(createTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(createTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        
        loop
        {
            // Allocate memory for token stream
            LDA ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FLENGTHH
            STA ZP.ACCH
            IncACC(); // for the EOF
            Memory.Allocate();  // Returns address in ZP.IDX
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)
            {
                // Allocation failed
                Error.OutOfMemory(); BIT ZP.EmulatorPCL
                CLC
                break;
            }
            
            // Set up copy: source = TokenizerBuffer + saved position
            CLC
            LDA ZP.TokenBufferL
            ADC ZP.FSOURCEADDRESSL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenBufferH
            ADC ZP.FSOURCEADDRESSH
            STA ZP.FSOURCEADDRESSH
            
            // Destination = allocated memory
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            // Copy the token stream
            Tools.CopyBytes();
            
            // ADD: Append EOF after the copied tokens
            // FDESTINATIONADDRESS points to start, add FLENGTH to get to end
            CLC
            LDA ZP.FDESTINATIONADDRESSL
            ADC ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FDESTINATIONADDRESSH
            ADC ZP.FLENGTHH
            STA ZP.ACCH
            
            // Write EOF token
            LDA #Token.EOF
            LDY #0
            STA [ZP.ACC], Y
            
            // Destination = allocated memory
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            SEC
            
            break;
        } // loop    
        
#ifdef TRACE
        LDA #(createTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(createTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDXH    
        PLA
        STA ZP.IDXL
        PLA
    }
}
