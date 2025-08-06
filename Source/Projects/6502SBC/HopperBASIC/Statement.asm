unit Statement // Statement.asm
{
    uses "Tokenizer"
    uses "Instructions"
    uses "FunctionDeclaration.asm"
    uses "Emit"
    uses "Compiler"
    
    friend FunctionDeclaration, Console;
    
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
    
    
    // Typically called when ZP.CurrentToken is Token.IDENTIFIER, or a keyword
    // Output: symbol or function in IDX, A = IdentifierType
    const string resolveIdentifierTrace = "ResolveId";
    ResolveIdentifier()
    {
        PHX
        PHY
        
        LDY ZP.ACCT
        PHY
        
#ifdef TRACE
        PHA LDA #(resolveIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(resolveIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        
        loop // Single exit block for clean error handling
        {
            Tokens.IsKeyword(); // Input: A = token value to check
            if (C)
            {
#ifdef DEBUG
                //LDA #'K' Debug.COut();
#endif
                LDA # IdentifierType.Keyword
                break; // success
            }
            
            // Get the identifier name for lookup
            Tokenizer.GetTokenString();  // Result in ZP.TOP
            Error.CheckError();
            if (NC) { break; }
            
            STZ ZP.SymbolIteratorFilter  // Accept any symbol type (variable or constant)
            Variables.Find(); // ZP.IDX = symbol node address
            if (C) // Symbol found
            {
                // Get symbol type and check
                Variables.GetType();
                LDA ZP.ACCT
                AND #0xF0  // Extract symbol type (high nibble)
                CMP # (SymbolType.VARIABLE << 4)
                if (Z)
                { 
#ifdef DEBUG
                    //LDA # 'V' Debug.COut();
#endif
                    LDA # IdentifierType.Global
                    break; // success
                }
                CMP # (SymbolType.CONSTANT << 4)
                if (Z)
                { 
#ifdef DEBUG
                    //LDA # 'C' Debug.COut();
#endif        
                    LDA # IdentifierType.Constant
                    break; // success
                }
                // what's this?
                Error.InternalError(); BIT ZP.EmulatorPCL
                CLC
            }
            Error.CheckError();
            if (NC) { break; }
            
            // function by same name exists? name pointer in TOP
            LDX #ZP.FunctionsList
            Objects.Find();
            if (C)  
            {
#ifdef DEBUG
                //LDA #'F' Debug.COut();
#endif    
                LDA # IdentifierType.Function
                break; // success
            }
            Error.CheckError();
            if (NC) { break; }
#ifdef DEBUG
            //Debug.HOut(); LDA #'U' Debug.COut();
            //PrintStringTOP();
#endif

            Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
            LDA # IdentifierType.Undefined
            CLC  // undefined identifier
            break;
        } // end of single exit block

#ifdef TRACE
        PHA LDA #(resolveIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(resolveIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA
#endif

        PLY
        STY ZP.ACCT
        
        PLY
        PLX
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
            // Set literal base to BasicTokenizerBuffer for REPL
            LDA #(Address.BasicTokenizerBuffer % 256)
            STA ZP.IDYL  
            LDA #(Address.BasicTokenizerBuffer / 256) 
            STA ZP.IDYH
            NOP
            Compiler.SetLiteralBase();
            Compiler.CompileExpression();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation (important for function calls from REPL)
            LDA ZP.OpCodeBufferLengthL
            PHA
            LDA ZP.OpCodeBufferLengthH
            PHA
            
            States.SetSuccess(); // clear
            
            // 2. Execute opcodes ? result on stack
            Executor.ExecuteOpCodes();
            
            // Restore opcode buffer length after execution (important for function calls from REPL)
            PLA
            STA ZP.OpCodeBufferLengthH
            PLA
            STA ZP.OpCodeBufferLengthL
            
            Error.CheckError(); 
            if (NC)
            {
                States.SetFailure(); 
                break;
            } 
            // Exiting means the Executor ran out of OpCodes before it encountered a RETURN
            // Still a good outcome (when evaluation an expression rather than executing a statement)
            States.GetState();
            CMP #State.Exiting
            if (Z)
            {
                States.SetSuccess();  // Convert EXITING to SUCCESS for expressions
            }
            
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
            // Set literal base to BasicTokenizerBuffer for REPL (same as EvaluateExpression)
            LDA #(Address.BasicTokenizerBuffer % 256)
            STA ZP.IDYL  
            LDA #(Address.BasicTokenizerBuffer / 256) 
            STA ZP.IDYH
            Compiler.SetLiteralBase();
            
            // Initialize opcode buffer if this is the start of compilation  
            Compiler.InitOpCodeBuffer();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Compile the statement (not expression)
            Compiler.CompileStatement();
            Error.CheckError();
            if (NC) { States.SetFailure(); break; }
            
            // Save opcode buffer length after compilation 
            LDA ZP.OpCodeBufferLengthL
            PHA
            LDA ZP.OpCodeBufferLengthH
            PHA
            
            States.SetSuccess(); // Clear state
            
            // Execute the compiled statement opcodes
            Executor.ExecuteOpCodes();
            Error.CheckError();
            
            // Restore opcode buffer length
            PLA
            STA ZP.OpCodeBufferLengthH
            PLA
            STA ZP.OpCodeBufferLengthL
            
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
            if (NC) { break; } // error exit
            
            // we want a constant expression
            LDA #1
            SetIsConstant();
            
            LDA #(SymbolType.CONSTANT << 4)
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

        LDA #(SymbolType.VARIABLE << 4)
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

            LDA ZP.CurrentToken
            STA stmtType
            
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
                    LDA stmtSymbol // New symbol type (VARIABLE or CONSTANT << 4)
                    CMP ZP.NEXTT
                    if (NZ)
                    {
                        // Type mismatch - show appropriate error
                        LDA ZP.NEXTT
                        CMP #(SymbolType.CONSTANT << 4)
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
                        CMP # (SymbolType.CONSTANT << 4)
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
                    }
                    case Token.EOL:
                    case Token.COLON:
                    {
                        LDA stmtSymbol
                        CMP # (SymbolType.CONSTANT << 4)
                        if (Z)
                        {
                            // constants must be initialized
                            Error.ConstantExpressionExpected(); BIT ZP.EmulatorPCL
                            CLC
                            break; // error exit
                        }
                        LDA stmtType
                        CMP #Token.STRING
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
                }
                
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
        
        LDX stmtType
        loop
        {
            if (NC) { break; } // error exit
            switch(X)
            {
                case Token.WORD:
                {
                    LDX #BASICType.WORD
                }
                case Token.INT:
                {
                    LDX # BASICType.INT
                }
                case Token.BIT:
                {
                    LDX #BASICType.BIT
                }
                case Token.BYTE:
                {
                    LDX #BASICType.BYTE
                }
                case Token.STRING:
                {
                    LDX #BASICType.STRING
                }
                default:
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    CLC // what's this?
                    break;
                }
            }
            
            PHX
            LDA ZP.TOPL
            PHA
            LDA ZP.TOPH
            PHA
            LDA ZP.NEXTL
            PHA
            LDA ZP.NEXTH
            PHA

            STX ZP.NEXTT // LHS type
            
            LDA ZP.TOPT // did we have "= <expression>"?
            if (NZ)
            {
                LDA ZP.NEXTL
                STA ZP.TOPL
                LDA ZP.NEXTH
                STA ZP.TOPH
                LDA ZP.NEXTT
                STA ZP.TOPT
                
                // RHS in TOP
                // LHS type in NEXTT
                CheckRHSTypeCompatibility();
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
            PLX

            if (NC)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Pack symbolType|dataType: VARIABLE(1) in high nibble, dataType in low nibble
            TXA  // dataType in A
            ORA stmtSymbol // high nibble is VARIABLE<<4 of CONSTANT<<4
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
        
#ifdef DEBUG
        //DumpBuffers();
        //DumpHeap();
#endif

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
    const string createTokenStreamTrace = "CreateTok";
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
            
            // Set up copy: source = BasicTokenizerBuffer + saved position
            CLC
            LDA # ( Address.BasicTokenizerBuffer & 0xFF)
            ADC ZP.FSOURCEADDRESSL
            STA ZP.FSOURCEADDRESSL
            LDA # ( Address.BasicTokenizerBuffer >> 8)
            ADC ZP.FSOURCEADDRESSH
            STA ZP.FSOURCEADDRESSH
            
            // Destination = allocated memory
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            
            // Copy the token stream
            Tools.CopyBytes();
            
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
