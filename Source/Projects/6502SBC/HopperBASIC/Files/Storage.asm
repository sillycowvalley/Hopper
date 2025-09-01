unit Storage
{
    uses "File"

    // Combined buffer: TokenizerBuffer (512) + FunctionOpCodeBuffer (512) = 1024 bytes  
    const uint LoadBuffer     = Address.TokenizerBuffer;
    const uint LoadBufferSize = Limits.TokenizerBufferSize + Limits.OpCodeBufferSize;
    const uint RefillTrigger = 128;  // Refill when current position passes this point (at the end of the current token)
    
    // ZP slots ZP.SS0..ZP.SS9 available
    const byte LoadBufferPos     = ZP.SS0; // 0..(LoadBufferLength-1)
    const byte LoadBufferPosL    = ZP.SS0;
    const byte LoadBufferPosH    = ZP.SS1;
    
    const byte LoadBufferIndex   = ZP.SS2; // (LoadBuffer + LoadBufferPos)
    const byte LoadBufferIndexL  = ZP.SS2;
    const byte LoadBufferIndexH  = ZP.SS3;
    
    const byte LoadBufferLength  = ZP.SS4; // 0..1024
    const byte LoadBufferLengthL = ZP.SS4;
    const byte LoadBufferLengthH = ZP.SS5;
    
    // Bit 0 - set if no more data to read
    // Bit 1 - set if we've seen CONST
    // Bit 2 - set if we're in $MAIN
    // Bit 3 - set if we're using InitializeGlobals() to initialize constants too
    
    const byte LoaderFlags       = ZP.SS6;
    
#ifdef DEBUG    
    dumpBuffers()
    {
        PHP
        
Debug.NL();
Debug.NL(); HOut(); // last token
Debug.NL(); LDA LoadBufferIndexH HOut(); LDA LoadBufferIndexL HOut(); Space(); 
            LDA LoadBufferPosH HOut(); LDA LoadBufferPosL HOut(); Space(); 
            LDA LoadBufferLengthH HOut(); LDA LoadBufferLengthL HOut(); Space();
Debug.NL();            

        LDA #(LoadBuffer / 256 + 0)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 1)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 2)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 3)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 4)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 5)
        Debug.DumpPage();
        
        Debug.DumpHeap();
        
        PLP
    }
#endif
    
    
    // Load program from EEPROM file
    // Input: ZP.STR = filename, A = 0 for no NEW, A = 1 for NEW first
    // Output: C set if successful, program state restored
    LoadProgram()
    {
        loop // Single exit block
        {
            
            CMP #0
            if (NZ) // Non-zero means do NEW first
            {
                // 1. Clear current program state (like NEW)
                Variables.Clear();
                Functions.Clear();
            }
            
            // 2. Open file for reading
            LDA # DirWalkAction.FindExecutable
            File.StartLoad(); // Input: ZP.STR = filename
            if (NC) { break; }
            
            // 3. Set up sliding window buffer and TokenIterator
            setupSlidingWindowBuffer();
            if (NC) { break; }
            
#ifdef DEBUG                        
            //Storage.dumpBuffers();
#endif
            
            STZ LoaderFlags
            
            // 4. parse and build objects
            loop
            {
                nextToken();
                switch (A)
                {
                    case Token.FUNC:
                    {
                        parseFunctionHeader();
                        if (NC) { break; }
                        parseFunctionOrMain();
                        if (NC) { break; }
                    }
                    case Token.BEGIN:
                    {
                        parseMainHeader();   
                        if (NC) { break; }
                        SMB2 LoaderFlags
                        parseFunctionOrMain();
                        if (NC) { break; }
                    }
                    case Token.CONST:
                    {
                        SMB1 LoaderFlags
                    }
                    
                    // for ARRAY:
                    case Token.BIT:
                    case Token.INT:
                    case Token.BYTE:
                    case Token.WORD:
                    case Token.CHAR:
                    case Token.LONG:
                    
                    case Token.VAR:
                    case Token.STRING:
                    {
                        parseVariableOrConst();
                        if (NC) { break; }
                    }
                    case Token.EOF:
                    {
                        SEC
                        break;
                    }
                    default:
                    {
#ifdef DEBUG
                        Storage.dumpBuffers();
                        TODO(); BIT ZP.EmulatorPCL CLC // what's this?, DEBUG
                        
                        loop {  }
                        break;
#endif
                    }
                } // switch
            } // loop
#ifdef DEBUG        
            if (NC)
            {
                Storage.dumpBuffers();
            }
#endif            
            break;
        } // single exit
        
        // 5. initialize the global variables and constants
        
        SMB3 Storage.LoaderFlags // initialize constants too, not just variables
        Console.InitializeGlobals();
        
    }
    
    // Save current program state to EEPROM file
    // Input: ZP.STR = filename
    // Output: C set if successful
    const string saveProgramTrace = "SaveProg";
    SaveProgram()
    {
#ifdef TRACEFILE
        LDA #(saveProgramTrace % 256) STA ZP.TraceMessageL LDA #(saveProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // 0. setup our use of TokenBuffer
        prepareTokenBuffer();
        
        loop // Single exit block
        {
            // 1. Create file using File.StartSave()
            File.StartSave(); // Input: ZP.STR = filename
            if (NC) { break; }

            // 2. Save all constants first
            saveConstants();
            if (NC) { break; }
            
            // 3. Save all variables
            saveVariables();
            if (NC) { break; }
            
            
            // 4. Save all functions (including main program under "$MAIN")
            saveFunctions();
            if (NC) { break; }
            
            //  add EOF and stream
            LDA # Token.EOF
            appendByteToBuffer();

            // 5. flush TokenBuffer to EEPROM
            flushTokenBuffer();

            // 6. Finalize file
            LDA #0x80 // executable file
            File.EndSave();
            if (NC) { break; }
                                    
            // Success
            SEC
            break;
        }

#ifdef TRACEFILE
        LDA #(saveProgramTrace % 256) STA ZP.TraceMessageL LDA #(saveProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Save all constants to file
    // Output: C set if successful, NC on error
    const string saveConstantsTrace = "saveConst";
    saveConstants()
    {
#ifdef TRACEFILE
        LDA #(saveConstantsTrace % 256) STA ZP.TraceMessageL LDA #(saveConstantsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Iterate through all constants
            Variables.IterateConstants();
            loop
            {
                if (NC) { break; } // No more constants
                // Stream this constant to file
                
                // CONST token (only difference from variables)
                LDA #Token.CONST
                appendByteToBuffer();
                // Use shared variable/constant streaming logic
                streamVariableOrConstant(); // Input: ZP.IDX = constant node
                if (NC) { CLC break; } // Propagate error
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(saveConstantsTrace % 256) STA ZP.TraceMessageL LDA #(saveConstantsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Save all variables to file  
    // Output: C set if successful, NC on error
    const string saveVariablesTrace = "saveVars";
    saveVariables()
    {
#ifdef TRACEFILE
        LDA #(saveVariablesTrace % 256) STA ZP.TraceMessageL LDA #(saveVariablesTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Iterate through all variables
            Variables.IterateVariables();
            loop
            {
                if (NC) { break; } // No more variables
                // No CONST token for variables - go straight to shared logic
                streamVariableOrConstant(); // Input: ZP.IDX = variable node
                if (NC) { CLC break; } // Propagate error
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(saveVariablesTrace % 256) STA ZP.TraceMessageL LDA #(saveVariablesTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Shared logic for streaming variable or constant declarations
    // Input: ZP.IDX = variable/constant node, TokenizerBuffer prepared
    // Output: C set if successful, NC on error
    const string streamVariableOrConstantTrace = "strmVarConst";
    streamVariableOrConstant()
    {
#ifdef TRACEFILE
        LDA #(streamVariableOrConstantTrace % 256) STA ZP.TraceMessageL LDA #(streamVariableOrConstantTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Get type and name
            Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
            
            // Type token (extract base type)
            LDA ZP.ACCT
            AND # BASICType.VAR
            if (Z) // not VAR type
            {
                LDA ZP.ACCT
                AND # BASICType.TYPEMASK
            }
            BASICTypes.ToToken();  // Input: A = base type, Output: A = token value
            appendByteToBuffer(); // uses XID
            
            // IDENTIFIER token and name
            LDA #Token.IDENTIFIER
            appendByteToBuffer();  // uses XID

            Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
            appendStringToBuffer(); // Input: ZP.STR = name pointer, uses XID
            if (NC) { break; }
            
            // Check if this is an array
            LDA ZP.ACCT
            AND # BASICType.ARRAY
            if (NZ) // Is an array
            {
                // Arrays use their token stream for dimension expression
                streamArrayDeclaration(); // Input: ZP.IDX = variable node
                if (NC) { break; }
            }
            else
            {
                // Check if has initialization tokens
                Variables.GetTokens(); // Input: ZP.IDX, Output: ZP.NEXT = tokens pointer or null
                LDA ZP.NEXTL
                ORA ZP.NEXTH
                if (NZ) // Has initialization/dimension tokens
                {
                    // EQUALS token
                    LDA #Token.EQUALS
                    appendByteToBuffer();
                    
                    // Stream initialization tokens directly (includes EOF which is stripped)
                    Variables.GetTokens(); // Input: ZP.IDX, Output: ZP.ACC = tokens pointer or null
                    LDA ZP.NEXTL
                    STA ZP.IDYL
                    LDA ZP.NEXTH
                    STA ZP.IDYH
                    writeTokenStream(); // Input: ZP.IDY
                    if (NC) { break; }
                }
            }
            LDA # Token.EOE //  add EOE and stream after constant or variable
            appendByteToBuffer();
            
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(streamVariableOrConstantTrace % 256) STA ZP.TraceMessageL LDA #(streamVariableOrConstantTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream array declaration syntax: LBRACKET <dimension_tokens> RBRACKET
    // Input: ZP.IDX = array variable node
    // Output: C set if successful, NC on error
    const string streamArrayDeclarationTrace = "strmArrDecl";
    streamArrayDeclaration()
    {
#ifdef TRACEFILE
        LDA #(streamArrayDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(streamArrayDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // LBRACKET token
            LDA #Token.LBRACKET
            appendByteToBuffer();
            
            // Get dimension tokens (same as initialization tokens for arrays)
            Variables.GetTokens(); // Input: ZP.IDX, Output: ZP.NEXT = tokens pointer
            LDA ZP.NEXTL
            ORA ZP.NEXTH
            if (NZ) // Has dimension tokens
            {
                // Stream dimension expression directly
                LDA ZP.NEXTL
                STA ZP.IDYL
                LDA ZP.NEXTH
                STA ZP.IDYH
                writeTokenStream(); // Input: ZP.IDY
                if (NC) { break; }
            }
            
            // RBRACKET token
            LDA #Token.RBRACKET
            appendByteToBuffer();
            if (NC) { break; }
            
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(streamArrayDeclarationTrace % 256) STA ZP.TraceMessageL LDA #(streamArrayDeclarationTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Save all functions to file
    // Output: C set if successful, NC on error
    const string saveFunctionsTrace = "saveFuncs";
    saveFunctions()
    {
#ifdef TRACEFILE
        LDA #(saveFunctionsTrace % 256) STA ZP.TraceMessageL LDA #(saveFunctionsTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Iterate through all functions
            Functions.IterateFunctions();
            loop
            {
                if (NC) { break; } // No more functions
                
                // Check if this is the main program ("$MAIN") or regular function
                Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
                // Compare with "$MAIN" to determine format
                LDA [ZP.STR]
                CMP #'$'
                if (Z)
                {
                    streamMainProgram(); // Input: ZP.IDX = function node
                }
                else
                {
                    streamFunction(); // Input: ZP.IDX = function node
                }
                if (NC) { break; }
                
                Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(saveFunctionsTrace % 256) STA ZP.TraceMessageL LDA #(saveFunctionsTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Append null-terminated string to TokenizerBuffer as inline data
    // Input: ZP.STR = string pointer
    // Output: C set if successful, NC if buffer overflow
    const string appendStringToBufferTrace = "appStr";
    appendStringToBuffer()
    {
#ifdef TRACEFILE
        LDA #(appendStringToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendStringToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        PHY
        LDY #0
        loop
        {
            // Get character from string
            LDA [ZP.STR], Y
            appendByteToBuffer(); // A ->
            
            // Check for null terminator
            LDA [ZP.STR], Y
            if (Z) { SEC break; } // Success - null terminator written
            INY
            // TODO: Add buffer overflow check here if needed
        }
        PLY

#ifdef TRACEFILE
        LDA #(appendStringToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendStringToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Append function arguments to TokenizerBuffer
    // Input: ZP.IDY = arguments list head pointer (or null)
    // Output: C set if successful, NC on error
    const string appendArgumentsToBufferTrace = "appArgs";
    appendArgumentsToBuffer()
    {
#ifdef TRACEFILE
        LDA #(appendArgumentsToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendArgumentsToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        PHX
        loop // Single exit block
        {
            LDA ZP.IDYL
            ORA ZP.IDYH
            if (Z)
            {
                SEC break; // No arguments - success
            }
            // Iterate through arguments
            LDX #0 // Argument counter for comma separation
            Locals.IterateStart(); // IDX -> IDY
            loop
            {
                if (NC) { SEC break; } // No more arguments
                // Check if this is really an argument (not a local variable)
                Locals.GetType(); // Input: ZP.IDY, Output: ZP.ACCT = symbolType|dataType
                LDA ZP.ACCT
                AND # SymbolType.MASK
                CMP # SymbolType.ARGUMENT
                if (Z)
                {
                    // Add comma separator if not first argument
                    CPX #0
                    if (NZ)
                    {
                        LDA # Token.COMMA
                        appendByteToBuffer();
                    }
                    // Add IDENTIFIER token
                    LDA # Token.IDENTIFIER
                    appendByteToBuffer();
                    // Get argument name and append it
                    Locals.GetName(); // Input: ZP.IDY, Output: ZP.STR = name pointer
                    appendStringToBuffer(); // Input: ZP.STR

                    if (NC) { CLC break; } // Propagate error
                    INX // Increment argument counter
                }
                Locals.IterateNext(); // Input: ZP.IDY = current, Output: ZP.IDY = next, munts ZP.LCURRENT
            }
            break;
        }
        PLX

#ifdef TRACEFILE
        LDA #(appendArgumentsToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendArgumentsToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream a function to file
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error
    const string streamFunctionTrace = "strmFunc";
    streamFunction()
    {
#ifdef TRACEFILE
        LDA #(streamFunctionTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // Write function header
            streamFunctionHeader();
            if (NC) { break; }
            
            // Stream function body (common logic)
            streamFunctionBody();
            if (NC) { break; }
            
            // Write function footer
            streamFunctionFooter();
            if (NC) { break; }
            
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(streamFunctionTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream main program to file
    // Input: ZP.IDX = main program function node  
    // Output: C set if successful, NC on error
    const string streamMainProgramTrace = "strmMain";
    streamMainProgram()
    {
#ifdef TRACEFILE
        LDA #(streamMainProgramTrace % 256) STA ZP.TraceMessageL LDA #(streamMainProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

        loop // Single exit block
        {
            // Write main program header
            streamMainHeader();
            if (NC) { break; }
            
            // Stream function body (common logic)
            streamFunctionBody();
            if (NC) { break; }
            
            // Write main program footer
            streamMainFooter();
            if (NC) { break; }
            
            SEC // Success
            break;
        }

#ifdef TRACEFILE
        LDA #(streamMainProgramTrace % 256) STA ZP.TraceMessageL LDA #(streamMainProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream function header: FUNC IDENTIFIER "name" LPAREN <args> RPAREN
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error
    const string streamFunctionHeaderTrace = "strmFuncHdr";
    streamFunctionHeader()
    {
#ifdef TRACEFILE
        LDA #(streamFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop // Single exit block
        {
            // FUNC token
            LDA #Token.FUNC
            appendByteToBuffer();
            
            // IDENTIFIER and function name
            LDA #Token.IDENTIFIER
            appendByteToBuffer();
            
            Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
            appendStringToBuffer(); // Input: ZP.STR = string pointer
            if (NC) { break; }
            
            // LPAREN
            LDA #Token.LPAREN
            appendByteToBuffer();

            // Function arguments
            Functions.GetArguments(); // Input: ZP.IDX, Output: ZP.IDY = args list or null
            appendArgumentsToBuffer(); // Input: ZP.IDY = arguments list
            if (NC) { break; }
                        
            // RPAREN
            LDA #Token.RPAREN
            appendByteToBuffer();
            
            break; // Return result from AppendStream
        }

#ifdef TRACEFILE
        LDA #(streamFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream main program header: BEGIN
    // Output: C set if successful, NC on error
    const string streamMainHeaderTrace = "strmMainHdr";
    streamMainHeader()
    {
#ifdef TRACEFILE
        LDA #(streamMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(streamMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA #Token.BEGIN
        appendByteToBuffer();
#ifdef TRACEFILE
        LDA #(streamMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(streamMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream function body tokens directly from memory
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error  
    const string streamFunctionBodyTrace = "strmBody";
    streamFunctionBody()
    {
#ifdef TRACEFILE
        LDA #(streamFunctionBodyTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        Functions.GetBody(); // Input: ZP.IDX, Output: ZP.IDY = tokens pointer
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            SEC // No body - success
#ifdef TRACEFILE
            LDA #(streamFunctionBodyTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
            return;
        }
        writeTokenStream(); // Input: ZP.IDY

#ifdef TRACEFILE
        LDA #(streamFunctionBodyTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionBodyTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream function footer: ENDFUNC
    // Output: C set if successful, NC on error
    const string streamFunctionFooterTrace = "strmFuncFtr";
    streamFunctionFooter()
    {
#ifdef TRACEFILE
        LDA #(streamFunctionFooterTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionFooterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

        LDA #Token.ENDFUNC
        appendByteToBuffer();

#ifdef TRACEFILE
        LDA #(streamFunctionFooterTrace % 256) STA ZP.TraceMessageL LDA #(streamFunctionFooterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Stream main program footer: END
    // Output: C set if successful, NC on error
    const string streamMainFooterTrace = "strmMainFtr";
    streamMainFooter()
    {
#ifdef TRACEFILE
        LDA #(streamMainFooterTrace % 256) STA ZP.TraceMessageL LDA #(streamMainFooterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif

        LDA #Token.END
        appendByteToBuffer();

#ifdef TRACEFILE
        LDA #(streamMainFooterTrace % 256) STA ZP.TraceMessageL LDA #(streamMainFooterTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Measure length of token stream until EOF
    // Input: ZP.IDY = token stream pointer
    // Output: flushes Variables or Functions token stream to file
    const string writeTokenStreamTrace = "wrtTokStrm";
    writeTokenStream()
    {
#ifdef TRACEFILE
        LDA #(writeTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(writeTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // EEPROM.WritePage munts IDX and IDY
        LDA ZP.IDXH
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDYH
        STA ZP.XIDH
        PHA
        LDA ZP.IDYL
        STA ZP.XIDL
        PHA
        
        flushTokenBuffer(); // flush what we have in TokenBuffer before switching to Variable or Function token stream        
        STZ File.TransferLengthL
        STZ File.TransferLengthH
        
        loop
        {
            LDA [ZP.XID]          // Check for EOF
            CMP # Token.EOF
            if (Z) { break; } // Don't include EOF
            
            // Increment length counter
            INC File.TransferLengthL
            if (Z)
            {
                INC File.TransferLengthH
            }
            INC ZP.XIDL           // Advance to next token
            if (Z)
            {
                INC ZP.XIDH
            }
        }
        
        LDA ZP.IDYL
        STA File.SectorSourceL
        LDA ZP.IDYH
        STA File.SectorSourceH
        File.AppendStream(); 
        
        prepareTokenBuffer(); // switch back to our use of TokenBuffer, reseting to length = 0
        
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.IDXH

#ifdef TRACEFILE
        LDA #(writeTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(writeTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Append single token to TokenizerBuffer and update position
    // Input: A = token value
    // Output: Token written to buffer, position advanced
    // Uses: ZP.TokenBufferContentLength as write position
    const string appendByteToBufferTrace = "appByte";
    appendByteToBuffer()
    {
#ifdef TRACEFILE
        PHA LDA #(appendByteToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendByteToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        PHA
        // Calculate write position: TokenBuffer + ContentLength
        CLC
        LDA ZP.TokenBufferL
        ADC ZP.TokenBufferContentLengthL
        STA ZP.XIDL
        LDA ZP.TokenBufferH  
        ADC ZP.TokenBufferContentLengthH
        STA ZP.XIDH
        PLA
        STA [ZP.XID]                       // Write token
        INC ZP.TokenBufferContentLengthL   // Increment content length
        if (Z)
        {
            INC ZP.TokenBufferContentLengthH
        }
        
        SEC // all good (TODO : buffer overflow)
        
#ifdef TRACEFILE
        LDA #(appendByteToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendByteToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    // Reset TokenizerBuffer and get its address/prepare for writing
    // Output: SectorSource = TokenizerBuffer address, buffer ready for token writing
    const string prepareTokenBufferTrace = "prepTokBuf";
    prepareTokenBuffer()
    {
#ifdef TRACEFILE
        LDA #(prepareTokenBufferTrace % 256) STA ZP.TraceMessageL LDA #(prepareTokenBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // Reset the tokenizer buffer for our use
        STZ ZP.TokenBufferContentLengthL
        STZ ZP.TokenBufferContentLengthH
        
        // Set up SectorSource to point to TokenizerBuffer for File.AppendStream
        LDA ZP.TokenBufferL
        STA File.SectorSourceL
        LDA ZP.TokenBufferH
        STA File.SectorSourceH

#ifdef TRACEFILE
        LDA #(prepareTokenBufferTrace % 256) STA ZP.TraceMessageL LDA #(prepareTokenBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    const string flushTokenBufferTrace = "flushTokBuf";
    flushTokenBuffer()
    {
#ifdef TRACEFILE
        LDA #(flushTokenBufferTrace % 256) STA ZP.TraceMessageL LDA #(flushTokenBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // EEPROM.WritePage munts IDX and IDY
        LDA ZP.IDXH
        PHA
        LDA ZP.IDXL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.IDYL
        PHA
        
        LDA ZP.TokenBufferContentLengthL
        STA File.TransferLengthL
        LDA ZP.TokenBufferContentLengthH
        STA File.TransferLengthH
        File.AppendStream(); // C or NC ->
        
        STZ ZP.TokenBufferContentLengthL
        STZ ZP.TokenBufferContentLengthH
        
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.IDXH

#ifdef TRACEFILE
        LDA #(flushTokenBufferTrace % 256) STA ZP.TraceMessageL LDA #(flushTokenBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
            
    nextToken()
    {
        LDA [LoadBufferIndex]
        INC LoadBufferPosL
        if (Z)
        {
            INC LoadBufferPosH
        }
        INC LoadBufferIndexL
        if (Z)
        {
            INC LoadBufferIndexH
        }
    }
    peekToken()
    {
        LDA [LoadBufferIndex]
    }

    const string parseIdentifierTrace = "parseIdentifier";        
    parseIdentifier()
    {
#ifdef TRACEFILE
        LDA #(parseIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(parseIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            nextToken();
            CMP # Token.IDENTIFIER
            if (NZ) { Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // IDENTIFIER expected
            
            LDA LoadBufferIndexL
            STA ZP.STRL
            LDA LoadBufferIndexH
            STA ZP.STRH
            
            nextToken();
            Char.IsAlpha();
            if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // <alpha> expected
            loop
            {
                nextToken();
                CMP #0
                if (Z)
                {
                    // null terminator
                    SEC 
                    break; 
                }
                Char.IsAlphaNumeric();
                if (NC) {  Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // <alphanumeric> expected
            }
            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(parseIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // terminator in ZP.ACCL
    const string parseTokenStreamTrace = "parseTokenStream";
    parseTokenStream() // -> IDY
    {
#ifdef TRACEFILE
        LDA #(parseTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(parseTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {
            
            LDA LoadBufferIndexL
            STA ZP.FSOURCEADDRESSL
            LDA LoadBufferIndexH
            STA ZP.FSOURCEADDRESSH
            loop
            {
                nextToken();
                CMP ZP.ACCL
                if (Z) { break; }
            }
            
            // LoadBufferIndex - 1 = terminator : EOE, RBRACKET, END, ENDFUNC ..
            SEC
            LDA LoadBufferIndexL
            SBC # 1
            STA ZP.FLENGTHL
            LDA LoadBufferIndexH
            SBC # 0
            STA ZP.FLENGTHH
            LDA # Token.EOF
            STA [ZP.FLENGTH] // patch terminator -> EOF 
            
            SEC 
            LDA ZP.FLENGTHL
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.FLENGTHH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
            IncLENGTH();
            
            LDA ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FLENGTHH
            STA ZP.ACCH
            
            /*
            LDA ZP.ACCH
            AND #0xF0
            if (NZ)
            {
                dumpBuffers();
            }
            */
            
            Memory.Allocate(); // Input: ZP.ACC = size, Munts: ZP.M*, ZP.FREELIST, ZP.ACCL, -> ZP.IDX
            if (NC) { BIT ZP.EmulatorPCL break; }
            
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            STA ZP.IDYH
            
            Memory.Copy();
            
            SEC
            break;
        } // single exit
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
#ifdef TRACEFILE
        PHP PHA LDA #(parseTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(parseTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }               
    
    // A is type Token
    const string parseVariableOrConstTrace = "parseVariableOrConst";
    parseVariableOrConst()
    {
#ifdef TRACEFILE
        PHA LDA #(parseVariableOrConstTrace % 256) STA ZP.TraceMessageL LDA #(parseVariableOrConstTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        loop
        {
            TAX
            BASICTypes.FromToken(); // X -> A
            if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // <type> expected
            if (BBS1, LoaderFlags)
            {
                ORA # SymbolType.CONSTANT
            }
            else
            {
                ORA # SymbolType.VARIABLE
            }
            STA ZP.ACCT
            
            // name
            parseIdentifier();
            if (NC) { break; }
            
             // initial value
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            nextToken();
            CMP # Token.EOL
            if (Z)
            {
                nextToken();
            }
            switch (A)
            {
                case Token.LBRACKET:
                {
                    // not EOE, create dimension stream in IDY
                    LDA # Token.RBRACKET // terminator
                    STA ZP.ACCL
                    parseTokenStream(); // -> IDY
                    if (NC) { break; }

                    nextToken();
                    CMP # Token.EOE // RBRACKET already converted to EOF
                    if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // <type> expected

                    LDA ZP.ACCT
                    ORA # BASICType.ARRAY
                    STA ZP.ACCT
                    
                    // place holder size for array until it is initialized
                    LDA #10
                    STA ZP.NEXTL
                    CLC
                }
                case Token.EOE:
                {
                    // no initializer stream
                    STZ ZP.IDYL
                    STZ ZP.IDYH
                    
                    LDA ZP.ACCT
                    AND # BASICType.TYPEMASK
                    CMP # BASICType.STRING
                    if (Z)
                    {
                        // STRING default: allocate copy of EmptyString
                        LDA #(Variables.EmptyString % 256)
                        STA ZP.TOPL
                        LDA #(Variables.EmptyString / 256)
                        STA ZP.TOPH
                        
                        Variables.AllocateAndCopyString(); // Input: ZP.TOP = source, Output: ZP.IDY = allocated copy
                        CheckError();
                        if (NC) { break; } // allocation failed
                        
                        // Use allocated copy as the variable value
                        LDA ZP.IDYL
                        STA ZP.NEXTL
                        LDA ZP.IDYH
                        STA ZP.NEXTH
                    }
                }
                case Token.EQUALS:
                {
                    // not EOE or ARRAY: create new initializer stream in IDY
                    LDA # Token.EOE // terminator
                    STA ZP.ACCL
                    parseTokenStream(); // -> IDY
                    if (NC) { break; }
                }
                default:
                {
#ifdef DEBUG
                    TODO(); BIT ZP.EmulatorPCL CLC // what's this?, DEBUG
                    break;
#endif
                }
            } // switch

            RMB1 LoaderFlags
            
            // name from IDENTIFIER
            MoveSTRtoTOP();
            
            Variables.Declare(); // -> C or NC
            CheckError();
            if (NC)
            {
                 Error.InternalError(); BIT ZP.EmulatorPCL CLC // what's this?
            }
            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseVariableOrConstTrace % 256) STA ZP.TraceMessageL LDA #(parseVariableOrConstTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    const string parseMainHeaderTrace = "parseMainHeader";
    parseMainHeader()
    {
#ifdef TRACEFILE
        LDA #(parseMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            // argument list:
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            Messages.Main(); // point ZP.TOP -> "$MAIN"
                        
            STZ ZP.IDYL
            STZ ZP.IDYH
            Functions.Declare();
            CheckError();

            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }             
    
    const string parseFunctionHeaderTrace = "parseFunctionHeader";
    parseFunctionHeader()
    {
#ifdef TRACEFILE
        LDA #(parseFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            // name
            parseIdentifier();
            if (NC) { break; }
            
            // name from IDENTIFIER
            MoveSTRtoTOP();
            
            // create argument list in ZP.NEXT
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            STZ ZP.IDYL
            STZ ZP.IDYH
            Functions.Declare();
            CheckError(); 
            if (NC) { break; }

            nextToken();
            CMP # Token.LPAREN
            if (NZ) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // ( expected
            loop
            {
                peekToken();
                CMP # Token.RPAREN
                if (Z)
                {
                    nextToken(); // consume )
                    break;
                }
                CMP # Token.COMMA
                if (Z)
                {
                    nextToken(); // consume ,
                }

                // argument
                parseIdentifier();
                
                // argument from IDENTIFIER
                MoveSTRtoTOP();
                
                LDA #SymbolType.ARGUMENT
                ORA #BASICType.VAR 
                STA ZP.SymbolType // argument for Locals.Add()
                Locals.Add();
                CheckError();
                if (NC) { break; }
            }
            if (NC) { break; }
                        
            break;
        }
#ifdef TRACEFILE
        PHP PHA LDA #(parseFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }

    const string parseFunctionOrMainTrace = "parseFunctionOrMain";
    parseFunctionOrMain()
    {
#ifdef TRACEFILE
        LDA #(parseFunctionOrMainTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionOrMainTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop
        {
            if (BBR0, LoaderFlags) // have not seen EOF yet
            {
                slideWindow();
                if (NC) { break; }
            }

            // create function token stream in IDY
            if (BBS2, LoaderFlags)
            {
                LDA # Token.END // terminator
                STA ZP.ACCL
            }
            else
            {
                LDA #Token.ENDFUNC // terminator
                STA ZP.ACCL
            }
            parseTokenStream(); // -> IDY
            if (NC) { break; }
            
            Functions.SetBody();
            break;
        }
        RMB2 LoaderFlags
        
#ifdef TRACEFILE
        PHP PHA LDA #(parseFunctionOrMainTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionOrMainTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // 1. Move the remaining content to the front of the buffer
    // 2. Try to fill buffer with >= 512 bytes of data
    const string slideWindowTrace = "slideWindow";
    slideWindow() 
    {
#ifdef TRACEFILE
        LDA #(slideWindowTrace % 256) STA ZP.TraceMessageL LDA #(slideWindowTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // slide
        LDA #(LoadBuffer % 256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(LoadBuffer / 256) 
        STA ZP.FDESTINATIONADDRESSH
        
        CLC
        LDA ZP.FDESTINATIONADDRESSL
        ADC LoadBufferPosL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.FDESTINATIONADDRESSH
        ADC LoadBufferPosH
        STA ZP.FSOURCEADDRESSH
        
        SEC
        LDA LoadBufferLengthL
        SBC LoadBufferPosL
        STA LoadBufferLengthL
        LDA LoadBufferLengthH
        SBC LoadBufferPosH
        STA LoadBufferLengthH
        
        LDA #(LoadBuffer % 256)
        STA LoadBufferIndexL
        LDA #(LoadBuffer / 256)
        STA LoadBufferIndexH
        STZ LoadBufferPosL
        STZ LoadBufferPosH
        
        LDA LoadBufferLengthL
        STA ZP.FLENGTHL
        LDA LoadBufferLengthH
        STA ZP.FLENGTHH
        
        Memory.Copy();
        
//Debug.NL();
        // fill
        loop
        {
            if (BBS0, LoaderFlags)       { SEC break; } // no more data to read
            
            LDA LoadBufferLengthH
            CMP #0x05
            if (C) { SEC break; } // current data >= 1280 bytes (potentially less than 256 bytes remaining in our buffer space)
            File.NextStream();
            if (NC) 
            { 
                States.IsSuccess();
                if (C)
                {
                    // no content, end of file
                    SEC
                    SMB0 LoaderFlags
                }
                break; 
            }
            appendSectorToBuffer();
//LDA #'+' COut();
//PHX dumpBuffers(); PLX
        }
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
#ifdef TRACEFILE
        PHP LDA #(slideWindowTrace % 256) STA ZP.TraceMessageL LDA #(slideWindowTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLP
#endif
    }
    
    // Set up 1024-byte sliding window buffer and configure TokenIterator
    // Output: C set if successful, NC if file empty or error
    const string setupSlidingWindowBufferTrace = "setupSlidingWindowBuffer";
    setupSlidingWindowBuffer()
    {
#ifdef TRACEFILE
        LDA #(setupSlidingWindowBufferTrace % 256) STA ZP.TraceMessageL LDA #(setupSlidingWindowBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Initialize buffer state
        STZ LoadBufferPosL
        STZ LoadBufferPosH
        STZ LoadBufferLengthL
        STZ LoadBufferLengthH
        LDA #(LoadBuffer % 256)
        STA LoadBufferIndexL
        LDA #(LoadBuffer / 256)
        STA LoadBufferIndexH
        
//Debug.NL();
        
        // Load up first sectors to fill buffer
        LDX #(LoadBufferSize / 256)
        loop
        {
//LDA #'<' COut();
            File.NextStream();
            if (NC) 
            { 
                States.IsSuccess();
                if (C)
                { 
                    // no content, end of file
                    SMB0 LoaderFlags
                }
                break; 
            }
//LDA #'>' COut();            
            appendSectorToBuffer();
//LDA #'.' COut();  
//PHX dumpBuffers(); PLX
            
            DEX
            if (Z) { break; } // Buffer full
        }
        
        // Check if we loaded any data
        LDA LoadBufferLengthL
        ORA LoadBufferLengthH
        if (Z) { CLC return; } // Empty file
        
        SEC // Success
#ifdef TRACEFILE
        PHP PHA LDA #(setupSlidingWindowBufferTrace % 256) STA ZP.TraceMessageL LDA #(setupSlidingWindowBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // Append current sector to load buffer
    // Input: File.NextStream() result available
    // Output: Data appended, LoadContent updated
    const string appendSectorToBufferTrace = "appendSectorToBuffer";
    appendSectorToBuffer()
    {
#ifdef TRACEFILE
        LDA #(appendSectorToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendSectorToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif



        // Calculate destination: LoadBuffer + LoadContent
        CLC
        LDA #(LoadBuffer % 256)
        ADC LoadBufferLengthL
        STA ZP.FDESTINATIONADDRESSL
        LDA #(LoadBuffer / 256) 
        ADC LoadBufferLengthH
        STA ZP.FDESTINATIONADDRESSH
        
        // Source = FileDataBuffer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.FSOURCEADDRESSL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.FSOURCEADDRESSH
        
        // Length = File.TransferLength
        LDA File.TransferLengthL
        STA ZP.FLENGTHL
        LDA File.TransferLengthH
        STA ZP.FLENGTHH
        
//LDA #'[' COut(); LDA ZP.FLENGTHH HOut();LDA ZP.FLENGTHL HOut();
        // Input: ZP.FSOURCEADDRESS = source pointer
        //        ZP.FDESTINATIONADDRESS = destination pointer  
        //        ZP.FLENGTH = number of bytes to copy (16-bit)
        Memory.Copy();
//LDA #']' COut();
        
        // Update buffer content length (16-bit add)
        CLC
        LDA LoadBufferLengthL
        ADC File.TransferLengthL
        STA LoadBufferLengthL
        LDA LoadBufferLengthH
        ADC File.TransferLengthH
        STA LoadBufferLengthH

#ifdef DEBUG
        // Side Effect: ZP.FDESTINATIONADDRESS points one byte beyond last byte (after Memory.Copy())
        
        // Input: ZP.FDESTINATIONADDRESS = destination pointer  
        //        ZP.FLENGTH = number of bytes to zero (16-bit)
        // Clear()
        SEC
        LDA #0
        SBC ZP.FDESTINATIONADDRESSL
        STA ZP.FLENGTHL
        LDA #((LoadBuffer / 256) + (LoadBufferSize / 256)) // 0x0C + 0x60
        SBC ZP.FDESTINATIONADDRESSH
        STA ZP.FLENGTHH

//LDA #'{' COut(); LDA ZP.FLENGTHH HOut();LDA ZP.FLENGTHL HOut();                        
        Clear();
/*
        loop
        {
            // Check if FLENGTH == 0
            LDA ZP.FLENGTHL
            ORA ZP.FLENGTHH
            if (Z) { break; }  // Nothing left to zero
            
            // Write zero: *FDESTINATIONADDRESS = 0
            LDA #0x55
            STA [ZP.FDESTINATIONADDRESS]
            
            // Increment FDESTINATIONADDRESS  
            INC ZP.FDESTINATIONADDRESSL
            if (Z)
            {
                INC ZP.FDESTINATIONADDRESSH
            }
            
            // Decrement FLENGTH
            LDA ZP.FLENGTHL
            if (Z)
            {
                DEC ZP.FLENGTHH
            }
            DEC ZP.FLENGTHL
        }
        */
//LDA #'}' COut();        

//Debug.NL(); LDA #'D' COut();LDA #'>' COut(); LDA ZP.FDESTINATIONADDRESSH HOut(); LDA ZP.FDESTINATIONADDRESSL HOut();          
//Debug.NL(); LDA #'L' COut();LDA #'>' COut(); LDA ZP.FLENGTHH HOut(); LDA ZP.FLENGTHL HOut();          
                
#endif        
        
#ifdef TRACEFILE
        LDA #(appendSectorToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendSectorToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
}
