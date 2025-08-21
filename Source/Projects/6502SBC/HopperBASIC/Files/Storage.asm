unit Storage
{
    uses "File"
    
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

            // 5. flush TokenBuffer to EEPROM
            flushTokenBuffer();

            // 6. Finalize file
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
            //  add EOF and stream
            LDA # Token.EOF
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
            
    // Load program from EEPROM file  
    // Input: ZP.STR = filename
    // Output: C set if successful, program state restored
    const string loadProgramTrace = "LoadProg";
    LoadProgram()
    {
#ifdef TRACEFILE
        LDA #(loadProgramTrace % 256) STA ZP.TraceMessageL LDA #(loadProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        // TODO: Implement loading (reverse of save process)
        // 1. Clear current state (like NEW command)
        // 2. File.OpenFile() 
        // 3. Stream data back into token buffer and rebuild symbol tables
        // 4. File.EndLoad()
        Error.TODO(); BIT ZP.EmulatorPCL

#ifdef TRACEFILE
        LDA #(loadProgramTrace % 256) STA ZP.TraceMessageL LDA #(loadProgramTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
}
