unit Storage
{
    uses "File"
    
    // Save current program state to EEPROM file
    // Input: ZP.STR = filename
    // Output: C set if successful
    SaveProgram()
    {
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
            
            // 5. Finalize file
            File.EndSave();
            if (NC) { break; }
            
            // Success
            SEC
            break;
        }
    }
    
    // Save all constants to file
    // Output: C set if successful, NC on error
    saveConstants()
    {
        loop // Single exit block
        {
            // Iterate through all constants
            Variables.IterateConstants();
            loop
            {
                if (NC) { break; } // No more constants
                // Stream this constant to file
                prepareTokenBuffer();
                // CONST token (only difference from variables)
                LDA #Token.CONST
                appendTokenToBuffer();
                // Use shared variable/constant streaming logic
                streamVariableOrConstant(); // Input: ZP.IDX = constant node
                if (NC) { CLC break; } // Propagate error
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            SEC // Success
            break;
        }
    }
    
    // Save all variables to file  
    // Output: C set if successful, NC on error
    saveVariables()
    {
        loop // Single exit block
        {
            // Iterate through all variables
            Variables.IterateVariables();
            loop
            {
                if (NC) { break; } // No more variables
                // Stream this variable to file
                prepareTokenBuffer();
                // No CONST token for variables - go straight to shared logic
                streamVariableOrConstant(); // Input: ZP.IDX = variable node
                if (NC) { CLC break; } // Propagate error
                Variables.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            SEC // Success
            break;
        }
    }
    
        
    
    // Shared logic for streaming variable or constant declarations
    // Input: ZP.IDX = variable/constant node, TokenizerBuffer prepared
    // Output: C set if successful, NC on error
    streamVariableOrConstant()
    {
        loop // Single exit block
        {
            // Get type and name
            Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
            
            
            // Type token (extract base type)
            LDA ZP.ACCT
            AND #BASICType.TYPEMASK
            BASICTypes.ToToken();  // Input: A = base type, Output: A = token value
            appendTokenToBuffer(); // uses XID
            
            // IDENTIFIER token and name
            LDA #Token.IDENTIFIER
            appendTokenToBuffer();  // uses XID
            
            Variables.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
            appendStringToBuffer(); // Input: ZP.STR = name pointer, uses XID
            if (NC) { break; }
            
            // Check if this is an array
            LDA ZP.ACCT
            AND #BASICType.ARRAY
            if (NZ) // Is an array
            {
                // Arrays use their token stream for dimension expression
                streamArrayDeclaration(); // Input: ZP.IDX = variable node
                if (NC) { break; }
            }
            
            // Check if has initialization tokens
            Variables.GetTokens(); // Input: ZP.IDX, Output: ZP.ACC = tokens pointer or null
            LDA ZP.ACCL
            ORA ZP.ACCH
            if (NZ) // Has initialization/dimension tokens
            {
                // EQUALS token
                LDA #Token.EQUALS
                appendTokenToBuffer();
                
                // Stream header first
                flushTokenBuffer();
                if (NC) { break; }
                
                // Stream initialization tokens directly (includes EOF)
                LDA ZP.ACCL
                STA ZP.IDYL
                LDA ZP.ACCH
                STA ZP.IDYH
                writeTokenStream(); // Input: ZP.IDY
                               
                if (NC) { break; }
            }
            else
            {
                // No initialization - add EOF and stream
                LDA #Token.EOF
                appendTokenToBuffer();
                
                flushTokenBuffer();
                if (NC) { break; }
            }
            
            SEC // Success
            break;
        }
    }
    
    // Stream array declaration syntax: LBRACKET <dimension_tokens> RBRACKET
    // Input: ZP.IDX = array variable node
    // Output: C set if successful, NC on error
    streamArrayDeclaration()
    {
        loop // Single exit block
        {
            // LBRACKET token
            LDA #Token.LBRACKET
            appendTokenToBuffer();
            
            // Stream header with LBRACKET
            flushTokenBuffer();
            if (NC) { break; }
            
            // Get dimension tokens (same as initialization tokens for arrays)
            Variables.GetTokens(); // Input: ZP.IDX, Output: ZP.ACC = tokens pointer
            LDA ZP.ACCL
            ORA ZP.ACCH
            if (NZ) // Has dimension tokens
            {
                // Stream dimension expression directly
                LDA ZP.ACCL
                STA ZP.IDYL
                LDA ZP.ACCH
                STA ZP.IDYH
                writeTokenStream(); // Input: ZP.IDY
                if (NC) { break; }
            }
            
            // RBRACKET token
            prepareTokenBuffer();
            LDA #Token.RBRACKET
            appendTokenToBuffer();
            
            flushTokenBuffer();
            if (NC) { break; }
            
            SEC // Success
            break;
        }
    }
    
    // Save all functions to file
    // Output: C set if successful, NC on error
    saveFunctions()
    {
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
                if (NC) { CLC break; } // Propagate error
                
                Functions.IterateNext(); // Input: ZP.IDX = current, Output: ZP.IDX = next
            }
            
            SEC // Success
            break;
        }
    }
    
    // Append null-terminated string to TokenizerBuffer as inline data
    // Input: ZP.STR = string pointer
    // Output: C set if successful, NC if buffer overflow
    appendStringToBuffer()
    {

        PHY
        
Debug.NL(); LDA #'"' COut(); 
        
        LDY #0
        loop
        {
            // Get character from string
            LDA [ZP.STR], Y
Printable();
            // Calculate write position: TokenBuffer + ContentLength
            CLC
            LDA ZP.TokenBufferL
            ADC ZP.TokenBufferContentLengthL
            STA ZP.XIDL
            LDA ZP.TokenBufferH  
            ADC ZP.TokenBufferContentLengthH
            STA ZP.XIDH
            
            // Write character to buffer
            STA [ZP.XID]
            
            // Increment content length
            INC ZP.TokenBufferContentLengthL
            if (Z)
            {
                INC ZP.TokenBufferContentLengthH
            }
            
            // Check for null terminator
            LDA [ZP.STR], Y
            if (Z) { SEC break; } // Success - null terminator written
            INY
            // TODO: Add buffer overflow check here if needed
        }
LDA #'"' COut();
        PLY
    }
    
    // Append function arguments to TokenizerBuffer
    // Input: ZP.IDY = arguments list head pointer (or null)
    // Output: C set if successful, NC on error
    appendArgumentsToBuffer()
    {
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
            
            IterateStart();
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
                        appendTokenToBuffer();
                    }
                    // Add IDENTIFIER token
                    LDA # Token.IDENTIFIER
                    appendTokenToBuffer();
                    
                    // Get argument name and append it
                    Locals.GetName(); // Input: ZP.IDY, Output: ZP.TOP = name pointer
                    LDA ZP.TOPL
                    STA ZP.STRL
                    LDA ZP.TOPH
                    STA ZP.STRH
                    
LDA #'a' COut();                    
                    appendStringToBuffer(); // Input: ZP.STR
LDA #'b' COut();                    
                    if (NC) { CLC break; } // Propagate error
                    INX // Increment argument counter
                }
                Locals.IterateNext(); // Input: ZP.IDY = current, Output: ZP.IDY = next
            }
            break;
        }
        PLX
    }
    
    // Stream a function to file
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error
    streamFunction()
    {
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
    }
    
    // Stream main program to file
    // Input: ZP.IDX = main program function node  
    // Output: C set if successful, NC on error
    streamMainProgram()
    {
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
    }
    
    // Stream function header: FUNC IDENTIFIER "name" LPAREN <args> RPAREN
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error
    streamFunctionHeader()
    {
        loop // Single exit block
        {
            prepareTokenBuffer();
            
            // FUNC token
            LDA #Token.FUNC
            appendTokenToBuffer();
            
            // IDENTIFIER and function name
            LDA #Token.IDENTIFIER
            appendTokenToBuffer();
            
            Functions.GetName(); // Input: ZP.IDX, Output: ZP.STR = name pointer
LDA #'c' COut();
            appendStringToBuffer(); // Input: ZP.STR = string pointer
LDA #'d' COut();
            if (NC) { break; }
            
            // LPAREN
            LDA #Token.LPAREN
            appendTokenToBuffer();
            
            // Function arguments
            Functions.GetArguments(); // Input: ZP.IDX, Output: ZP.IDY = args list or null
            appendArgumentsToBuffer(); // Input: ZP.IDY = arguments list
            if (NC) { break; }
            
            // RPAREN
            LDA #Token.RPAREN
            appendTokenToBuffer();
            
            // Stream header to file
            flushTokenBuffer();
            break; // Return result from AppendStream
        }
    }
    
    // Stream main program header: BEGIN
    // Output: C set if successful, NC on error
    streamMainHeader()
    {
        prepareTokenBuffer();
        LDA #Token.BEGIN
        appendTokenToBuffer();
        flushTokenBuffer();
    }
    
    // Stream function body tokens directly from memory
    // Input: ZP.IDX = function node
    // Output: C set if successful, NC on error  
    streamFunctionBody()
    {
Debug.NL(); XOut();        
        Functions.GetBody(); // Input: ZP.IDX, Output: ZP.IDY = tokens pointer
YOut();        
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            SEC // No body - success
            return;
        }
        writeTokenStream(); // Input: ZP.IDY
    }
    
    // Stream function footer: ENDFUNC
    // Output: C set if successful, NC on error
    streamFunctionFooter()
    {
        prepareTokenBuffer();
        LDA #Token.ENDFUNC
        appendTokenToBuffer();
        flushTokenBuffer();
    }
    
    // Stream main program footer: END
    // Output: C set if successful, NC on error
    streamMainFooter()
    {
        prepareTokenBuffer();
        LDA #Token.END
        appendTokenToBuffer();
        flushTokenBuffer();
    }
    
    // Measure length of token stream until EOF
    // Input: ZP.IDY = token stream pointer
    // Output: flushes Variables or Functions token stream to file
    writeTokenStream()
    {
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
        
        STZ File.TransferLengthL
        STZ File.TransferLengthH
        
        loop
        {
            // Increment length counter
            INC File.TransferLengthL
            if (Z)
            {
                INC File.TransferLengthH
            }
            LDA [ZP.XID]          // Check for EOF
            CMP #Token.EOF  
            if (Z) { break; }
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
        
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDXL
        PLA
        STA ZP.IDXH
    }
    
    // Append single token to TokenizerBuffer and update position
    // Input: A = token value
    // Output: Token written to buffer, position advanced
    // Uses: ZP.TokenBufferContentLength as write position
    appendTokenToBuffer()
    {
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
Debug.NL(); HOut(); Space(); LDA ZP.TokenBufferContentLengthH HOut(); LDA ZP.TokenBufferContentLengthL HOut();
        
        
        
    }
    
    // Reset TokenizerBuffer and get its address/prepare for writing
    // Output: SectorSource = TokenizerBuffer address, buffer ready for token writing
    prepareTokenBuffer()
    {
        // Reset the tokenizer buffer for our use
        STZ ZP.TokenBufferContentLengthL
        STZ ZP.TokenBufferContentLengthH
        
        // Set up SectorSource to point to TokenizerBuffer for File.AppendStream
        LDA ZP.TokenBufferL
        STA File.SectorSourceL
        LDA ZP.TokenBufferH
        STA File.SectorSourceH
    }
    
    flushTokenBuffer()
    {
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
    }
            
    
     
    // Load program from EEPROM file  
    // Input: ZP.STR = filename
    // Output: C set if successful, program state restored
    LoadProgram()
    {
        // TODO: Implement loading (reverse of save process)
        // 1. Clear current state (like NEW command)
        // 2. File.OpenFile() 
        // 3. Stream data back into token buffer and rebuild symbol tables
        // 4. File.EndLoad()
        Error.TODO(); BIT ZP.EmulatorPCL
    }
}
     
           
        
    
