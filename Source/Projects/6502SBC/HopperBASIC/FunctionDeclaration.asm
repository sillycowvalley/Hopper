unit FunctionDeclaration
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Error"
    uses "Tokenizer"
    uses "Tools"
    
    uses "Instructions"
    
    uses "Functions" 
    uses "Arguments"
    
    
    // Execute BEGIN declaration statement (main program)
    // Input: ZP.CurrentToken = BEGIN token
    // Output: Main program declared as special "BEGIN" function
    //         ZP.CurrentToken = token after BEGIN declaration
    // Modifies: Functions table, memory allocation, all statement buffer locations
    // Error: Sets ZP.LastError if syntax error, name conflict, or memory allocation fails
    ExecuteBeginDeclaration()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'B'
        Debug.COut();
        LDA #'E'
        Debug.COut();
        LDA #'G'
        Debug.COut();
    #endif
    
        loop // Single exit block for clean error handling
        {
            // Check if "BEGIN" function already exists and remove it
            LDA #(Messages.BeginFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.BeginFunctionName / 256)
            STA ZP.TOPH
            
            Functions.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // BEGIN function exists - remove it (name pointer already in ZP.TOP)
                Functions.Remove();
                Error.CheckError();
                if (NC) { break; }
                
                // Restore the name pointer for declaration
                LDA #(Messages.BeginFunctionName % 256)
                STA ZP.TOPL
                LDA #(Messages.BeginFunctionName / 256)
                STA ZP.TOPH
            }
            
            // Create empty "BEGIN" function with no arguments
            LDA #(Messages.BeginFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.BeginFunctionName / 256)
            STA ZP.TOPH
            
            // Initialize empty arguments list and body tokens
            STZ ZP.NEXTL // Arguments list head = null (no arguments)
            STZ ZP.NEXTH
            STZ ZP.IDYL  // Function body tokens = null (will be set later)
            STZ ZP.IDYH
            
            // Declare the function
            Functions.Declare(); // Input: ZP.TOP = name, ZP.NEXT = args head, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            // Save function node address
            LDA ZP.IDXL
            STA (Statement.stmtObjectPtr + 0)
            LDA ZP.IDXH
            STA (Statement.stmtObjectPtr + 1)
            
            // Get next token after BEGIN
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Check if this is an incomplete BEGIN block (ends with EOL)
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                // Incomplete BEGIN block - set up for capture mode
                SEC // Success - incomplete function ready for capture
                break;
            }
            
            // Complete BEGIN block on same line - capture body from current position to END
            captureBeginBody();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'B'
        Debug.COut();
        LDA #'E'
        Debug.COut();
        LDA #'G'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
    
    // Execute function declaration statement
    // Input: ZP.CurrentToken = FUNC token
    // Output: Function declared and added to symbol table with arguments and body
    //         ZP.CurrentToken = token after function declaration
    // Modifies: Stack, ZP.CurrentToken, symbol tables, memory allocation,
    //          all statement buffer locations, all parsing variables
    // Error: Sets ZP.LastError if syntax error, name conflict, or memory allocation fails
    ExecuteFunctionDeclaration()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'D'
        Debug.COut();
    #endif
    
        loop // Single exit block for clean error handling
        {
            // Get next token - should be function name
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP #Tokens.IDENTIFIER
            if (NZ)
            {
                Tokenizer.IsKeyword();
                if (C)
                {
                    Error.IllegalIdentifier(); BIT ZP.EmulatorPCL
                }
                else
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                }
                break;
            }
            
            // Get the function name
            Tokenizer.GetTokenString(); // Returns pointer in ZP.TOP
            Error.CheckError();
            if (NC) { break; }
            
            // Save function name pointer in statement storage
            LDA ZP.TOPL
            STA (Statement.stmtNamePtr + 0)
            LDA ZP.TOPH
            STA (Statement.stmtNamePtr + 1)
            
            // Check if function with this name already exists and remove it
            Functions.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // Function exists - remove it (name pointer already in ZP.TOP)
                Functions.Remove();
                Error.CheckError();
                if (NC) { break; }
            }
            
            // Check if variable/constant with this name exists - this is an error
            STZ ZP.SymbolIteratorFilter // Accept any symbol type
            Variables.Find(); // Input: ZP.TOP = name
            if (C)
            {
                // Variable/constant exists - determine type and show appropriate error
                Variables.GetType(); // Input: ZP.IDX, Output: ZP.ACCT = symbolType|dataType
                Error.CheckError();
                if (NC) { break; }
                
                LDA ZP.ACCT
                AND #0xF0  // Extract symbol type (high nibble)
                CMP #(SymbolType.CONSTANT << 4)
                if (Z)
                {
                    Error.ConstantExists(); BIT ZP.EmulatorPCL
                }
                else
                {
                    Error.VariableExists(); BIT ZP.EmulatorPCL
                }
                break;
            }
            
            // Restore function name pointer after potential corruption from Remove operations
            LDA (Statement.stmtNamePtr + 0)
            STA ZP.TOPL
            LDA (Statement.stmtNamePtr + 1)
            STA ZP.TOPH
            
            // Get next token - should be opening parenthesis
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Tokens.LPAREN
            if (NZ)
            {
                Error.ExpectedLeftParen(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Parse parameter list - create empty function first, then add arguments
            // Restore function name to ZP.TOP
            LDA (Statement.stmtNamePtr + 0)
            STA ZP.TOPL
            LDA (Statement.stmtNamePtr + 1)
            STA ZP.TOPH
            
            // Initialize empty arguments list and body tokens for now
            STZ ZP.NEXTL // Arguments list head = null
            STZ ZP.NEXTH
            STZ ZP.IDYL  // Function body tokens = null (will be set later)
            STZ ZP.IDYH
            
            // Declare the function with empty arguments and body
            Functions.Declare(); // Input: ZP.TOP = name, ZP.NEXT = args head, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            // Save function node address for adding arguments
            LDA ZP.IDXL
            STA (Statement.stmtObjectPtr + 0)
            LDA ZP.IDXH
            STA (Statement.stmtObjectPtr + 1)
            
            // Parse parameters
            parseParameterList();
            Error.CheckError();
            if (NC) { break; }
            
            // Expect closing parenthesis (should be current token after parseParameterList)
            LDA ZP.CurrentToken
            CMP #Tokens.RPAREN
            if (NZ)
            {
                Error.ExpectedRightParen(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Get next token - start of function body or EOL
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Check if this is an incomplete function (ends with EOL)
            LDA ZP.CurrentToken
            CMP #Tokens.EOL
            if (Z)
            {
                // Incomplete function - set up for capture mode
                // Function is already declared, Console will handle capture mode
                SEC // Success - incomplete function ready for capture
                break;
            }
            
            // Complete function on same line - capture function body from current position to ENDFUNC
            captureFunctionBody();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'F'
        Debug.COut();
        LDA #'D'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
    
    // Parse parameter list and add arguments to function
    // Input: ZP.CurrentToken = token after opening parenthesis
    //        Function node address in stmtObjectPtr
    // Output: Arguments added to function, ZP.CurrentToken = closing parenthesis or error
    // Modifies: ZP.CurrentToken, Arguments list, ZP.IDX, ZP.TOP
    parseParameterList()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'P'
        Debug.COut();
        LDA #'L'
        Debug.COut();
    #endif
    
        loop // Single exit block
        {
            // Get next token after opening parenthesis
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            // Check for empty parameter list
            LDA ZP.CurrentToken
            CMP #Tokens.RPAREN
            if (Z)
            {
                SEC // Success - empty parameter list
                break;
            }
            
            // Parse parameter list
            loop
            {
                // Expect identifier
                LDA ZP.CurrentToken
                CMP #Tokens.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get parameter name
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                Error.CheckError();
                if (NC) { break; }
                
                // Add argument to function
                // Restore function node address to ZP.IDX
                LDA (Statement.stmtObjectPtr + 0)
                STA ZP.IDXL
                LDA (Statement.stmtObjectPtr + 1)
                STA ZP.IDXH
                
                Arguments.Add(); // Input: ZP.IDX = function node, ZP.TOP = argument name
                Error.CheckError();
                if (NC) { break; }
                
                // Get next token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Check what comes next
                LDA ZP.CurrentToken
                CMP #Tokens.RPAREN
                if (Z)
                {
                    SEC // Success - end of parameter list
                    break;
                }
                
                // Expect comma for more parameters
                CMP #Tokens.COMMA
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get next token after comma
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
                
                // Continue parsing parameters
            }
            
            break; // Exit outer loop
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'P'
        Debug.COut();
        LDA #'L'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
    
    // Capture BEGIN body tokens from current position to END
    // Input: ZP.CurrentToken = first token of BEGIN body
    //        Function node address in stmtObjectPtr
    // Output: BEGIN body tokens captured and stored in function node
    //         ZP.CurrentToken = token after END
    // Modifies: Memory allocation, function node, ZP.CurrentToken, tokenizer position
    captureBeginBody()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'B'
        Debug.COut();
        LDA #'B'
        Debug.COut();
    #endif
    
        loop // Single exit block
        {
            // Save tokenizer position at start of BEGIN body
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Scan to find END token
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.EOF
                if (Z)
                {
                    // Hit end of input without finding END
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                CMP #Tokens.END
                if (Z)
                {
                    // Found END - we're done
                    SEC
                    break;
                }
                
                // Get next token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) 
                { 
                    CLC
                    break; 
                }
            }
            
            if (NC) { break; } // Error during scanning
            
            // Calculate length of BEGIN body tokens
            SEC
            LDA ZP.TokenizerPosL
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.TokenizerPosH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
            // Create token stream copy
            CreateTokenStream(); // Uses ZP.FSOURCEADDRESS, ZP.FLENGTH
            Error.CheckError();
            if (NC) { break; }
            
            // Set function body tokens in function node
            LDA (Statement.stmtObjectPtr + 0)
            STA ZP.IDXL
            LDA (Statement.stmtObjectPtr + 1)
            STA ZP.IDXH
            
            LDA ZP.FDESTINATIONADDRESSL
            STA ZP.IDYL
            LDA ZP.FDESTINATIONADDRESSH
            STA ZP.IDYH
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
            // Get next token after END
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'B'
        Debug.COut();
        LDA #'B'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
    
    // Capture function body tokens from current position to ENDFUNC
    // Input: ZP.CurrentToken = first token of function body
    //        Function node address in stmtObjectPtr
    // Output: Function body tokens captured and stored in function node
    //         ZP.CurrentToken = token after ENDFUNC
    // Modifies: Memory allocation, function node, ZP.CurrentToken, tokenizer position
    captureFunctionBody()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'B'
        Debug.COut();
    #endif

        loop // Single exit block
        {
            // Save tokenizer position at start of function body
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Scan to find ENDFUNC token (no nesting support)
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.EOF
                if (Z)
                {
                    // Hit end of input without finding ENDFUNC
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                CMP #Tokens.ENDFUNC
                if (Z)
                {
                    // Found ENDFUNC - we're done
                    SEC
                    break;
                }
                
                // Get next token
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) 
                { 
                    CLC
                    break; 
                }
            }
            
            if (NC) { break; } // Error during scanning
            
            // Calculate length of function body tokens
            // Current position is at ENDFUNC, subtract start position
            SEC
            LDA ZP.TokenizerPosL
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.TokenizerPosH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
            // Create token stream copy using existing pattern
            CreateTokenStream(); // Uses ZP.FSOURCEADDRESS, ZP.FLENGTH
            Error.CheckError();
            if (NC) { break; }
            
            // Set function body tokens in function node
            // Restore function node address to ZP.IDX
            LDA (Statement.stmtObjectPtr + 0)
            STA ZP.IDXL
            LDA (Statement.stmtObjectPtr + 1)
            STA ZP.IDXH
            
            // Function body tokens are in ZP.FDESTINATIONADDRESS
            LDA ZP.FDESTINATIONADDRESSL
            STA ZP.IDYL
            LDA ZP.FDESTINATIONADDRESSH
            STA ZP.IDYH
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
            // Get next token after ENDFUNC
            Tokenizer.NextToken();
            Error.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'B'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
    
    // Complete a partial function that was captured in multiple lines
    // Input: Token buffer contains complete function from FUNC to ENDFUNC
    // Output: Function body tokens extracted and attached to existing function node
    // Modifies: Function node, memory allocation, tokenizer position
    // Error: Sets ZP.LastError if function not found, syntax error, or memory allocation fails
    CompletePartialFunction()
    {
    #ifdef DEBUG
        LDA #'<'
        Debug.COut();
        LDA #'C'
        Debug.COut();
        LDA #'P'
        Debug.COut();
        LDA #'F'
        Debug.COut();
    #endif
    
        loop // Single exit block for error handling
        {
            // Reset to start of function buffer
            STZ ZP.TokenizerPosL
            STZ ZP.TokenizerPosH
            
            // Skip FUNC token
            Tokenizer.NextToken(); // Gets FIRST token (should be FUNC)
            Error.CheckError();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Tokens.FUNC
            if (Z)
            {
                // Get function name
                Tokenizer.NextToken(); // Gets SECOND token (should be function name)
                Error.CheckError();
                if (NC) { break; }
                
                LDA ZP.CurrentToken
                CMP #Tokens.IDENTIFIER
                if (NZ)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                // Get the function name string
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                Error.CheckError();
                if (NC) { break; }
                
                // Skip to end of function signature (past closing parenthesis)
                loop
                {
                    Tokenizer.NextToken();
                    Error.CheckError();
                    if (NC) { break; }
                    
                    LDA ZP.CurrentToken
                    CMP #Tokens.RPAREN
                    if (Z) 
                    { 
                        Tokenizer.NextToken(); // Move past RPAREN to start of body
                        Error.CheckError();
                        if (NC) { break; }
                        break; 
                    }
                    
                    CMP #Tokens.EOF
                    if (Z)
                    {
                        Error.SyntaxError(); BIT ZP.EmulatorPCL
                        break;
                    }
                }
                if (NC) { break; }
                
                // Now we're at the start of the function body
                // Save this position as the start of body tokens
            }
            else
            {
                CMP #Tokens.BEGIN
                if (NZ)
                {
                    Error.InternalError(); BIT ZP.EmulatorPCL
                    break;
                }
                // "BEGIN" function name
                LDA #(Messages.BeginFunctionName % 256)
                STA ZP.TOPL
                LDA #(Messages.BeginFunctionName / 256)
                STA ZP.TOPH
                // Already at body start after BEGIN token - no parameters to skip
                // Tokenizer is positioned correctly after BEGIN
            }
            
            // Find the function that was already declared
            Functions.Find(); // Input: ZP.TOP = name, Output: ZP.IDX = function node
            if (NC)
            {
                Error.UndefinedIdentifier(); BIT ZP.EmulatorPCL
                break;
            }
            
            // Save function node address in statement storage
            LDA ZP.IDXL
            STA (Statement.stmtObjectPtr + 0)
            LDA ZP.IDXH
            STA (Statement.stmtObjectPtr + 1)
            
            LDA ZP.TokenizerPosL
            STA ZP.FSOURCEADDRESSL
            LDA ZP.TokenizerPosH
            STA ZP.FSOURCEADDRESSH
            
            // Find ENDFUNC or END to calculate body length
            loop
            {
                LDA ZP.CurrentToken
                CMP #Tokens.ENDFUNC
                if (Z) { break; }
                
                CMP #Tokens.END
                if (Z) { break; } 
                
                CMP #Tokens.EOF
                if (Z)
                {
                    Error.SyntaxError(); BIT ZP.EmulatorPCL
                    break;
                }
                
                Tokenizer.NextToken();
                Error.CheckError();
                if (NC) { break; }
            }
            if (NC) { break; }
            
            // Calculate body length (current pos - start pos)
            SEC
            LDA ZP.TokenizerPosL
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.TokenizerPosH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
    #ifdef DEBUG
            LDA #'L' // Length calculated
            Debug.COut();
            LDA ZP.FLENGTHH
            Debug.HOut();
            LDA ZP.FLENGTHL
            Debug.HOut();
    #endif
            
            // Create token stream for function body
            Statement.CreateTokenStream(); // Uses ZP.FSOURCEADDRESS, ZP.FLENGTH
            Error.CheckError();
            if (NC) { break; }
            
    #ifdef DEBUG
            LDA #'A' // Allocated
            Debug.COut();
            LDA ZP.FDESTINATIONADDRESSH
            Debug.HOut();
            LDA ZP.FDESTINATIONADDRESSL
            Debug.HOut();
    #endif
            
            // Restore function node address
            LDA (Statement.stmtObjectPtr + 0)
            STA ZP.IDXL
            LDA (Statement.stmtObjectPtr + 1)
            STA ZP.IDXH
            
            // Set function body tokens
            LDA ZP.FDESTINATIONADDRESSL
            STA ZP.IDYL
            LDA ZP.FDESTINATIONADDRESSH
            STA ZP.IDYH
            
            Functions.SetBody(); // Input: ZP.IDX = function node, ZP.IDY = body tokens
            Error.CheckError();
            if (NC) { break; }
            
            Functions.FreeAllOpCodes(); // compiled FUNCs potentially stale now
            
    #ifdef DEBUG
            LDA #'S' // Set body
            Debug.COut();
            LDA ZP.IDYL
            Debug.HOut();
            LDA ZP.IDYH
            Debug.HOut();
    #endif
            
            SEC // Success
            break;
        }
        
    #ifdef DEBUG
        LDA #'C'
        Debug.COut();
        LDA #'P'
        Debug.COut();
        LDA #'F'
        Debug.COut();
        LDA #'>'
        Debug.COut();
    #endif
    }
}
