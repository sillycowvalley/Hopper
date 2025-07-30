unit FunctionDeclaration
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    uses "Messages"
    uses "Tokenizer"
    uses "Tools"
    
    uses "Instructions"
    
    uses "Functions" 
    uses "Arguments"
    
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
        Tools.COut();
        LDA #'F'
        Tools.COut();
        LDA #'D'
        Tools.COut();
    #endif
    
        loop // Single exit block for clean error handling
        {
            // Get next token - should be function name
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            // Check that we have an identifier
            LDA ZP.CurrentToken
            CMP #Tokens.IDENTIFIER
            if (NZ)
            {
                Tokenizer.IsKeyword();
                if (C)
                {
                    LDA #(Messages.IllegalFunctionName % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.IllegalFunctionName / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                }
                else
                {
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                }
                CLC
                break;
            }
            
            // Get the function name
            Tokenizer.GetTokenString(); // Returns pointer in ZP.TOP
            Messages.CheckError();
            if (NC) { break; }
            
            // Save function name pointer in statement storage
            LDA ZP.TOPL
            STA (Statement.stmtNamePtr + 0)
            LDA ZP.TOPH
            STA (Statement.stmtNamePtr + 1)
            
            // Check if function or variable with this name already exists
            LDX #ZP.FunctionsList
            Objects.Find(); // Input: ZP.TOP = name
            if (C)
            {
                LDA #(Messages.FunctionExists % 256)
                STA ZP.LastErrorL
                LDA #(Messages.FunctionExists / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                break;
            }
            
            STZ ZP.SymbolIteratorFilter // Accept any symbol type
            Variables.Find(); // Input: ZP.TOP = name
            if (C)
            {
                LDA #(Messages.FunctionExists % 256) // Same error message
                STA ZP.LastErrorL
                LDA #(Messages.FunctionExists / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                break;
            }
            
            // Get next token - should be opening parenthesis
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            LDA ZP.CurrentToken
            CMP #Tokens.LPAREN
            if (NZ)
            {
                LDA #(Messages.ExpectedLeftParen % 256)
                STA ZP.LastErrorL
                LDA #(Messages.ExpectedLeftParen / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
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
            Messages.CheckError();
            if (NC) { break; }
            
            // Save function node address for adding arguments
            LDA ZP.IDXL
            STA (Statement.stmtObjectPtr + 0)
            LDA ZP.IDXH
            STA (Statement.stmtObjectPtr + 1)
            
            // Parse parameters
            parseParameterList();
            Messages.CheckError();
            if (NC) { break; }
            
            // Expect closing parenthesis (should be current token after parseParameterList)
            LDA ZP.CurrentToken
            CMP #Tokens.RPAREN
            if (NZ)
            {
                LDA #(Messages.ExpectedRightParen % 256)
                STA ZP.LastErrorL
                LDA #(Messages.ExpectedRightParen / 256)
                STA ZP.LastErrorH
                Messages.StorePC(); // 6502 PC -> IDY
                CLC
                break;
            }
            
            // Get next token - start of function body or EOL
            Tokenizer.NextToken();
            Messages.CheckError();
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
            Messages.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'F'
        Tools.COut();
        LDA #'D'
        Tools.COut();
        LDA #'>'
        Tools.COut();
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
        Tools.COut();
        LDA #'P'
        Tools.COut();
        LDA #'L'
        Tools.COut();
    #endif
    
        loop // Single exit block
        {
            // Get next token after opening parenthesis
            Tokenizer.NextToken();
            Messages.CheckError();
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
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                    CLC
                    break;
                }
                
                // Get parameter name
                Tokenizer.GetTokenString(); // Result in ZP.TOP
                Messages.CheckError();
                if (NC) { break; }
                
                // Add argument to function
                // Restore function node address to ZP.IDX
                LDA (Statement.stmtObjectPtr + 0)
                STA ZP.IDXL
                LDA (Statement.stmtObjectPtr + 1)
                STA ZP.IDXH
                
                Arguments.Add(); // Input: ZP.IDX = function node, ZP.TOP = argument name
                Messages.CheckError();
                if (NC) { break; }
                
                // Get next token
                Tokenizer.NextToken();
                Messages.CheckError();
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
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                    CLC
                    break;
                }
                
                // Get next token after comma
                Tokenizer.NextToken();
                Messages.CheckError();
                if (NC) { break; }
                
                // Continue parsing parameters
            }
            
            break; // Exit outer loop
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'P'
        Tools.COut();
        LDA #'L'
        Tools.COut();
        LDA #'>'
        Tools.COut();
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
        Tools.COut();
        LDA #'C'
        Tools.COut();
        LDA #'B'
        Tools.COut();
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
    Tools.NL();
    LDA #'E' // Error set here
    Tools.COut();
    DumpBasicBuffers();
    

                
                    // Hit end of input without finding ENDFUNC
                    LDA #(Messages.SyntaxError % 256)
                    STA ZP.LastErrorL
                    LDA #(Messages.SyntaxError / 256)
                    STA ZP.LastErrorH
                    Messages.StorePC(); // 6502 PC -> IDY
                    CLC
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
                Messages.CheckError();
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
            Messages.CheckError();
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
            Messages.CheckError();
            if (NC) { break; }
            
            // Get next token after ENDFUNC
            Tokenizer.NextToken();
            Messages.CheckError();
            if (NC) { break; }
            
            SEC // Success
            break;
        } // Single exit block
        
    #ifdef DEBUG
        LDA #'C'
        Tools.COut();
        LDA #'B'
        Tools.COut();
        LDA #'>'
        Tools.COut();
    #endif
    }
    
}
