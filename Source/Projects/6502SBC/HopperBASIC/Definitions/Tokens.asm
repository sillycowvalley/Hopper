unit Tokens
{
    enum IdentifierType
    {
        Undefined,
        Global,
        Constant,
        Function,
        Local, // Local or Argument depending on BP offset (+ve or -ve)
        Keyword
    }    
    
    enum Token
    {
        // Console commands
        NEW      = 0x80,
        LIST     = 0x81,
        RUN      = 0x82,
        CLEAR    = 0x83,
        VARS     = 0x84,
        FUNCS    = 0x85,
        FORGET   = 0x86,
        SAVE     = 0x87,
        LOAD     = 0x88,
        DIR      = 0x89,
        DEL      = 0x8A,
        FORMAT   = 0x8B,
        MEM      = 0x8C,
        
        // only DEBUG
        HEAP     = 0x8D,
        BUFFERS  = 0x8E,
        DUMP     = 0x8F,
        DASM     = 0x90,

        BYE      = 0x91,
        
        // only defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
        TRON     = 0x92,
        TROFF    = 0x93,

        REM      = 0x94,
        COMMENT  = 0x95,
        
        // Type declarations
        INT      = 0x96,
        WORD     = 0x97,
        BIT      = 0x98,
        BYTE     = 0x99,
        LONG     = 0x9A,
        STRING   = 0x9B,
        CHAR     = 0x9C,
        VAR      = 0x9D,
        CONST    = 0x9E,
        
        // Language keywords
        PRINT    = 0x9F,
        INPUT    = 0xA0,
        IF       = 0xA1,
        THEN     = 0xA2,
        ELSE     = 0xA3,
        ENDIF    = 0xA4,
        FUNC     = 0xA5,
        ENDFUNC  = 0xA6,
        RETURN   = 0xA7,
        BEGIN    = 0xA8,
        END      = 0xA9,
        FOR      = 0xAA,
        TO       = 0xAB,
        STEP     = 0xAC,
        NEXT     = 0xAD,
        WHILE    = 0xAE,
        WEND     = 0xAF,
        DO       = 0xB0,
        UNTIL    = 0xB1,
        //BREAK    = 0xB2,
        
        //CONT     = 0xB4,
        
        
        ARRAY    = 0xB7,
        AND      = 0xB8,
        OR       = 0xB9,
        NOT      = 0xBA,
        MOD      = 0xBB,
        TRUE     = 0xBC,
        FALSE    = 0xBD,
        
        // Built-in functions
        ABS      = 0xC0,
        MILLIS   = 0xC1,
        PEEK     = 0xC2,
        POKE     = 0xC3,
        SECONDS  = 0xC5,
        DELAY    = 0xC6,
        CLS      = 0xC7,
        
        // Character/String functions
        ASC      = 0xC8,
        CHR      = 0xC9,
        LEN      = 0xCA,
        
        // Hardware I/O functions
        PINMODE  = 0xCB,
        READ     = 0xCC,
        WRITE    = 0xCD,
        RND      = 0xCE,
        
        // I2C functions
        I2CFIND  = 0xCF,
        I2CBEGIN = 0xD0,
        I2CPUT   = 0xD1,
        I2CEND   = 0xD2,
        I2CGET   = 0xD3,
        I2CNEXT  = 0xD4,
        
        lastKeyword = 0xD4,  // Updated from 0xCE
        
        // Literals and identifiers (shifted down by 6)
        NUMBER     = 0xD5,  // was 0xCF
        STRINGLIT  = 0xD6,  // was 0xD0
        CHARLIT    = 0xD7,  // was 0xD1
        IDENTIFIER = 0xD8,  // was 0xD2
        
        // Stream/File control tokens
        EOF      = 0xD9,  // was 0xD3
        EOL      = 0xDA,  // was 0xD4
        EOE      = 0xDB,  // was 0xD5
        
        // Basic punctuation
        COLON     = 0xDC,  // was 0xD6
        COMMA     = 0xDD,  // was 0xD7
        SEMICOLON = 0xDE,  // was 0xD8
        
        // Basic operators
        EQUALS   = 0xDF,  // was 0xD9
        PLUS     = 0xE0,  // was 0xDA
        MINUS    = 0xE1,  // was 0xDB
        LPAREN   = 0xE2,  // was 0xDC
        RPAREN   = 0xE3,  // was 0xDD
        
        // Additional comparison operators
        NOTEQUAL = 0xE4,  // was 0xDE
        LT       = 0xE5,  // was 0xDF
        GT       = 0xE6,  // was 0xE0
        LE       = 0xE7,  // was 0xE1
        GE       = 0xE8,  // was 0xE2
        
        // Arithmetic operators  
        MULTIPLY = 0xE9,  // was 0xE3
        DIVIDE   = 0xEA,  // was 0xE4
        
        // Bitwise operators
        BITWISE_AND = 0xEB,  // was 0xE5
        BITWISE_OR  = 0xEC,  // was 0xE6
        BITWISE_NOT = 0xED,  // was 0xE7
        
        // Array and string operators
        LBRACKET = 0xEE,  // was 0xE8
        RBRACKET = 0xEF,  // was 0xE9
        LBRACE   = 0xF0,  // was 0xEA
        RBRACE   = 0xF1,  // was 0xEB
    }
    
        
    // Keywords A-I (first character < 'J')
    const byte[] keywordsAI = {
        // VERY FREQUENT (Rank 1-10)
        3, Token.FOR, 'F', 'O', 'R',             // Rank 3 - FOR/NEXT loops (0xAA)
        2, Token.IF, 'I', 'F',                   // Rank 4 - Conditionals (0xA1)
        3, Token.INT, 'I', 'N', 'T',             // Rank 5 - Common numeric type (0x96)
        3, Token.END, 'E', 'N', 'D',             // Rank 10 - Program termination (0xA9)
        
        // FREQUENT (Rank 11-20)
        3, Token.AND, 'A', 'N', 'D',             // Rank 11 - Logical AND (0xB8)
        5, Token.INPUT, 'I', 'N', 'P', 'U', 'T', // Rank 12 - User input (0xA0)
        3, Token.ABS, 'A', 'B', 'S',             // Rank 13 - Absolute value (0xC0)
        4, Token.ELSE, 'E', 'L', 'S', 'E',       // Rank 14 - Alternative branch (0xA3)
        2, Token.DO, 'D', 'O',                   // Rank 15 - DO/UNTIL loops (0xB0)
        3, Token.CHR, 'C', 'H', 'R',             // Rank 16 - Character conversion (0xC9)
        3, Token.ASC, 'A', 'S', 'C',             // Rank 17 - ASCII conversion (0xC8)
        
        // MODERATE (Rank 21-30)
        5, Token.ENDIF, 'E', 'N', 'D', 'I', 'F', // Rank 19 - End IF block (0xA4)
        5, Token.CLEAR, 'C', 'L', 'E', 'A', 'R', // Rank 20 - Clear screen/variables (0x83)
        3, Token.CLS, 'C', 'L', 'S',             // Rank 21 - Clear screen (0xC7)
        5, Token.FALSE, 'F', 'A', 'L', 'S', 'E', // Rank 23 - Boolean constant (0xBD)
        4, Token.BYTE, 'B', 'Y', 'T', 'E',       // Rank 24 - HopperBASIC data type (0x99)
        3, Token.BIT, 'B', 'I', 'T',             // Rank 25 - HopperBASIC data type (0x98)
        4, Token.CHAR, 'C', 'H', 'A', 'R',       // Rank 26 - Character type (0x9C)
        4, Token.FUNC, 'F', 'U', 'N', 'C',       // Rank 28 - Function declaration (0xA5)
        5, Token.CONST, 'C', 'O', 'N', 'S', 'T', // Rank 29 - Constant declaration (0x9E)
        5, Token.DELAY, 'D', 'E', 'L', 'A', 'Y', // Rank 30 - Timing function (0xC6)
        7, Token.ENDFUNC, 'E', 'N', 'D', 'F', 'U', 'N', 'C', // Rank 31 - End function (0xA6)
        5, Token.BEGIN, 'B', 'E', 'G', 'I', 'N', // Rank 32 - Main program start (0xA8)
        
        // INFREQUENT (Everything else alphabetically)
        5, Token.ARRAY, 'A', 'R', 'R', 'A', 'Y', // Array type declaration (0xB7)
        //5, Token.BREAK, 'B', 'R', 'E', 'A', 'K', // Loop control (0xB2)
        3, Token.BYE, 'B', 'Y', 'E',             // Exit interpreter (0x91)
        3, Token.DEL, 'D', 'E', 'L',             // Delete file (0x8A)
        3, Token.DIR, 'D', 'I', 'R',             // Directory listing (0x89)
        6, Token.FORGET, 'F', 'O', 'R', 'G', 'E', 'T', // Remove symbol (0x86)
        6, Token.FORMAT, 'F', 'O', 'R', 'M', 'A', 'T', // Format file system (0x8B)
        5, Token.FUNCS, 'F', 'U', 'N', 'C', 'S', // List functions (0x85)
        
        // I2C functions
        7, Token.I2CFIND,  'I', '2', 'C', 'F', 'I', 'N', 'D',      // I2C device discovery (0xCF)
        8, Token.I2CBEGIN, 'I', '2', 'C', 'B', 'E', 'G', 'I', 'N', // Start I2C write (0xD0)
        6, Token.I2CPUT,   'I', '2', 'C', 'P', 'U', 'T',           // Send I2C byte (0xD1)  
        6, Token.I2CEND,   'I', '2', 'C', 'E', 'N', 'D',           // End I2C transaction (0xD2)
        6, Token.I2CGET,   'I', '2', 'C', 'G', 'E', 'T',           // Read I2C bytes (0xD3)
        7, Token.I2CNEXT,  'I', '2', 'C', 'N', 'E', 'X', 'T',      // Get buffered byte (0xD4)
        
        0  // End marker
    };
    
    // Keywords J-Z (first character >= 'J')
    const byte[] keywordsJZ = {
        // VERY FREQUENT (Rank 1-10)
        5, Token.PRINT, 'P', 'R', 'I', 'N', 'T', // Rank 1 - Output data (0x9F)
        4, Token.NEXT, 'N', 'E', 'X', 'T',       // Rank 2 - FOR/NEXT loops (0xAD)
        4, Token.THEN, 'T', 'H', 'E', 'N',       // Rank 3 - IF/THEN conditionals (0xA2)
        3, Token.MOD, 'M', 'O', 'D',             // Rank 7 - Remainder arithmetic (0xBB)
        
        // FREQUENT (Rank 11-20)
        3, Token.LEN, 'L', 'E', 'N',             // Rank 18 - String length (0xCA)
        3, Token.VAR, 'V', 'A', 'R',             // Variable declaration (0x9D)
        4, Token.LONG, 'L', 'O', 'N', 'G',       // 32-bit type (0x9A)
        
        // MODERATE (Rank 21-30)
        5, Token.WHILE, 'W', 'H', 'I', 'L', 'E', // Rank 16 - WHILE/WEND loops (0xAE)
        4, Token.WEND, 'W', 'E', 'N', 'D',       // Rank 16 - WHILE/WEND loops (0xAF)
        4, Token.STEP, 'S', 'T', 'E', 'P',       // Rank 18 - FOR loop increment (0xAC)
        4, Token.LIST, 'L', 'I', 'S', 'T',       // Rank 22 - Display program (0x81)
        4, Token.LOAD, 'L', 'O', 'A', 'D',       // Rank 27 - Load from storage (0x88)
        
        // INFREQUENT (Everything else - HopperBASIC specific and console commands)
        4, Token.WORD, 'W', 'O', 'R', 'D',       // HopperBASIC data type (0x97)
        6, Token.STRING, 'S', 'T', 'R', 'I', 'N', 'G', // HopperBASIC data type (0x9B)
        6, Token.RETURN, 'R', 'E', 'T', 'U', 'R', 'N', // HopperBASIC structured programming (0xA7)
        3, Token.NOT, 'N', 'O', 'T',             // Logic operator (0xBA)
        2, Token.OR, 'O', 'R',                   // Logic operator (0xB9)
        4, Token.TRUE, 'T', 'R', 'U', 'E',       // Logic constant (0xBC)
        2, Token.TO, 'T', 'O',                   // Control flow extras (0xAB)
        5, Token.UNTIL, 'U', 'N', 'T', 'I', 'L', // Control flow extras (0xB1)
        
        // Console commands (all infrequent)
        3, Token.NEW, 'N', 'E', 'W',             // Console command (0x80)
        3, Token.RUN, 'R', 'U', 'N',             // Console command (0x82)
        3, Token.MEM, 'M', 'E', 'M',             // Console command (0x8C)
        4, Token.SAVE, 'S', 'A', 'V', 'E',       // File operation (0x87)
        4, Token.VARS, 'V', 'A', 'R', 'S',       // Console command (0x84)
        3, Token.REM, 'R', 'E', 'M',             // Comment (infrequent in programs) (0x94)
        
        // Built-in functions (all infrequent)
        6, Token.MILLIS, 'M', 'I', 'L', 'L', 'I', 'S', // Built-in function (0xC1)
        4, Token.PEEK, 'P', 'E', 'E', 'K',       // Built-in function (0xC2)
        4, Token.POKE, 'P', 'O', 'K', 'E',       // Built-in function (0xC3)
        3, Token.RND, 'R', 'N', 'D',             // Random number generation (0xCE)
        7, Token.SECONDS, 'S', 'E', 'C', 'O', 'N', 'D', 'S', // Built-in function (0xC5)
        
        // Hardware I/O functions
        7, Token.PINMODE, 'P', 'I', 'N', 'M', 'O', 'D', 'E', // Configure pin direction (0xCB)
        4, Token.READ, 'R', 'E', 'A', 'D',       // Digital input (0xCC)
        5, Token.WRITE, 'W', 'R', 'I', 'T', 'E', // Digital output (0xCD)
        
        0  // End marker
    };
                
                        
      
#if defined(TRACE) || defined(DEBUG) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
    
    // DEBUG and TRACE keywords
    const byte[] keywordsDebug  = {
    
        7, Token.BUFFERS, 'B', 'U', 'F', 'F', 'E', 'R', 'S', // Debug command (0x8E)
        4, Token.DASM,    'D', 'A', 'S', 'M',                // Disassemble function (0x90)
        4, Token.DUMP,    'D', 'U', 'M', 'P',                // Debug dump (0x8F)
        4, Token.HEAP,    'H', 'E', 'A', 'P',                // Heap inspection command (0x8D)  
        
        4, Token.TRON,    'T', 'R', 'O', 'N',                // Debug command (0x92)
        5, Token.TROFF,   'T', 'R', 'O', 'F', 'F',           // Debug command (0x93)
        0,
    };
#endif    
    
    // Search a single keyword table for a match
    // Input: ZP.TableIndex = address of keyword table to search
    //        Address.BasicProcessBuffer = null-terminated identifier to find
    // Output: A = token value if found, or 0 if not found
    // Munts: X, Y, ZP.KeywordLength, ZP.TokenValue
    searchKeywordTable()
    {
        LDY #0  // Start at beginning of keyword table
        loop
        {
            LDA [ZP.TableIndex], Y  // Get length of this keyword
            if (Z) { break; }       // End of table - not found
            
            STA ZP.KeywordLength    // Save keyword length
            INY
            LDA [ZP.TableIndex], Y  // Get token value
            STA ZP.TokenValue       // Save token value
            INY
            
            // Compare characters
            LDX #0  // Character index in our identifier
            loop
            {
                LDA Address.BasicProcessBuffer, X  // Get char from our identifier
                if (Z)  // Hit null terminator in our identifier
                {
                    // Check if we've matched the full keyword length
                    CPX ZP.KeywordLength
                    if (Z)
                    {
                        LDA ZP.TokenValue  // Return token value - exact match!
                        return;
                    }
                    break; // Length mismatch
                }
                
                // Check if we've exceeded keyword length
                CPX ZP.KeywordLength
                if (Z) { break; }       // Our identifier is longer than keyword
                
                CMP [ZP.TableIndex], Y  // Compare with expected character
                if (NZ) { break; }      // Mismatch
                
                INX
                INY
            } // loop
            
            // Mismatch - skip to next keyword
            loop
            {
                CPX ZP.KeywordLength  // Have we reached the end of keyword?
                if (Z) { break; }     // Yes, Y now points to start of next keyword
                INX                   // Move to next character position  
                INY                   // Advance Y to next character
            }
        } // loop
        LDA #0  // Not found
    }
    
    // Find keyword match for current identifier in working buffer
    // Input: Working buffer at Address.BasicProcessBuffer, null-terminated
    // Output: A = token value if found, or 0 if not found
    // Munts: A, X, Y, ZP.ACC, ZP.IDY
    FindKeyword()
    {
        PHX
        PHY
        
        // Choose primary table based on first character
        LDA Address.BasicProcessBuffer
        CMP #'J'  // Split point changed to 'J'
        if (C)    // >= 'J', use J-Z table
        {
            LDA #(keywordsJZ % 256)
            STA ZP.TableIndexL
            LDA #(keywordsJZ / 256)
            STA ZP.TableIndexH
        }
        else      // < 'J', use A-I table
        {
            LDA #(keywordsAI % 256)
            STA ZP.TableIndexL
            LDA #(keywordsAI / 256)
            STA ZP.TableIndexH
        }
        
        searchKeywordTable();
        
#if defined(DEBUG) || defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
        // If not found in primary table, check debug table
        if (Z)  // A is 0 = not found
        {
            LDA #(keywordsDebug % 256)
            STA ZP.TableIndexL
            LDA #(keywordsDebug / 256)
            STA ZP.TableIndexH
            
            searchKeywordTable();
        }
#endif
        
        PLY
        PLX
        // A contains token value or 0, Z may not be correct though ..
    }

    // Print keyword corresponding to token value
    // Input: A = token value (e.g., Token.CONST, Token.INT, etc.)
    // Output: Keyword printed to serial
    // Modifies: A, X, Y (internal search), preserves token value concept
    // Error: If token not found in keywords table, prints nothing, C if printed, NC if not
    PrintKeyword()
    {
        
        PHX
        PHY
        
        STA ZP.TokenValue  // Store target token value
        
        // Load keywords table address into ZP.IDY
        LDA #(keywordsAI % 256)
        STA ZP.TableIndexL
        LDA #(keywordsAI / 256)
        STA ZP.TableIndexH
        
        PrintKeywordFromTable();
        if (NC)
        {
            // perhaps it is in the other table
            LDA #(keywordsJZ % 256)
            STA ZP.TableIndexL
            LDA #(keywordsJZ / 256)
            STA ZP.TableIndexH
            PrintKeywordFromTable();
            
#if defined(DEBUG) || defined(TRACE) || defined(TRACEEXE) || defined(TRACEFILE) || defined(TRACEPARSE)
            // If not found in primary table, check debug table            
            if (NC)
            {
                LDA #(keywordsDebug % 256)
                STA ZP.TableIndexL
                LDA #(keywordsDebug / 256)
                STA ZP.TableIndexH
                PrintKeywordFromTable();
            }
#endif            
#ifdef DEBUG
            if (NC)
            {
                ALOut();
                Error.InternalError(); BIT ZP.EmulatorPCL
            }
#endif
        }
        
        PLY
        PLX
    }

    // Helper method to search table and print keyword
    // Input: ZP.TokenValue = target token value, ZP.TableIndex = table address
    // Output: Keyword printed to serial if found, C if found, NC if not
    // Modifies: A, X, Y, ZP.KeywordLength (internal use only)
    PrintKeywordFromTable()
    {
        LDY #0  // Index into keywords table
        loop
        {
            LDA [ZP.TableIndex], Y     // Get length of this keyword
            if (Z) 
            { 
                CLC
                break; 
            }   // End of table - not found
            
            STA ZP.KeywordLength       // Save keyword length
            INY
            LDA [ZP.TableIndex], Y     // Get token value 
            CMP ZP.TokenValue          // Compare with target
            if (Z)
            {
                // Found it! Print the keyword
                INY  // Move to first character
                LDX ZP.KeywordLength   // X = character count
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    
                    LDA [ZP.TableIndex], Y  // Access character 
                    Serial.WriteChar();
                    INY
                    DEX
                } // loop
                
                SEC
                break;  // Done printing
            }
            
            // Skip to next keyword: advance Y by keyword length + 1 (for token byte)
            INY                   // Skip the token value byte first
            LDX ZP.KeywordLength  // Then skip the keyword characters
            loop
            {
                CPX #0
                if (Z) { break; }
                INY
                DEX
            }
        } // loop
    }
    
    
    // Check if a token value represents a keyword
    // Input: A = token value to check
    // Output: C set if token is a keyword, NC if not a keyword  
    // Modifies: Processor flags only
    IsKeyword()
    {
        CMP # Token.IDENTIFIER
        if (Z)
        {
            CLC  // Not a keyword    
        }
        else
        {
            CMP #( Token.lastKeyword + 1)
            if (C)  // A >= (lastKeyword + 1), meaning A > lastKeyword
            {
                CLC  // Not a keyword (operators, literals, etc.)
            }
            else    // A < (lastKeyword + 1), meaning A <= lastKeyword
            {
                SEC  // Is a keyword
            }
        }
    }
    
    // Check if current token is a statement terminator
    // Input: A = token to check
    // Output: C set if token ends a statement, NC if statement continues
    // Checks for: COLON (statement separator), EOF (end of stream), EOL (end of line)
    // Preserves: A, X, Y
    // Usage: Helps PRINT recognize when to stop processing arguments
    IsEndOfPrintStatement()
    {
        switch (A)
        {
            case Token.COLON:   // another statement follows
            case Token.EOF:     // end of stream
            case Token.EOL:     // end of line
            case Token.COMMENT: // end of line comment
            {
                SEC
            }
            default:
            {
                CLC
            }
        }
    }
    
#ifdef DEBUG
    // Validate that a keyword table doesn't exceed 256 bytes
    // Input: ZP.IDY = address of keyword table to validate
    // Output: C set if valid, NC if table exceeds 256 bytes
    //         A = total size in bytes
    // Preserves: ZP.IDY
    ValidateKeywordTable()
    {
        PHX
        PHY
        STZ ZP.TOPH
#ifdef VERBOSEDEBUG
        Debug.NL();
#endif
        LDY #0      // Current offset into table
        
        loop
        {
            // Get length of this keyword
            LDA [ZP.IDY], Y
            if (Z) 
            { 
                // End of table marker - success!
    #ifdef VERBOSEDEBUG
                LDA #'E'
                Debug.COut();
                LDA #'N'
                Debug.COut();
                LDA #'D'
                Debug.COut();
    #endif
                INY         // Include the terminator byte in size
                TYA         // Return total size in A
                SEC         // Set C for success
                break; 
            }
            
            INY                 // Skip length byte
        
            // Check token value is valid (in both verbose and non-verbose modes)
            LDA [ZP.IDY], Y     // Get token byte
            CMP #(Token.lastKeyword + 1)
            if (C)              // Token > lastKeyword
            {
                // Invalid token - return error
                DEY             // Back up to length byte position
                TYA             // Return offset of entry with bad token
                CLC             // Clear C for error
                break;
            }
            // reload length
            DEY             
            LDA [ZP.IDY], Y
            
    #ifdef VERBOSEDEBUG
            // Print the keyword entry for debugging
            // Format: [offset] length:token keyword
            TYA
            Debug.HOut();    // Print current offset
            LDA #':'
            Debug.COut();
            
            LDA [ZP.IDY], Y     // Get length again
            STA ZP.TOPL
            STZ ZP.TOPT
            Print.Decimal();
            Debug.Space();
            TAX                 // X = keyword length
            
            INY                 // Move to token byte
            LDA #'0'
            Debug.COut();
            LDA #'x'
            Debug.COut();
            LDA [ZP.IDY], Y
            Debug.HOut();    // Print token value
            Debug.Space();
            
            INY                 // Move to first character
            
            // Print the keyword characters
            loop
            {
                DEX
                if (MI) { break; }  // Done with keyword chars
                LDA [ZP.IDY], Y
                Debug.COut();
                INY
            }
            Debug.NL();
            
            // Continue with next entry - Y is already positioned
    #else
            // Non-verbose mode - just skip through the entry
            TAX                 // X = keyword length
            INY                 // Skip length byte
            INY                 // Skip token byte
            
            // Skip keyword characters
            loop
            {
                DEX
                if (MI) { break; }
                INY
            }
    #endif
            
            // Check if Y wrapped around (table too big)
            CPY #0
            if (Z)
            {
                LDA #0xFF
                CLC
                break;
            }
        }
        
        PLY
        PLX
    }

    
    // Validate all keyword tables during initialization
    // Call this from BASIC.Initialize or similar
    ValidateAllKeywordTables()
    {
        PHA
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        // Validate keywordsAH table
        LDA #(keywordsAI % 256)
        STA ZP.IDYL
        LDA #(keywordsAI / 256)
        STA ZP.IDYH
        
        ValidateKeywordTable();
        STA ZP.TOPL
        if (NC)
        {
            Debug.NL();
            LDA #'A' Debug.COut();
            LDA #'I' Debug.COut();
            LDA #':' Debug.COut();
            Debug.HOut();  // Print offset where error occurred
            Error.InternalError(); BIT ZP.EmulatorPCL
        }
        else
        {
#ifdef VERBOSEDEBUG
            STZ ZP.TOPH
            Debug.NL();
            LDA #'A' Debug.COut();
            LDA #'I' Debug.COut();
            LDA #'=' Debug.COut();
            STZ ZP.TOPT
            Print.Decimal();
            Debug.Space();
#endif
        }
        
        
        // Validate keywordsIZ table
        LDA #(keywordsJZ % 256)
        STA ZP.IDYL
        LDA #(keywordsJZ / 256)
        STA ZP.IDYH
        
        ValidateKeywordTable();
        STA ZP.TOPL
        if (NC)
        {
            Debug.NL();
            LDA #'J' Debug.COut();
            LDA #'Z' Debug.COut();
            LDA #':' Debug.COut();
            Debug.HOut();  // Print offset where error occurred
            Error.InternalError(); BIT ZP.EmulatorPCL
        }
        else
        {
#ifdef VERBOSEDEBUG
            STZ ZP.TOPH
            Debug.NL();
            LDA #'J' Debug.COut();
            LDA #'Z' Debug.COut();
            LDA #'=' Debug.COut();
            STZ ZP.TOPT
            Print.Decimal();
            Debug.NL();
#endif
        }
    
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLA
    }
#endif
}
