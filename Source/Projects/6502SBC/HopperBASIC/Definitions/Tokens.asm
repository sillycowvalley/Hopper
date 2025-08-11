unit Tokens
{
    enum IdentifierType
    {
        Undefined,
        Global,
        Constant,
        Function,
        Local, // Local or Argument depending on BP offser (+ve or -ve)
        Keyword
    }    
    
    // Complete Token definitions for HopperBASIC
    // All values >= 0x80 
    // See the implementation of Tokenizer.Rollback to understand why we set the high bit of tokens and
    // why we limit our characaters in literals to ASCII (0..127)
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
        MEM      = 0x8B,
        HEAP     = 0x8C,
        BUFFERS  = 0x8D,
        DUMP     = 0x8E,
        DASM     = 0x8F,
        BYE      = 0x90,
        TRON     = 0x91,
        TROFF    = 0x92,
        REM      = 0x93,
        COMMENT  = 0x94,
        EOL      = 0x95,
        
        // Type declarations
        INT      = 0x96,
        WORD     = 0x97,
        BIT      = 0x98,
        BYTE     = 0x99,
        STRING   = 0x9A,
        CONST    = 0x9B, 
        
        // Language keywords
        PRINT    = 0x9C,
        INPUT    = 0x9D,
        IF       = 0x9E,
        THEN     = 0x9F,
        ELSE     = 0xA0,
        ENDIF    = 0xA1,
        FUNC     = 0xA2,
        ENDFUNC  = 0xA3,
        RETURN   = 0xA4,
        BEGIN    = 0xA5,
        END      = 0xA6,
        FOR      = 0xA7,
        TO       = 0xA8,
        STEP     = 0xA9,
        NEXT     = 0xAA,
        WHILE    = 0xAB,
        WEND     = 0xAC,
        DO       = 0xAD,
        UNTIL    = 0xAE,
        BREAK    = 0xAF,
        CONTINUE = 0xB0,
        CONT     = 0xB1,
        GOSUB    = 0xB2,
        GOTO     = 0xB3,
        ARRAY    = 0xB4,
        AND      = 0xB5,
        OR       = 0xB6,
        NOT      = 0xB7,
        MOD      = 0xB8,
        TRUE     = 0xB9,
        FALSE    = 0xBA,
        REPEAT   = 0xBB,
        STOP     = 0xBC,
        
        // Built-in functions
        ABS      = 0xBD,
        MILLIS   = 0xBE,
        PEEK     = 0xBF,
        POKE     = 0xC0,
        RND      = 0xC1,
        SECONDS  = 0xC2,
        DELAY    = 0xC3,
        CLS      = 0xC4,  // Clear screen command
        
        // Hardware I/O functions
        PINMODE  = 0xC5,
        READ     = 0xC6,
        WRITE    = 0xC7,
        
        lastKeyword = 0xC7,  // Updated to include hardware commands
        
        // Literals and identifiers
        NUMBER     = 0xC7,  // Numeric literal (followed by string)
        STRINGLIT  = 0xC8,  // String literal (followed by string)
        IDENTIFIER = 0xC9,  // Variable/function name (followed by string)
        
        // Special punctuation (no inline data)
        EOF      = 0xCA,  // End of file/stream
        COLON    = 0xCB,  // Statement separator :
        COMMA    = 0xCC,  // Argument separator ,
        SEMICOLON = 0xCD, // Print separator ;
        
        // Basic operators
        EQUALS   = 0xCE,  // =
        PLUS     = 0xCF,  // +
        MINUS    = 0xD0,  // -
        LPAREN   = 0xD1,  // (
        RPAREN   = 0xD2,  // )
        
        // Additional comparison operators
        NOTEQUAL = 0xD3,  // <>
        LT       = 0xD4,  // <
        GT       = 0xD5,  // >
        LE       = 0xD6,  // <=
        GE       = 0xD7,  // >=
        
        // Arithmetic operators
        MULTIPLY = 0xD8,  // *
        DIVIDE   = 0xD9,  // /
        
        // Bitwise operators
        BITWISE_AND = 0xDA,  // &
        BITWISE_OR  = 0xDB,  // |
        
        // Array and string operators
        LBRACKET = 0xDC,  // [
        RBRACKET = 0xDD,  // ]
        LBRACE   = 0xDE,  // {
        RBRACE   = 0xDF,  // }
        
        VAR      = 0xE0,  // Uninitialized type
    }
    
    // Keywords A-L (first character < 'M') - Reorganized by frequency
    const byte[] keywordsAL = {
        // VERY FREQUENT (Rank 1-10)
        3, Token.FOR, 'F', 'O', 'R',             // Rank 3 - FOR/NEXT loops
        2, Token.IF, 'I', 'F',                   // Rank 4 - Conditionals  
        3, Token.INT, 'I', 'N', 'T',             // Rank 5 - Common numeric type
        4, Token.GOTO, 'G', 'O', 'T', 'O',       // Rank 8 - Jump to line
        5, Token.GOSUB, 'G', 'O', 'S', 'U', 'B', // Rank 9 - Subroutine call
        3, Token.END, 'E', 'N', 'D',             // Rank 10 - Program termination
        
        // FREQUENT (Rank 11-20)
        3, Token.AND, 'A', 'N', 'D',             // Rank 11 - Logical AND
        5, Token.INPUT, 'I', 'N', 'P', 'U', 'T', // Rank 12 - User input
        3, Token.ABS, 'A', 'B', 'S',             // Rank 13 - Absolute value
        4, Token.ELSE, 'E', 'L', 'S', 'E',       // Rank 14 - Alternative branch
        2, Token.DO, 'D', 'O',                   // Rank 15 - DO/UNTIL loops
        
        // MODERATE (Rank 21-30)
        5, Token.ENDIF, 'E', 'N', 'D', 'I', 'F', // Rank 17 - End IF block
        5, Token.CLEAR, 'C', 'L', 'E', 'A', 'R', // Rank 19 - Clear screen/variables
        3, Token.CLS, 'C', 'L', 'S',             // Rank 19 - Clear screen
        4, Token.LIST, 'L', 'I', 'S', 'T',       // Rank 20 - Display program
        5, Token.FALSE, 'F', 'A', 'L', 'S', 'E', // Rank 21 - Boolean constant
        4, Token.BYTE, 'B', 'Y', 'T', 'E',       // Rank 22 - HopperBASIC data type
        3, Token.BIT, 'B', 'I', 'T',             // Rank 23 - HopperBASIC data type
        4, Token.LOAD, 'L', 'O', 'A', 'D',       // Rank 25 - Load from storage
        4, Token.FUNC, 'F', 'U', 'N', 'C',       // Rank 26 - Function declaration
        5, Token.CONST, 'C', 'O', 'N', 'S', 'T', // Rank 27 - Constant declaration
        5, Token.DELAY, 'D', 'E', 'L', 'A', 'Y', // Rank 28 - Timing function
        7, Token.ENDFUNC, 'E', 'N', 'D', 'F', 'U', 'N', 'C', // Rank 29 - End function
        5, Token.BEGIN, 'B', 'E', 'G', 'I', 'N', // Rank 30 - Main program start
        
        // INFREQUENT (Everything else alphabetically)
        5, Token.ARRAY, 'A', 'R', 'R', 'A', 'Y', // Array type declaration
        5, Token.BREAK, 'B', 'R', 'E', 'A', 'K', // Loop control
        7, Token.BUFFERS, 'B', 'U', 'F', 'F', 'E', 'R', 'S', // Debug command
        3, Token.BYE, 'B', 'Y', 'E',             // Exit interpreter
        4, Token.CONT, 'C', 'O', 'N', 'T',       // Continue from break (or CONTINUE)
        8, Token.CONTINUE, 'C', 'O', 'N', 'T', 'I', 'N', 'U', 'E', // Loop control
        4, Token.DASM, 'D', 'A', 'S', 'M',       // Disassemble function
        3, Token.DEL, 'D', 'E', 'L',             // Delete file
        3, Token.DIR, 'D', 'I', 'R',             // Directory listing
        4, Token.DUMP, 'D', 'U', 'M', 'P',       // Debug dump
        6, Token.FORGET, 'F', 'O', 'R', 'G', 'E', 'T', // Remove symbol
        5, Token.FUNCS, 'F', 'U', 'N', 'C', 'S', // List functions
        4, Token.HEAP, 'H', 'E', 'A', 'P',       // Heap inspection command
        
        0  // End marker
    };
    
    // Keywords M-Z (first character >= 'M') - Reorganized by frequency  
    const byte[] keywordsMZ = {
        // VERY FREQUENT (Rank 1-10)
        5, Token.PRINT, 'P', 'R', 'I', 'N', 'T', // Rank 1 - Output data
        4, Token.NEXT, 'N', 'E', 'X', 'T',       // Rank 2 - FOR/NEXT loops
        4, Token.THEN, 'T', 'H', 'E', 'N',       // Rank 3 - IF/THEN conditionals
        
        // FREQUENT (Rank 11-20)
        3, Token.MOD, 'M', 'O', 'D',             // Rank 7 - Remainder arithmetic
        3, Token.VAR, 'V', 'A', 'R',             // Uninitialized type
        
        // MODERATE (Rank 21-30)
        5, Token.WHILE, 'W', 'H', 'I', 'L', 'E', // Rank 16 - WHILE/WEND loops
        4, Token.WEND, 'W', 'E', 'N', 'D',       // Rank 16 - WHILE/WEND loops
        4, Token.STEP, 'S', 'T', 'E', 'P',       // Rank 18 - FOR loop increment
        3, Token.RND, 'R', 'N', 'D',             // Rank 24 - Random number generation
        
        // INFREQUENT (Everything else - HopperBASIC specific and console commands)
        4, Token.WORD, 'W', 'O', 'R', 'D',       // HopperBASIC data type
        6, Token.STRING, 'S', 'T', 'R', 'I', 'N', 'G', // HopperBASIC data type
        6, Token.RETURN, 'R', 'E', 'T', 'U', 'R', 'N', // HopperBASIC structured programming
        3, Token.NOT, 'N', 'O', 'T',             // Logic operator
        2, Token.OR, 'O', 'R',                   // Logic operator
        4, Token.TRUE, 'T', 'R', 'U', 'E',       // Logic constant
        2, Token.TO, 'T', 'O',                   // Control flow extras
        5, Token.UNTIL, 'U', 'N', 'T', 'I', 'L', // Control flow extras
        
        // Console commands (all infrequent)
        3, Token.NEW, 'N', 'E', 'W',             // Console command
        3, Token.RUN, 'R', 'U', 'N',             // Console command
        3, Token.MEM, 'M', 'E', 'M',             // Console command
        4, Token.SAVE, 'S', 'A', 'V', 'E',       // File operation
        4, Token.VARS, 'V', 'A', 'R', 'S',       // Console command
        3, Token.REM, 'R', 'E', 'M',             // Comment (infrequent in programs)
        4, Token.TRON, 'T', 'R', 'O', 'N',       // Debug command
        5, Token.TROFF, 'T', 'R', 'O', 'F', 'F', // Debug command
        
        // Built-in functions (all infrequent)
        6, Token.MILLIS, 'M', 'I', 'L', 'L', 'I', 'S', // Built-in function
        4, Token.PEEK, 'P', 'E', 'E', 'K',       // Built-in function
        4, Token.POKE, 'P', 'O', 'K', 'E',       // Built-in function
        7, Token.SECONDS, 'S', 'E', 'C', 'O', 'N', 'D', 'S', // Built-in function
        
        // Hardware I/O functions (new)
        7, Token.PINMODE, 'P', 'I', 'N', 'M', 'O', 'D', 'E', // Configure pin direction
        4, Token.READ, 'R', 'E', 'A', 'D',       // Digital input
        5, Token.WRITE, 'W', 'R', 'I', 'T', 'E', // Digital output
        
        0  // End marker
    };
    
    
    // Find keyword match for current identifier in working buffer
    // Input: Working buffer at Address.BasicProcessBuffer1, null-terminated
    // Output: A = token value if found, or 0 if not found
    // Munts: A, X, Y, ZP.ACC, ZP.IDY
    FindKeyword()
    {
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        
        
        // Choose table based on first character
        LDA Address.BasicProcessBuffer
        CMP #'M'
        if (C)  // >= 'M', use M-Z table
        {
            LDA #(keywordsMZ % 256)
            STA ZP.IDYL
            LDA #(keywordsMZ / 256)
            STA ZP.IDYH
        }
        else    // < 'M', use A-L table
        {
            LDA #(keywordsAL % 256)
            STA ZP.IDYL
            LDA #(keywordsAL / 256)
            STA ZP.IDYH
        }
        
        LDY #0  // Start at beginning of keyword table
        loop
        {
            LDA [ZP.IDY], Y    // Get length of this keyword
            if (Z) { break; }  // End of table - not found
            
            STA ZP.ACCL        // Save keyword length
            INY
            LDA [ZP.IDY], Y    // Get token value
            STA ZP.ACCH        // Save token value
            INY
            
            // Compare characters
            LDX #0  // Character index in our identifier
            loop
            {
                LDA Address.BasicProcessBuffer, X  // Get char from our identifier
                if (Z)  // Hit null terminator in our identifier
                {
                    // Check if we've matched the full keyword length
                    CPX ZP.ACCL
                    if (Z)
                    {
                        LDX ZP.ACCH  // Return token value - exact match!
                    
                        PLA
                        STA ZP.IDYH
                        PLA
                        STA ZP.IDYL
                        PLA
                        STA ZP.ACCH
                        PLA
                        STA ZP.ACCL
                        
                        TXA // Return token value
                        
                        PLY
                        PLX
                        return;
                    }
                    break; // Length mismatch
                }
                
                // Check if we've exceeded keyword length
                CPX ZP.ACCL
                if (Z) { break; }  // Our identifier is longer than keyword
                
                CMP [ZP.IDY], Y  // Compare with expected character
                if (NZ) { break; } // Mismatch
                
                INX
                INY
            } // loop
            
            // Mismatch - skip to next keyword
            loop
            {
                CPX ZP.ACCL       // Have we reached the end of keyword?
                if (Z) { break; } // Yes, Y now points to start of next keyword
                INX               // Move to next character position  
                INY               // Advance Y to next character
            }
        } // loop
        
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        
        LDA #0  // Not found
    }

    // Print keyword corresponding to token value
    // Input: A = token value (e.g., Token.CONST, Token.INT, etc.)
    // Output: Keyword printed to serial
    // Modifies: A, X, Y (internal search), preserves token value concept
    // Error: If token not found in keywords table, prints nothing, C if printed, NC if not
    PrintKeyword()
    {
        
        PHA  // Save token value
        PHX
        PHY
        
        TAX
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        STX ZP.ACCL  // Store target token value
        
        // Load keywords table address into ZP.IDY
        LDA #(keywordsAL % 256)
        STA ZP.IDYL
        LDA #(keywordsAL / 256)
        STA ZP.IDYH
        
        printKeywordFromTable();
        if (NC)
        {
            // perhaps it is in the other table
            LDA #(keywordsMZ % 256)
            STA ZP.IDYL
            LDA #(keywordsMZ / 256)
            STA ZP.IDYH
            printKeywordFromTable();
#ifdef DEBUG
            if (NC)
            {
                ALOut();
            }
#endif
        }
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLY
        PLX
        PLA
    }

    // Helper method to search table and print keyword
    // Input: ZP.ACCL = target token value, ZP.IDY = table address
    // Output: Keyword printed to serial if found, C if found, NC if not
    // Modifies: A, X, Y (internal use only)
    printKeywordFromTable()
    {
        LDY #0  // Index into keywords table
        loop
        {
            LDA [ZP.IDY], Y     // Get length of this keyword
            if (Z) 
            { 
                CLC
                break; 
            }   // End of table - not found
            
            STA ZP.ACCH         // Save keyword length
            INY
            LDA [ZP.IDY], Y     // Get token value 
            CMP ZP.ACCL         // Compare with target
            if (Z)
            {
                // Found it! Print the keyword
                INY  // Move to first character
                LDX ZP.ACCH  // X = character count
                
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    
                    LDA [ZP.IDY], Y  // Access character 
                    Serial.WriteChar();
                    INY
                    DEX
                } // loop
                
                SEC
                break;  // Done printing
            }
            
            // Skip to next keyword: advance Y by keyword length + 1 (for token byte)
            INY  // Skip the token value byte first
            LDX ZP.ACCH  // Then skip the keyword characters
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
    // Usage: Helps PRINT and other statements recognize when to stop processing arguments
    IsEndOfStatement()
    {
        switch (A)
        {
            case Token.COLON: // another statement follows
            case Token.EOF:   // end of stream
            case Token.EOL:   // end of line
            {
                SEC
            }
            default:
            {
                CLC  // (fixed typo: was SLC)
            }
        }
    }
}