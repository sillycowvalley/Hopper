unit Error // ErrorID.asm
{
    
    // Error word IDs - using bits 6-5 for table selection, bits 4-0 for word index
    // Bit 7 = 0 (distinguishes from keywords which have bit 7 = 1)
    
    enum ErrorWord
    {
        // Table 0 (bits 6-5 = 00): Common error words (0x00-0x1F)
        ERROR      = 0x00,  // "ERROR"
        SYNTAX     = 0x01,  // "SYNTAX"
        TYPE       = 0x02,  // "TYPE"
        MISMATCH   = 0x03,  // "MISMATCH"
        OUT        = 0x04,  // "OUT"
        OF         = 0x05,  // "OF"
        MEMORY     = 0x06,  // "MEMORY"
        DIVISION   = 0x07,  // "DIVISION"
        BY         = 0x08,  // "BY"
        ZERO       = 0x09,  // "ZERO"
        WITHOUT    = 0x0A,  // "WITHOUT"
        EXISTS     = 0x0B,  // "EXISTS"
        UNDEFINED  = 0x0C,  // "UNDEFINED"
        IDENTIFIER = 0x0D,  // "IDENTIFIER"
        ILLEGAL    = 0x0E,  // "ILLEGAL"
        INVALID    = 0x0F,  // "INVALID"
        NOT        = 0x10,  // "NOT"
        IMPLEMENTED= 0x11,  // "IMPLEMENTED"
        INTERNAL   = 0x12,  // "INTERNAL"
        ONLY       = 0x13,  // "ONLY"
        IN         = 0x14,  // "IN"
        DEBUG      = 0x15,  // "DEBUG"
        BUILD      = 0x16,  // "BUILD"
        TRACE      = 0x17,  // "TRACE"
        VARIABLE   = 0x18,  // "VARIABLE"
        FILE       = 0x19,  // "FILE"
        FOUND      = 0x1A,  // "FOUND"
        NUMERIC    = 0x1B,  // "NUMERIC"
        OVERFLOW   = 0x1C,  // "OVERFLOW"
        TOO        = 0x1D,  // "TOO"
        LONG       = 0x1E,  // "LONG"
        BAD        = 0x1F,  // "BAD"
        
        // Table 1 (bits 6-5 = 01): Expression/parsing words (0x20-0x3F)
        INDEX      = 0x20,  // "INDEX"
        EXPECTED   = 0x21,  // "EXPECTED"
        ASSIGNMENT = 0x22,  // "ASSIGNMENT"
        CHARACTER  = 0x23,  // "CHARACTER"
        OPERATOR   = 0x24,  // "OPERATOR"
        BUFFER     = 0x25,  // "BUFFER"
        RPAREN     = 0x26,  // ")"
        LPAREN     = 0x27,  // "("
        EQUALS     = 0x28,  // "="
        UNEXPECTED = 0x29,  // "UNEXPECTED"
        END        = 0x2A,  // "END"
        LINE       = 0x2B,  // "LINE"
        LITERAL    = 0x2C,  // "LITERAL"
        VALUE      = 0x2D,  // "VALUE"
        MODE       = 0x2E,  // "MODE"
        AT         = 0x2F,  // "AT"
        CONSOLE    = 0x30,  // "CONSOLE"
        HEAP       = 0x31,  // "HEAP"
        CORRUPT    = 0x32,  // "CORRUPT"
        CANNOT     = 0x33,  // "CANNOT"
        ROLLBACK   = 0x34,  // "ROLLBACK"
        BREAK      = 0x35,  // "BREAK"
        NO         = 0x36,  // "NO"
        MORE       = 0x37,  // "MORE"
        LOCALS     = 0x38,  // "LOCALS"
        MISSING    = 0x39,  // "MISSING"
        ITERATOR   = 0x3A,  // "ITERATOR"
        MUST       = 0x3B,  // "MUST"
        BE         = 0x3C,  // "BE"
        LOCAL      = 0x3D,  // "LOCAL"
        RANGE      = 0x3E,  // "RANGE"
        RBRACKET   = 0x3F,  // "]"
        
        // Table 2 (bits 6-5 = 10): Additional words (0x40-0x5F)
        EXPRESSION = 0x40,  // "EXPRESSION"
        
        // Tables 2 & 3 available for future expansion (0x41-0x7F)
    }
    
    // Table 0: Common error words (0x00-0x1F)
    const byte[] errorWordsTable0 = {
        5,  ErrorWord.ERROR,      'E', 'R', 'R', 'O', 'R',
        6,  ErrorWord.SYNTAX,     'S', 'Y', 'N', 'T', 'A', 'X',
        4,  ErrorWord.TYPE,       'T', 'Y', 'P', 'E',
        8,  ErrorWord.MISMATCH,   'M', 'I', 'S', 'M', 'A', 'T', 'C', 'H',
        3,  ErrorWord.OUT,        'O', 'U', 'T',
        2,  ErrorWord.OF,         'O', 'F',
        6,  ErrorWord.MEMORY,     'M', 'E', 'M', 'O', 'R', 'Y',
        8,  ErrorWord.DIVISION,   'D', 'I', 'V', 'I', 'S', 'I', 'O', 'N',
        2,  ErrorWord.BY,         'B', 'Y',
        4,  ErrorWord.ZERO,       'Z', 'E', 'R', 'O',
        7,  ErrorWord.WITHOUT,    'W', 'I', 'T', 'H', 'O', 'U', 'T',
        6,  ErrorWord.EXISTS,     'E', 'X', 'I', 'S', 'T', 'S',
        9,  ErrorWord.UNDEFINED,  'U', 'N', 'D', 'E', 'F', 'I', 'N', 'E', 'D',
        10, ErrorWord.IDENTIFIER, 'I', 'D', 'E', 'N', 'T', 'I', 'F', 'I', 'E', 'R',
        7,  ErrorWord.ILLEGAL,    'I', 'L', 'L', 'E', 'G', 'A', 'L',
        7,  ErrorWord.INVALID,    'I', 'N', 'V', 'A', 'L', 'I', 'D',
        3,  ErrorWord.NOT,        'N', 'O', 'T',
        11, ErrorWord.IMPLEMENTED,'I', 'M', 'P', 'L', 'E', 'M', 'E', 'N', 'T', 'E', 'D',
        8,  ErrorWord.INTERNAL,   'I', 'N', 'T', 'E', 'R', 'N', 'A', 'L',
        4,  ErrorWord.ONLY,       'O', 'N', 'L', 'Y',
        2,  ErrorWord.IN,         'I', 'N',
        5,  ErrorWord.DEBUG,      'D', 'E', 'B', 'U', 'G',
        5,  ErrorWord.BUILD,      'B', 'U', 'I', 'L', 'D',
        5,  ErrorWord.TRACE,      'T', 'R', 'A', 'C', 'E',
        8,  ErrorWord.VARIABLE,   'V', 'A', 'R', 'I', 'A', 'B', 'L', 'E',
        4,  ErrorWord.FILE,       'F', 'I', 'L', 'E',
        5,  ErrorWord.FOUND,      'F', 'O', 'U', 'N', 'D',
        7,  ErrorWord.NUMERIC,    'N', 'U', 'M', 'E', 'R', 'I', 'C',
        8,  ErrorWord.OVERFLOW,   'O', 'V', 'E', 'R', 'F', 'L', 'O', 'W',
        3,  ErrorWord.TOO,        'T', 'O', 'O',
        4,  ErrorWord.LONG,       'L', 'O', 'N', 'G',
        3,  ErrorWord.BAD,        'B', 'A', 'D',
        0  // End marker
    };
    
    // Table 2: Additional words (0x40-0x5F)
    const byte[] errorWordsTable2 = {
        10, ErrorWord.EXPRESSION, 'E', 'X', 'P', 'R', 'E', 'S', 'S', 'I', 'O', 'N',
        0  // End marker
    };
    
    // Table 1: Expression/parsing words (0x20-0x3F)
    const byte[] errorWordsTable1 = {
        5,  ErrorWord.INDEX,      'I', 'N', 'D', 'E', 'X',
        8,  ErrorWord.EXPECTED,   'E', 'X', 'P', 'E', 'C', 'T', 'E', 'D',
        10, ErrorWord.ASSIGNMENT, 'A', 'S', 'S', 'I', 'G', 'N', 'M', 'E', 'N', 'T',
        9,  ErrorWord.CHARACTER,  'C', 'H', 'A', 'R', 'A', 'C', 'T', 'E', 'R',
        8,  ErrorWord.OPERATOR,   'O', 'P', 'E', 'R', 'A', 'T', 'O', 'R',
        6,  ErrorWord.BUFFER,     'B', 'U', 'F', 'F', 'E', 'R',
        1,  ErrorWord.RPAREN,     ')',
        1,  ErrorWord.LPAREN,     '(',
        1,  ErrorWord.EQUALS,     '=',
        10, ErrorWord.UNEXPECTED, 'U', 'N', 'E', 'X', 'P', 'E', 'C', 'T', 'E', 'D',
        3,  ErrorWord.END,        'E', 'N', 'D',
        4,  ErrorWord.LINE,       'L', 'I', 'N', 'E',
        7,  ErrorWord.LITERAL,    'L', 'I', 'T', 'E', 'R', 'A', 'L',
        5,  ErrorWord.VALUE,      'V', 'A', 'L', 'U', 'E',
        4,  ErrorWord.MODE,       'M', 'O', 'D', 'E',
        2,  ErrorWord.AT,         'A', 'T',
        7,  ErrorWord.CONSOLE,    'C', 'O', 'N', 'S', 'O', 'L', 'E',
        4,  ErrorWord.HEAP,       'H', 'E', 'A', 'P',
        7,  ErrorWord.CORRUPT,    'C', 'O', 'R', 'R', 'U', 'P', 'T',
        6,  ErrorWord.CANNOT,     'C', 'A', 'N', 'N', 'O', 'T',
        8,  ErrorWord.ROLLBACK,   'R', 'O', 'L', 'L', 'B', 'A', 'C', 'K',
        5,  ErrorWord.BREAK,      'B', 'R', 'E', 'A', 'K',
        2,  ErrorWord.NO,         'N', 'O',
        4,  ErrorWord.MORE,       'M', 'O', 'R', 'E',
        6,  ErrorWord.LOCALS,     'L', 'O', 'C', 'A', 'L', 'S',
        7,  ErrorWord.MISSING,    'M', 'I', 'S', 'S', 'I', 'N', 'G',
        8,  ErrorWord.ITERATOR,   'I', 'T', 'E', 'R', 'A', 'T', 'O', 'R',
        4,  ErrorWord.MUST,       'M', 'U', 'S', 'T',
        2,  ErrorWord.BE,         'B', 'E',
        5,  ErrorWord.LOCAL,      'L', 'O', 'C', 'A', 'L',
        5,  ErrorWord.RANGE,      'R', 'A', 'N', 'G', 'E',
        1,  ErrorWord.RBRACKET,   ']',
        0  // End marker
    };
    
    enum ErrorID
    {
        InternalError = 1, // 0 is no error
        SyntaxError,
        NotImplemented,
        OnlyInDebug,
        OnlyInTrace,
        TypeMismatch,
        FunctionExists,
        ConstantExists,
        VariableExists,
        OutOfMemory,
        FileNotFound,
        NextWithoutFor,
        DivisionByZero,
        NumericOverflow,
        StringTooLong,
        BadIndex,
        UndefinedIdentifier,
        ConstantExpected,
        ConstantExpressionExpected,
        ExpectedThen,
        IllegalIdentifier,
        IllegalAssignment,
        IllegalCharacter,
        InvalidOperator,
        BufferOverflow,
        ExpectedRightParen,
        ExpectedLeftParen,
        ExpectedEqual,
        UnexpectedEOL,
        ExpectedExpression,
        InvalidBitValue,
        IllegalInFunctionMode,
        OnlyAtConsole,
        HeapCorrupt,
        CannotRollback,
        Break,
        LateDeclaration,
        MissingNext,
        NextMismatch,
        ForIteratorLocal,
        RangeError,
        ExpectedRightBracket,
    }
    
    const byte[] errorMessages = {
        2, ErrorID.InternalError,              ErrorWord.INTERNAL, ErrorWord.ERROR,
        2, ErrorID.SyntaxError,                ErrorWord.SYNTAX, ErrorWord.ERROR,
        2, ErrorID.NotImplemented,             ErrorWord.NOT, ErrorWord.IMPLEMENTED,
        4, ErrorID.OnlyInDebug,                ErrorWord.ONLY, ErrorWord.IN, ErrorWord.DEBUG, ErrorWord.BUILD,
        4, ErrorID.OnlyInTrace,                ErrorWord.ONLY, ErrorWord.IN, ErrorWord.TRACE, ErrorWord.BUILD,
        2, ErrorID.TypeMismatch,               ErrorWord.TYPE, ErrorWord.MISMATCH,
        2, ErrorID.FunctionExists,             Token.FUNC, ErrorWord.EXISTS,
        2, ErrorID.ConstantExists,             Token.CONST, ErrorWord.EXISTS,
        2, ErrorID.VariableExists,             ErrorWord.VARIABLE, ErrorWord.EXISTS,
        3, ErrorID.OutOfMemory,                ErrorWord.OUT, ErrorWord.OF, ErrorWord.MEMORY,
        3, ErrorID.FileNotFound,               ErrorWord.FILE, ErrorWord.NOT, ErrorWord.FOUND,
        3, ErrorID.NextWithoutFor,             Token.NEXT, ErrorWord.WITHOUT, Token.FOR,
        3, ErrorID.DivisionByZero,             ErrorWord.DIVISION, ErrorWord.BY, ErrorWord.ZERO,
        2, ErrorID.NumericOverflow,            ErrorWord.NUMERIC, ErrorWord.OVERFLOW,
        3, ErrorID.StringTooLong,              Token.STRING, ErrorWord.TOO, ErrorWord.LONG,
        2, ErrorID.BadIndex,                   ErrorWord.BAD, ErrorWord.INDEX,
        2, ErrorID.UndefinedIdentifier,        ErrorWord.UNDEFINED, ErrorWord.IDENTIFIER,
        2, ErrorID.ConstantExpected,           Token.CONST, ErrorWord.EXPECTED,
        3, ErrorID.ConstantExpressionExpected, Token.CONST, ErrorWord.EXPRESSION, ErrorWord.EXPECTED,
        2, ErrorID.IllegalIdentifier,         ErrorWord.ILLEGAL, ErrorWord.IDENTIFIER,
        2, ErrorID.IllegalAssignment,         ErrorWord.ILLEGAL, ErrorWord.ASSIGNMENT,
        2, ErrorID.IllegalCharacter,          ErrorWord.ILLEGAL, ErrorWord.CHARACTER,
        2, ErrorID.InvalidOperator,            ErrorWord.INVALID, ErrorWord.OPERATOR,
        2, ErrorID.BufferOverflow,             ErrorWord.BUFFER, ErrorWord.OVERFLOW,
        2, ErrorID.ExpectedRightParen,        ErrorWord.RPAREN, ErrorWord.EXPECTED,
        2, ErrorID.ExpectedLeftParen,         ErrorWord.LPAREN, ErrorWord.EXPECTED,
        2, ErrorID.ExpectedEqual,             ErrorWord.EQUALS, ErrorWord.EXPECTED,
        6, ErrorID.UnexpectedEOL,             ErrorWord.UNEXPECTED, ErrorWord.END, ErrorWord.OF, ErrorWord.LINE, ErrorWord.IN, ErrorWord.LITERAL,
        2, ErrorID.ExpectedExpression,        ErrorWord.EXPRESSION, ErrorWord.EXPECTED,
        3, ErrorID.InvalidBitValue,            ErrorWord.INVALID, Token.BIT, ErrorWord.VALUE,
        4, ErrorID.IllegalInFunctionMode,     ErrorWord.ILLEGAL, ErrorWord.IN, Token.FUNC, ErrorWord.MODE,
        3, ErrorID.OnlyAtConsole,             ErrorWord.ONLY, ErrorWord.AT, ErrorWord.CONSOLE,
        2, ErrorID.HeapCorrupt,               ErrorWord.HEAP, ErrorWord.CORRUPT,
        2, ErrorID.CannotRollback,            ErrorWord.CANNOT, ErrorWord.ROLLBACK,
        1, ErrorID.Break,                     ErrorWord.BREAK,
        3, ErrorID.LateDeclaration,           ErrorWord.NO, ErrorWord.MORE, ErrorWord.LOCALS,
        2, ErrorID.MissingNext,               ErrorWord.MISSING, Token.NEXT,
        2, ErrorID.NextMismatch,              Token.NEXT, ErrorWord.MISMATCH,
        5, ErrorID.ForIteratorLocal,          Token.FOR, ErrorWord.ITERATOR, ErrorWord.MUST, ErrorWord.BE, ErrorWord.LOCAL,
        4, ErrorID.RangeError,                ErrorWord.VALUE, ErrorWord.OUT, ErrorWord.OF, ErrorWord.RANGE,
        2, ErrorID.ExpectedRightBracket,      ErrorWord.RBRACKET, ErrorWord.EXPECTED,
        2, ErrorID.ExpectedThen,              Token.THEN, ErrorWord.EXPECTED,
        
        0  // End marker
    };
    
    // Print error word corresponding to word ID
    // Input: A = word ID (0x00-0x7F for error words, 0x80+ for keywords)
    // Output: Word printed to serial, C set if found, NC if not found
    PrintWord()
    {
        PHX
        PHY
        TAX
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.ACCL
        PHA
        
        TXA
        if (MI)
        {
            // Input: A = token value (e.g., Token.CONST, Token.INT, etc.)
            Tokens.PrintKeyword();
        }
        else
        {
            AND #0x60   // Extract bits 6-5 (table selector)
            switch (A)
            {
                case 0x00:  // Table 0 (0x00-0x1F)
                {
                    LDA #(errorWordsTable0 % 256)
                    STA ZP.IDYL
                    LDA #(errorWordsTable0 / 256)
                    STA ZP.IDYH
                    TXA
                    STA ZP.ACCL
                    Tokens.PrintKeywordFromTable();// Input: ZP.ACCL = target token value, ZP.IDY = table address
                }
                case 0x20:  // Table 1 (0x20-0x3F)
                {
                    LDA #(errorWordsTable1 % 256)
                    STA ZP.IDYL
                    LDA #(errorWordsTable1 / 256)
                    STA ZP.IDYH
                    TXA
                    STA ZP.ACCL
                    Tokens.PrintKeywordFromTable();// Input: ZP.ACCL = target token value, ZP.IDY = table address
                }
                case 0x40:  // Table 2 (0x40-0x5F)
                {
                    LDA #(errorWordsTable2 % 256)
                    STA ZP.IDYL
                    LDA #(errorWordsTable2 / 256)
                    STA ZP.IDYH
                    TXA
                    STA ZP.ACCL
                    Tokens.PrintKeywordFromTable();// Input: ZP.ACCL = target token value, ZP.IDY = table address
                }
                default:
                {
                    CLC  // Tables 2 & 3 not implemented yet
                }
            }   
        }
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLY
        PLX
        
    }
    
    // Input A = error ID
    PrintError()
    {
        STA ZP.ACCL  // Store target error ID
                
        // Set up pointer to errorMessages table
        LDA #(errorMessages % 256)
        STA ZP.IDYL
        LDA #(errorMessages / 256)
        STA ZP.IDYH
        
        LDY #0  // Start at beginning of table
        loop
        {
            LDA [ZP.IDY], Y    // Get word count for this message
            if (Z) { break; }  // End of table - not found
            
            TAX                // X = word count (keep it in X, don't store in ZP!)
            INY
            LDA [ZP.IDY], Y    // Get error ID
            CMP ZP.ACCL        // Compare with target
            if (Z)
            {
                // Found it! Print the words
                INY  // Move to first word ID
                loop
                {
                    CPX #0
                    if (Z) { break; }
                    
                    LDA [ZP.IDY], Y    // Get word ID
                    PrintWord(); // word in A
                    INY
                    DEX
                    
                    CPX #0
                    if (Z) { break; }  // Don't add space after last word
                    
                    LDA #' '
                    Serial.WriteChar();
                }
                break;  // Done printing
            }
            
            // Skip to next message: advance Y by word count + 1 (for error ID)
            INY  // Skip the error ID byte first
            loop
            {
                CPX #0
                if (Z) { break; }
                INY
                DEX
            }
        }
        
                
         CLC  // Indicate error occurred
    }
    
    SyntaxError() 
    { 
        LDA #ErrorID.SyntaxError
        STA ZP.LastError
        CLC
    }

    InternalError() 
    { 
        LDA #ErrorID.InternalError
        STA ZP.LastError
        CLC
    }

    TODO() 
    { 
        LDA #ErrorID.NotImplemented
        STA ZP.LastError
        CLC
    }

    ExpectedRightBracket()
    {
        LDA #ErrorID.ExpectedRightBracket
        STA ZP.LastError
        CLC
    }

    RangeError()
    {
        LDA #ErrorID.RangeError
        STA ZP.LastError
        CLC
    }

    Break()
    {
        LDA #ErrorID.Break
        STA ZP.LastError
        CLC
    }

    LateDeclaration()
    {
        LDA #ErrorID.LateDeclaration
        STA ZP.LastError
        CLC
    }

    ForIteratorLocal()
    {
        LDA #ErrorID.ForIteratorLocal
        STA ZP.LastError
        CLC
    }

    CannotRollback()
    {
        LDA #ErrorID.CannotRollback
        STA ZP.LastError
        CLC
    }

    HeapCorruptError()
    {
        LDA #ErrorID.HeapCorrupt
        STA ZP.LastError
        CLC
    }

    OnlyInDebug() 
    { 
        LDA #ErrorID.OnlyInDebug
        STA ZP.LastError
        CLC
    }

    OnlyInTrace() 
    { 
        LDA #ErrorID.OnlyInTrace
        STA ZP.LastError
        CLC
    }

    OnlyAtConsole() 
    { 
        LDA #ErrorID.OnlyAtConsole
        STA ZP.LastError
        CLC
    }

    TypeMismatch() 
    { 
        LDA #ErrorID.TypeMismatch
        STA ZP.LastError
        CLC
    }

    FunctionExists() 
    { 
        LDA #ErrorID.FunctionExists
        STA ZP.LastError
        CLC
    }

    ConstantExists() 
    { 
        LDA #ErrorID.ConstantExists
        STA ZP.LastError
        CLC
    }

    VariableExists() 
    { 
        LDA #ErrorID.VariableExists
        STA ZP.LastError
        CLC
    }

    OutOfMemory() 
    { 
        LDA #ErrorID.OutOfMemory
        STA ZP.LastError
        CLC
    }

    FileNotFound() 
    { 
        LDA #ErrorID.FileNotFound
        STA ZP.LastError
        CLC
    }

    NextWithoutFor() 
    { 
        LDA #ErrorID.NextWithoutFor
        STA ZP.LastError
        CLC
    }

    MissingNext() 
    { 
        LDA #ErrorID.MissingNext
        STA ZP.LastError
        CLC
    }

    NextMismatch() 
    { 
        LDA #ErrorID.NextMismatch
        STA ZP.LastError
        CLC
    }

    DivisionByZero() 
    { 
        LDA #ErrorID.DivisionByZero
        STA ZP.LastError
        CLC
    }

    NumericOverflow() 
    { 
        LDA #ErrorID.NumericOverflow
        STA ZP.LastError
        CLC
    }

    StringTooLong() 
    { 
        LDA #ErrorID.StringTooLong
        STA ZP.LastError
        CLC
    }

    BadIndex() 
    { 
        LDA #ErrorID.BadIndex
        STA ZP.LastError
        CLC
    }

    UndefinedIdentifier() 
    { 
        LDA #ErrorID.UndefinedIdentifier
        STA ZP.LastError
        CLC
    }

    ConstantExpected() 
    { 
        LDA #ErrorID.ConstantExpected
        STA ZP.LastError
        CLC
    }

    ConstantExpressionExpected() 
    { 
        LDA #ErrorID.ConstantExpressionExpected
        STA ZP.LastError
        CLC
    }

    IllegalCharacter()
    { 
        LDA #ErrorID.IllegalCharacter
        STA ZP.LastError
        CLC
    }

    IllegalIdentifier() 
    { 
        LDA #ErrorID.IllegalIdentifier
        STA ZP.LastError
        CLC
    }

    IllegalAssignment() 
    { 
        LDA #ErrorID.IllegalAssignment
        STA ZP.LastError
        CLC
    }

    InvalidOperator() 
    { 
        LDA #ErrorID.InvalidOperator
        STA ZP.LastError
        CLC
    }

    BufferOverflow() 
    { 
        LDA #ErrorID.BufferOverflow
        STA ZP.LastError
        CLC
    }

    ExpectedEqual() 
    { 
        LDA #ErrorID.ExpectedEqual
        STA ZP.LastError
        CLC
    }

    ExpectedRightParen() 
    { 
        LDA #ErrorID.ExpectedRightParen
        STA ZP.LastError
        CLC
    }

    ExpectedLeftParen() 
    { 
        LDA #ErrorID.ExpectedLeftParen
        STA ZP.LastError
        CLC
    }

    UnexpectedEOL() 
    { 
        LDA #ErrorID.UnexpectedEOL
        STA ZP.LastError
        CLC
    }

    ExpectedExpression() 
    { 
        LDA #ErrorID.ExpectedExpression
        STA ZP.LastError
        CLC
    }

    InvalidBitValue() 
    { 
        LDA #ErrorID.InvalidBitValue
        STA ZP.LastError
        CLC
    }

    IllegalInFunctionMode() 
    { 
        LDA #ErrorID.IllegalInFunctionMode
        STA ZP.LastError
        CLC
    }
    ExpectedThen()
    {
        LDA #ErrorID.ExpectedThen
        STA ZP.LastError
        CLC
    }
    
    // Clear error state
    // Input: None
    // Output: ZP.LastError cleared (set to 0x0000)
    ClearError()
    {
        STZ ZP.LastError
        RMB0 ZP.SerialBreakFlag   // Clear the BREAK flag
        States.SetSuccess();
    }
    
    // Check if error has occurred, or if State.Failure
    // Input: None
    // Output: C set if ok, NC if not ok (error occurred)
    // Modifies: Processor flags only
    CheckError()
    {
        PHA
        
        // Returns C if no error, NC if error occurred
        LDA ZP.LastError
        if (Z)
        {
            States.IsFailure();
            if (C)
            {
#if defined(DEBUG)
                LDA #'F' Debug.COut(); LDA #'!' Debug.COut();
#endif
                CLC  // Failure
            }
            else
            {
                SEC  // No error or Failure
            }
        }
        else
        {
            States.IsSuccess(); // don't alter Exiting or Return
            if (C)
            {
                States.SetFailure();
            }
#if defined(DEBUG)
            IsTracing();
            if (C)
            {
                LDA #'E' Debug.COut(); LDA #'!' Debug.COut();
            }
#endif
            CLC  // Error occurred
        }
        PLA
    }
    
    // Check if error has occurred
    // Input: None
    // Output: C set if ok, NC if not ok (error occurred)
    // Modifies: Processor flags only
    CheckErrorAndStatus()
    {
        PHA
        CheckError(); // C if ok, NC if not ok (error)
        if (C)
        {
            // LastError not set, check SystemState
            States.CanContinue(); // C if all good, NC if error or exit
        }
        PLA
    }
    
    // Check for error and print it if found
    // Input: None
    // Output: C set if no error, NC if error was printed
    //         Error cleared after printing
    // Modifies: ZP.LastError (cleared if error was printed)
    CheckAndPrint()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register  
        PHY  // Preserve Y register
        
        // Returns C if no error, NC if error was printed
        CheckError();
        if (C) 
        { 
            // Restore registers
            PLY
            PLX
            PLA
            return;  // No error
        }
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Print the error message
        LDA #'?'
        Serial.WriteChar(); // '?' prefix
        LDA ZP.LastError
        PrintError();
#if defined(DEBUG) || defined(TRACE)
        // 6502 PC
        LDA #' '
        Serial.WriteChar();
        LDA #'('
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        LDA ZP.EmulatorPCH
        Serial.HexOut();
        LDA ZP.EmulatorPCL
        Serial.HexOut();
        LDA #')'
        Serial.WriteChar();
#endif        
        LDA #'\n'
        Serial.WriteChar(); // '\n' suffix
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        Error.ClearError();  // Clear error state before next command
        States.SetSuccess();  // Reset state for clean start
        
        // Restore registers
        PLY
        PLX
        PLA
        
        CLC  // Error was found and printed
    }
    
    // Check if the current error in ZP.LastError is fatal
    // Input: None (reads ZP.LastErrorL/H)  
    // Output: C set if fatal, NC if not fatal
    // Preserves: A, X, Y
    // Munts: Processor flags only
    IsFatal()
    {
        PHA
        
        loop // Single exit point
        {
            // Compare against fatal error IDs
            LDA ZP.LastError
            
            // Check for NotImplemented
            CMP #ErrorID.NotImplemented
            if (Z) { SEC break; } // Fatal
            
            // Check for InternalError
            CMP #ErrorID.InternalError
            if (Z) { SEC break; } // Fatal
            
            // Check for OutOfMemory
            CMP #ErrorID.OutOfMemory
            if (Z) { SEC break; } // Fatal
            
            // Check for BufferOverflow
            CMP #ErrorID.BufferOverflow
            if (Z) { SEC break; } // Fatal
            
            // Check for HeapCorrupt
            CMP #ErrorID.HeapCorrupt
            if (Z) { SEC break; } // Fatal
            
            // Not a fatal error
            CLC
            break;
        } // Single exit loop
        
        PLA
    }
}
