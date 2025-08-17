unit Error // Error.asm
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
    
    enum Error
    {
        InternalError,
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
        2, Error.InternalError,              ErrorWord.INTERNAL, ErrorWord.ERROR,
        2, Error.SyntaxError,                ErrorWord.SYNTAX, ErrorWord.ERROR,
        2, Error.NotImplemented,             ErrorWord.NOT, ErrorWord.IMPLEMENTED,
        4, Error.OnlyInDebug,                ErrorWord.ONLY, ErrorWord.IN, ErrorWord.DEBUG, ErrorWord.BUILD,
        4, Error.OnlyInTrace,                ErrorWord.ONLY, ErrorWord.IN, ErrorWord.TRACE, ErrorWord.BUILD,
        2, Error.TypeMismatch,               ErrorWord.TYPE, ErrorWord.MISMATCH,
        2, Error.FunctionExists,             Token.FUNC, ErrorWord.EXISTS,
        2, Error.ConstantExists,             Token.CONST, ErrorWord.EXISTS,
        2, Error.VariableExists,             ErrorWord.VARIABLE, ErrorWord.EXISTS,
        3, Error.OutOfMemory,                ErrorWord.OUT, ErrorWord.OF, ErrorWord.MEMORY,
        3, Error.FileNotFound,               ErrorWord.FILE, ErrorWord.NOT, ErrorWord.FOUND,
        3, Error.NextWithoutFor,             Token.NEXT, ErrorWord.WITHOUT, Token.FOR,
        3, Error.DivisionByZero,             ErrorWord.DIVISION, ErrorWord.BY, ErrorWord.ZERO,
        2, Error.NumericOverflow,            ErrorWord.NUMERIC, ErrorWord.OVERFLOW,
        3, Error.StringTooLong,              Token.STRING, ErrorWord.TOO, ErrorWord.LONG,
        2, Error.BadIndex,                   ErrorWord.BAD, ErrorWord.INDEX,
        2, Error.UndefinedIdentifier,        ErrorWord.UNDEFINED, ErrorWord.IDENTIFIER,
        2, Error.ConstantExpected,           Token.CONST, ErrorWord.EXPECTED,
        3, Error.ConstantExpressionExpected, Token.CONST, ErrorWord.EXPRESSION, ErrorWord.EXPECTED,
        2, Error.IllegalIdentifier,         ErrorWord.ILLEGAL, ErrorWord.IDENTIFIER,
        2, Error.IllegalAssignment,         ErrorWord.ILLEGAL, ErrorWord.ASSIGNMENT,
        2, Error.IllegalCharacter,          ErrorWord.ILLEGAL, ErrorWord.CHARACTER,
        2, Error.InvalidOperator,            ErrorWord.INVALID, ErrorWord.OPERATOR,
        2, Error.BufferOverflow,             ErrorWord.BUFFER, ErrorWord.OVERFLOW,
        2, Error.ExpectedRightParen,        ErrorWord.RPAREN, ErrorWord.EXPECTED,
        2, Error.ExpectedLeftParen,         ErrorWord.LPAREN, ErrorWord.EXPECTED,
        2, Error.ExpectedEqual,             ErrorWord.EQUALS, ErrorWord.EXPECTED,
        6, Error.UnexpectedEOL,             ErrorWord.UNEXPECTED, ErrorWord.END, ErrorWord.OF, ErrorWord.LINE, ErrorWord.IN, ErrorWord.LITERAL,
        2, Error.ExpectedExpression,        ErrorWord.EXPRESSION, ErrorWord.EXPECTED,
        3, Error.InvalidBitValue,            ErrorWord.INVALID, Token.BIT, ErrorWord.VALUE,
        4, Error.IllegalInFunctionMode,     ErrorWord.ILLEGAL, ErrorWord.IN, Token.FUNC, ErrorWord.MODE,
        3, Error.OnlyAtConsole,             ErrorWord.ONLY, ErrorWord.AT, ErrorWord.CONSOLE,
        2, Error.HeapCorrupt,               ErrorWord.HEAP, ErrorWord.CORRUPT,
        2, Error.CannotRollback,            ErrorWord.CANNOT, ErrorWord.ROLLBACK,
        1, Error.Break,                     ErrorWord.BREAK,
        3, Error.LateDeclaration,           ErrorWord.NO, ErrorWord.MORE, ErrorWord.LOCALS,
        2, Error.MissingNext,               ErrorWord.MISSING, Token.NEXT,
        2, Error.NextMismatch,              Token.NEXT, ErrorWord.MISMATCH,
        5, Error.ForIteratorLocal,          Token.FOR, ErrorWord.ITERATOR, ErrorWord.MUST, ErrorWord.BE, ErrorWord.LOCAL,
        4, Error.RangeError,                ErrorWord.VALUE, ErrorWord.OUT, ErrorWord.OF, ErrorWord.RANGE,
        2, Error.ExpectedRightBracket,      ErrorWord.RBRACKET, ErrorWord.EXPECTED,
        
        0  // End marker
    };
    
    // Print error word corresponding to word ID
    // Input: X = word ID (0x00-0x7F for error words, 0x80+ for keywords)
    // Output: Word printed to serial, C set if found, NC if not found
    PrintWord()
    {
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
    }
    
    // Input A = error ID
    PrintError()
    {
        PHX
        PHY
        
        LDA ZP.IDYL
        PHA
        LDA ZP.IDYH
        PHA
        LDA ZP.ACCL
        PHA
        
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
            
            TAX                // X = word count
            INY
            LDA [ZP.IDY], Y    // Get error ID
            CMP ZP.ACCL        // Compare with target
            if (Z)
            {
                // Found it! Print the words
                INY
                loop
                {
                    CPX #0
                    if (Z) { break; }  // Done with all words
                    
                    LDA [ZP.IDY], Y    // Get next word ID
                    PHA                // Save word ID
                    TXA                // Save word count 
                    PHA
                    
                    PLA                // Get word ID back
                    TAX                // Put word ID in X for PrintWord
                    PrintWord();
                    
                    PLA                // Get word count back
                    TAX
                    INY
                    DEX                // Decrement word count
                    
                    CPX #0
                    if (Z) { break; }  // Don't add space after last word
                    
                    LDA #' '
                    Serial.WriteChar(); // Add space between words
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
        
        PLA
        STA ZP.ACCL
        PLA
        STA ZP.IDYH
        PLA
        STA ZP.IDYL
        
        PLY
        PLX
        
        CLC  // Indicate error occurred
    }
            
    SyntaxError() 
    { 
        LDA # Error.SyntaxError
        PrintError();
    }
    
    InternalError() 
    { 
        LDA # Error.InternalError
        PrintError();
    }
    
    TODO() 
    { 
        LDA # Error.NotImplemented
        PrintError();
    }
    
    ExpectedRightBracket()
    {
        LDA # Error.ExpectedRightBracket
        PrintError();
    }
    
    RangeError()
    {
        LDA # Error.RangeError
        PrintError();
    }
    
    Break()
    {
        LDA # Error.Break
        PrintError();
    }
    
    LateDeclaration()
    {
        LDA # Error.LateDeclaration
        PrintError();
    }
    
    ForIteratorLocal()
    {
        LDA # Error.ForIteratorLocal
        PrintError();
    }
    
    CannotRollback()
    {
        LDA # Error.CannotRollback
        PrintError();
    }
    
    HeapCorruptError()
    {
        LDA # Error.HeapCorrupt
        PrintError();
    }
    
    OnlyInDebug() 
    { 
        LDA # Error.OnlyInDebug
        PrintError();
    }
    
    OnlyInTrace() 
    { 
        LDA # Error.OnlyInTrace
        PrintError();
    }
    
    OnlyAtConsole() 
    { 
        LDA # Error.OnlyAtConsole
        PrintError();
    }
    
    TypeMismatch() 
    { 
        LDA # Error.TypeMismatch
        PrintError();
    }
    
    FunctionExists() 
    { 
        LDA # Error.FunctionExists
        PrintError();
    }
    
    ConstantExists() 
    { 
        LDA # Error.ConstantExists
        PrintError();
    }
    
    VariableExists() 
    { 
        LDA # Error.VariableExists
        PrintError();
    }
    
    OutOfMemory() 
    { 
        LDA # Error.OutOfMemory
        PrintError();
    }
    
    FileNotFound() 
    { 
        LDA # Error.FileNotFound
        PrintError();
    }
    
    NextWithoutFor() 
    { 
        LDA # Error.NextWithoutFor
        PrintError();
    }
    
    MissingNext() 
    { 
        LDA # Error.MissingNext
        PrintError();
    }
    
    NextMismatch() 
    { 
        LDA # Error.NextMismatch
        PrintError();
    }
    
    DivisionByZero() 
    { 
        LDA # Error.DivisionByZero
        PrintError();
    }
    
    NumericOverflow() 
    { 
        LDA # Error.NumericOverflow
        PrintError();
    }
    
    StringTooLong() 
    { 
        LDA # Error.StringTooLong
        PrintError();
    }
    
    BadIndex() 
    { 
        LDA # Error.BadIndex
        PrintError();
    }
    
    UndefinedIdentifier() 
    { 
        LDA # Error.UndefinedIdentifier
        PrintError();
    }
    
    ConstantExpected() 
    { 
        LDA # Error.ConstantExpected
        PrintError();
    }
    
    ConstantExpressionExpected() 
    { 
        LDA # Error.ConstantExpressionExpected
        PrintError();
    }
    
    IllegalCharacter()
    { 
        LDA # Error.IllegalCharacter
        PrintError();
    }
    
    IllegalIdentifier() 
    { 
        LDA # Error.IllegalIdentifier
        PrintError();
    }
    
    IllegalAssignment() 
    { 
        LDA # Error.IllegalAssignment
        PrintError();
    }
    
    InvalidOperator() 
    { 
        LDA # Error.InvalidOperator
        PrintError();
    }
    
    BufferOverflow() 
    { 
        LDA # Error.BufferOverflow
        PrintError();
    }
    
    ExpectedEqual() 
    { 
        LDA # Error.ExpectedEqual
        PrintError();
    }
    
    ExpectedRightParen() 
    { 
        LDA # Error.ExpectedRightParen
        PrintError();
    }
    
    ExpectedLeftParen() 
    { 
        LDA # Error.ExpectedLeftParen
        PrintError();
    }
    
    UnexpectedEOL() 
    { 
        LDA # Error.UnexpectedEOL
        PrintError();
    }
    
    ExpectedExpression() 
    { 
        LDA # Error.ExpectedExpression
        PrintError();
    }
    
    InvalidBitValue() 
    { 
        LDA # Error.InvalidBitValue
        PrintError();
    }
    
    IllegalInFunctionMode() 
    { 
        LDA # Error.IllegalInFunctionMode
        PrintError();
    }
    
    // Clear error state
    // Input: None
    // Output: ZP.LastError cleared (set to 0x0000)
    ClearError()
    {
        STZ ZP.LastError
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
            LDA ZP.LastErrorL
            
            // Check for NotImplemented
            CMP #Error.NotImplemented
            if (Z) { SEC break; } // Fatal
            
            // Check for InternalError
            CMP #Error.InternalError
            if (Z) { SEC break; } // Fatal
            
            // Check for OutOfMemory
            CMP #Error.OutOfMemory
            if (Z) { SEC break; } // Fatal
            
            // Check for BufferOverflow
            CMP #Error.BufferOverflow
            if (Z) { SEC break; } // Fatal
            
            // Check for HeapCorrupt
            CMP #Error.HeapCorrupt
            if (Z) { SEC break; } // Fatal
            
            // Not a fatal error
            CLC
            break;
        } // Single exit loop
        
        PLA
    }
}
