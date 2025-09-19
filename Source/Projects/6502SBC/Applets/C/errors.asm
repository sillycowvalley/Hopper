unit Errors
{
    enum Error
    {
        None = 0,
        SourceNotFound = 0x01,
        SourceLoadingError = 0x02,
        OutOfMemory = 0x03,
        UnexpectedCharacter = 0x04,
        UnterminatedComment = 0x05,
        UnterminatedString = 0x06,
        TokenTooLong = 0x07,
        StringTooLong = 0x08,
        SyntaxError = 0x09,
        UnexpectedFailure = 0x0A,
        
        NoEntryPoint = 0x0B,
        NotImplemented = 0x0C,
        
        TooFewArguments = 0x0D,
        UndefinedIdentifier = 0x0E,
        
        TypeExpected =0x0F,
        VoidFunction = 0x10,
        ExpressionExpected = 0x11,
        UnsupportedFormatter = 0x12,
        UnsupportedLValue = 0x13,
        UnsupportedLocalScope = 0x14,
        BreakOutsideLoop = 0x15,
        ContinueOutsideForLoop = 0x16, // only works in While for now
        
        FilenameTooLong = 0x17,
        FileSaveError = 0x18,
    }
    
    const string msgExpected  = "Expected ";
    const string msgUnexpected  = "Unexpected ";
    const string msgError     = "Error: 0x";
    const string expectVoid   = "void";
    const string expectIdent  = "identifier";
    const string msgLine      = "Line ";
    const string msgColon     = ": ";
    
    printErrorLine()
    {
        LDA #(msgLine % 256)
        STA ZP.STRL
        LDA #(msgLine / 256)
        STA ZP.STRH
        Print.String();
        
        Shared.MoveAccToTop();
        Long.Print();
        
        LDA #(msgColon % 256)
        STA ZP.STRL
        LDA #(msgColon / 256)
        STA ZP.STRH
        Print.String();
    }
       
    Show()
    {
        PHA
        
        LDA #(msgError % 256)
        STA ZP.STRL
        LDA #(msgError / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        Print.Hex(); // error #
        Print.NewLine();
        CLC
    }
    
    ShowLine()
    {
        PHA
        Lexer.GetLineNumber(); // -> ACC
        printErrorLine();
        
        PLA
        Show();
    }
    
    // Show error with specific line number
    // Input: A = error code, line numberfrom AST node in IDX
    ShowIDX()
    {
        PHY
        PHA  // Save error code
        
        LDY #AST.iLineNumber
        LDA [ZP.IDX], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDX], Y
        STA ZP.ACCH
        
        printErrorLine();
        
        PLA
        Show();
        
        PLY
    }
    
    // Show error with specific line number
    // Input: A = error code, line numberfrom AST node in IDY
    ShowIDY()
    {
        PHY
        PHA  // Save error code
        
        LDY #AST.iLineNumber
        LDA [ZP.IDY], Y
        STA ZP.ACCL
        INY
        LDA [ZP.IDY], Y
        STA ZP.ACCH
        
        printErrorLine();
        
        PLA
        Show();
        
        PLY
    }
    OutOfMemory()
    {
        LDA # Error.OutOfMemory
        Show();
        CLC
    }
    
    // Helper: print "Expected X" error
    // Input: A = token that was expected
    Expected()
    {
        PHA
        
        Lexer.GetLineNumber(); // -> ACC
        printErrorLine();
               
        LDA #(msgExpected % 256)
        STA ZP.STRL
        LDA #(msgExpected / 256)
        STA ZP.STRH
        Print.String();
        PLA
        
        // A contains the token that was expected
        loop
        {
            switch (A)
            {
                case Token.Void:
                {
                    LDA #(expectVoid % 256)
                    STA ZP.STRL
                    LDA #(expectVoid / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.Identifier:
                {
                    LDA #(expectIdent % 256)
                    STA ZP.STRL
                    LDA #(expectIdent / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.LeftParen:
                {
                    LDA #'('
                }
                case Token.RightParen:
                {
                    LDA #')'
                }
                case Token.LeftBrace:
                {
                    LDA #'{'
                }
                case Token.RightBrace:
                {
                    LDA #'}'
                }
                case Token.Semicolon:
                {
                    LDA #';'
                }
                case Token.Star:
                {
                    LDA #'*'
                }
                default:
                {
                    LDA #'?'
                }
            }
            Print.Char();
            break;
        } // loop
        Print.NewLine();
        CLC
    }
    
    // Helper: print "Unexpected X" error
    // Input: A = token that was expected
    Unexpected()
    {
        PHA
        
        Lexer.GetLineNumber(); // -> ACC
        printErrorLine();
               
        LDA #(msgUnexpected % 256)
        STA ZP.STRL
        LDA #(msgUnexpected / 256)
        STA ZP.STRH
        Print.String();
        PLA
        
        // A contains the token that was expected
        loop
        {
            switch (A)
            {
                case Token.Void:
                {
                    LDA #(expectVoid % 256)
                    STA ZP.STRL
                    LDA #(expectVoid / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.Identifier:
                {
                    LDA #(expectIdent % 256)
                    STA ZP.STRL
                    LDA #(expectIdent / 256)
                    STA ZP.STRH
                    Print.String();
                    break;
                }
                case Token.LeftParen:
                {
                    LDA #'('
                }
                case Token.RightParen:
                {
                    LDA #')'
                }
                case Token.LeftBrace:
                {
                    LDA #'{'
                }
                case Token.RightBrace:
                {
                    LDA #'}'
                }
                case Token.Semicolon:
                {
                    LDA #';'
                }
                default:
                {
                    LDA #'?'
                }
            }
            Print.Char();
            break;
        } // loop
        Print.NewLine();
        CLC
    }
}
