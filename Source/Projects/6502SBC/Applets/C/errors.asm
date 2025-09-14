unit Errors
{
    enum Error
    {
        None = 0,
        SourceNotFound,
        SourceLoadingError,
        OutOfMemory,
        UnexpectedCharacter, // 4
        UnterminatedComment,
        UnterminatedString,
        TokenTooLong,
        StringTooLong,
        SyntaxError,
        UnexpectedFailure, // consume() failed?
        
        NoEntryPoint,
        NotImplemented,    // 0x0C
        
        FilenameTooLong,
        FileSaveError,
    }
    
    const string msgExpected  = "Expected ";
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
        
        Lexer.GetLineNumber(); // -> ACC
        printErrorLine();
        
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
        
        // Print error code
        LDA #(msgError % 256)
        STA ZP.STRL
        LDA #(msgError / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        Print.Hex(); // error #
        Print.NewLine();
        CLC
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
        
        // Print error code
        LDA #(msgError % 256)
        STA ZP.STRL
        LDA #(msgError / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        Print.Hex(); // error #
        Print.NewLine();
        CLC
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
