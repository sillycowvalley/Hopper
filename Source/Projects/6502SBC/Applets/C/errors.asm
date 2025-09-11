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
        
    }
    
    const string errorMessage = "Error: ";
    
    ShowError()
    {
        PHA 
        
        LDA # (errorMessage % 256)
        STA ZP.STRL
        LDA # (errorMessage / 256)
        STA ZP.STRH
        Print.String();
        
        PLA
        LDX # ZP.TOP
        Shared.LoadByte();
        Long.Print();
        Print.NewLine();
    }
}
