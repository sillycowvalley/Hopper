program CC
{
    #define CPU_65C02S
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    
    uses "Errors"
    uses "Tokens"
    uses "Lexer"
    
    const string messageCompiling = "Compiling ";
    
    const byte ccSlots = 0x60;
    const byte sourceName  = ccSlots+1;
    const byte sourceNameL = ccSlots+1;
    const byte sourceNameH = ccSlots+2;
    
    Hopper()
    {
        Args.HasFilename();
        if (NC)
        {
            LDA # Error.SourceNotFound
            Errors.ShowError();
            return;
        }
        Args.GetFilename();
        LDA ZP.STRL
        STA sourceNameL
        LDA ZP.STRH
        STA sourceNameH
        
        LDA # FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA # Error.SourceLoadingError
            Errors.ShowError();
            return;     
        }
        
        LDA # (messageCompiling % 256)
        STA ZP.STRL
        LDA # (messageCompiling / 256)
        STA ZP.STRH
        Print.String();
        
        LDA sourceNameL
        STA ZP.STRL
        LDA sourceNameH
        STA ZP.STRH
        Print.String();
        
        Lexer.Initialize();
        
        loop
        {
            Lexer.NextToken();  // Returns token type in A
            if (Z) { break; }   // EOF is 0
            
            LDA Lexer.TokenType
            Print.Hex();
            Print.Space();
        }   
        
        Lexer.Dispose();
        Print.NewLine();
    }
}
