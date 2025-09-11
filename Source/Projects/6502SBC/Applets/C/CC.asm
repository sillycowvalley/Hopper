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
        
        loop
        {
        //         File.TransferLengthL/H = bytes read (max 256)
        //         Data in FileDataBuffer (0x0600-0x06FF)
    
            File.NextStream();  
            if (NC) { break; }
        }    
        Print.NewLine();
    }
}
