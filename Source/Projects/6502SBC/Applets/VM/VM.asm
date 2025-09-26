program VM
{
    #define CPU_65C02S
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    uses "../System/Memory"
    uses "../System/Char"
    
    uses "Loader"
    
    const byte vmSlots = 0x60; // 0x60..0x6F
    
    const uint functionCount    = vmSlots+0;
    
    const uint dataSizeL        = vmSlots+1;
    const uint dataSizeH        = vmSlots+2;
    
    const uint bufferIndexL     = vmSlots+3;
    const uint bufferIndexH     = vmSlots+4;
    
    const string msgFileNotFound = "File not found";
    const string msgOutOfMemory  = "Out of memory";
    const string msgBadBinary    = "Bad Binary";
    
    
    // Get filename argument from command line
    // Input:  None (reads from command line buffer at Address.LineBuffer)
    // Output: C set if filename found, clear if none
    //         ZP.STR = pointer to filename (null-terminated, uppercase)
    //         A = filename length
    // Note:   Returns first argument after program name
    //         BIOS has already uppercased the input
    GetFilename()
    {
        PHY
        PHX
        
        Args.GetArgument(); // filename length -> A, X != 0 means "." seen
        
        // append ".VMA" if there is no "."
        CPX #0
        if (Z)
        {
            // getArgument gives us a STR that is pointing into Address.LineBuffer so we can extend the filename safely
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; } 
                INY
            }
            LDA #'.'
            STA [ZP.STR], Y
            INY
            LDA #'B'
            STA [ZP.STR], Y
            INY
            LDA #'I'
            STA [ZP.STR], Y
            INY
            LDA #'N'
            STA [ZP.STR], Y
            INY
            LDA #0
            STA [ZP.STR], Y
            TYA // length -> A
        }
        PLX
        PLY
    }
    
    ReadByte()
    {
        LDA bufferIndexH
        CMP File.TransferLengthH
        if (Z)
        {
            LDA bufferIndexL
            CMP File.TransferLengthL
            if (Z)  // Need more data
            {
                File.NextStream();
                if (NC)
                {
                    return;
                }
                STZ bufferIndexL
                STZ bufferIndexH
            }
        }
        
        // Get character from buffer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.IDXL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.IDXH
        
        LDY bufferIndexL
        LDA [ZP.IDX], Y
        
        INC bufferIndexL
        if (Z) { INC bufferIndexH }
        SEC
    }
    
    Hopper()
    {
        Args.HasFilename();
        if (NC)
        {
            LDA #(msgFileNotFound / 256) STA ZP.STRH LDA #(msgFileNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        // "<source>.BIN" -> STR
        GetFilename();
        
        // open source
        LDA # FileType.Any // all files
        File.Exists();
        if (NC)
        {
            LDA #(msgFileNotFound / 256) STA ZP.STRH LDA #(msgFileNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // Open file for reading
        LDA # FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(msgFileNotFound / 256) STA ZP.STRH LDA #(msgFileNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        // read the header
        loop
        {
            File.NextStream();
            if (NC) { break; }
            
            STZ bufferIndexL
            STZ bufferIndexH
            
            ReadByte(); if (NC) { break; }
            CMP #'V'
            if (NZ) { break; }
            ReadByte(); if (NC) { break; }
            CMP #'M'
            if (NZ) { break; }
            ReadByte(); if (NC) { break; }
            CMP #'B'
            if (NZ) { break; }
            ReadByte(); if (NC) { break; }
            STA functionCount
            ReadByte(); if (NC) { break; }
            STA dataSizeL
            ReadByte(); if (NC) { break; }
            STA dataSizeH
            SEC
            break;
        }
        if (NC)
        {
            LDA #(msgBadBinary / 256) STA ZP.STRH LDA #(msgBadBinary % 256) STA ZP.STRL
            Print.String();
        }
        
        // Allocate 2K
        STZ ZP.ACCL
        LDA #0x08
        STZ ZP.ACCH
        Memory.Allocate();
        
        Print.NewLine(); LDA ZP.IDXH Print.Hex(); LDA ZP.IDXL Print.Hex();
        
    }
}
