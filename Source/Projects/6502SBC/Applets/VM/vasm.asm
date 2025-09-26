// Assembles VM bytecode from .VMA files to .BIN files
program VASM
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
    
    uses "Buffer"
    uses "Parser"
    
    const string msgBanner         = "VM Assembler v1.0\n";
    const string msgOutOfMemory    = "Out of memory\n";
    const string msgAssembled      = "Assembled\n0x";
    const string msgBytes          = " bytes\n";
    const string msgSourceNotFound = "Source Not Found\n";
    const string msgFailedLoading  = "Failed Loading Source\n";
    const string msgFailedSaving   = "Failed Writing Output\n";
    
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
            LDA #'V'
            STA [ZP.STR], Y
            INY
            LDA #'M'
            STA [ZP.STR], Y
            INY
            LDA #'A'
            STA [ZP.STR], Y
            INY
            LDA #0
            STA [ZP.STR], Y
            TYA // length -> A
        }
        PLX
        PLY
    }
    
    // Create output filename: "HELLO" | "HELLO.VMA" -> "HELLO.BIN"
    // STR -> STR
    makeOutputName()
    {
        // Allocate buffer for output name (14 bytes max + ".BIN" + null)
        LDA #20
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            return;
        }
        
        // Copy source name
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // Found null
            CMP #'.'
            if (Z) { break; }  // Found "." (like ".VMA")
            
            STA [ZP.IDX], Y
            INY
            CPY #13  // Max filename length
            if (Z)
            {
                CLC
                return;
            }
        }
        
        // Append ".BIN"
        LDA #'.'
        STA [ZP.IDX], Y
        INY
        LDA #'B'
        STA [ZP.IDX], Y
        INY
        LDA #'I'
        STA [ZP.IDX], Y
        INY
        LDA #'N'
        STA [ZP.IDX], Y
        INY
        
        // Null terminate
        LDA #0
        STA [ZP.IDX], Y
        
        LDA ZP.IDXL
        STA ZP.STRL
        LDA ZP.IDXH
        STA ZP.STRH
        
        SEC
    }
    freeOutputName()
    {
        LDA ZP.STRL
        STA ZP.IDXL
        LDA ZP.STRH
        STA ZP.IDXH
        Memory.Free();
    }
    
    Hopper()
    {
        LDA #(msgBanner / 256) STA ZP.STRH LDA #(msgBanner % 256) STA ZP.STRL
        Print.String();
        
        Args.HasFilename();
        if (NC)
        {
            LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        // "<source>.VMA" -> STR
        GetFilename();
        
        Buffer.Initialize();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // open source
        LDA # FileType.Any // all files
        File.Exists();
        if (NC)
        {
            LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // Open file for reading
        LDA # FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        Parser.Initialize();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // file header
        LDA #'V' Buffer.Emit(); LDA #'M' Buffer.Emit(); LDA #'B' Buffer.Emit();
        LDA #0 Buffer.Emit(); // number of functions (0..128)
        LDA #0 Buffer.Emit(); LDA #0 Buffer.Emit();// bytes of globals    (0..256)
        
        // reserve space for the function table (256 bytes)
        LDY #128
        LDA #0
        loop {
            Buffer.Emit();
            Buffer.Emit();
            DEY
            if (Z) { break; }
        }
        
        // reserve space for the globals (256 bytes)
        LDY #128
        LDA #0
        loop {
            Buffer.Emit();
            Buffer.Emit();
            DEY
            if (Z) { break; }
        }
        
        // assemble source
        Parser.Parse();
        if (C)
        {
            // save output: STR -> STR
            makeOutputName();
            if (C)
            {
                Buffer.Save();
                if (NC)
                {
                    LDA #(msgFailedSaving / 256) STA ZP.STRH LDA #(msgFailedSaving % 256) STA ZP.STRL
                    Print.String();
                }
                freeOutputName();
            }
            Parser.Dispose();
        }
    }
}
