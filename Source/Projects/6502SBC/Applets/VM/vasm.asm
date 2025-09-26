// Assembles VM bytecode from .VMA files to .BIN files
program VASM
{
    #define CPU_65C02S
    //#define DEBUG
    
    const byte vasmSlots = 0x90; // 0x90..0x9F
    
    const uint filenameL    = vasmSlots+0;
    const uint filenameH    = vasmSlots+1;
    
    const uint outFilenameL = vasmSlots+2;
    const uint outFilenameH = vasmSlots+3;
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    uses "../System/Memory"
    uses "../System/Char"
    
    uses "Buffer"
    uses "Symbols"
    uses "Parser"
    
    const string msgStart = "Memory: ";
    const string msgCompiled = "Memory after compile: ";
    const string msgCleaned = "Memory after cleanup: ";
    
    const string msgBanner         = "VM Assembler v1.0\n";
    const string msgOutOfMemory    = "Out of memory\n";
    const string msgAssembled      = "Assembled\n0x";
    const string msgBytes          = " bytes\n";
    const string msgSourceNotFound = "Source Not Found\n";
    const string msgFailedLoading  = "Failed Loading Source\n";
    const string msgFailedSaving   = "Failed Writing Output\n";
    const string msgSuccess        = " Created\n";
    
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
        LDA outFilenameL
        STA ZP.IDXL
        LDA outFilenameH
        STA ZP.IDXH
        Memory.Free();
    }
    
    Error()
    {
        Print.String();
        CLC
    }
    
    Hopper()
    {
        LDA #(msgBanner / 256) STA ZP.STRH LDA #(msgBanner % 256) STA ZP.STRL
        Print.String();
        
        STZ outFilenameL
        STZ outFilenameH
        
        LDA #(msgStart % 256)
        STA ZP.STRL
        LDA #(msgStart / 256)
        STA ZP.STRH
        Print.String();
        Memory.Available();  // Returns in ZP.ACC
        Shared.MoveAccToTop();
        Long.Print();
        Print.NewLine();
        
        loop
        {
            Args.HasFilename();
            if (NC)
            {
                LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
                Error();
                break;
            }
            // "<source>.VMA" -> STR
            GetFilename();
            
            Buffer.Initialize();
            if (NC)
            {
                LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
                Error();
                break;
            }
            
            // open source
            LDA # FileType.Any // all files
            File.Exists();
            if (NC)
            {
                LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
                Error();
                break;
            }
            
            // Open file for reading
            LDA # FileType.Any
            File.StartLoad();
            if (NC)
            {
                LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
                Error();
                break;
            }
            
            Parser.Initialize();
            if (NC)
            {
                LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
                Error();
                break;
            }
            
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
            LDA STRL
            STA filenameL
            LDA STRH
            STA filenameH
            
            // assemble source
            Parser.Parse();
            if (C)
            {
                LDA filenameL
                STA STRL
                LDA filenameH
                STA STRH
                // save output: STR -> STR
                makeOutputName();
                if (C)
                {
                    LDA STRL
                    STA outFilenameL
                    LDA STRH
                    STA outFilenameH
                    Buffer.Save();
                    if (NC)
                    {
                        LDA #(msgFailedSaving / 256) STA ZP.STRH LDA #(msgFailedSaving % 256) STA ZP.STRL
                        Error();
                        break;
                    }
                    
                    LDA #(msgCompiled % 256)
                    STA ZP.STRL
                    LDA #(msgCompiled / 256)
                    STA ZP.STRH
                    Print.String();
                    Memory.Available();
                    Shared.MoveAccToTop();
                    Long.Print();
                    Print.NewLine();
                    
                    LDA outFilenameL
                    STA ZP.STRL
                    LDA outFilenameH
                    STA ZP.STRH
                    
                    Print.String();
                    LDA #(msgSuccess / 256) STA ZP.STRH LDA #(msgSuccess % 256) STA ZP.STRL
                    Print.String();
                    
                    SEC
                }
            }
            break;
        } // single exit
        
        if (C)
        {
            Buffer.Dispose();
            freeOutputName();
            Parser.Dispose();
            
            LDA #(msgCleaned % 256)
            STA ZP.STRL
            LDA #(msgCleaned / 256)
            STA ZP.STRH
            Print.String();
            Memory.Available();
            Shared.MoveAccToTop();
            Long.Print();
            Print.NewLine();
        }
    }
}
