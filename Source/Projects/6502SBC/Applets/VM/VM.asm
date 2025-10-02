program VM
{
    #define UNIVERSAL
    
#ifdef UNIVERSAL        
    #define CPU_65C02
#else
    #define CPU_65C02S    
#endif

    //#define DEBUG
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    uses "../System/Memory"
    uses "../System/Char"
    
    uses "Runtime"
    
    const byte vmSlots = 0x60; // 0x60..0x6F
    
    const byte programMemory    = vmSlots+0;
    const byte programMemoryL   = vmSlots+0;
    const byte programMemoryH   = vmSlots+1;
    
    const byte functionCount    = vmSlots+2;
    
    const byte dataSizeL        = vmSlots+3;
    const byte dataSizeH        = vmSlots+4;

    // used in ReadByte()    
    const byte bufferIndexL     = vmSlots+5;
    const byte bufferIndexH     = vmSlots+6;
    
    const byte index            = vmSlots+7;
    const byte indexL           = vmSlots+7;
    const byte indexH           = vmSlots+8;
    
    const byte countL           = vmSlots+9;
    const byte countH           = vmSlots+10;
    
    const byte sizeTable        = vmSlots+11;
    const byte sizeTableL       = vmSlots+11;
    const byte sizeTableH       = vmSlots+12;
    
    
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
#ifdef UNIVERSAL
        TYA PHA TXA PHA
#else        
        PHY PHX
#endif
        
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
#ifdef UNIVERSAL
        STA ZP.TEMP
        PLA TAX PLA TAY
        LDA ZP.TEMP
#else        
        PLX PLY
#endif
    }
    
    ReadByte()
    {
#ifdef UNIVERSAL        
        TYA PHA TXA PHA
#else
        PHY PHX
#endif
        loop
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
                        break;
                    }
#ifdef UNIVERSAL
                    LDA #0
                    STA bufferIndexL
                    STA bufferIndexH
#else
                    STZ bufferIndexL
                    STZ bufferIndexH
#endif
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
            break;
        }
#ifdef UNIVERSAL
        STA ZP.TEMP
        PLA TAX PLA TAY
        LDA ZP.TEMP
#else        
        PLX PLY
#endif
    }
    
    // Dump 256-byte page in hex/ASCII format
    // Input: ZP.IDXH = start address of page
    DumpPage()
    {
        PHY
        
        STZ ZP.ACCL  // Offset counter
        STZ ZP.IDXL
        
        loop
        {
            // Print offset (4 hex digits)
            LDA ZP.IDXH
            Print.Hex();
            LDA ZP.ACCL
            Print.Hex();
            LDA #':'
            Print.Char();
            Print.Space();
            
            // Print 16 bytes in hex
            LDY #0
            loop
            {
                LDA [ZP.IDX], Y
                Print.Hex();
                Print.Space();
                
                // Extra space after 8 bytes
                INY
                CPY #8
                if (Z)
                {
                    Print.Space();
                }
                CPY #16
                if (Z) { break; }
            }
            
            // Print ASCII representation
            Print.Space();
            
            LDY #0
            loop
            {
                LDA [ZP.IDX], Y
                // Check if printable (32-126)
                CMP #32
                if (C)  // >= 32
                {
                    CMP #128
                    if (C)  // >= 128
                    {
                        LDA #'.'    
                    }
                }
                else
                {
                    LDA #'.'
                }
                Print.Char();
                
                INY
                CPY #16
                if (Z) { break; }
            }
            
            Print.NewLine();
            
            // Advance to next line
            CLC
            LDA ZP.IDXL
            ADC #16
            STA ZP.IDXL
            if (C)
            {
                INC ZP.IDXH
            }
            
            // Update offset
            CLC
            LDA ZP.ACCL
            ADC #16
            STA ZP.ACCL
            if (Z) { break; }  // Wrapped after 256 bytes
        }
        PLY
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
            
#ifdef UNIVERSAL
            LDA #0
            STA bufferIndexL
            STA bufferIndexH
#else            
            STZ bufferIndexL
            STZ bufferIndexH
#endif
            
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
        
        // Allocate 2K (including the 2 byte size field = 0x800 - 2 = 0x7FE)
        // - 256 bytes for function table
        // - 256 bytes reserved for globals
        // - round up to nearest page for constant data
        // - 1 page per function (for now)
        // - subtract 2 bytes to ignore the Memory size word
#ifdef UNIVERSAL
        LDA #0
        STA ZP.ACCL
#else        
        STZ ZP.ACCL
#endif
        LDA dataSizeH
        STA ZP.ACCH
        LDA dataSizeL
        if (NZ)
        {
            INC ZP.ACCH
        }
        INC ZP.ACCH // function table
        INC ZP.ACCH // globals
        
        CLC
        LDA ZP.ACCH 
        ADC functionCount
        STA ZP.ACCH
 
        SEC  
        LDA ZP.ACCL
        SBC #2
        STA ZP.ACCL
        LDA ZP.ACCH
        SBC #0
        STA ZP.ACCH

        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
#ifdef UNIVERSAL
        LDA #0
        STA programMemoryL
        STA ZP.ACCH
#else        
        STZ programMemoryL
        STZ ZP.ACCH
#endif
        LDA ZP.IDXH
        STA programMemoryH
        
        // temporary space for function sizes
        LDA #0xFE
        STA ZP.ACCL
        
        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
#ifdef UNIVERSAL
        LDA #0
        STA sizeTableL
#else        
        STZ sizeTableL
#endif
        LDA ZP.IDXH
        STA sizeTableH
        
        // == Page 0 is the function table starting with .MAIN at slot 1 (slot 0 is empty because it is the size for Memory.Allocate) ==
        
        LDX #0
        LDY #2
        loop
        {
            ReadByte();
            STA [sizeTable], Y
            INY
            ReadByte();
            STA [sizeTable], Y
            INY
            
            INX
            CPX functionCount
            if (C) { break; } // X >= functionCount
        }
        
        // == Page 1 is reserved for globals ==
        
        // == Page 2 is where we start loading constant data ==
        LDA dataSizeL
        STA countL
        LDA dataSizeH
        STA countH
                
        CLC
        LDA programMemoryH
        ADC #2
        STA indexH
#ifdef UNIVERSAL
        LDA #0
        STA indexL
#else
        STZ indexL
#endif   
        loop
        {
            LDA countL
            ORA countH
            if (Z) { break; }
            ReadByte();
#ifdef UNIVERSAL
            LDY #0
            STA [index], Y
#else            
            STA [index]
#endif
            INC indexL if (Z) { INC indexH }
            
            LDA countL
            if (Z)
            {
                DEC countH
            }
            DEC countL
        }
        // == done loading constant data ==
        
        
        // set index to be first function page
        LDA indexL
        if (NZ)
        {
            INC indexH
        }
#ifdef UNIVERSAL
        LDA #0
        STA indexL
#else        
        STZ indexL
#endif
        
        LDA #2
        STA countL
        
        // == load functions ==
        LDX #0
        loop
        {
            // function index = (function number + 1) * 2
            // - get the function size in count
            // - set the new function offsets in the function table
            TXA
#ifdef UNIVERSAL
            CLC
            ADC #1
#else
            INC
#endif
            ASL
            TAY
            LDA #0
            STA [programMemory], Y
            LDA [sizeTable], Y
            STA countL
            INY
            LDA indexH
            STA [programMemory], Y
            LDA [sizeTable], Y
            STA countH
            
            loop
            {
                ReadByte(); if (NC) { break; }
#ifdef UNIVERSAL
                LDY #0
                STA [index], Y
#else
                STA [index]
#endif
                INC indexL if (Z) { INC indexH }
                
                LDA countL
                if (Z)
                {
                    DEC countH
                }
                DEC countL
                LDA countH
                ORA countL
                if (Z) { break; }
            } // loop
            
            // next function page:
            LDA indexL
            if (NZ)
            {
                INC indexH
            }
#ifdef UNIVERSAL
            LDA #0
            STA indexL
#else            
            STZ indexL
#endif
            
            INX
            CPX functionCount
            if (C) { break; } //  >= functionCount
        } // outer loop
        
#ifdef DEBUG        
        LDA programMemoryH
        STA ZP.IDXH
        DumpPage();
        DumpPage();
        DumpPage();
        DumpPage();
        DumpPage();
        
        Print.NewLine();
        LDA sizeTableH
        STA ZP.IDXH
        DumpPage();
#endif        
        
        LDA #2
        STA ZP.IDXL
        LDA sizeTableH
        STA ZP.IDXH
        Memory.Free();
        
        
        Runtime.Initialize();
        Runtime.Execute();
        
        
        LDA #2
        STA ZP.IDXL
        LDA programMemoryH
        STA ZP.IDXH
        Memory.Free();
    }
}
