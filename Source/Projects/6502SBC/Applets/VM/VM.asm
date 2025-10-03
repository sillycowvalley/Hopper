program VM
{
    //#define UNIVERSAL
    
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
    
    const byte binName          = vmSlots+13;
    const byte binNameL         = vmSlots+13;
    const byte binNameH         = vmSlots+14;
    
    const string msgFileNotFound = "File not found";
    const string msgOutOfMemory  = "Out of memory";
    const string msgBadBinary    = "Bad Binary";
    
    getFilename() // appends .BIN if missing
    {
        // must be 256 bytes
        LDA #(256-2)
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        LDA ZP.IDXL
        STA binNameL
        LDA ZP.IDXH
        STA binNameH
        
        LDA #1
        Args.GetArg();  // ZP.STR points to filename
        
        // Scan for dot in filename
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            STA [binName], Y
            if (Z) { break; }    // End of string - no dot found
            CMP #'.'
            if (Z)
            {
                return; // Found dot - extension exists, done
            }   
            INY
        }
        
        // No dot found, Y points to null terminator
        // Append ".BIN" 
        LDA #'.'
        STA [binName], Y
        INY
        LDA #'B'
        STA [binName], Y
        INY
        LDA #'I'
        STA [binName], Y
        INY
        LDA #'N'
        STA [binName], Y
        INY
        LDA #0               // New null terminator
        STA [binName], Y
        
        LDA binNameL
        STA ZP.STRL
        LDA binNameH
        STA ZP.STRH
    }
    
    ReadByte()
    {
        PHY PHX
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
            break;
        }
        PLX PLY
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
        STZ binNameL
        STZ binNameH
        
        Args.HasFilename();
        if (NC)
        {
            LDA #(msgFileNotFound / 256) STA ZP.STRH LDA #(msgFileNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        // "<source>.BIN" -> STR
        getFilename();
        
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
        
        // done with the file name
        LDA binNameL
        ORA binNameH
        if (NZ)
        {
            LDA binNameL
            STA ZP.IDXL
            LDA binNameH
            STA ZP.IDXH
            Memory.Free();
        }
        
        // Allocate 2K (including the 2 byte size field = 0x800 - 2 = 0x7FE)
        // - 256 bytes for function table
        // - 256 bytes reserved for globals
        // - round up to nearest page for constant data
        // - 1 page per function (for now)
        // - subtract 2 bytes to ignore the Memory size word
        STZ ZP.ACCL

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
        
        STZ programMemoryL
        STZ ZP.ACCH

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
        STZ sizeTableL

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
        STZ indexL

        loop
        {
            LDA countL
            ORA countH
            if (Z) { break; }
            ReadByte();
            STA [index]

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
        STZ indexL
        
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
            INC

            ASL
            TAY
            LDA indexL
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
                STA [index]

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
            
            // Check if next function fits in current page
            INX
            CPX functionCount
            if (C) { break; }      // No more functions
            
            // Get this function size: 0.. 256 bytes
            TXA
            ASL
            TAY
            LDA [sizeTable], Y     // Next function size LSB
            STA ZP.ACCL
            INY
            LDA [sizeTable], Y     // Next function size MSB
            STA ZP.ACCH
            
            // Calculate remaining space: 256 - indexL
            SEC
            LDA #0           // Low byte of 256
            SBC indexL       // 256 - indexL  
            STA ZP.TOP0      // Low byte of remaining space
            LDA #1           // High byte of 256
            SBC #0           // Propagate borrow if any
            STA ZP.TOP1      // High byte of remaining space
            
            LDA ZP.TOP1
            CMP ZP.ACCH
            if (Z)
            {
                LDA ZP.TOP0
                CMP ZP.ACCL
            }
            if (C)  // TOP >= ACC : next function fits in current page
            {
                // Stay on current page - don't change indexH/indexL
            }
            else
            {
                // Move to next page
                LDA indexL
                if (NZ)
                {
                    INC indexH
                }
                STZ indexL
            }
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
