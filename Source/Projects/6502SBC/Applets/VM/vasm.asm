// Assembles VM bytecode from .VMA files to .BIN files
program VMA
{
    #define CPU_65C02S
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    uses "../System/Memory"
    
    // Constants
    const byte MAX_LINE = 255;
    const uint OUTPUT_SIZE = 0x8000;
    
    // Global workspace in zero page (0x70-0x7F available)
    const byte OUTPUT_L = 0x70;  // Output buffer pointer
    const byte OUTPUT_H = 0x71;
    const byte PC_L = 0x72;       // Program counter
    const byte PC_H = 0x73;
    const byte LINE_L = 0x74;     // Line buffer pointer  
    const byte LINE_H = 0x75;
    const byte TOKEN_L = 0x76;    // Token buffer pointer
    const byte TOKEN_H = 0x77;
    const byte ARG_L = 0x78;      // Arg buffer pointer
    const byte ARG_H = 0x79;
    const byte CHAR_POS = 0x7A;   // Current position in line
    const byte TOKEN_LEN = 0x7B;  // Token length
    const byte ARG_VAL_L = 0x7C;  // Parsed argument value
    const byte ARG_VAL_H = 0x7D;
    const byte OPCODE = 0x7E;     // Found opcode value
    const byte OPCODE_ARGS = 0x7F; // Opcode argument count
    
    const byte bufferIndexL = 0x80;
    const byte bufferIndexH = 0x81;
    
    const string msgBanner         = "VM Assembler v1.0\n";
    const string msgOutOfMemory    = "Out of memory\n";
    const string msgAssembled      = "Assembled\n0x";
    const string msgBytes          = " bytes\n";
    const string msgSourceNotFound = "Source Not Found\n";
    const string msgFailedLoading  = "Failed Loading Source\n";
    const string msgFailedSaving   = "Failed Writing Output\n";
    
    const string msgUnknown        = "Unknown 0x";
    
    Hopper()
    {
        LDA #(msgBanner / 256) STA ZP.STRH LDA #(msgBanner % 256) STA ZP.STRL
        Print.String();
        
        // Allocate output buffer
        LDA #(OUTPUT_SIZE % 256)
        STA ZP.ACCL
        LDA #(OUTPUT_SIZE / 256)
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        LDA ZP.IDXL
        STA OUTPUT_L
        LDA ZP.IDXH
        STA OUTPUT_H
        
        // Allocate line buffer (256 bytes)
        LDA #0
        STA ZP.ACCL
        LDA #1
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        LDA ZP.IDXL
        STA LINE_L
        LDA ZP.IDXH
        STA LINE_H
        
        // Allocate token buffer (32 bytes)
        LDA #32
        STA ZP.ACCL
        LDA #0
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        LDA ZP.IDXL
        STA TOKEN_L
        LDA ZP.IDXH
        STA TOKEN_H
        
        // Allocate arg buffer (32 bytes)
        LDA #32
        STA ZP.ACCL
        LDA #0
        STA ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            LDA #(msgOutOfMemory / 256) STA ZP.STRH LDA #(msgOutOfMemory % 256) STA ZP.STRL
            Print.String();
            return;
        }
        LDA ZP.IDXL
        STA ARG_L
        LDA ZP.IDXH
        STA ARG_H
        
        Args.HasFilename();
        if (NC)
        {
            LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        Args.GetFilenameVM(); // ->STR
        if (NC)
        {
            LDA #(msgSourceNotFound / 256) STA ZP.STRH LDA #(msgSourceNotFound % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // Initialize PC
        LDA #0
        STA PC_L
        STA PC_H
        
        // Process input file
        processFile();
        if (NC)
        {
            return;
        }
        
        // Write output file
        writeOutput();
        
        // Report results
        LDA #(msgAssembled / 256) STA ZP.STRH LDA #(msgAssembled % 256) STA ZP.STRL
        Print.String();
        
        LDA PC_H
        Print.Hex();
        LDA PC_L
        Print.Hex();
        LDA #(msgBytes / 256) STA ZP.STRH LDA #(msgBytes % 256) STA ZP.STRL
        Print.String();
        
        // Free all buffers
        LDA ARG_L
        STA ZP.IDXL
        LDA ARG_H
        STA ZP.IDXH
        Memory.Free();
        
        LDA TOKEN_L
        STA ZP.IDXL
        LDA TOKEN_H
        STA ZP.IDXH
        Memory.Free();
        
        LDA LINE_L
        STA ZP.IDXL
        LDA LINE_H
        STA ZP.IDXH
        Memory.Free();
        
        LDA OUTPUT_L
        STA ZP.IDXL
        LDA OUTPUT_H
        STA ZP.IDXH
        Memory.Free();
    }
    
    processFile()
    {
        // Open input file from STR
        
        LDA # FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(msgFailedLoading / 256) STA ZP.STRH LDA #(msgFailedLoading % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // Prime the pump - get first chunk
        refillBuffer();
        if (NC) 
        { 
            LDA #(msgFailedLoading / 256) STA ZP.STRH LDA #(msgFailedLoading % 256) STA ZP.STRL
            Print.String();
            return;
        }
        
        // Process lines
        loop
        {
            readLine();
            if (NC)  // EOF
            {
                break;
            }
            processLine();
            SEC
        }
    }
    
    readLine()  // Read line into LINE buffer
    {
        LDY #0
        loop
        {
            readByte();
            if (NC)  // EOF
            {
                CLC
                return;
            }
            
            CMP # '\n'  // Newline
            if (Z)
            {
                LDA #0
                STA [LINE_L], Y
                SEC
                return;
            }
            
            STA [LINE_L], Y
            INY
            if (Z)  // Line too long
            {
                DEY
                LDA #0
                STA [LINE_L], Y
                SEC
                return;
            }
        }
    }
    
    readByte() // -> A
    {
        PHY
        
        LDA bufferIndexH
        CMP File.TransferLengthH
        if (Z)
        {
            LDA bufferIndexL
            CMP File.TransferLengthL
            if (Z)  // Need more data
            {
                refillBuffer();
                if (NC)
                {
                    PLY
                    return; // EOF
                }
            }
        }
        
        // Load FileDataBuffer address into zero page pointer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.IDXL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.IDXH
        
        // Now read via indirect indexed
        LDY bufferIndexL // 0..255
        LDA [ZP.IDX], Y
        
        INC bufferIndexL
        if (Z) { INC bufferIndexH }
        PLY
    }
    
    refillBuffer()
    {
        File.NextStream();
        if (NC)
        {
            CLC
            return;
        }
        
        STZ bufferIndexL
        STZ bufferIndexH
        
        SEC
    }
    
    processLine()
    {
        // Skip whitespace
        LDY #0
        loop
        {
            LDA [LINE_L], Y
            if (Z)  // Empty line
            {
                return;
            }
            CMP #' '
            if (NZ)
            {
                break;
            }
            INY
        }
        
        // Check for comment
        CMP #'/'
        if (Z)
        {
            INY
            LDA [LINE_L], Y
            CMP #'/'
            if (Z)  // Comment line
            {
                return;
            }
            DEY
        }
        
        // Extract token
        STY CHAR_POS
        extractToken();
        
        // Check for .FUNC directive
        LDY #0
        LDA [TOKEN_L], Y
        CMP #'.'
        if (Z)
        {
            INY
            LDA [TOKEN_L], Y
            CMP #'F'
            if (Z)
            {
                handleFunc();
                return;
            }
        }
        
        // Must be an opcode
        compareToken();
        if (NC)  // Not found
        {
            LDA #(msgUnknown / 256) STA ZP.STRH LDA #(msgUnknown % 256) STA ZP.STRL
            Print.String();
            printToken();
            LDA # '\n'
            Print.Char();
            return;
        }
        
        // Emit opcode
        LDA OPCODE
        emitByte();
        
        // Handle arguments
        LDA OPCODE_ARGS
        if (Z)  // No arguments
        {
            return;
        }
        
        // Extract argument
        extractArg();
        parseNumber();
        
        // Emit argument(s)
        LDA OPCODE_ARGS
        CMP #1
        if (Z)  // Single byte argument
        {
            LDA ARG_VAL_L
            
            // Special handling for CALL - multiply by 2
            LDY OPCODE
            CPY #0x80
            if (Z)
            {
                ASL
            }
            
            emitByte();
            return;
        }
        
        // Word argument
        LDA ARG_VAL_L
        emitByte();
        LDA ARG_VAL_H
        emitByte();
    }
    
    extractToken()  // Extract token from LINE at CHAR_POS to TOKEN buffer
    {
        LDY CHAR_POS
        
        LDA TOKEN_L
        STA IDYL
        LDA TOKEN_H
        STA IDYH
        
        loop
        {
            LDA [LINE_L], Y
            if (Z)  // End of line
            {
                break;
            }
            CMP #' '
            if (Z)  // End of token
            {
                break;
            }
            
            STA [IDY]
            INY
            IncIDY();
            CPX #31  // Max token length
            if (Z)
            {
                break;
            }
        }
        
        LDA #0
        STA [IDY]     // Null terminate
        STY CHAR_POS  // Update position
    }
    
    extractArg()  // Extract argument from LINE at CHAR_POS to ARG buffer
    {
        // Skip whitespace
        LDY CHAR_POS
        loop
        {
            LDA [LINE_L], Y
            if (Z)
            {
                break;
            }
            CMP #' '
            if (NZ)
            {
                break;
            }
            INY
        }
        
        STY CHAR_POS
        LDX #0
        
        LDA ARG_L
        STA IDYL
        LDA ARG_H
        STA IDYH
        
        loop
        {
            LDA [LINE_L], Y
            if (Z)  // End of line
            {
                break;
            }
            CMP #' '
            if (Z)  // End of arg
            {
                break;
            }
            
            STA [IDY]
            INY
            IncIDY();
            CPX #31  // Max arg length
            if (Z)
            {
                break;
            }
        }
        
        LDA #0
        STA [IDY]  // Null terminate
    }
    
    handleFunc()  // Handle .FUNC directive
    {
        // Extract function ID
        extractArg();
        parseNumber();
        
        // Set PC to function start
        LDA ARG_VAL_L
        if (NZ)  // Must be even
        {
            AND #0xFE
        }
        
        // PC = 0x2000 + (id * 0x100)
        STA PC_H
        LDA #0x20
        CLC
        ADC PC_H
        STA PC_H
        
        LDA #0
        STA PC_L
    }
    
    emitByte()  // A = byte to emit
    {
        PHA
        
        // Calculate output address
        CLC
        LDA OUTPUT_L
        ADC PC_L
        STA ZP.IDXL
        LDA OUTPUT_H
        ADC PC_H
        STA ZP.IDXH
        
        // Store byte
        PLA
        LDY #0
        STA [ZP.IDXL], Y
        
        // Increment PC
        INC PC_L
        if (Z)
        {
            INC PC_H
        }
    }
    
    printToken()  // Print TOKEN buffer to serial
    {
        LDY #0
        loop
        {
            LDA [TOKEN_L], Y
            if (Z)
            {
                break;
            }
            Print.Char();
            INY
        }
    }
    
    writeOutput()
    {
        // Create output filename
        
        Args.GetFilenameBIN();
        loop
        {
            File.StartSave();
            if (NC)
            {
                break;
            }
            
            // Write all bytes
            // Set source to our code buffer
            LDA OUTPUT_L
            STA File.SectorSourceL
            LDA OUTPUT_H
            STA File.SectorSourceH
            
            // Set transfer length to amount of code generated
            LDA PC_L
            STA File.TransferLengthL
            LDA PC_H
            STA File.TransferLengthH
            
            // Write the code buffer
            File.AppendStream();
            if (NC)
            {
                break;
            }
        
            LDA #0x00
            File.EndSave();
            
        } // loop
        if (NC)
        {
            LDA #(msgFailedSaving / 256) STA ZP.STRH LDA #(msgFailedSaving % 256) STA ZP.STRL
            Print.String();
            CLC
        }
    }
    
    parseNumber()  // Parse number from ARG buffer to ARG_VAL_L/H
    {
        // Clear result in NEXT
        LDA #0
        STA ZP.NEXT0
        STA ZP.NEXT1
        STA ZP.NEXT2
        STA ZP.NEXT3
        
        LDY #0
        
        // Check for hex prefix
        LDA [ARG_L], Y
        CMP #'0'
        if (Z)
        {
            INY
            LDA [ARG_L], Y
            CMP #'x'
            if (Z)
            {
                INY
                parseHex();
                // Copy result
                LDA ZP.NEXT0
                STA ARG_VAL_L
                LDA ZP.NEXT1
                STA ARG_VAL_H
                return;
            }
            DEY  // Back to start for decimal
        }
        
        // Check for negative
        LDA [ARG_L], Y
        CMP #'-'
        if (Z)
        {
            INY
            parseDecimal();
            
            // Negate result
            SEC
            LDA #0
            SBC ZP.NEXT0
            STA ZP.NEXT0
            LDA #0
            SBC ZP.NEXT1
            STA ZP.NEXT1
            
            // Copy to ARG_VAL
            LDA ZP.NEXT0
            STA ARG_VAL_L
            LDA ZP.NEXT1
            STA ARG_VAL_H
            return;
        }
        
        parseDecimal();
        
        // Copy result to ARG_VAL
        LDA ZP.NEXT0
        STA ARG_VAL_L
        LDA ZP.NEXT1
        STA ARG_VAL_H
    }
    
    parseDecimal()  // Y = start position in ARG buffer
    {
        loop
        {
            LDA [ARG_L], Y
            if (Z)  // End of string
            {
                break;
            }
            
            SEC
            SBC #'0'
            if (NC)  // Less than '0'
            {
                break;
            }
            CMP #10
            if (C)  // Greater than '9'
            {
                break;
            }
            
            PHA  // Save digit
            
            // Multiply current value by 10
            LDA #10
            STA ZP.TOP0
            LDA #0
            STA ZP.TOP1
            STA ZP.TOP2
            STA ZP.TOP3
            
            Long.Mul();
            
            // Add digit
            PLA
            STA ZP.TOP0
            LDA #0
            STA ZP.TOP1
            STA ZP.TOP2
            STA ZP.TOP3
            
            Long.Add();
            
            INY
        }
    }
    
    parseHex()  // Y points after "0x" in ARG buffer
    {
        loop
        {
            LDA [ARG_L], Y
            if (Z)
            {
                break;
            }
            
            // Multiply by 16 (shift left 4)
            ASL ZP.NEXT0
            ROL ZP.NEXT1
            ASL ZP.NEXT0
            ROL ZP.NEXT1
            ASL ZP.NEXT0
            ROL ZP.NEXT1
            ASL ZP.NEXT0
            ROL ZP.NEXT1
            
            // Convert hex digit
            LDA [ARG_L], Y
            CMP #'0'
            if (C)  // >= '0'
            {
                CMP #'9' + 1
                if (NC)  // <= '9'
                {
                    SEC
                    SBC #'0'
                }
                else
                {
                    CMP #'A'
                    if (C)  // >= 'A'
                    {
                        CMP #'F' + 1
                        if (NC)  // <= 'F'
                        {
                            SEC
                            SBC #'A' - 10
                        }
                        else
                        {
                            CMP #'a'
                            if (C)  // >= 'a'
                            {
                                CMP #'f' + 1
                                if (NC)  // <= 'f'
                                {
                                    SEC
                                    SBC #'a' - 10
                                }
                            }
                        }
                    }
                }
                
                // Add to result
                CLC
                ADC ZP.NEXT0
                STA ZP.NEXT0
                if (C)
                {
                    INC ZP.NEXT1
                }
            }
            
            INY
        }
    }
    
    compareToken()  // Compare TOKEN buffer with opcode names
    {
        // Returns: OPCODE = opcode value, OPCODE_ARGS = arg count
        // C set if found, C clear if not found
        
        // Default to not found
        LDA #0
        STA OPCODE
        STA OPCODE_ARGS
        
        LDY #0
        LDA [TOKEN_L], Y
        
        loop  // Single exit point
        {
            switch (A)
            {
                case 'P':
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'U':  // PUSH_xxx
                        {
                            LDY #5
                            LDA [TOKEN_L], Y
                            switch (A)
                            {
                                case 'B':  // PUSH_BYTE
                                {
                                    LDA #0x00
                                    STA OPCODE
                                    LDA #1
                                    STA OPCODE_ARGS
                                    SEC
                                    break;
                                }
                                case 'W':  // PUSH_WORD
                                {
                                    LDA #0x02
                                    STA OPCODE
                                    LDA #2
                                    STA OPCODE_ARGS
                                    SEC
                                    break;
                                }
                                case 'Z':  // PUSH_ZERO
                                {
                                    LDA #0x04
                                    STA OPCODE
                                    SEC
                                    break;
                                }
                                case 'O':  // PUSH_ONE
                                {
                                    LDA #0x06
                                    STA OPCODE
                                    SEC
                                    break;
                                }
                                case 'S':  // PUSH_STRING_x
                                {
                                    LDY #12
                                    LDA [TOKEN_L], Y
                                    switch (A)
                                    {
                                        case '0':  // PUSH_STRING_0
                                        {
                                            LDA #0x74
                                            STA OPCODE
                                            SEC
                                            break;
                                        }
                                        case 'B':  // PUSH_STRING_B
                                        {
                                            LDA #0x70
                                            STA OPCODE
                                            LDA #1
                                            STA OPCODE_ARGS
                                            SEC
                                            break;
                                        }
                                        default:
                                        {
                                            CLC
                                        }
                                    }
                                }
                                default:
                                {
                                    CLC
                                }
                            }
                        }
                        case 'R':  // PRINT_xxx
                        {
                            LDY #6
                            LDA [TOKEN_L], Y
                            switch (A)
                            {
                                case 'S':  // PRINT_STRING
                                {
                                    LDA #0x98
                                    STA OPCODE
                                    SEC
                                    break;
                                }
                                case 'B':  // PRINT_BYTE
                                {
                                    LDA #0x92
                                    STA OPCODE
                                    SEC
                                    break;
                                }
                                case 'W':  // PRINT_WORD
                                {
                                    LDA #0x96
                                    STA OPCODE
                                    SEC
                                    break;
                                }
                                default:
                                {
                                    CLC
                                }
                            }
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'D':
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'U':  // DUP or DUP2
                        {
                            INY
                            LDA [TOKEN_L], Y
                            if (Z)  // Just DUP
                            {
                                LDA #0x08
                                STA OPCODE
                                SEC
                                break;
                            }
                            CMP #'2'
                            if (Z)  // DUP2
                            {
                                LDA #0x0A
                                STA OPCODE
                                SEC
                                break;
                            }
                            CLC
                        }
                        case 'R':  // DROP or DROP2
                        {
                            LDY #4
                            LDA [TOKEN_L], Y
                            if (Z)  // Just DROP
                            {
                                LDA #0x0C
                                STA OPCODE
                                SEC
                                break;
                            }
                            CMP #'2'
                            if (Z)  // DROP2
                            {
                                LDA #0x0E
                                STA OPCODE
                                SEC
                                break;
                            }
                            CLC
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'A':  // ADD or ADD_BYTE
                {
                    LDY #3
                    LDA [TOKEN_L], Y
                    if (Z)  // Just ADD
                    {
                        LDA #0x14
                        STA OPCODE
                        SEC
                    }
                    else
                    {
                        CMP #'_'
                        if (Z)  // ADD_BYTE
                        {
                            LDA #0x1E
                            STA OPCODE
                            SEC
                        }
                        else
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'S':
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'U':  // SUB or SUB_BYTE
                        {
                            LDY #3
                            LDA [TOKEN_L], Y
                            if (Z)  // Just SUB
                            {
                                LDA #0x16
                                STA OPCODE
                                SEC
                            }
                            else
                            {
                                CMP #'_'
                                if (Z)  // SUB_BYTE
                                {
                                    LDA #0x20
                                    STA OPCODE
                                    SEC
                                }
                                else
                                {
                                    CLC
                                }
                            }
                            break;
                        }
                        case 'T':  // STORE_xxx
                        {
                            LDY #6
                            LDA [TOKEN_L], Y
                            switch (A)
                            {
                                case 'G':  // STORE_GLOBAL_B or STORE_GLOBAL2_B
                                {
                                    LDY #12
                                    LDA [TOKEN_L], Y
                                    if (Z)  // STORE_GLOBAL_B
                                    {
                                        LDA #0x50
                                        STA OPCODE
                                        LDA #1
                                        STA OPCODE_ARGS
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // STORE_GLOBAL2_B
                                        {
                                            LDA #0x54
                                            STA OPCODE
                                            LDA #1
                                            STA OPCODE_ARGS
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                case 'L':  // STORE_LOCAL or STORE_LOCAL2
                                {
                                    LDY #11
                                    LDA [TOKEN_L], Y
                                    if (Z)  // STORE_LOCAL
                                    {
                                        LDA #0x60
                                        STA OPCODE
                                        LDA #1
                                        STA OPCODE_ARGS
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // STORE_LOCAL2
                                        {
                                            LDA #0x62
                                            STA OPCODE
                                            LDA #1
                                            STA OPCODE_ARGS
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                case 'I':  // STORE_IND or STORE_IND2
                                {
                                    LDY #9
                                    LDA [TOKEN_L], Y
                                    if (Z)  // STORE_IND
                                    {
                                        LDA #0x68
                                        STA OPCODE
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // STORE_IND2
                                        {
                                            LDA #0x6A
                                            STA OPCODE
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                default:
                                {
                                    CLC
                                }
                            }
                            break;
                        }
                        case 'W':  // SWAP or SWAP2
                        {
                            LDY #4
                            LDA [TOKEN_L], Y
                            if (Z)  // Just SWAP
                            {
                                LDA #0x10
                                STA OPCODE
                                SEC
                            }
                            else
                            {
                                CMP #'2'
                                if (Z)  // SWAP2
                                {
                                    LDA #0x12
                                    STA OPCODE
                                    SEC
                                }
                                else
                                {
                                    CLC
                                }
                            }
                            break;
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'M':  // MUL or MOD
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'U':  // MUL
                        {
                            LDA #0x18
                            STA OPCODE
                            SEC
                        }
                        case 'O':  // MOD
                        {
                            LDA #0x1C
                            STA OPCODE
                            SEC
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'E':  // EQ
                {
                    LDA #0x26
                    STA OPCODE
                    SEC
                    break;  // Exit main loop
                }
                case 'N':  // NE or NEG
                {
                    INY
                    LDA [TOKEN_L], Y
                    CMP #'E'
                    if (Z)
                    {
                        INY
                        LDA [TOKEN_L], Y
                        if (Z)  // NE
                        {
                            LDA #0x28
                            STA OPCODE
                            SEC
                        }
                        else
                        {
                            CMP #'G'
                            if (Z)  // NEG
                            {
                                LDA #0x22
                                STA OPCODE
                                SEC
                            }
                            else
                            {
                                CLC
                            }
                        }
                    }
                    else
                    {
                        CLC
                    }
                    break;  // Exit main loop
                }
                case 'L':
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'T':  // LT
                        {
                            LDA #0x2A
                            STA OPCODE
                            SEC
                            break;
                        }
                        case 'E':  // LE
                        {
                            LDA #0x2E
                            STA OPCODE
                            SEC
                            break;
                        }
                        case 'O':  // LOAD_xxx
                        {
                            LDY #5
                            LDA [TOKEN_L], Y
                            switch (A)
                            {
                                case 'G':  // LOAD_GLOBAL_B or LOAD_GLOBAL2_B
                                {
                                    LDY #11
                                    LDA [TOKEN_L], Y
                                    if (Z)  // LOAD_GLOBAL_B
                                    {
                                        LDA #0x48
                                        STA OPCODE
                                        LDA #1
                                        STA OPCODE_ARGS
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // LOAD_GLOBAL2_B
                                        {
                                            LDA #0x4C
                                            STA OPCODE
                                            LDA #1
                                            STA OPCODE_ARGS
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                case 'L':  // LOAD_LOCAL or LOAD_LOCAL2
                                {
                                    LDY #10
                                    LDA [TOKEN_L], Y
                                    if (Z)  // LOAD_LOCAL
                                    {
                                        LDA #0x5C
                                        STA OPCODE
                                        LDA #1
                                        STA OPCODE_ARGS
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // LOAD_LOCAL2
                                        {
                                            LDA #0x5E
                                            STA OPCODE
                                            LDA #1
                                            STA OPCODE_ARGS
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                case 'I':  // LOAD_IND or LOAD_IND2
                                {
                                    LDY #8
                                    LDA [TOKEN_L], Y
                                    if (Z)  // LOAD_IND
                                    {
                                        LDA #0x64
                                        STA OPCODE
                                        SEC
                                    }
                                    else
                                    {
                                        CMP #'2'
                                        if (Z)  // LOAD_IND2
                                        {
                                            LDA #0x66
                                            STA OPCODE
                                            SEC
                                        }
                                        else
                                        {
                                            CLC
                                        }
                                    }
                                    break;
                                }
                                default:
                                {
                                    CLC
                                }
                            }
                            break;
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'G':
                {
                    INY
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'T':  // GT
                        {
                            LDA #0x2C
                            STA OPCODE
                            SEC
                        }
                        case 'E':  // GE
                        {
                            LDA #0x30
                            STA OPCODE
                            SEC
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'C':  // CALL
                {
                    LDA #0x80
                    STA OPCODE
                    LDA #1
                    STA OPCODE_ARGS
                    SEC
                    break;  // Exit main loop
                }
                case 'R':
                {
                    INY
                    LDA [TOKEN_L], Y
                    if (Z)  // Just 'R' - not valid
                    {
                        CLC
                    }
                    else
                    {
                        CMP #'E'
                        if (Z)  // RETURN or READ_BYTE
                        {
                            INY
                            LDA [TOKEN_L], Y
                            CMP #'T'
                            if (Z)  // RETURN
                            {
                                LDA #0x82
                                STA OPCODE
                                SEC
                            }
                            else
                            {
                                CMP #'A'
                                if (Z)  // READ_BYTE
                                {
                                    LDA #0x9A
                                    STA OPCODE
                                    SEC
                                }
                                else
                                {
                                    CLC
                                }
                            }
                        }
                        else
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'J':  // JUMP_xxx
                {
                    LDY #5
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'C':  // JUMP_CHAR
                        {
                            LDA #0x84
                            STA OPCODE
                            LDA #1
                            STA OPCODE_ARGS
                            SEC
                        }
                        case 'I':  // JUMP_INT
                        {
                            LDA #0x86
                            STA OPCODE
                            LDA #2
                            STA OPCODE_ARGS
                            SEC
                        }
                        case 'Z':  // JUMP_Z_CHAR
                        {
                            LDA #0x88
                            STA OPCODE
                            LDA #1
                            STA OPCODE_ARGS
                            SEC
                        }
                        case 'N':  // JUMP_NZ_CHAR
                        {
                            LDA #0x8C
                            STA OPCODE
                            LDA #1
                            STA OPCODE_ARGS
                            SEC
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                case 'I':  // INDEX_BYTE or INDEX_WORD
                {
                    LDY #6
                    LDA [TOKEN_L], Y
                    switch (A)
                    {
                        case 'B':  // INDEX_BYTE
                        {
                            LDA #0x6C
                            STA OPCODE
                            SEC
                        }
                        case 'W':  // INDEX_WORD
                        {
                            LDA #0x6E
                            STA OPCODE
                            SEC
                        }
                        default:
                        {
                            CLC
                        }
                    }
                    break;  // Exit main loop
                }
                default:
                {
                    CLC
                    break;  // Exit main loop
                }
            }
            
            break;  // Always exit after switch
        }
        
        // Single exit point - carry flag already set appropriately
    }
}
