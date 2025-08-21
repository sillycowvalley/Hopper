program EEPROMTest
{
    
    #define ROM_48K
    #define CPU_65C02S
    #define HOPPER_BASIC
    
    #define HASEEPROM
    #define TRACEFILE
    
    #define DEBUG
    
    uses "./Definitions/ZeroPage"
    uses "./Definitions/Limits"
    uses "./Definitions/MemoryMap"
    uses "./Definitions/BASICTypes"
    uses "./Definitions/Tokens"
    uses "./Definitions/States"
    uses "./Definitions/Messages"
    
    uses "./Debugging/Error"
    uses "./Debugging/Debug"
    uses "./Debugging/Trace"
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Parallel"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/I2C"
    uses "/Source/Runtime/6502/Time"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "./Files/EEPROM"
    uses "./Files/File"
    
    uses "./Objects/Table"
    uses "./Objects/Objects"
    uses "./Objects/String"
    uses "./Objects/Char"
    uses "./Objects/Long"
    uses "./Objects/Variables"
    uses "./Objects/Functions"
    uses "./Objects/Array"
    
    uses "./Utilities/Print"
    uses "./Utilities/Tools"
    
    
    
     // String constants for test output
    const string msgStarting         = "EEPROM Test Program Starting...\n\n";
    const string msgInitializing     = "Initializing EEPROM...\n";
    const string msgInitializeFailed = "Initialize() failed\n";
    const string msgDetecting        = "Detecting EEPROM...";
    const string msgDetected         = " DETECTED\n";
    const string msgNotFound         = " NOT FOUND\n";
    const string msgErrorNoEEPROM    = "ERROR: No EEPROM detected. Test aborted.\n";
    const string msgSize             = "EEPROM Size: ";
    const string msgKBytes           = "K bytes\n\n";
    const string msgUnknown          = "Unknown\n\n";
    const string msgComplete         = "\n=== Test Complete ===\n";
    
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
        Parallel.ISR();
    }
    
    NMI()
    {
        // Hardware break - could be used for BASIC BREAK functionality
        SMB0 ZP.SerialBreakFlag
    }
    
    Initialize()
    {
        // Clear Zero Page
        LDX #0
        loop
        {
            CPX # ZP.ACIADATA // don't write to ACIA data register
            if (NZ) 
            {
                STZ 0x00, X
            }
            DEX
            if (Z) { break; }
        } 
        
        Error.ClearError();
        States.SetSuccess();
        Trace.Initialize(); 
        
        // Clear serial buffer
        LDA # (SerialInBuffer >> 8)
        Memory.ClearPage();
        
        Serial.Initialize();
        Parallel.Initialize();
        
        // Initialize Hopper VM runtime components
        Memory.InitializeHeapSize();
        Stacks.Initialize();
        
    }
    
    const string TestFileSieve  = "TEST";
    //const string TestFileSieve  = "SIEVE";
    const string TestFileNone   = "NONE";
    
    LoadAndDisplay()
    {
Debug.NL(); Print.String();
Debug.NL();         
        File.StartLoad();
        if (C)
        {
            loop
            {
Debug.NL();                
                File.NextStream();
                if (NC) 
                { 
                    States.IsFailure();
                    if (C)
                    {
                        Error.CheckAndPrint();
                    }
                    break;
                }
                
                // Set up pointer to returned data
                LDA SectorSourceL
                STA ZP.IDXL
                LDA SectorSourceH  
                STA ZP.IDXH
                
                // Use TransferLength as countdown (we're allowed to munt it)
                loop
                {
                    // Check if done (16-bit zero test)
                    LDA TransferLengthL
                    ORA TransferLengthH
                    if (Z) { break; }
                    
                    // Print character
                    LDY #0
                    LDA [ZP.IDX], Y
                    Debug.Printable();
                    
                    // Increment pointer
                    INC ZP.IDXL
                    if (Z) { INC ZP.IDXH }
                    
                    // Decrement count (16-bit)
                    LDA TransferLengthL
                    if (Z) 
                    {
                        DEC TransferLengthH 
                    }
                    DEC TransferLengthL
                }     
            } // loop
        }
        else
        {
            Error.CheckAndPrint();
        }
    }
    
    // Main entry point
    Hopper()
    {
        
        Initialize();
        
        
        
        LDA #(msgStarting % 256)
        STA ZP.STRL
        LDA #(msgStarting / 256)
        STA ZP.STRH
        Print.String();
        
        // Initialize EEPROM system
        LDA #(msgInitializing % 256)
        STA ZP.STRL
        LDA #(msgInitializing / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.Initialize();
        if (NC)
        {
            LDA #(msgInitializeFailed % 256)
            STA ZP.STRL
            LDA #(msgInitializeFailed / 256)
            STA ZP.STRH
            Print.String();
        }
        
        // Test EEPROM detection
        LDA #(msgDetecting % 256)
        STA ZP.STRL
        LDA #(msgDetecting / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.Detect();
        if (C)  // Set C (detected)
        {
            LDA #(msgDetected % 256)
            STA ZP.STRL
            LDA #(msgDetected / 256)
            STA ZP.STRH
            Print.String();
        }
        else    // Set NC (not detected)
        {
            LDA #(msgNotFound % 256)
            STA ZP.STRL
            LDA #(msgNotFound / 256)
            STA ZP.STRH
            Print.String();
            
            LDA #(msgErrorNoEEPROM % 256)
            STA ZP.STRL
            LDA #(msgErrorNoEEPROM / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Get and display EEPROM size
        LDA #(msgSize % 256)
        STA ZP.STRL
        LDA #(msgSize / 256)
        STA ZP.STRH
        Print.String();
        
        EEPROM.GetSize();
        if (C)  // Set C (size retrieved)
        {
            Serial.HexOut();
            LDA #(msgKBytes % 256)
            STA ZP.STRL
            LDA #(msgKBytes / 256)
            STA ZP.STRH
            Print.String();
        }
        else    // Set NC (size unknown)
        {
            LDA #(msgUnknown % 256)
            STA ZP.STRL
            LDA #(msgUnknown / 256)
            STA ZP.STRH
            Print.String();
        }
        File.Dir();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        File.DumpDriveState();
        
        //SMB2 ZP.FLAGS  // TRON
        RMB2 ZP.FLAGS  // TROFF
        
        
        LDA #(TestFileSieve % 256)
        STA ZP.STRL
        LDA #(TestFileSieve / 256)
        STA ZP.STRH
        Print.String();
        LoadProgram();
        if (NC)
        {
            Error.CheckAndPrint();
        }
        
        
        

        // To make EEPROM.WritePage be included in the build:
        LDA #(TestFileNone % 256)
        STA ZP.STRL
        LDA #(TestFileNone / 256)
        STA ZP.STRH
        File.Delete();
        
        LDA #(msgComplete % 256)
        STA ZP.STRL
        LDA #(msgComplete / 256)
        STA ZP.STRH
Debug.NL(); Print.String();        
        
        
       
        loop { }
    }
    
    DumpBuffers()
    {
        PHP
        
Debug.NL();
Debug.NL(); HOut(); // last token
Debug.NL(); LDA LoadBufferIndexH HOut(); LDA LoadBufferIndexL HOut(); Space(); 
            LDA LoadBufferPosH HOut(); LDA LoadBufferPosL HOut(); Space(); 
            LDA LoadBufferLengthH HOut(); LDA LoadBufferLengthL HOut(); Space();
Debug.NL();            

        LDA #(LoadBuffer / 256)
        Debug.DumpPage();
        
        LDA #(LoadBuffer / 256 + 1)
        Debug.DumpPage();
        
        Debug.DumpHeap();
        
        PLP
    }
    
    // Combined buffer: TokenizerBuffer (512) + FunctionOpCodeBuffer (512) = 1024 bytes  
    const uint LoadBuffer     = Address.TokenizerBuffer;
    const uint LoadBufferSize = Limits.TokenizerBufferSize + Limits.OpCodeBufferSize;
    const uint RefillTrigger = 128;  // Refill when current position passes this point (at the end of the current token)
    
    // ZP slots ZP.SS0..ZP.SS9 available
    const byte LoadBufferPos     = ZP.SS0; // 0..(LoadBufferLength-1)
    const byte LoadBufferPosL    = ZP.SS0;
    const byte LoadBufferPosH    = ZP.SS1;
    
    const byte LoadBufferIndex   = ZP.SS2; // (LoadBuffer + LoadBufferPos)
    const byte LoadBufferIndexL  = ZP.SS2;
    const byte LoadBufferIndexH  = ZP.SS3;
    
    const byte LoadBufferLength  = ZP.SS4; // 0..1024
    const byte LoadBufferLengthL = ZP.SS4;
    const byte LoadBufferLengthH = ZP.SS5;
    
    const byte LoaderFlags       = ZP.SS6;
    
    
    // Load program from EEPROM file
    // Input: ZP.STR = filename
    // Output: C set if successful, program state restored
    LoadProgram()
    {
        loop // Single exit block
        {
            // 1. Clear current program state (like NEW)
            Variables.Clear();
            Functions.Clear();
            
            // 2. Open file for reading
            File.StartLoad(); // Input: ZP.STR = filename
            if (NC) { break; }
            
            // 3. Set up sliding window buffer and TokenIterator
            setupSlidingWindowBuffer();
            if (NC) { break; }
            
            STZ LoaderFlags
            // Bit 0 - set if no more data to read
            // Bit 1 - set if we've seen CONST
            // Bit 2 - set if we're in $MAIN
            
            // 4. parse and build objects
            loop
            {
                nextToken();
                switch (A)
                {
                    case Token.FUNC:
                    {
                        SMB2 ZP.FLAGS  // TRON
                        
                        parseFunctionHeader();
                        if (NC) { break; }
                        parseFunctionOrMain();
                        if (NC) { break; }
                    }
                    case Token.BEGIN:
                    {
                        SMB2 ZP.FLAGS  // TRON
                        
                        parseMainHeader();   
                        if (NC) { break; }
                        SMB2 LoaderFlags
                        parseFunctionOrMain();
                        if (NC) { break; }
                    }
                    case Token.CONST:
                    {
                        SMB1 LoaderFlags
                    }
                    case Token.VAR:
                    case Token.BIT:
                    case Token.INT:
                    case Token.BYTE:
                    case Token.WORD:
                    case Token.CHAR:
                    case Token.STRING:
                    {
                        parseVariableOrConst();
                        if (NC) { break; }
                    }
                    case Token.EOF:
                    {
                        SEC
                        break;
                    }
                    default:
                    {
                        DumpBuffers();
                        TODO(); BIT ZP.EmulatorPCL CLC // what's this?
                        break;
                    }
                } // switch
            } // loop
            break;
        } // single exit
        
        // 5. initialize the global variables and constants
        DumpBuffers();
        
        // Console.InitializeGlobals(); // TODO
        
        RMB2 ZP.FLAGS  // TROFF
    }
    
    nextToken()
    {
        LDA [LoadBufferIndex]
        INC LoadBufferPosL
        if (Z)
        {
            INC LoadBufferPosH
        }
        INC LoadBufferIndexL
        if (Z)
        {
            INC LoadBufferIndexH
        }
    }
    peekToken()
    {
        LDA [LoadBufferIndex]
    }

    const string parseIdentifierTrace = "parseIdentifier";        
    parseIdentifier()
    {
#ifdef TRACEFILE
        LDA #(parseIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(parseIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            nextToken();
            CMP # Token.IDENTIFIER
            if (NZ) { Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // IDENTIFIER expected
            
            LDA LoadBufferIndexL
            STA ZP.STRL
            LDA LoadBufferIndexH
            STA ZP.STRH
            
            nextToken();
            Char.IsAlpha();
            if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // <alpha> expected
            loop
            {
                nextToken();
                CMP #0
                if (Z)
                {
                    // null terminator
                    SEC 
                    break; 
                }
                Char.IsAlphaNumeric();
                if (NC) {  Error.InternalError(); BIT ZP.EmulatorPCL CLC break; } // <alphanumeric> expected
            }
            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseIdentifierTrace % 256) STA ZP.TraceMessageL LDA #(parseIdentifierTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // terminator in ZP.ACCL
    const string parseTokenStreamTrace = "parseTokenStream";
    parseTokenStream() // -> IDY
    {
#ifdef TRACEFILE
        LDA #(parseTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(parseTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        loop
        {
            LDA LoadBufferIndexL
            STA ZP.FSOURCEADDRESSL
            LDA LoadBufferIndexH
            STA ZP.FSOURCEADDRESSH
            loop
            {
                nextToken();
                CMP ZP.ACCL
                if (Z) { break; }
            }
            // LoadBufferIndex - 1 = terminator : EOE, RBRACKET ..
            SEC
            LDA LoadBufferIndexL
            SBC # 1
            STA ZP.FLENGTHL
            LDA LoadBufferIndexH
            SBC # 0
            STA ZP.FLENGTHH
            LDA # Token.EOF
            STA [ZP.FLENGTH] // patch terminator -> EOF       
            
            SEC 
            LDA ZP.FLENGTHL
            SBC ZP.FSOURCEADDRESSL
            STA ZP.FLENGTHL
            LDA ZP.FLENGTHH
            SBC ZP.FSOURCEADDRESSH
            STA ZP.FLENGTHH
            
            IncLENGTH();
            
            LDA ZP.FLENGTHL
            STA ZP.ACCL
            LDA ZP.FLENGTHH
            STA ZP.ACCH
            
            Memory.Allocate();
            
            LDA ZP.IDXL
            STA ZP.FDESTINATIONADDRESSL
            STA ZP.IDYL
            LDA ZP.IDXH
            STA ZP.FDESTINATIONADDRESSH
            STA ZP.IDYH
            
            Memory.Copy();
            
            SEC
            break;
        } // single exit
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
#ifdef TRACEFILE
        PHP PHA LDA #(parseTokenStreamTrace % 256) STA ZP.TraceMessageL LDA #(parseTokenStreamTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }               
    
    // A is type Token
    const string parseVariableOrConstTrace = "parseVariableOrConst";
    parseVariableOrConst()
    {
#ifdef TRACEFILE
        PHA LDA #(parseVariableOrConstTrace % 256) STA ZP.TraceMessageL LDA #(parseVariableOrConstTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry(); PLA
#endif
        loop
        {
            TAX
            BASICTypes.FromToken(); // X -> A
            if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // <type> expected
            if (BBS1, LoaderFlags)
            {
                ORA # SymbolType.CONSTANT
            }
            else
            {
                ORA # SymbolType.VARIABLE
            }
            STA ZP.ACCT
            
            // name
            parseIdentifier();
            if (NC) { break; }
            
             // initial value
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            nextToken();
            CMP # Token.EOL
            if (Z)
            {
                nextToken();
            }
            switch (A)
            {
                case Token.LBRACKET:
                {
                    // not EOE, create dimension stream in IDY
                    LDA # Token.RBRACKET // terminator
                    STA ZP.ACCL
                    parseTokenStream(); // -> IDY
                    if (NC) { break; }

                    nextToken();
                    CMP # Token.EOE // RBRACKET already converted to EOF
                    if (NC) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // <type> expected

                    LDA ZP.ACCT
                    ORA # BASICType.ARRAY
                    STA ZP.ACCT
                    
                    // place holder size for array until it is initialized
                    LDA #10
                    STA ZP.NEXTL
                    CLC
                }
                case Token.EOE:
                {
                    // no initializer stream
                    STZ ZP.IDYL
                    STZ ZP.IDYH
                    
                    LDA ZP.ACCT
                    AND # BASICType.TYPEMASK
                    CMP # BASICType.STRING
                    if (Z)
                    {
                        // STRING default: allocate copy of EmptyString
                        LDA #(Variables.EmptyString % 256)
                        STA ZP.TOPL
                        LDA #(Variables.EmptyString / 256)
                        STA ZP.TOPH
                        
                        Variables.AllocateAndCopyString(); // Input: ZP.TOP = source, Output: ZP.IDY = allocated copy
                        CheckError();
                        if (NC) { break; } // allocation failed
                        
                        // Use allocated copy as the variable value
                        LDA ZP.IDYL
                        STA ZP.NEXTL
                        LDA ZP.IDYH
                        STA ZP.NEXTH
                    }
                }
                case Token.EQUALS:
                {
                    // not EOE or ARRAY: create new initializer stream in IDY
                    LDA # Token.EOE // terminator
                    STA ZP.ACCL
                    parseTokenStream(); // -> IDY
                    if (NC) { break; }
                }
                default:
                {
                    TODO(); BIT ZP.EmulatorPCL CLC // what's this?
                    break;
                }
            } // switch

            RMB1 LoaderFlags
            
            // name from IDENTIFIER
            LDA ZP.STRL
            STA ZP.TOPL
            LDA ZP.STRH
            STA ZP.TOPH
            
            Variables.Declare(); // -> C or NC
            CheckError();
            if (NC)
            {
                 Error.InternalError(); BIT ZP.EmulatorPCL CLC // what's this?
            }
            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseVariableOrConstTrace % 256) STA ZP.TraceMessageL LDA #(parseVariableOrConstTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    const string parseMainHeaderTrace = "parseMainHeader";
    parseMainHeader()
    {
#ifdef TRACEFILE
        LDA #(parseMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            // argument list:
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            LDA #(Messages.BeginFunctionName % 256)
            STA ZP.TOPL
            LDA #(Messages.BeginFunctionName / 256)
            STA ZP.TOPH
Debug.NL(); PrintStringTOP();
                        
            STZ ZP.IDYL
            STZ ZP.IDYH
            Functions.Declare();
            CheckError();
Debug.NL(); XOut();
            break;
        } // single exit
#ifdef TRACEFILE
        PHP PHA LDA #(parseMainHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseMainHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }             
    
    const string parseFunctionHeaderTrace = "parseFunctionHeader";
    parseFunctionHeader()
    {
#ifdef TRACEFILE
        LDA #(parseFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        loop
        {
            // name
            parseIdentifier();
            if (NC) { break; }
            
            // name from IDENTIFIER
            LDA ZP.STRL
            STA ZP.TOPL
            LDA ZP.STRH
            STA ZP.TOPH
Debug.NL(); Print.String();
            
            // create argument list in ZP.NEXT
            STZ ZP.NEXTL
            STZ ZP.NEXTH
            
            STZ ZP.IDYL
            STZ ZP.IDYH
            Functions.Declare();
            CheckError(); 
            if (NC) { break; }
Debug.NL(); XOut();                       
            nextToken();
            CMP # Token.LPAREN
            if (NZ) { Error.InternalError(); BIT ZP.EmulatorPCL break; } // ( expected
            loop
            {
Debug.NL(); LDA #'a' COut(); Space();
                
                peekToken();
HOut();                
                CMP # Token.RPAREN
                if (Z)
                {
                    nextToken(); // consume )
                    break;
                }
                CMP # Token.COMMA
                if (Z)
                {
                    nextToken(); // consume ,
                }
Debug.NL(); LDA #'b' COut(); 

                // argument
                parseIdentifier();
                
                // argument from IDENTIFIER
                LDA ZP.STRL
                STA ZP.TOPL
                LDA ZP.STRH
                STA ZP.TOPH
                
Debug.NL(); XOut();
                LDA #SymbolType.ARGUMENT
                ORA #BASICType.VAR 
                STA ZP.SymbolType // argument for Locals.Add()
                Locals.Add();
                CheckError();
                if (NC) { break; }
            }
            if (NC) { break; }
                        
            break;
        }
#ifdef TRACEFILE
        PHP PHA LDA #(parseFunctionHeaderTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionHeaderTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }

    const string parseFunctionOrMainTrace = "parseFunctionOrMain";
    parseFunctionOrMain()
    {
#ifdef TRACEFILE
        LDA #(parseFunctionOrMainTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionOrMainTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif

        loop
        {
Debug.NL(); XOut();
            if (BBR0, LoaderFlags) // have not seen EOF yet
            {
                slideWindow();
                if (NC) { break; }
            }
            
Debug.NL(); XOut();            

            // create function token stream in IDY
            LDA # Token.EOE // terminator
            STA ZP.ACCL
            parseTokenStream(); // -> IDY
            if (NC) { break; }
            
Debug.NL(); XOut(); YOut();
            Functions.SetBody();
            break;
        }
        RMB2 LoaderFlags
        
#ifdef TRACEFILE
        PHP PHA LDA #(parseFunctionOrMainTrace % 256) STA ZP.TraceMessageL LDA #(parseFunctionOrMainTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // 1. Move the remaining content to the front of the buffer
    // 2. Try to fill buffer with >= 512 bytes of data
    const string slideWindowTrace = "slideWindow";
    slideWindow() 
    {
#ifdef TRACEFILE
        LDA #(slideWindowTrace % 256) STA ZP.TraceMessageL LDA #(slideWindowTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // slide
        LDA #(LoadBuffer % 256)
        STA ZP.FDESTINATIONADDRESSL
        LDA #(LoadBuffer / 256) 
        STA ZP.FDESTINATIONADDRESSH
        
        CLC
        LDA ZP.FDESTINATIONADDRESSL
        ADC LoadBufferPosL
        STA ZP.FSOURCEADDRESSL
        LDA ZP.FDESTINATIONADDRESSH
        ADC LoadBufferPosH
        STA ZP.FSOURCEADDRESSH
        
        SEC
        LDA LoadBufferLengthL
        SBC LoadBufferPosL
        STA LoadBufferLengthL
        LDA LoadBufferLengthH
        SBC LoadBufferPosH
        STA LoadBufferLengthH
        
        LDA #(LoadBuffer % 256)
        STA LoadBufferIndexL
        LDA #(LoadBuffer / 256)
        STA LoadBufferIndexH
        STZ LoadBufferPosL
        STZ LoadBufferPosH
        
        LDA LoadBufferLengthL
        STA ZP.FLENGTHL
        LDA LoadBufferLengthH
        STA ZP.FLENGTHH
        
        Memory.Copy();
        
        // fill
        loop
        {
            if (BBS0, LoaderFlags)       { SEC break; } // no more data to read
            if (BBS0, LoadBufferLengthH) { SEC break; } // current data >= 512 bytes
            File.NextStream();
            if (NC) 
            { 
                States.IsSuccess();
                if (C)
                {
                    // no content, end of file
                    SEC
                    SMB0 LoaderFlags
                }
                break; 
            }
            appendSectorToBuffer();
        }
        
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
#ifdef TRACEFILE
        PHP LDA #(slideWindowTrace % 256) STA ZP.TraceMessageL LDA #(slideWindowTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLP
#endif
    }
    
    // Set up 1024-byte sliding window buffer and configure TokenIterator
    // Output: C set if successful, NC if file empty or error
    const string setupSlidingWindowBufferTrace = "setupSlidingWindowBuffer";
    setupSlidingWindowBuffer()
    {
#ifdef TRACEFILE
        LDA #(setupSlidingWindowBufferTrace % 256) STA ZP.TraceMessageL LDA #(setupSlidingWindowBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Initialize buffer state
        STZ LoadBufferPosL
        STZ LoadBufferPosH
        STZ LoadBufferLengthL
        STZ LoadBufferLengthH
        LDA #(LoadBuffer % 256)
        STA LoadBufferIndexL
        LDA #(LoadBuffer / 256)
        STA LoadBufferIndexH
        
        // Load up first sectors to fill buffer
        LDX #1 // #4
        loop
        {
            File.NextStream();
            if (NC) 
            { 
                States.IsSuccess();
                if (C)
                { 
                    // no content, end of file
                    SMB0 LoaderFlags
                }
                break; 
            }
            appendSectorToBuffer();
            
            DEX
            if (Z) { break; } // Buffer full
        }
        
        // Check if we loaded any data
        LDA LoadBufferLengthL
        ORA LoadBufferLengthH
        if (Z) { CLC return; } // Empty file
        
        SEC // Success
#ifdef TRACEFILE
        PHP PHA LDA #(setupSlidingWindowBufferTrace % 256) STA ZP.TraceMessageL LDA #(setupSlidingWindowBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit(); PLA PLP
#endif
    }
    
    // Append current sector to load buffer
    // Input: File.NextStream() result available
    // Output: Data appended, LoadContent updated
    const string appendSectorToBufferTrace = "appendSectorToBuffer";
    appendSectorToBuffer()
    {
#ifdef TRACEFILE
        LDA #(appendSectorToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendSectorToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
        // Calculate destination: LoadBuffer + LoadContent
        CLC
        LDA #(LoadBuffer % 256)
        ADC LoadBufferLengthL
        STA ZP.FDESTINATIONADDRESSL
        LDA #(LoadBuffer / 256) 
        ADC LoadBufferLengthH
        STA ZP.FDESTINATIONADDRESSH
        
        // Source = FileDataBuffer
        LDA #(File.FileDataBuffer % 256)
        STA ZP.FSOURCEADDRESSL
        LDA #(File.FileDataBuffer / 256)
        STA ZP.FSOURCEADDRESSH
        
        // Length = File.TransferLength
        LDA File.TransferLengthL
        STA ZP.FLENGTHL
        LDA File.TransferLengthH
        STA ZP.FLENGTHH
        
        // Input: ZP.FSOURCEADDRESS = source pointer
        //        ZP.FDESTINATIONADDRESS = destination pointer  
        //        ZP.FLENGTH = number of bytes to copy (16-bit)
        Memory.Copy();
        
        // Update buffer content length (16-bit add)
        CLC
        LDA LoadBufferLengthL
        ADC File.TransferLengthL
        STA LoadBufferLengthL
        LDA LoadBufferLengthH
        ADC File.TransferLengthH
        STA LoadBufferLengthH
#ifdef TRACEFILE
        LDA #(appendSectorToBufferTrace % 256) STA ZP.TraceMessageL LDA #(appendSectorToBufferTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    }
    
    
    
    
}
