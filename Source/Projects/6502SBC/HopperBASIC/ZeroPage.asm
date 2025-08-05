unit ZP // ZeroPage.asm
{
    // 0x00..0x1F : core to Hopper VM
    //                   - Heap Manager : FREELIST, HEAPSTART, HEAPSIZE
    //                   - Call Stack   : CSP, PC
    //                   - Value Stack  : SP, BP

    const byte PC                   = 0x00;
    const byte PCL                  = 0x00;
    const byte PCH                  = 0x01;
    
    const byte FLAGS                = 0x02;
    
    // Bit 6 - Program exited  (ended well or badly via Crash or Die)
    // Bit 3 - 8 bit SP and BP (always true?)
    // Bit 2 - TRON | TROFF
    // Bit 0 - a program has been loaded
    
    const byte SP                   = 0x03;
    const byte BP                   = 0x04;
    const byte CSP                  = 0x05;
    
    const byte FREELIST             = 0x06;
    const byte FREELISTL            = 0x06;
    const byte FREELISTH            = 0x07;
    const byte HEAPSTART            = 0x08;
    const byte HEAPSIZE             = 0x09;
    
    const byte SerialInWritePointer = 0x0A;
    const byte SerialInReadPointer  = 0x0B;
    const byte SerialBreakFlag      = 0x0C;
    
    const byte CODESTART            = 0x0D;
    const byte CODESTARTL           = 0x0D;
    const byte CODESTARTH           = 0x0E;

    const byte TraceIndent          = 0x0F; // used by Trace.asm
    
    const byte ACC                  = 0x10;
    const byte ACCL                 = 0x10;
    const byte ACCH                 = 0x11;
    
    const byte TOP                  = 0x12;
    const byte TOPL                 = 0x12;
    const byte TOPH                 = 0x13;
    
    const byte NEXT                 = 0x14;
    const byte NEXTL                = 0x14;
    const byte NEXTH                = 0x15;
    
    const byte IDX                  = 0x16;
    const byte IDXL                 = 0x16;
    const byte IDXH                 = 0x17;
    
    const byte IDY                  = 0x18;
    const byte IDYL                 = 0x18;
    const byte IDYH                 = 0x19;
    
    const byte ACCT                 = 0x1A;
    const byte TOPT                 = 0x1B;
    const byte NEXTT                = 0x1C;
    

    const byte PROGSIZE             = 0x1D;
    
    const byte I2CInWritePtr        = 0x1E;
    const byte I2CInReadPtr         = 0x1F;
    
    // 0x20 .. 0x2F
    
    // used by 'T'ime APIs Millis and Delay (these need to be updated in the .NET Emulator if changed)
    const byte TICK0                = 0x20;
    const byte TICK1                = 0x21;
    const byte TICK2                = 0x22;
    const byte TICK3                = 0x23;
    const byte TARGET0              = 0x24;
    const byte TARGET1              = 0x25;
    const byte TARGET2              = 0x26;
    const byte TARGET3              = 0x27;
    
    // dribs and draps of Hopper Runtime
    const byte WorkSpaceHexIn        = 0x28; // Serial.asm
    const byte WorkSpaceWaitForChar  = 0x29; // Serial.asm
    
    
    
    // HopperBASIC allocation: 0x30..0x3F

    // === CONSOLE INPUT (0x30) ===
    const byte BasicInputLength     = 0x30;  // Length of current input in BasicInputBuffer

    // === TOKENIZER STATE (0x31-0x34) ===
    const byte TokenBufferLength    = 0x31;  // Length of tokens in BasicTokenizerBuffer (16-bit)
    const byte TokenBufferLengthL   = 0x31;  // Low byte
    const byte TokenBufferLengthH   = 0x32;  // High byte
    const byte TokenizerPos         = 0x33;  // Current position in token buffer (16-bit)
    const byte TokenizerPosL        = 0x33;  // Low byte  
    const byte TokenizerPosH        = 0x34;  // High byte

    // === ERROR HANDLING (0x35-0x36) ===
    const byte LastErrorL           = 0x35;  // Error message pointer low byte
    const byte LastErrorH           = 0x36;  // Error message pointer high byte

    // === CURRENT TOKEN CACHE (0x37) ===
    const byte CurrentToken         = 0x37;  // Current token type/value from token buffer

    // === LITERAL POSITION TRACKING (0x38-0x39) ===
    const byte TokenLiteralPosL     = 0x38;  // Literal data position low byte
    const byte TokenLiteralPosH     = 0x39;  // Literal data position high byte

    // === JIT COMPILER STATE (0x3A-0x3F) ===
    const byte OpCodeBufferLength   = 0x3A;  // Length of opcodes in BasicOpcodeBuffer (16-bit)
    const byte OpCodeBufferLengthL  = 0x3A;  // Low byte
    const byte OpCodeBufferLengthH  = 0x3B;  // High byte
    // Use ZP.PCL/ZP.PCH for opcode execution program counter
    const byte CompilerTokenPos     = 0x3C;  // Token position during compilation (16-bit)
    const byte CompilerTokenPosL    = 0x3C;  // Low byte
    const byte CompilerTokenPosH    = 0x3D;  // High byte
    const byte CompilerFlags        = 0x3E;  // Compilation flags (bit 0: in function, etc.)
    const byte OpCodeTemp           = 0x3F;  // Temporary byte for opcode construction

    
    // === SYMBOL TABLE (0x40-0x4F) ===

    // Table Head Pointers
    const byte VariablesList        = 0x40;  // Variable (and Constant) table head pointer (16-bit)
    const byte VariablesListL       = 0x40;  // low byte
    const byte VariablesListH       = 0x41;  // high byte

    const byte FunctionsList        = 0x42;  // Function (and Argument) table head pointer (16-bit)
    const byte FunctionsListL       = 0x42;  // low byte
    const byte FunctionsListH       = 0x43;  // high byte

    // Symbol Node Working Storage (survives Memory.Allocate calls)
    const byte SymbolType           = 0x44;  // Storage for symbolType|dataType
    const byte SymbolValue          = 0x45;  // Storage for symbol value (16-bit)
    const byte SymbolValueL         = 0x45;
    const byte SymbolValueH         = 0x46;
    const byte SymbolName           = 0x47;  // Storage for symbol name pointer (16-bit)
    const byte SymbolNameL          = 0x47;
    const byte SymbolNameH          = 0x48;
    const byte SymbolTokens         = 0x49;  // Storage for symbol tokens pointer
    const byte SymbolTokensL        = 0x49;
    const byte SymbolTokensH        = 0x4A;
    const byte SymbolIteratorFilter = 0x4B;

    // Temporary Storage
    const byte SymbolLength         = 0x4C;  // Storage for symbol name length
    const byte SymbolTemp0          = 0x4D;  // General temporary storage
    const byte SymbolTemp1          = 0x4E;  // General temporary storage
    const byte SymbolTemp2          = 0x4F;  // General temporary storage

    const byte EmulatorPCL          = 0x50;  // BIT this address to lock the current PC into this and the next slot (update in .NET emulator if changed)
    const byte EmulatorPCH          = 0x51;
    const byte TraceMessageL        = 0x52;  // used by Trace.asm
    const byte TraceMessageH        = 0x53;
    const byte SystemState          = 0x54;  // Success, Failure, Exiting
    
    
    
    
    
    // 0x30 .. 0x3F
    // used by 'M'emory manager functions : Memory.Allocate and Memory.Free
    const byte M0                   = 0x60;
    const byte M1                   = 0x61;
    const byte M2                   = 0x62;
    const byte M3                   = 0x63;
    const byte M4                   = 0x64;
    const byte M5                   = 0x65;
    const byte M6                   = 0x66;
    const byte M7                   = 0x67;
    const byte M8                   = 0x68;
    const byte M9                   = 0x69;
    const byte M10                  = 0x6A;
    const byte M11                  = 0x6B;
    const byte M12                  = 0x6C;
    const byte M13                  = 0x6D;
    const byte M14                  = 0x6E;
    const byte M15                  = 0x6F;
    
    // used by Debug.asm (no calls to Memory.Allocate or Memory.Free)
    const byte DB0                  = M0;
    const byte DB1                  = M1;
    const byte DB2                  = M2;
    const byte DB3                  = M3;
    const byte DB4                  = M4;
    const byte DB5                  = M5;
    const byte DB6                  = M6;
    const byte DB7                  = M7;
    const byte DB8                  = M8;
    const byte DB9                  = M9;
    const byte DB10                 = M10;
    const byte DB11                 = M11;
    const byte DB12                 = M12;
    const byte DB13                 = M13;
    const byte DB14                 = M14;
    const byte DB15                 = M15;
    
    // Potential cross-over with Hopper VM APIs
    const byte FSOURCEADDRESS  = 0x80;
    const byte FSOURCEADDRESSL = 0x80;
    const byte FSOURCEADDRESSH = 0x81;
    
    const byte FDESTINATIONADDRESS  = 0x82;
    const byte FDESTINATIONADDRESSL = 0x82;
    const byte FDESTINATIONADDRESSH = 0x83;

    const byte FLENGTH  = 0x84;
    const byte FLENGTHL = 0x84;
    const byte FLENGTHH = 0x85;
    
    const byte LCURRENT  = 0x86;
    const byte LCURRENTL = 0x86;
    const byte LCURRENTH = 0x87;
    
    const byte LHEAD  = 0x88;
    const byte LHEADL = 0x88;
    const byte LHEADH = 0x89;
    
    const byte FSIGN = 0x8A;
    
    const byte LHEADX = 0x8B;
    
    const byte LPREVIOUS  = 0x8C;
    const byte LPREVIOUSL = 0x8C;
    const byte LPREVIOUSH = 0x8D;
    
    const byte LNEXT  = 0x8E;
    const byte LNEXTL = 0x8E;
    const byte LNEXTH = 0x8F;

    // used by IntMath.asm
    const byte UWIDE0 = 0x90;
    const byte UWIDE1 = 0x91;
    const byte UWIDE2 = 0x92;
    const byte UWIDE3 = 0x93;
    // used by Time.asm (Seconds) - calls Long.DivMod()
    const byte LNEXT0 = 0x94;
    const byte LNEXT1 = 0x95;
    const byte LNEXT2 = 0x96;
    const byte LNEXT3 = 0x97;
    const byte LTOP0  = 0x98;
    const byte LTOP1  = 0x99;
    const byte LTOP2  = 0x9A;
    const byte LTOP3  = 0x9B;
    const byte LRESULT0 = 0x9C;
    const byte LRESULT1  = 0x9D;
    const byte LRESULT2  = 0x9E;
    const byte LRESULT3  = 0x9F;
    const byte LRESULT4  = 0xA0;
    const byte LRESULT5  = 0xA1;
    const byte LRESULT6  = 0xA2;
    const byte LRESULT7  = 0xA3;
    

    
    
    
    // Hardware (CPLD constrained)
    // 0xEC..0xFF

    // Motorola 6850 ACIA
    const byte ACIACONTROL          = 0xEC; //0x1E;
    const byte ACIASTATUS           = 0xEC; //0x1E;
    const byte ACIADATA             = 0xED; //0x1F;
    
    const byte PORTB                = 0xF0;
    const byte PORTA                = 0xF1;
    const byte DDRB                 = 0xF2;
    const byte DDRA                 = 0xF3;
    const byte T1CL                 = 0xF4; // Timer 1 counter low
    const byte T1CH                 = 0xF5; // Timer 1 counter high

    const byte T1LL                 = 0xF6; // Timer 1 Latch Low
    const byte T1LH                 = 0xF7; // Timer 1 Latch High
    const byte T2CL                 = 0xF8; // Timer 2 Counter Low
    const byte T2CH                 = 0xF9; // Timer 2 Counter High
    const byte SR                   = 0xFA; // Shift Register

    const byte ACR                  = 0xFB; // Auxiliary Control Register
    const byte PCR                  = 0xFC; // Peripheral Control Register
    const byte IFR                  = 0xFD; // Interrupt Flag Register
    const byte IER                  = 0xFE; // Interrupt Enable Register
    
    const byte ORA_NO_HANDSHAKE     = 0xFF; // Output Register A with no handshake


}
