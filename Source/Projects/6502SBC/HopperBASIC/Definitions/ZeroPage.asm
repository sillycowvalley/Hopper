unit ZP // ZeroPage.asm
{
    // ==============================================================================
    // HOPPER 6502 ZERO PAGE ALLOCATION MAP
    // ==============================================================================
    //
    // CRITICAL: The following zero page locations are IMMOVABLE and must not be changed:
    //
    // 0x00-0x05: CORE VM REGISTERS (Architecture Dependent)
    //   - PC (0x00-0x01): Program counter - core to instruction execution
    //   - FLAGS (0x02): System flags register - bit positions are API contracts
    //   - SP, BP, CSP (0x03-0x05): Stack pointers - assumed by all stack operations
    //
    // 0x22-0x25: EMULATOR INTERFACE (Hardcoded in C# Emulator) 
    //   - TICK0-3 (0x22-0x25): Timer tick counters [SHIFTED +2 from original 0x20-0x23]
    //     * Reading TICK0 triggers snapshot of all 4 bytes in emulator
    //     * Reading TICK3 triggers snapshot of all 4 bytes in emulator  
    //   - EmulatorPCL/H (0x1D-0x1E): PC capture for debugging
    //     * BIT $1D instruction triggers PC capture in emulator
    //     * Used by Debug.asm for crash dumps and breakpoints
    //
    // 0xEC-0xFF: HARDWARE I/O (Platform Hardware Addresses)
    //   - ACIA registers (0xEC-0xED): Serial communication
    //   - VIA registers (0xF0-0xFF): Parallel I/O, timers, interrupts
    //   - These are physical hardware addresses, not arbitrary choices
    //
    // All other zero page locations can be reorganized as needed for optimization.
    // When modifying this file, ensure these critical addresses remain unchanged.
    // ==============================================================================

    // CORE VM REGISTERS (IMMOVABLE)
    const byte PC                   = 0x00;  // Program counter low
    const byte PCL                  = 0x00;  // Program counter low (alias)
    const byte PCH                  = 0x01;  // Program counter high
    
    const byte FLAGS                = 0x02;  // System flags register
    // FLAGS bits:
    // Bit 7 - (unused in BASIC)
    // Bit 6 - (unused in BASIC)
    // Bit 5 - (unused in BASIC)
    // Bit 4 - (unused in BASIC)
    // Bit 3 - (reserved for REPL mode flag - not currently in use)
    // Bit 2 - TRON | TROFF (trace on/off)
    // Bit 1 - Temporary exit flag for Console.processTokens() loop control
    // Bit 0 - Program has been loaded
    
    const byte SP                   = 0x03;  // Stack pointer
    const byte BP                   = 0x04;  // Base pointer
    const byte CSP                  = 0x05;  // Call stack pointer

    // VM HEAP & STACK MANAGEMENT
    const byte FREELIST             = 0x06;  // Heap free list pointer
    const byte FREELISTL            = 0x06;  // Free list low (alias)
    const byte FREELISTH            = 0x07;  // Free list high
    const byte HEAPSTART            = 0x08;  // Heap start page
    const byte HEAPSIZE             = 0x09;  // Heap size in pages
    
    const byte SerialInWritePointer = 0x0A;  // Serial buffer write position
    const byte SerialInReadPointer  = 0x0B;  // Serial buffer read position
    const byte SerialBreakFlag      = 0x0C;  // Serial break detected flag

    const byte TraceIndent          = 0x0D;  // Debug trace indentation level

    // VM Working Registers (heavily used - "fighting over 12 bytes")
    const byte ACC                  = 0x0E;  // Accumulator
    const byte ACCL                 = 0x0E;  // Accumulator low (alias)
    const byte ACCH                 = 0x0F;  // Accumulator high
    
    const byte TOP                  = 0x10;  // Top of stack value
    const byte TOPL                 = 0x10;  // Top low (alias)
    const byte TOPH                 = 0x11;  // Top high
    
    const byte NEXT                 = 0x12;  // Next stack value
    const byte NEXTL                = 0x12;  // Next low (alias)
    const byte NEXTH                = 0x13;  // Next high
    
    const byte IDX                  = 0x14;  // Index register X
    const byte IDXL                 = 0x14;  // Index X low (alias)
    const byte IDXH                 = 0x15;  // Index X high
    
    const byte IDY                  = 0x16;  // Index register Y
    const byte IDYL                 = 0x16;  // Index Y low (alias)
    const byte IDYH                 = 0x17;  // Index Y high
    
    const byte ACCT                 = 0x18;  // Accumulator type
    const byte TOPT                 = 0x19;  // Top type
    const byte NEXTT                = 0x1A;  // Next type
    
    const byte I2CInWritePtr        = 0x1B;  // I2C buffer write pointer
    const byte I2CInReadPtr         = 0x1C;  // I2C buffer read pointer

    const byte EmulatorPCL          = 0x1D;  // BIT this to capture PC
    const byte EmulatorPCH          = 0x1E;  // Captured PC high byte
    
    // 0x1F: Available
    
    const byte JumpTableLSB         = 0x20;  // RESERVED - Jump table LSB
    const byte JumpTableMSB         = 0x21;  // RESERVED - Jump table MSB
    
    // EMULATOR INTERFACE (IMMOVABLE - shifted +2 from original)
    const byte TICK0                = 0x22;  // Timer tick byte 0 (LSB)
    const byte TICK1                = 0x23;  // Timer tick byte 1
    const byte TICK2                = 0x24;  // Timer tick byte 2
    const byte TICK3                = 0x25;  // Timer tick byte 3 (MSB)
    
    // MISC VM WORKSPACE
    const byte WorkSpaceHexIn       = 0x26;  // Serial.asm hex input workspace
    const byte WorkSpaceWaitForChar = 0x27;  // Serial.asm wait workspace
    
    // HOPPER BASIC CORE - TOKENIZER & COMPILER
    const byte BasicInputLength     = 0x28;  // Input buffer character count
    
    const byte TokenBufferContentSize    = 0x29;  // current Token buffer content size (16-bit)
    const byte TokenBufferContentSizeL   = 0x29;  // Token buffer content size low (alias)
    const byte TokenBufferContentSizeH   = 0x2A;  // Token buffer content size high
    const byte TokenizerPos         = 0x2B;  // Current tokenizer position (16-bit)
    const byte TokenizerPosL        = 0x2B;  // Tokenizer pos low (alias)  
    const byte TokenizerPosH        = 0x2C;  // Tokenizer pos high

    const byte LastErrorL           = 0x2D;  // Error message pointer low
    const byte LastErrorH           = 0x2E;  // Error message pointer high

    const byte CurrentToken         = 0x2F;  // Cached current token type

    const byte TokenLiteralPosL     = 0x30;  // Literal data position low
    const byte TokenLiteralPosH     = 0x31;  // Literal data position high

    const byte OpCodeBufferContentSize   = 0x32;  // size of content in JIT buffer (16-bit)
    const byte OpCodeBufferContentSizeL  = 0x32;  // OpCode content size low (alias)
    const byte OpCodeBufferContentSizeH  = 0x33;  // OpCode content size high
    const byte CompilerTokenPos     = 0x34;  // Compiler position (16-bit)
    const byte CompilerTokenPosL    = 0x34;  // Compiler pos low (alias)
    const byte CompilerTokenPosH    = 0x35;  // Compiler pos high
    const byte CompilerFlags        = 0x36;  // Compilation state flags
    const byte OpCodeTemp           = 0x37;  // Temporary opcode construction
    
    // Buffer pointers - point to the currently active buffer set (either BASIC or REPL)
    const byte TokenBuffer          = 0x38;  // Low byte of current tokenizer buffer pointer
    const byte TokenBufferL         = 0x38;
    const byte TokenBufferH         = 0x39;  // High byte of current tokenizer buffer pointer
    const byte OpCodeBuffer         = 0x3A;  // Low byte of current opcode buffer pointer
    const byte OpCodeBufferL        = 0x3A;
    const byte OpCodeBufferH        = 0x3B;  // High byte of current opcode buffer pointer

    // SYMBOL TABLE MANAGEMENT
    const byte VariablesList        = 0x3C;  // Variables table head (16-bit)
    const byte VariablesListL       = 0x3C;  // Variables list low (alias)
    const byte VariablesListH       = 0x3D;  // Variables list high

    const byte FunctionsList        = 0x3E;  // Functions table head (16-bit)
    const byte FunctionsListL       = 0x3E;  // Functions list low (alias)
    const byte FunctionsListH       = 0x3F;  // Functions list high

    const byte SymbolType           = 0x40;  // Current symbol type|datatype
    const byte SymbolValue          = 0x41;  // Symbol value (16-bit)
    const byte SymbolValueL         = 0x41;  // Symbol value low (alias)
    const byte SymbolValueH         = 0x42;  // Symbol value high
    const byte SymbolName           = 0x43;  // Symbol name pointer (16-bit)
    const byte SymbolNameL          = 0x43;  // Symbol name low (alias)
    const byte SymbolNameH          = 0x44;  // Symbol name high
    const byte SymbolTokens         = 0x45;  // Symbol tokens pointer (16-bit)
    const byte SymbolTokensL        = 0x45;  // Symbol tokens low (alias)
    const byte SymbolTokensH        = 0x46;  // Symbol tokens high
    const byte SymbolIteratorFilter = 0x47;  // Symbol iteration filter

    const byte SymbolLength         = 0x48;  // Symbol name length
    const byte SymbolTemp0          = 0x49;  // Symbol temporary 0
    const byte SymbolTemp1          = 0x4A;  // Symbol temporary 1
    const byte SymbolTemp2          = 0x4B;  // Symbol temporary 2

    // DEBUG & TRACE SUPPORT
    const byte TraceMessageL        = 0x4C;  // Trace message pointer low
    const byte TraceMessageH        = 0x4D;  // Trace message pointer high
    const byte SystemState          = 0x4E;  // Success/Failure/Exiting state
    
    // SHARED LEAF FUNCTION WORKSPACE
    // Complex leaf methods that never call each other can share this space:
    // - Memory.Allocate and Memory.Free
    // - Debug.asm (never calls Memory functions)
    // - Time.Delay() and Time.Seconds() (mutually exclusive)
    // - IntMath operations (leaf functions)
    
    const byte M0                   = 0x4F;  // Multi-use workspace 0
    const byte M1                   = 0x50;  // Multi-use workspace 1
    const byte M2                   = 0x51;  // Multi-use workspace 2
    const byte M3                   = 0x52;  // Multi-use workspace 3
    const byte M4                   = 0x53;  // Multi-use workspace 4
    const byte M5                   = 0x54;  // Multi-use workspace 5
    const byte M6                   = 0x55;  // Multi-use workspace 6
    const byte M7                   = 0x56;  // Multi-use workspace 7
    const byte M8                   = 0x57;  // Multi-use workspace 8
    const byte M9                   = 0x58;  // Multi-use workspace 9
    const byte M10                  = 0x59;  // Multi-use workspace 10
    const byte M11                  = 0x5A;  // Multi-use workspace 11
    const byte M12                  = 0x5B;  // Multi-use workspace 12
    const byte M13                  = 0x5C;  // Multi-use workspace 13
    const byte M14                  = 0x5D;  // Multi-use workspace 14
    const byte M15                  = 0x5E;  // Multi-use workspace 15
    
    // Debug.asm aliases (never calls Memory functions)
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
    
    // Time.Delay() workspace (only uses M0-M3)
    const byte TARGET0              = M0;
    const byte TARGET1              = M1;
    const byte TARGET2              = M2;
    const byte TARGET3              = M3;
    
    // Time.Seconds() workspace (uses M0-M7)
    const byte LRESULT0             = M0;
    const byte LRESULT1             = M1;
    const byte LRESULT2             = M2;
    const byte LRESULT3             = M3;
    const byte LRESULT4             = M4;
    const byte LRESULT5             = M5;
    const byte LRESULT6             = M6;
    const byte LRESULT7             = M7;
    
    // IntMath extended workspace (only uses M0-M3)
    const byte UWIDE4               = M0;
    const byte UWIDE5               = M1;
    const byte UWIDE6               = M2;
    const byte UWIDE7               = M3;
    
    // TokenIterator used by listing commands (LIST, FUNCS) to enumerate function bodies
    const byte TOKCUR     = M0;  // Current token value
    const byte TOKBASE    = M1;
    const byte TOKBASEL   = M1;  // Token stream base pointer low
    const byte TOKBASEH   = M2;  // Token stream base pointer high
    const byte TOKINDENT  = M3;  // Current indentation level
    const byte TOKPOS     = M4;
    const byte TOKPOSL    = M4;  // Token position/offset low
    const byte TOKPOSH    = M5;  // Token position/offset high
    
    // FUNCTION PARAMETER WORKSPACE
    const byte FSOURCEADDRESS       = 0x5F;  // Source address parameter
    const byte FSOURCEADDRESSL      = 0x5F;  // Source low (alias)
    const byte FSOURCEADDRESSH      = 0x60;  // Source high
    
    const byte FDESTINATIONADDRESS  = 0x61;  // Destination address parameter
    const byte FDESTINATIONADDRESSL = 0x61;  // Destination low (alias)
    const byte FDESTINATIONADDRESSH = 0x62;  // Destination high

    const byte FLENGTH              = 0x63;  // Length parameter
    const byte FLENGTHL             = 0x63;  // Length low (alias)
    const byte FLENGTHH             = 0x64;  // Length high
    
    const byte LCURRENT             = 0x65;  // List current pointer
    const byte LCURRENTL            = 0x65;  // Current low (alias)
    const byte LCURRENTH            = 0x66;  // Current high
    
    const byte LHEAD                = 0x67;  // List head pointer
    const byte LHEADL               = 0x67;  // Head low (alias)
    const byte LHEADH               = 0x68;  // Head high
    
    const byte FSIGN                = 0x69;  // Sign flag for math operations
    
    const byte LHEADX               = 0x6A;  // List head extension
    
    const byte LPREVIOUS            = 0x6B;  // List previous pointer
    const byte LPREVIOUSL           = 0x6B;  // Previous low (alias)
    const byte LPREVIOUSH           = 0x6C;  // Previous high
    
    const byte LNEXT                = 0x6D;  // List next pointer
    const byte LNEXTL               = 0x6D;  // Next low (alias)
    const byte LNEXTH               = 0x6E;  // Next high

    // MATH WORKSPACE
    const byte UWIDE0               = 0x6F;  // IntMath 32-bit multiply low
    const byte UWIDE1               = 0x70;  // IntMath 32-bit multiply
    const byte UWIDE2               = 0x71;  // IntMath 32-bit multiply
    const byte UWIDE3               = 0x72;  // IntMath 32-bit multiply high
    
    const byte LNEXT0               = 0x73;  // Long math next operand byte 0
    const byte LNEXT1               = 0x74;  // Long math next operand byte 1
    const byte LNEXT2               = 0x75;  // Long math next operand byte 2
    const byte LNEXT3               = 0x76;  // Long math next operand byte 3
    const byte LTOP0                = 0x77;  // Long math top operand byte 0
    const byte LTOP1                = 0x78;  // Long math top operand byte 1
    const byte LTOP2                = 0x79;  // Long math top operand byte 2
    const byte LTOP3                = 0x7A;  // Long math top operand byte 3
    
    const byte STR                  = 0x7B;  // String pointer
    const byte STRL                 = 0x7B;  // String low (alias)
    const byte STRH                 = 0x7C;  // String high
    
    const byte STR2                 = 0x7D;  // String pointer 2
    const byte STR2L                = 0x7D;  // String 2 low (alias)
    const byte STR2H                = 0x7E;  // String 2 high

    // TODO : move to Compiler section
    const byte XPC                  = 0x7F;  // Compiler Program counter low
    const byte XPCL                 = 0x7F;  // Compiler Program counter low (alias)
    const byte XPCH                 = 0x80;  // Compiler Program counter high
    const byte CompilerTemp         = 0x81;  // Temporary workspace used by the Compiler
    
    const byte XID                  = 0x82;  // Executor token buffer pointer Program counter low
    const byte XIDL                 = 0x82;
    const byte XIDH                 = 0x83;
    
    // 0x84-0xEB: Available (104 bytes!)

    // HARDWARE I/O (IMMOVABLE - Platform Hardware Addresses)
    const byte ACIACONTROL          = 0xEC;  // 6850 ACIA control register
    const byte ACIASTATUS           = 0xEC;  // 6850 ACIA status (same address)
    const byte ACIADATA             = 0xED;  // 6850 ACIA data register
    
    // 0xEE-0xEF: Reserved for hardware expansion
    
    const byte PORTB                = 0xF0;  // VIA Port B data
    const byte PORTA                = 0xF1;  // VIA Port A data
    const byte DDRB                 = 0xF2;  // Data Direction Register B
    const byte DDRA                 = 0xF3;  // Data Direction Register A
    const byte T1CL                 = 0xF4;  // Timer 1 Counter Low
    const byte T1CH                 = 0xF5;  // Timer 1 Counter High
    const byte T1LL                 = 0xF6;  // Timer 1 Latch Low
    const byte T1LH                 = 0xF7;  // Timer 1 Latch High
    const byte T2CL                 = 0xF8;  // Timer 2 Counter Low
    const byte T2CH                 = 0xF9;  // Timer 2 Counter High
    const byte SR                   = 0xFA;  // Shift Register
    const byte ACR                  = 0xFB;  // Auxiliary Control Register
    const byte PCR                  = 0xFC;  // Peripheral Control Register
    const byte IFR                  = 0xFD;  // Interrupt Flag Register
    const byte IER                  = 0xFE;  // Interrupt Enable Register
    const byte ORA_NO_HANDSHAKE     = 0xFF;  // Output Register A (no handshake)
}