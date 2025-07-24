unit ZP
{
#ifndef TIGGERC
    const byte PC                   = 0x00;
    const byte PCL                  = 0x00;
    const byte PCH                  = 0x01;
    
    const byte FLAGS                = 0x02;
    
    // Bit 7 - MCU platform
    // Bit 6 - Program exited (ended well or badly via Die)
    // Bit 5 - breakpoint/s exist
    // Bit 4 - in debugger (Runtime.InDebugger, not FLAGS)
    // Bit 3 - 8 bit SP and BP
    // Bit 2 - checked build
    // Bit 1 - running at Warp (no checks for <ctrl><C>)
    // Bit 0 - a program has been loaded
    
    const byte SP                   = 0x03;
#endif
    const byte BP                   = 0x04;
#ifndef TIGGERC
    const byte CSP                  = 0x05;
#endif
    
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
    
    const byte CNP                  = 0x0F;
    
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


    // used by firmware for 'W'orkspace
    // (for example, Serial also uses these, especially on 6502 where PLX, PLY, PHX and PHY are missing)
    const byte W0                   = 0x20;
    const byte W1                   = 0x21;
    const byte W2                   = 0x22; // used by packed syscalls
    const byte W3                   = 0x23;
    const byte W4                   = 0x24;
    const byte W5                   = 0x25;
    const byte W6                   = 0x26;
    const byte W7                   = 0x27;


#ifndef TIGGERC
    const byte PLUGNPLAY            = W7; // devices on this machine
    // Bit 1 - I2C 0x50 EEPROM exists
    // Bit 0 - I2C 0x3C OLED exists
#endif

    // used by I2C
    const byte I2CADDR              = W3;
    const byte OutB                 = W4;
    const byte InB                  = W5;
    const byte LastAck              = W6; // LackAck == 0 means ACK, 1 means NACK
    
    // used by 'T'ime APIs Millis and Delay
    const byte TICK0                = 0x28;
    const byte TICK1                = 0x29;
    const byte TICK2                = 0x2A;
    const byte TICK3                = 0x2B;
    const byte TARGET0              = 0x2C;
    const byte TARGET1              = 0x2D;
    const byte TARGET2              = 0x2E;
    const byte TARGET3              = 0x2F;
    
#if !defined(TIGGERC) && !defined(HOPPER_BASIC)
    const byte BRKL                 = 0x30; // .. 0x3F
    const byte BRKH                 = 0x40; // .. 0x4F
#endif

#if defined(HOPPER_BASIC)
    // HopperBASIC allocation: 0x30..0x3F (16 bytes primary)
    
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
    
    // === AVAILABLE PRIMARY (0x3A-0x3F) ===
    const byte SymbolList           = 0x3A;
    const byte SymbolListL          = 0x3A;  // Symbol table head pointer low byte
    const byte SymbolListH          = 0x3B;  // Symbol table head pointer high byte
    
    // === AVAILABLE PRIMARY (0x3A-0x3F) ===
    // 4 bytes available for additional BASIC features (0x3C-0x3F)
    
#endif

#ifndef TIGGERC
    // used by 'M'emory manager functions
    const byte M0                   = 0x50;
    const byte M1                   = 0x51;
    const byte M2                   = 0x52;
    const byte M3                   = 0x53;
    const byte M4                   = 0x54;
    const byte M5                   = 0x55;
    const byte M6                   = 0x56;
    const byte M7                   = 0x57;
    const byte M8                   = 0x58;
    const byte M9                   = 0x59;
    const byte M10                  = 0x5A;
    const byte M11                  = 0x5B;
    const byte M12                  = 0x5C;
    const byte M13                  = 0x5D;
    const byte M14                  = 0x5E;
    const byte M15                  = 0x5F;
#else
    // used by 'M'emory manager functions
    const byte M0                   = 0x30;
    const byte M1                   = 0x31;
    const byte M2                   = 0x32;
    const byte M3                   = 0x33;
    const byte M4                   = 0x34;
    const byte M5                   = 0x35;
    const byte M6                   = 0x36;
    const byte M7                   = 0x37;
    const byte M8                   = 0x38;
    const byte M9                   = 0x39;
    const byte M10                  = 0x3A;
    const byte M11                  = 0x3B;
    const byte M12                  = 0x3C;
    const byte M13                  = 0x3D;
    const byte M14                  = 0x3E;
    const byte M15                  = 0x3F;
#endif
    
    // used by general syscall 'F'unctions  
    
    
#ifndef TIGGERC
    const byte F0                   = 0x60;
    const byte F1                   = 0x61;
    const byte F2                   = 0x62;
    const byte F3                   = 0x63;
    const byte F4                   = 0x64;
    const byte F5                   = 0x65;
    const byte F6                   = 0x66;
    const byte F7                   = 0x67;
    const byte F8                   = 0x68;
    const byte F9                   = 0x69;
    const byte F10                  = 0x6A;
    const byte F11                  = 0x6B;
    const byte F12                  = 0x6C;
    const byte F13                  = 0x6D;
    const byte F14                  = 0x6E;
    const byte F15                  = 0x6F;
    
  #if !defined(HOPPER_BASIC)     
    // dictionary types (safely unused)
    const byte D0                   = 0x70;
    const byte D1                   = 0x71;
    const byte D2                   = 0x72;
    const byte D3                   = 0x73;
    const byte D4                   = 0x74;
    const byte D5                   = 0x75;
    const byte D6                   = 0x76;
  #endif
#else
    const byte F0                   = 0x05;
#endif

#ifndef TIGGERC
    // used my 'U'Int functions
    const byte U0                   = 0x80;
    const byte U1                   = 0x81;
    const byte U2                   = 0x82;
    const byte U3                   = 0x83;
    const byte U4                   = 0x84;
    const byte U5                   = 0x85;
    const byte U6                   = 0x86;
    const byte U7                   = 0x87;
#else
    const byte U0                   = 0x00;
    const byte U1                   = 0x01;
    const byte U2                   = 0x02;
    const byte U3                   = 0x03;
    
#endif

#if !defined(BENEATER_IO) && !defined(X16_IO) && !defined(ZEROPAGE_IO) && !defined(MECB6502_IO)
    #define ZEROPAGE_IO // default if IO is not specified
#endif

    // MECB addresses for IO:
    //
    // MC6840 PTM  = 0xF000
    
    
#ifdef MECB6502_IO    
    
    reserve 0xF000, 256
    
    // Motorola 6840 PTM (Programmable Timer Module)
    const uint TCR                  = 0xF000;  // Write: Timer Control Registers 1 & 3   Read: NOP
    const uint TCSR2                = 0xF001;  // Write: Control Register 2              Read: Status Register (least significant bit selects TCR as TCSR1 or TCSR3)
    const uint TIMER1_MSB           = 0xF002;  // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint TIMER1_LSB           = 0xF003;  // Write: Timer #1 Latches                Read: LSB Buffer Register
    const uint TIMER2_MSB           = 0xF004;  // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint TIMER2_LSB           = 0xF005;  // Write: Timer #1 Latches                Read: LSB Buffer Register
    const uint TIMER3_MSB           = 0xF006;  // Write: MSB Buffer Register             Read: Timer 1 Counter
    const uint TIMER3_LSB           = 0xF007;  // Write: Timer #1 Latches                Read: LSB Buffer Register
    
    // Motorola 6850 ACIA
    const uint ACIACONTROL          = 0xF008;
    const uint ACIASTATUS           = 0xF008;
    const uint ACIADATA             = 0xF009;
    
    // Motorola 6821 PIA (Peripheral Interface Adapter)
    const uint PORTA                = 0xF010; // Peripheral A Data Register
    const uint PORTB                = 0xF011; // Peripheral B Data Register
    const uint CRA                  = 0xF012; // Control Register A
    const uint CRB                  = 0xF013; // Control Register B
    // Data Direction Registers
    const uint DDRA                 = 0xF010; // Data Direction Register A (Shared with PORTA)
    const uint DDRB                 = 0xF011; // Data Direction Register B (Shared with PORTB)
    
    
#endif

#if defined(X16_IO)
    
    reserve 0x9F00, 256
    
#ifdef ACIA_6551
    // Rockwell 6551
    const uint ACIADATA             = 0x9F10;
    const uint ACIASTATUS           = 0x9F11;
    const uint ACIACOMMAND          = 0x9F12;
    const uint ACIACONTROL          = 0x9F13;
#endif

#ifdef ACIA_6850
    // Motorola 6850 ACIA
    const uint ACIACONTROL          = 0x9F10;
    const uint ACIASTATUS           = 0x9F10;
    const uint ACIADATA             = 0x9F11;
#endif

    // W65C22 VIA
    const uint PORTB                = 0x9F20;
    const uint PORTA                = 0x9F21;
    const uint DDRB                 = 0x9F22;
    const uint DDRA                 = 0x9F23;
    const uint T1CL                 = 0x9F24; // Timer 1 counter low
    const uint T1CH                 = 0x9F25; // Timer 1 counter high
    
    const byte T1LL                 = 0xF6; // Timer 1 Latch Low
    const byte T1LH                 = 0xF7; // Timer 1 Latch High
    const byte T2CL                 = 0xF8; // Timer 2 Counter Low
    const byte T2CH                 = 0xF9; // Timer 2 Counter High
    const byte SR                   = 0xFA; // Shift Register
    
    const uint ACR                  = 0x9F2B; // Auxiliary Control Register
    const uint PCR                  = 0x9F2C; // Peripheral Control Register
    const uint IFR                  = 0x9F2D; // Interrupt Flag Register
    const uint IER                  = 0x9F2E; // Interrupt Enable Register
#endif

#if defined(BENEATER_IO)

#ifdef ACIA_6551
    // Rockwell 6551
    const uint ACIADATA             = 0x5000;
    const uint ACIASTATUS           = 0x5001;
    const uint ACIACOMMAND          = 0x5002;
    const uint ACIACONTROL          = 0x5003;
#endif
#ifdef ACIA_6850
    // Motorola 6850 ACIA
    const uint ACIACONTROL          = 0x5000;
    const uint ACIASTATUS           = 0x5000;
    const uint ACIADATA             = 0x5001;
#endif
    
    // W65C22 VIA
    const uint PORTB                = 0x6000;
    const uint PORTA                = 0x6001;
    const uint DDRB                 = 0x6002;
    const uint DDRA                 = 0x6003;
    const uint T1CL                 = 0x6004; // Timer 1 counter low
    const uint T1CH                 = 0x6005; // Timer 1 counter high
    
    const byte T1LL                 = 0xF6; // Timer 1 Latch Low
    const byte T1LH                 = 0xF7; // Timer 1 Latch High
    const byte T2CL                 = 0xF8; // Timer 2 Counter Low
    const byte T2CH                 = 0xF9; // Timer 2 Counter High
    const byte SR                   = 0xFA; // Shift Register
    
    const uint ACR                  = 0x600B; // Auxiliary Control Register
    const uint PCR                  = 0x600C; // Peripheral Control Register
    const uint IFR                  = 0x600D; // Interrupt Flag Register
    const uint IER                  = 0x600E; // Interrupt Enable Register
#endif
      
#if defined(ZEROPAGE_IO)
    // Motorola 6850 ACIA
    const byte ACIACONTROL          = 0xEC; //0x1E;
    const byte ACIASTATUS           = 0xEC; //0x1E;
    const byte ACIADATA             = 0xED; //0x1F;
    
    // W65C22 VIA
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
#endif    
    
    // used for UInt library
    const byte UWIDE0               = U0;
    const byte UWIDE1               = U1;
    const byte UWIDE2               = U2;
    const byte UWIDE3               = U3;
    

    // These are shared across type sysCalls and opCodes that don't call each other:

    // used for Int library for sign
    const byte FSIGN                = F0;

#ifndef TIGGERC
    const byte FSIZE = F1;
    const byte FSIZEL = F1;
    const byte FSIZEH = F2;

    // used by strings, long, dictionaries and arrays

    const byte FSOURCEADDRESS  = F3;
    const byte FSOURCEADDRESSL = F3;
    const byte FSOURCEADDRESSH = F4;

    const byte FDESTINATIONADDRESS  = F5;
    const byte FDESTINATIONADDRESSL = F5;
    const byte FDESTINATIONADDRESSH = F6;

    const byte FTYPE = F7;

    const byte FLENGTH  = F8;
    const byte FLENGTHL = F8;
    const byte FLENGTHH = F9;

    const byte FVALUE  = F10;
    const byte FVALUEL = F10;
    const byte FVALUEH = F11;
    
    // used by long and float:
    const byte LSIGNNEXT = F0;
    const byte LSIGNTOP  = F1;
    
    const byte LTOP0 = F5;
    const byte LTOP1 = F6;
    const byte LTOP2 = F7;
    const byte LTOP3 = F8;
    
    const byte LNEXT0 = F9;
    const byte LNEXT1 = F10;
    const byte LNEXT2 = F11;
    const byte LNEXT3 = F12;
    
    // used by syscallLongDiv, syscallLongMod, syscallLongMul:
    const byte LRESULT0 = U0;
    const byte LRESULT1 = U1;
    const byte LRESULT2 = U2;
    const byte LRESULT3 = U3;
    const byte LRESULT4 = U4;
    const byte LRESULT5 = U5;
    const byte LRESULT6 = U6;
    const byte LRESULT7 = U7;
    
    // used by lists:
    const byte LTYPE    = F3;

    const byte LLENGTH  = F4;
    const byte LLENGTHL = F4;
    const byte LLENGTHH = F5;

    const byte LPREVIOUS  = F6;
    const byte LPREVIOUSL = F6;
    const byte LPREVIOUSH = F7;
    
    // preserved during recursive clone calls
    const byte LNEXT  = F8;
    const byte LNEXTL = F8;
    const byte LNEXTH = F9;

    // preserved during recursive clone calls
    const byte LCURRENT  = F10;
    const byte LCURRENTL = F10;
    const byte LCURRENTH = F11;

    
#if defined(HOPPER_BASIC)
    const byte LHEAD  = F12;
    const byte LHEADL = F12;
    const byte LHEADH = F13;
#else
    const byte FITEM  = F12;
    const byte FITEML = F12;
    const byte FITEMH = F13;
#endif

    const byte LCOUNT  = F14;
    const byte LCOUNTL = F14;
    const byte LCOUNTH = F15;

#if defined(HOPPER_BASIC)        
    const byte LHEADX  = F14;
#else    
    const byte LITYPE  = F14;
#endif

    // used by arrays
    const byte ACARRY   = F14;
#endif
}
