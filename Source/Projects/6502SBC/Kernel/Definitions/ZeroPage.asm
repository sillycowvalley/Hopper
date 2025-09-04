unit ZP // ZeroPage.asm
{
    const byte FLAGS                = 0x00;  // System flags register
    // FLAGS bits:
    // Bit 1 - serial XON / XOFF : set if stopped
    // Bit 0 - NMI break flag    : break detected
    
     // Heap:
    const byte FREELIST             = 0x06;  // Heap free list pointer
    const byte FREELISTL            = 0x06;  // Free list low (alias)
    const byte FREELISTH            = 0x07;  // Free list high
    const byte HEAPSTART            = 0x08;  // Heap start page
    const byte HEAPSIZE             = 0x09;  // Heap size in pages
    
    // Serial
    const byte SerialInWritePointer = 0x0A;  // Serial buffer write position
    const byte SerialInReadPointer  = 0x0B;  // Serial buffer read position
    
    const byte TOP0                 = 0x11;  // 
    const byte TOP1                 = 0x12;
    const byte TOP2                 = 0x13;
    const byte TOP3                 = 0x14;
    
    const byte NEXT0                = 0x16;
    const byte NEXT1                = 0x17;
    const byte NEXT2                = 0x18;
    const byte NEXT3                = 0x19;
    
    const byte IDX                  = 0x1A;
    const byte IDXL                 = 0x1A;
    const byte IDXH                 = 0x1B;
    
    const byte IDY                  = 0x1C;
    const byte IDYL                 = 0x1C;
    const byte IDYH                 = 0x1D;
    
    const byte ACC                  = 0x1E;
    const byte ACCL                 = 0x1E;
    const byte ACCH                 = 0x1F;
    
    // Hopper Assembler jump table slots: always 0x20 and 0x21
    const byte JumpTableLSB         = 0x20;  // Jump table LSB
    const byte JumpTableMSB         = 0x21;  // Jump table MSB
    
    // Timer ticks (move here -> move in 6502 emulator)
    const byte TICK0                = 0x22;  // Timer tick byte 0 (LSB)
    const byte TICK1                = 0x23;  // Timer tick byte 1
    const byte TICK2                = 0x24;  // Timer tick byte 2
    const byte TICK3                = 0x25;  // Timer tick byte 3 (MSB)
    
    const byte STR                  = 0x26;
    const byte STRL                 = 0x26;
    const byte STRH                 = 0x27;
    
    
    
    // I2C
    const byte I2CInWritePtr        = 0x77;  // I2C buffer write pointer
    const byte I2CInReadPtr         = 0x78;  // I2C buffer read pointer
    
    // File System: I2C for EEPROM:
    const byte OutB                 = 0x88;
    const byte InB                  = 0x89;
    const byte LastAck              = 0x8A;
    const byte PLUGNPLAY            = 0x8B;
    

        
    // SHARED LEAF FUNCTION WORKSPACE
    // Complex leaf methods that never call each other can share this space:
    // - Memory.Allocate and Memory.Free
    // - Time.Delay() and Time.Seconds() (mutually exclusive)
    
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
    const byte M16                  = 0x5F;  // Multi-use workspace 16
    const byte M17                  = 0x60;  // Multi-use workspace 17
    
    // Time.Delay() workspace (only uses M0-M3)
    const byte TARGET0              = M0;
    const byte TARGET1              = M1;
    const byte TARGET2              = M2;
    const byte TARGET3              = M3;
    
    // Time.Seconds(), Long workspace (uses M0-M7)
    const byte RESULT0              = M0;
    const byte RESULT1              = M1;
    const byte RESULT2              = M2;
    const byte RESULT3              = M3;
    const byte RESULT4              = M4;
    const byte RESULT5              = M5;
    const byte RESULT6              = M6;
    const byte RESULT7              = M7;
    
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
    
    
    // DON'T MOVE THESE WITHOUT UPDATING EMULATOR:
    const byte EmulatorPCL          = 0x75;  // BIT this to capture PC
    const byte EmulatorPCH          = 0x76;  // Captured PC high byte
    
    // File unit:
    const byte FS0                  = 0x8C;
    const byte FS1                  = 0x8D;
    const byte FS2                  = 0x8E;
    const byte FS3                  = 0x8F;
    const byte FS4                  = 0x90;
    const byte FS5                  = 0x91;
    const byte FS6                  = 0x92;
    const byte FS7                  = 0x93;
    const byte FS8                  = 0x94;
    const byte FS9                  = 0x95;
    const byte FS10                 = 0x96;
    const byte FS11                 = 0x97;
    const byte FS12                 = 0x98;
    const byte FS13                 = 0x99;
    const byte FS14                 = 0x9A;
    const byte FS15                 = 0x9B;
    
#if defined(ZEROPAGE_IO)    
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
#endif
}
