unit Runtime
{
    const byte runtimeSlots = 0x60; // 0x60..0x6F
    
    // same slot as programMemory : read-only!
    const byte functionTable    = runtimeSlots+0;
    const byte functionTableL   = runtimeSlots+0;
    const uint functionTableH   = runtimeSlots+1;
    
    //const byte PC             = runtimeSlots+2;
    const byte BP               = runtimeSlots+3;
    
    const byte globals          = runtimeSlots+4;
    const byte globalsL         = runtimeSlots+4;
    const byte globalsH         = runtimeSlots+5;
    
    const byte constants        = runtimeSlots+6;
    const byte constantsL       = runtimeSlots+6;
    const byte constantsH       = runtimeSlots+7;
    
    const byte codePage         = runtimeSlots+8;
    const byte codePageL        = runtimeSlots+8;
    const byte codePageH        = runtimeSlots+9;
    
    const byte opCode           = runtimeSlots+10;
    const byte operand          = runtimeSlots+11;
    
    const byte vmFlags          = runtimeSlots+12;
    // Bit 0 - exiting
    
    const byte stackStore       = runtimeSlots+13;
    const byte yStore           = runtimeSlots+14;
    
    enum OpCode
    {
        // System Operations
        NOP      = 0x00,  // No operation
        
        // Stack Operations
        PUSHB    = 0x02,  // Push 8-bit immediate
        PUSHW    = 0x04,  // Push 16-bit immediate
        PUSH0    = 0x06,  // Push 0 (optimized)
        PUSH1    = 0x08,  // Push 1 (optimized)
        DUP      = 0x0A,  // Duplicate byte on TOS
        DUPW     = 0x0C,  // Duplicate word on TOS
        DROP     = 0x0E,  // Remove byte from TOS
        DROPW    = 0x10,  // Remove word from TOS
        SWAP     = 0x12,  // Swap top two bytes
        SWAPW    = 0x14,  // Swap top two words
        
        // Arithmetic Operations
        ADD      = 0x16,  // Pop 2 words, push sum
        SUB      = 0x18,  // Pop 2 words, push difference
        MUL      = 0x1A,  // Pop 2 words, push product
        DIV      = 0x1C,  // Pop 2 words, push quotient (unsigned)
        MOD      = 0x1E,  // Pop 2 words, push remainder
        ADDB     = 0x20,  // Pop 2 bytes, push sum
        SUBB     = 0x22,  // Pop 2 bytes, push difference
        NEG      = 0x24,  // Negate int (2's complement)
        NEGB     = 0x26,  // Negate char (2's complement)
        
        // Comparison Operations
        EQ       = 0x28,  // Pop 2 words/ints, push 1 if equal, 0 if not
        NE       = 0x2A,  // Pop 2 words/ints, push 1 if not equal
        LT       = 0x2C,  // Pop 2 words, unsigned less than
        GT       = 0x2E,  // Pop 2 words, unsigned greater than
        LE       = 0x30,  // Pop 2 words, unsigned less or equal
        GE       = 0x32,  // Pop 2 words, unsigned greater or equal
        LTC      = 0x34,  // Pop 2 chars, signed less than
        GTC      = 0x36,  // Pop 2 chars, signed greater than
        LTI      = 0x38,  // Pop 2 ints, signed less than
        GTI      = 0x3A,  // Pop 2 ints, signed greater than
        
        // Bitwise Operations
        AND      = 0x3C,  // Pop 2 words, push bitwise AND
        OR       = 0x3E,  // Pop 2 words, push bitwise OR
        XOR      = 0x40,  // Pop 2 words, push bitwise XOR
        NOT      = 0x42,  // Pop word, push bitwise NOT
        SHL      = 0x44,  // Pop word and count, shift left
        SHR      = 0x46,  // Pop word and count, logical shift right
        SAR      = 0x48,  // Pop int and count, arithmetic shift right
        
        // Memory Operations - Globals
        PUSHGB   = 0x4A,  // Push byte from global[offset]
        PUSHGB2  = 0x4C,  // Push byte from global[offset] (>255)
        PUSHGW   = 0x4E,  // Push word from global[offset]
        PUSHGW2  = 0x50,  // Push word from global[offset] (>255)
        POPGB    = 0x52,  // Pop byte to global[offset]
        POPGB2   = 0x54,  // Pop byte to global[offset] (>255)
        POPGW    = 0x56,  // Pop word to global[offset]
        POPGW2   = 0x58,  // Pop word to global[offset] (>255)
        
        // Memory Operations - Locals
        PUSHLB   = 0x5A,  // Push byte from BP[offset]
        PUSHLW   = 0x5C,  // Push word from BP[offset]
        POPLB    = 0x5E,  // Pop byte to BP[offset]
        POPLW    = 0x60,  // Pop word to BP[offset]
        
        // Data/String Operations
        PUSHD    = 0x62,  // Push string address (byte offset)
        PUSHD2   = 0x64,  // Push string address (word offset)
        STRC     = 0x66,  // Pop index, pop string, push char
        STRCMP   = 0x68,  // Pop 2 strings, push -1/0/1
        
        // Control Flow
        CALL     = 0x6A,  // Call function (ID × 2 for table lookup)
        RET      = 0x6C,  // Return from function
        BRAB     = 0x6E,  // Branch backward by byte (0-255)
        BRAF     = 0x70,  // Branch forward by byte (0-255)
        BZF      = 0x72,  // Branch forward by byte if TOS is zero
        BZB      = 0x74,  // Branch backward by byte if TOS is zero
        BNZF     = 0x76,  // Branch forward by byte if TOS is not zero
        BNZB     = 0x78,  // Branch backward by byte if TOS is not zero
        
        // Zero Page Operations
        PUSHZB   = 0x7A,  // Push byte from ZP[offset]
        PUSHZW   = 0x7C,  // Push word from ZP[offset]
        POPZB    = 0x7E,  // Pop byte to ZP[offset]
        POPZW    = 0x80,  // Pop word to ZP[offset]
        
        // System Operations
        SYSCALL  = 0x82,  // Call BIOS function via X register
        HALT     = 0x84,  // Stop execution (return to BIOS)
        
        // Register Operations
        POPA     = 0x86,  // Pop byte from stack to A register
        POPY     = 0x88,  // Pop byte from stack to Y register
        PUSHA    = 0x8A,  // Push A register to stack
        PUSHC    = 0x8C,  // Push carry flag (1 if set, 0 if clear)
        PUSHZ    = 0x8E,  // Push zero flag (1 if set, 0 if clear)
    }
    
    const OpCode[] opCodeJumps;
    
    const string msgBadOpCode    = "Bad OpCode: 0x";
    
    const string msgBadOpCodeAt  = " at 0x";
    
    
    Initialize()
    {
        STZ globalsL
        LDA functionTableH
        INC
        STA globalsH
        
        STZ constantsL
        INC
        STA constantsH
        
        LDY #2 // slot of .MAIN in function table
        LDA [functionTable], Y
        STA codePageL
        INY
        LDA [functionTable], Y
        STA codePageH
        
        LDY #0 // PC
        STZ BP
        
        STZ vmFlags
    }
    
    halt() noopt
    {
        SMB0 vmFlags // exiting
    }
    badOpCode()
    {
        PHX
        LDA #(msgBadOpCode % 256)
        STA ZP.STRL
        LDA #(msgBadOpCode / 256)
        STA ZP.STRH
        Print.String();
        PLX
        TXA
        PHX
        Print.Hex();
        
        LDA #(msgBadOpCodeAt % 256)
        STA ZP.STRL
        LDA #(msgBadOpCodeAt / 256)
        STA ZP.STRH
        Print.String();
        
        CLC
        LDA codePageL
        ADC PC
        STA ZP.ACCL
        LDA codePageH
        ADC # 0
        Print.Hex();
        LDA ZP.ACCL
        Print.Hex();
        
        PLX
        SMB0 vmFlags // exiting
    }
    
    dispatchBIOS() noopt
    {
        JMP [ZP.BIOSDISPATCH]
    }
    
    Execute() noopt
    {
        TSX
        STX stackStore
        
        loop
        {
            LDA [codePage], Y
            INY
            TAX
            
            JMP [opCodeJumps, X]
            
            
NOP:                    
            continue;
// Stack Operations
                
PUSHB:                    
            LDA [codePage], Y // operand LSB
            INY
            PHA
            continue;
            
PUSHW:                    
            LDA [codePage], Y // operand LSB
            INY
            PHA
            LDA [codePage], Y // operand MSB
            INY
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSH0:                    
            LDA #0
            PHA
            PHA
            continue;

PUSH1:                    
            LDA #1
            PHA
            LDA #0
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

DUPW:                    
            TSX               // Get stack pointer to X
            
              
            // Read the word from stack (without popping)
            // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
            STA operand       // Save in temp
            LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
            
            // Push the duplicate (LSB first, then MSB)
            PHA               // Push TOP0 (LSB)
            LDA operand       // Get TOP1 back
            PHA               // Push TOP1 (MSB)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
DROPW:                    
            PLA
            PLA
            continue;
                
SWAPW:                    
            TSX               // Get stack pointer to X
        
            // Swap TOP and NEXT
            // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            
            STY yStore
            
            // Swap the low bytes (TOP0 <-> NEXT0)
            LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
            LDY 0x0104, X     // Load NEXT0 (LSB at SP+4)
            STA 0x0104, X     // Store TOP0 to NEXT0 position
            TYA
            STA 0x0102, X     // Store NEXT0 to TOP0 position
            
            // Swap the high bytes (TOP1 <-> NEXT1)
            LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
            LDY 0x0103, X     // Load NEXT1 (MSB at SP+3)
            STA 0x0103, X     // Store TOP1 to NEXT1 position
            TYA
            STA 0x0101, X     // Store NEXT1 to TOP1 position
            
            LDY yStore
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

ADD:                    
            TSX               // Get stack pointer to X
            
            // Add low bytes with carry propagation
            // TOP:  SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            CLC               // Clear carry for addition
            LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
            ADC 0x0104, X     // Add NEXT0 (LSB at SP+4)
            STA 0x0104, X     // Store result to NEXT0
            
            // Add high bytes with carry
            LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
            ADC 0x0103, X     // Add NEXT1 (MSB at SP+3) with carry
            STA 0x0103, X     // Store result to NEXT1
            
            INX INX TXS       // Remove TOP from stack
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
                      
LE:
            TSX               // Get stack pointer to X
            
            LDA #1 // NEXT <= TOP
            STA operand
                    
            // Compare NEXT <= TOP
            // TOP is at SP+1/SP+2 (top)
            // NEXT is at SP+3/SP+4
            // First compare high bytes
            LDA 0x0103, X     // Load NEXT1
            CMP 0x0101, X     // Compare with TOP1
            if (Z)            // If NEXT1 == TOP1
            {
                LDA 0x0104, X // Load NEXT0 Low
                CMP 0x0102, X // Compare with TOP0 Low
            }
            if (NZ) // NEXT != TOP
            {
                if (C)        // NEXT >= TOP?
                {
                    STZ operand   // NEXT > TOP
                }
            }
            INX               // Adjust stack by 3
            INX
            INX
            TXS
            LDA operand
            STA 0x0101, X     // store the boolean result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
PUSHD:                    
            LDA [codePage], Y // byte offset    
            INY
            CLC
            ADC constantsL
            PHA               // LSB
            LDA constantsH
            ADC #0            // MSB
            PHA  
            continue;
            
BNZB:                    
            PLX                   // pop the boolean
            if (NZ)
            {
                SEC               // Branch backward by offset in A
                TYA
                SBC [codePage], Y // Subtract offset from PC
                TAY
            }
            else
            {
                INY
            }
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSHZW:                    
            LDA [codePage], Y // byte offset  
            INY
            TAX
            LDA 0x00, X 
            PHA               // LSB
            INX
            LDA 0x00, X 
            PHA               // MSB
            continue;
            
POPZW:
            LDA [codePage], Y // byte offset  
            INY

            TAX
            INX
            PLA               // MSB
            STA 0x00, X 
            DEX
            PLA               // LSB
            STA 0x00, X 
            continue;
                
SYSCALL:
            LDA [codePage], Y // byte BIOS call index
            INY
            TAX
            PHY
            dispatchBIOS();
            PLY
            continue;
            
HALT:         
            halt();
            break;
            
        } // loop
        
        LDX stackStore
        TXS
    }
}
