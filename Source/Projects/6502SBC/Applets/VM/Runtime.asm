unit Runtime
{
    uses "OpCodes"
    
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
    
    const OpCode[] opCodeJumps;
    
    const string msgBadOpCode    = "Bad OpCode: 0x";
    
    const string msgBadOpCodeAt  = " at 0x";
    
    const string debugHeader = "\n=== STACK DUMP ===\n";
    const string bpLabel     = "BP: 0x";
    
    debugStack()
    {
        // Preserve all registers
        PHA
        PHX  
        PHY
        
        // Print header
        LDA #(debugHeader % 256)
        STA ZP.STRL
        LDA #(debugHeader / 256)
        STA ZP.STRH
        Print.String();
        
        // Print BP value
        LDA #(bpLabel % 256)
        STA ZP.STRL
        LDA #(bpLabel / 256)
        STA ZP.STRH
        Print.String();
        LDA BP
        Print.Hex();
        Print.NewLine();
        
        // Start from BP+8 down to BP-8
        LDX BP
        INX INX INX INX INX INX INX INX  INX INX // BP+10
        
        LDY #21  // 21 bytes total (BP+10 to BP-10)
        loop
        {
            // Print address
            LDA #'0' PHX Print.Char(); PLX
            LDA #'1' PHX Print.Char(); PLX
            TXA
            PHX Print.Hex(); PLX
            LDA #':' PHX Print.Char(); PLX
            LDA #' ' PHX Print.Char(); PLX
            
            // Print hex value
            LDA 0x0100, X
            PHX Print.Hex(); PLX
            
            // Check if this is BP
            CPX BP
            if (Z)
            {
                LDA #' ' PHX Print.Char(); PLX
                LDA #'<' PHX Print.Char(); PLX
                LDA #'-' PHX Print.Char(); PLX
                LDA #'B' PHX Print.Char(); PLX
                LDA #'P' PHX Print.Char(); PLX
            }
            
            PHX Print.NewLine(); PLX
            
            DEX  // Move down stack
            DEY
            if (Z) { break; }
        }
        
        Print.NewLine();
        
        // Restore all registers
        PLY
        PLX
        PLA
    }
    
    
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
NOP:        // NOP must be first to get the ball rolling ..
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
// Stack Operations

PUSHW:                    
            LDA [codePage], Y
            INY
            PHA
                
PUSHB:                    
            LDA [codePage], Y
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
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

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
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
                
SWAPW:                    
            TSX               // Get stack pointer to X
        
            // Swap TOP and NEXT
            // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            
            // Swap the low bytes (TOP0 <-> NEXT0)
            LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
            STA operand
            LDA 0x0104, X     // Load NEXT0 (LSB at SP+4)
            STA 0x0102, X     // Store TOP0 to NEXT0 position
            LDA operand
            STA 0x0104, X     // Store NEXT0 to TOP0 position
            
            // Swap the high bytes (TOP1 <-> NEXT1)
            LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
            STA operand
            LDA 0x0103, X     // Load NEXT1 (MSB at SP+3)
            STA 0x0101, X     // Store TOP1 to NEXT1 position
            LDA operand
            STA 0x0103, X     // Store NEXT1 to TOP1 position
            
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
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
BNZB:       
            PLX                   // pop the boolean
            if (NZ)
            {
                SEC               // Branch backward by offset in A
                TYA
                SBC [codePage], Y // Subtract offset from PC
                TAY
            }
            INY
            
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
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
PUSHZQ:                    
            LDA [codePage], Y // byte offset  
            INY
            TAX
            LDA 0x00, X 
            PHA               // Push byte 0 (LSB)
            INX
            LDA 0x00, X 
            PHA               // Push byte 1
            INX               
            LDA 0x00, X 
            PHA               // Push byte 2
            INX
            LDA 0x00, X 
            PHA               // Push byte 3 (MSB
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
            
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
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
POPZQ:
            LDA [codePage], Y // byte offset  
            INY
            
            TAX
            INX INX INX       // Start at byte 3 position
            PLA               // Pop byte 3 (MSB)
            STA 0x00, X 
            DEX
            PLA               // Pop byte 2
            STA 0x00, X 
            DEX
            PLA               // Pop byte 1
            STA 0x00, X 
            DEX
            PLA               // Pop byte 0 (LSB)
            STA 0x00, X  
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
          
INCLW:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            INC 0x0100, X     // Increment LSB
            if (Z)            // If LSB wrapped to 0
            {
                DEX           // BP + offset - 1 (MSB address)
                INC 0x0100, X // Increment MSB
            }
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]          
            
PUSHLW:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            LDA 0x0100, X     // Load LSB
            PHA
            DEX               // BP + offset - 1 (MSB address)  
            LDA 0x0100, X     // Load MSB
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
PUSHLQ:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            LDA 0x0100, X     // Load byte 0 (LSB)
            PHA
            DEX               
            LDA 0x0100, X     // Load byte 1
            PHA
            DEX               
            LDA 0x0100, X     // Load byte 2
            PHA
            DEX               
            LDA 0x0100, X     // Load byte 3 (MSB)
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            

POPLW:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            DEX               // BP + offset - 1 (MSB address)
            PLA               // MSB
            STA 0x0100, X
            INX               // Back to LSB address
            PLA               // LSB
            STA 0x0100, X         
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
POPLQ:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            DEX DEX DEX       // Start at byte 3 position
            PLA               // Pop byte 3 (MSB)
            STA 0x0100, X
            INX
            PLA               // Pop byte 2
            STA 0x0100, X
            INX
            PLA               // Pop byte 1
            STA 0x0100, X
            INX
            PLA               // Pop byte 0 (LSB)
            STA 0x0100, X
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
            
                
SYSCALL:
            LDA [codePage], Y // byte BIOS call index
            INY
            TAX
            PHY
            dispatchBIOS();
            PLY
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
ENTER:
            LDA BP
            PHA
            TSX
            STX BP
            
            LDA [codePage], Y // number of zero bytes to push
            INY
            TAX
            LDA #0
            loop
            {
                CPX #0
                if (Z) { break; }
                PHA
                DEX
            }
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

LEAVE:
            PLA
            STA BP
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
              
DUMP:
            debugStack();
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]                                                                 
HALT:         
            halt();
            break;
            
        } // loop
        
        LDX stackStore
        TXS
    }
}
