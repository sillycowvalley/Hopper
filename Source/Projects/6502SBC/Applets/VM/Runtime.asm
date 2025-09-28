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
    
    const byte operand          = runtimeSlots+10;
    const byte operandL         = runtimeSlots+10;
    const byte operandH         = runtimeSlots+11;
    
    const byte vmFlags          = runtimeSlots+12;
    // Bit 0 - BIOS call Z return
    // Bit 1 - BIOS call C return
    
    const byte stackStore       = runtimeSlots+13;
    const byte yStore           = runtimeSlots+14;
    const byte aStore           = runtimeSlots+15;
    
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

PUSHA:
            LDA aStore       // aStore now has the value for SYSCALL
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

POPA:
            PLA              // Pop byte from stack
            STA aStore       // aStore now has the value for SYSCALL
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
POPY:
            PLA              // Pop byte from stack
            STA yStore       // yStore now has the value for SYSCALL
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]  
            
PUSHC:
            LDA #0               // Default to 0
            if (BBS1, vmFlags)   // If carry set from last BIOS call
            {
                LDA #1           // Change to 1
            }
            PHA                  // Push result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
PUSHZ:
            LDA #0               // Default to 0
            if (BBS0, vmFlags)   // If zero set from last BIOS call
            {
                LDA #1           // Change to 1
            }
            PHA                  // Push result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]                                                                         

PUSHW:                    
            LDA [codePage], Y
            INY
            PHA
            // fall through to PUSHB
PUSHB:                    
            LDA [codePage], Y
            INY
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSHB0:
            LDA #0
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
PUSHW0:
            LDA #0
            PHA
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSHB1:                    
            LDA #1
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
PUSHW1:                    
            LDA #1
            PHA
            LDA #0
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

DUPB:
            TSX
            LDA 0x0101, X     
            PHA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

DUPW:                    
            TSX
              
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
DROPB:                    
            PLA
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
SWAPB:
            TSX               // Get stack pointer
            
            // Stack: SP+1=TOP, SP+2=NEXT
            LDA 0x0101, X     // Load TOP (at SP+1)
            STA operand       // Save in temp
            LDA 0x0102, X     // Load NEXT (at SP+2)
            STA 0x0101, X     // Store NEXT at TOP position
            LDA operand       // Get TOP back
            STA 0x0102, X     // Store TOP at NEXT position
            
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

ADDW:                    
            TSX               // Get stack pointer to X
            
            // Add low bytes with carry propagation
            // TOP:  SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            CLC               // Clear carry for addition
            LDA 0x0104, X     // Load NEXT0 (LSB at SP+4)
            ADC 0x0102, X     // Add TOP0 (LSB at SP+2)
            STA 0x0104, X     // Store result to NEXT0
            
            // Add high bytes with carry
            LDA 0x0103, X     // Load NEXT1 (MSB at SP+3)
            ADC 0x0101, X     // Add TOP1 (MSB at SP+1) with carry
            STA 0x0103, X     // Store result to NEXT1
            
            INX INX TXS       // Remove TOP from stack
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
SUBW:                    
            TSX               // Get stack pointer to X
            
            // Add low bytes with carry propagation
            // TOP:  SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            SEC               // Set carry for subtraction
            LDA 0x0104, X     // Load NEXT0 (LSB at SP+4)
            SBC 0x0102, X     // Subtract TOP0 (LSB at SP+2)
            STA 0x0104, X     // Store result to NEXT0
            
            // Add high bytes with carry
            LDA 0x0103, X     // Load NEXT1 (MSB at SP+3)
            SBC 0x0101, X     // Subtract TOP1 (MSB at SP+1) with carry
            STA 0x0103, X     // Store result to NEXT1
            
            INX INX TXS       // Remove TOP from stack
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]  
            
XORB:                    
            TSX               // Get stack pointer to X
            
            // Bitwise XOR two bytes
            // TOP:  SP+1 (single byte)
            // NEXT: SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT (at SP+2)
            EOR 0x0101, X     // XOR with TOP (at SP+1)
            STA 0x0102, X     // Store result to NEXT position
            
            INX TXS           // Remove TOP from stack (1 byte)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
SHLW:
            LDA [codePage], Y // Get shift count
            INY
            STA operand       // Save count
            
            if (NZ)           // If count > 0
            {
                TSX           // NOW get stack pointer
                loop
                {
                    ASL 0x0102, X // Shift LSB left
                    ROL 0x0101, X // Rotate MSB left (with carry from LSB)
                    DEC operand
                    if (Z) { break; }
                }
            }
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

SHRW:
            LDA [codePage], Y // Get shift count
            INY
            STA operand       // Save count
            
            if (NZ)           // If count > 0
            {
                TSX           // NOW get stack pointer
                loop
                {
                    LSR 0x0101, X // Shift MSB right
                    ROR 0x0102, X // Rotate LSB right (with carry from MSB)
                    DEC operand
                    if (Z) { break; }
                }
            }
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
            
XORW:                    
            TSX               // Get stack pointer to X
            
            // Bitwise XOR two words
            // TOP:  SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
            LDA 0x0104, X     // Load NEXT0 (LSB at SP+4)
            EOR 0x0102, X     // XOR with TOP0 (LSB at SP+2)
            STA 0x0104, X     // Store result to NEXT0
            
            LDA 0x0103, X     // Load NEXT1 (MSB at SP+3)
            EOR 0x0101, X     // XOR with TOP1 (MSB at SP+1)
            STA 0x0103, X     // Store result to NEXT1
            
            INX INX TXS       // Remove TOP from stack
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X] 
            
ANDB:                    
            TSX               // Get stack pointer to X
            
            // Bitwise AND two bytes
            // TOP:  SP+1 (single byte)
            // NEXT: SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT (at SP+2)
            AND 0x0101, X     // AND with TOP (at SP+1)
            STA 0x0102, X     // Store result to NEXT position
            
            INX TXS           // Remove TOP from stack (1 byte)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
ORB:                    
            TSX               // Get stack pointer to X
            
            // Bitwise OR two bytes
            // TOP:  SP+1 (single byte)
            // NEXT: SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT (at SP+2)
            ORA 0x0101, X     // OR with TOP (at SP+1)
            STA 0x0102, X     // Store result to NEXT position
            
            INX TXS           // Remove TOP from stack (1 byte)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
NOTB:                    
            TSX               // Get stack pointer to X
            
            // Bitwise NOT on byte
            // TOP: SP+1 (single byte)
            LDA 0x0101, X     // Load TOP (at SP+1)
            EOR #0xFF         // XOR with 0xFF to flip all bits
            STA 0x0101, X     // Store result back
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
            
            

            
ADDB:                    
            TSX               // Get stack pointer to X
            
            // Add two bytes
            // TOP:  SP+1 (single byte)
            // NEXT: SP+2 (single byte)
            CLC               // Clear carry for addition
            LDA 0x0102, X     // Load NEXT (at SP+2)
            ADC 0x0101, X     // Add TOP (at SP+1)
            STA 0x0102, X     // Store result to NEXT position
            
            INX TXS           // Remove TOP from stack (1 byte)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
SUBB:                    
            TSX               // Get stack pointer to X
            
            // Subtract two bytes (NEXT - TOP)
            // TOP:  SP+1 (single byte)
            // NEXT: SP+2 (single byte)
            SEC               // Set carry for subtraction
            LDA 0x0102, X     // Load NEXT (at SP+2)
            SBC 0x0101, X     // Subtract TOP (at SP+1)
            STA 0x0102, X     // Store result to NEXT position
            
            INX TXS           // Remove TOP from stack (1 byte)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]    
            
NEGW:                     
            TSX               // Get stack pointer to X
            
            // Negate 16-bit value (2's complement)
            // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
            SEC               // Set carry for subtraction
            LDA #0          
            SBC 0x0102, X     // 0 - TOP0 (LSB at SP+2)
            STA 0x0102, X     // Store result back
            
            LDA #0
            SBC 0x0101, X     // 0 - TOP1 (MSB at SP+1) with borrow
            STA 0x0101, X     // Store result back
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
NEGB:                    
            TSX               // Get stack pointer to X
            
            // Negate 8-bit value (2's complement)
            // TOP: SP+1 (single byte)
            SEC               // Set carry for subtraction
            LDA #0
            SBC 0x0101, X     // 0 - TOP (at SP+1)
            STA 0x0101, X     // Store result back
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
                   
                                                                                          
LEB:
            TSX               // Get stack pointer to X
            
            LDA #1            // Default to 1 (NEXT <= TOP)
            STA operand
                    
            // Compare NEXT <= TOP
            // TOP is at SP+1 (single byte)
            // NEXT is at SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT
            CMP 0x0101, X     // Compare with TOP
            if (NZ)           // If NEXT != TOP
            {
                if (C)        // If NEXT >= TOP (carry set)
                {
                    STZ operand   // NEXT > TOP, so not <=
                }
            }
            
            INX               // Adjust stack by 1
            TXS
            LDA operand
            STA 0x0101, X     // store the boolean result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

LTB:
            TSX               // Get stack pointer to X
            
            STZ operand       // Default to 0 (NEXT >= TOP)
                    
            // Compare NEXT < TOP
            // TOP is at SP+1 (single byte)
            // NEXT is at SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT
            CMP 0x0101, X     // Compare with TOP
            if (NC)           // If NEXT < TOP (carry clear)
            {
                INC operand   // Set to 1 (NEXT < TOP)
            }
            
            INX               // Adjust stack by 1
            TXS
            LDA operand
            STA 0x0101, X     // store the boolean result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]                                                                       
                      
LEW:
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
            
LTW:
            TSX               // Get stack pointer to X
            
            STZ operand       // Default to 0 (NEXT >= TOP)
                    
            // Compare NEXT < TOP
            // TOP is at SP+1/SP+2 (top)
            // NEXT is at SP+3/SP+4
            // First compare high bytes
            LDA 0x0103, X     // Load NEXT1 (MSB)
            CMP 0x0101, X     // Compare with TOP1 (MSB)
            if (NC)           // If NEXT1 < TOP1 (carry clear)
            {
                INC operand   // Set to 1 (NEXT < TOP)
            }
            else
            {
                if (Z)        // If NEXT1 == TOP1
                {
                    LDA 0x0104, X     // Load NEXT0 (LSB)
                    CMP 0x0102, X     // Compare with TOP0 (LSB)
                    if (NC)           // If NEXT0 < TOP0
                    {
                        INC operand   // Set to 1 (NEXT < TOP)
                    }
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
            
EQB:
            TSX               // Get stack pointer to X
            
            STZ operand       // Default to 0 (not equal)
                    
            // Compare NEXT == TOP
            // TOP is at SP+1 (single byte)
            // NEXT is at SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT
            CMP 0x0101, X     // Compare with TOP
            if (Z)            // If NEXT == TOP
            {
                INC operand   // Set to 1 (equal)
            }
            
            INX               // Adjust stack by 1
            TXS
            LDA operand
            STA 0x0101, X     // store the boolean result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

NEB:
            TSX               // Get stack pointer to X
            
            STZ operand       // Default to 0 (equal)       
                    
            // Compare NEXT != TOP
            // TOP is at SP+1 (single byte)
            // NEXT is at SP+2 (single byte)
            LDA 0x0102, X     // Load NEXT
            CMP 0x0101, X     // Compare with TOP
            if (NZ)           // If NEXT != TOP
            {
                INC operand   // Set to 1 (they're not equal)
            }
            
            INX               // Adjust stack by 1
            TXS
            LDA operand
            STA 0x0101, X     // store the boolean result
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
                      
EQW:
            TSX               // Get stack pointer to X
            
            STZ operand       // Default to 0 (not equal)
                    
            // Compare NEXT == TOP
            // TOP is at SP+1/SP+2 (top)
            // NEXT is at SP+3/SP+4
            // First compare high bytes
            LDA 0x0103, X     // Load NEXT1 (MSB)
            CMP 0x0101, X     // Compare with TOP1 (MSB)
            if (Z)            // If NEXT1 == TOP1
            {
                LDA 0x0104, X // Load NEXT0 (LSB)
                CMP 0x0102, X // Compare with TOP0 (LSB)
                if (Z)        // If NEXT0 == TOP0
                {
                    INC operand   // Set to 1 (equal)
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
            
NEW:
            TSX               // Get stack pointer to X
            
            LDA #1            // Default to 1 (not equal)
            STA operand       
                    
            // Compare NEXT != TOP
            // TOP is at SP+1/SP+2 (top)
            // NEXT is at SP+3/SP+4
            // First compare high bytes
            LDA 0x0103, X     // Load NEXT1 (MSB)
            CMP 0x0101, X     // Compare with TOP1 (MSB)
            if (Z)            // If NEXT1 == TOP1
            {
                LDA 0x0104, X // Load NEXT0 (LSB)
                CMP 0x0102, X // Compare with TOP0 (LSB)
                if (Z)        // If NEXT0 == TOP0
                {
                    STZ operand   // Set to 0 (they're equal)
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
            
STRC:
            TSX               // Get stack pointer
            
            // Stack has:
            // SP+1: index (byte)
            // SP+2/SP+3: string address (word)
            
            // Get string address into zero page for indirect addressing
            LDA 0x0103, X     // String address LSB
            STA operandL      // Use operand as temporary pointer
            LDA 0x0102, X     // String address MSB  
            STA operandH
            
            // Get index into Y
            LDA 0x0101, X     // Index (byte)
            TAY               // Index in Y
            
            // Get character from string
            LDA [operand], Y  // Load character at string[index]
            
            // Adjust stack (remove 2 bytes net: 1+2-1)
            INX INX
            TXS
            
            // Push character result
            PHA
            
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
            
PUSHD2:                    
            LDA [codePage], Y // Low byte of word offset    
            INY
            CLC
            ADC constantsL
            PHA               // Push result LSB
            LDA [codePage], Y // High byte of word offset
            INY
            ADC constantsH    // Add with carry from low byte
            PHA               // Push result MSB
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
BNZR:       
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
            
BNZF:
            PLA                   // Pop the boolean
            if (NZ)               // If not zero
            {
                CLC               // Clear carry for addition
                TYA               // Current PC to A
                ADC [codePage], Y // Add branch offset
                TAY               // New PC back to Y
            }
            INY                   // Skip offset byte (whether branching or not)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
            
BZR:       
            PLX                   // pop the boolean
            if (Z)
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
            
BZF:
            PLA                   // Pop the boolean
            if (Z)                // If zero
            {
                CLC               // Clear carry for addition
                TYA               // Current PC to A
                ADC [codePage], Y // Add branch offset
                TAY               // New PC back to Y
            }
            INY                   // Skip offset byte (whether branching or not)
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]            
            
BRAF:
            CLC               // Clear carry for addition
            TYA               // Current PC to A
            ADC [codePage], Y // Add branch offset
            TAY               // New PC back to Y
            INY               // Skip offset byte
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]  
            
BRAR:
            SEC               // Set carry for subtraction
            TYA               // Current PC to A
            SBC [codePage], Y // Subtract branch offset
            TAY               // New PC back to Y
            INY               // Skip the offset byte
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSHGB:
            LDA [codePage], Y // byte offset  
            INY
            STY operand       // Save Y
            TAY
            LDA [globals], Y  // Load byte from globals[offset]
            LDY operand       // Restore Y
            PHA               // Push it
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

POPGB:
            LDA [codePage], Y // byte offset  
            INY
            STY operand       // Save Y
            TAY
            PLA               // Pop byte from stack
            STA [globals], Y  // Store to globals[offset]
            LDY operand       // Restore Y
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

PUSHGW:
            LDA [codePage], Y // byte offset  
            INY
            STY operand       // Save Y
            TAY
            LDA [globals], Y  // Load LSB from globals[offset]
            PHA               // Push LSB
            INY
            LDA [globals], Y  // Load MSB from globals[offset+1]
            PHA               // Push MSB
            LDY operand       // Restore Y
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]

POPGW:
            LDA [codePage], Y // byte offset  
            INY
            STY operand       // Save Y
            TAY
            INY               // Point to MSB position
            PLA               // Pop MSB
            STA [globals], Y  // Store MSB to globals[offset+1]
            DEY               // Back to LSB position
            PLA               // Pop LSB
            STA [globals], Y  // Store LSB to globals[offset]
            LDY operand       // Restore Y
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]
                              
PUSHZB:
            LDA [codePage], Y // byte offset  
            INY
            TAX
            LDA 0x00, X       // Load byte from ZP[offset]
            PHA               // Push it
            
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
            
POPZB:
            LDA [codePage], Y // byte offset  
            INY
            TAX
            PLA               // Pop byte
            STA 0x00, X       // Store at ZP[offset]
            
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
            
INCLB:
            CLC
            LDA BP            // BP + offset (LSB address)
            ADC [codePage], Y // byte offset
            INY
            TAX
            INC 0x0100, X     // Increment LSB
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]                      
            
PUSHLB:
            CLC
            LDA BP            // BP + offset
            ADC [codePage], Y // byte offset
            INY
            TAX
            LDA 0x0100, X     // Load byte at BP+offset
            PHA               // Push it
            
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
            
POPLB:
            CLC
            LDA BP            // BP + offset
            ADC [codePage], Y // byte offset
            INY
            TAX
            PLA               // Pop byte from stack
            STA 0x0100, X     // Store at BP+offset
            
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
            
                
SYSCALLX:
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
            
SYSCALL:
            LDA [codePage], Y // byte BIOS call index
            INY
            TAX
            PHY
            LDY yStore
            LDA aStore
            dispatchBIOS();
            if (Z) { SMB0 vmFlags } else { RMB0 vmFlags }
            if (C) { SMB1 vmFlags } else { RMB1 vmFlags }
            STA aStore
            PLY
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]    
            
READB:
            PLA
            STA operandH
            PLA
            STA operandL
            
            LDA [operand]
            PHA

            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X]                 
            
WRITEB:
            PLX
            PLA
            STA operandH
            PLA
            STA operandL
            
            TXA
            STA [operand]
            
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
            
CALL:
            LDA [codePage], Y  // function ID
            INY
            STA operand
            
            // return address PC (Y)
            PHY 
            // return address codePage
            LDA codePageL
            PHA
            LDA codePageH
            PHA
            
            // switch to new codePage
            LDY operand
            LDA [functionTable], Y
            STA codePageL
            INY
            LDA [functionTable], Y
            STA codePageH
            // Y = 0 (PC)
            LDY #0
            
            LDA [codePage], Y
            INY
            TAX
            JMP [opCodeJumps, X] 
            
RET:
            // restore codePage
            PLA
            STA codePageH
            PLA
            STA codePageL
            // restore PC (Y)
            PLY
            
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
            break;
            
        } // loop
        
        LDX stackStore
        TXS
    }
}
