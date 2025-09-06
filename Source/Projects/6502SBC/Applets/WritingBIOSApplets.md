# Hopper BIOS Applet Quick Reference

## Environment: Always 65C02S
No conditional compilation needed for CPU features - we always have the enhanced instruction set.

## Critical Zero Page Map
```hopper
// BIOS RESERVED - NEVER TOUCH:
0x00-0x05   System flags, error, heap management
0x06-0x0D   Serial, I2C, file system  
0x0E-0x0F   Debug only
0x10-0x1F   Core registers (ACC, TOP, NEXT, IDX, IDY, STR)
0x20-0x29   System slots (jump table, BIOS dispatch, emulator)
0x24-0x27   Timer ticks
0x30-0x41   M0-M17 workspace (BIOS functions use these!)
0x42-0x57   File system workspace

// FREE FOR APPLETS:
0x58-0xEB   YOUR SPACE (148 bytes available)

// HARDWARE I/O (immovable):
0xEC-0xFF   ACIA, VIA ports
```

## 65C02S Enhanced Instructions Always Available
```hopper
STZ address          // Store zero directly
PHX/PLX, PHY/PLY    // Direct stack ops for X,Y
BRA target          // Branch always
TSB/TRB             // Test and set/reset bits
SMB0-7/RMB0-7       // Set/reset memory bits directly
BBS0-7/BBR0-7       // Branch on bit set/reset
INC A/DEC A         // Modify accumulator directly
[ZP.PTR]            // Zero page indirect (cleaner than (ZP),Y)
```

## BIOS Call Pattern
```hopper
// All BIOS calls:
LDX #SysCall.FunctionName
JMP [ZP.BIOSDISPATCH]

// Common patterns:
Print.String():       ZP.STR = string pointer
Memory.Allocate():    ZP.ACC = size → returns ZP.IDX
Time.Delay():         ZP.TOP = milliseconds (32-bit)
GPIO.PinMode():       A = pin, Y = mode
Serial.WriteChar():   A = character
Long.Add():          ZP.NEXT + ZP.TOP → ZP.NEXT
```

## String Handling - Do It Right
```hopper
// ALWAYS use string constants:
const string message = "Hello World!\n";

// Print it properly:
LDA #(message % 256)
STA ZP.STRL
LDA #(message / 256)  
STA ZP.STRH
Print.String();

// NEVER spell out strings character by character!
```

## Register Preservation Rules
```hopper
PublicMethod()  // Uppercase = public
{
    PHX         // ONLY if you modify X
    PHY         // ONLY if you modify Y
    // A is NEVER preserved - caller saves
    
    // Do work...
    
    PLY         // Only if saved
    PLX         // Only if saved
    SEC         // C set = success
}

privateHelper() // Lowercase = private  
{
    // NO preservation requirements
    // Trash any registers freely
}
```

## Control Flow (Structured, Not Labels)
```hopper
// Flag tests:
if (Z)  { /* zero flag set */ }
if (NZ) { /* zero flag clear */ }
if (C)  { /* carry set (usually success) */ }
if (NC) { /* carry clear (usually failure) */ }
if (MI) { /* minus/negative */ }
if (PL) { /* plus/positive */ }

// 65C02S bit tests:
if (BBS0, ZP.FLAGS) { /* bit 0 set */ }
if (BBR7, ZP.STATUS) { /* bit 7 clear */ }

loop
{
    // work
    if (done) { break; }
    if (skip) { continue; }
}

// NO labels/jumps except within a single method
```

## Program Template
```hopper
program MyApplet
{
    uses "System/Definitions"  // Gets ZP, BIOS interface
    uses "System/Print"
    uses "System/Serial"
    uses "System/Memory"
    
    // Your zero page allocations (USE FREE ZONE!):
    const byte myCounter = 0x58;
    const byte myFlags = 0x59;
    const uint myPointer = 0x5A;  // 0x5A-0x5B for 16-bit
    
    const string title = "My Applet v1.0\n";
    const string error = "Error occurred!\n";
    
    Hopper()  // Entry point
    {
        // Initialize
        STZ myCounter       // 65C02S zero instruction
        STZ myFlags
        
        // Print title
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        Print.String();
        
        // Main work...
        
        // Check for user break:
        Serial.IsBreak();
        if (C) 
        { 
            // Cleanup and exit
            return;
        }
    }
}
```

## Common Operations
```hopper
// 16-bit value loading:
LDA #(value % 256)
STA ZP.ADDRL
LDA #(value / 256)
STA ZP.ADDRH

// 16-bit increment:
INC ZP.ADDRL
if (Z)
{
    INC ZP.ADDRH
}

// Clear 16-bit value (65C02S):
STZ ZP.ADDRL
STZ ZP.ADDRH

// Check allocation success:
Memory.Allocate();
LDA ZP.IDXL
ORA ZP.IDXH
if (Z)  // 0x0000 = failed
{
    CLC  // Failure
    return;
}

// Set/clear bits (65C02S):
SMB7 ZP.FLAGS      // Set bit 7
RMB0 ZP.FLAGS      // Clear bit 0
```

## Success/Failure Convention
```hopper
// ALWAYS use carry flag:
DoSomething()
{
    // Try operation...
    if (failed)
    {
        CLC  // Clear carry = failure
        return;
    }
    
    SEC      // Set carry = success
}

// Caller checks:
DoSomething();
if (NC)      // No carry = failed
{
    // Handle error
}
```

## Mistakes to Avoid
1. **Using BIOS zero page** - M0-M17 are BIOS workspace!
2. **Raw addresses** - Use `ZP.NAME` not `$xx` or `0xXX`
3. **Character-by-character strings** - Use string constants
4. **Preserving A** - Accumulator is never preserved
5. **Blanket register saves** - Only save X/Y if you modify them
6. **Missing parentheses** - `LDA (value + 1)` not `LDA value + 1`
7. **Using old 6502 patterns** - We have STZ, PHX/PLX, etc!

## Debug Pattern
```hopper
// Instrument first, speculate later:
LDA suspect_value
Print.Hex();
Print.Space();
LDA suspect_value + 1
Print.Hex();
Print.NewLine();

// Show progress:
LDA #'.'
Print.Char();
```

## Working Example - Counter Display
```hopper
program Counter
{
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Time"
    uses "System/Serial"
    
    const byte counter = 0x58;  // Our free ZP
    const string title = "Counter Demo\n";
    
    Hopper()
    {
        // Print title
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        Print.String();
        
        STZ counter  // 65C02S clear
        
        loop
        {
            // Display counter
            LDA counter
            Print.Hex();
            Print.Space();
            
            // 100ms delay
            LDA #100
            STA ZP.TOP0
            STZ ZP.TOP1  // 65C02S
            STZ ZP.TOP2
            STZ ZP.TOP3
            Time.Delay();
            
            // Increment and check
            INC counter
            CMP #20
            if (Z) { break; }
            
            // Check for break
            Serial.IsBreak();
            if (C) { break; }
        }
        
        Print.NewLine();
    }
}
```

Remember: We're on 65C02S hardware. Use those enhanced instructions! Respect the BIOS zero page. Use structured flow. String constants always.