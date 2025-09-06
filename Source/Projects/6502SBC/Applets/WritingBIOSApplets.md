# Hopper BIOS Applet Quick Reference

## 🚨 CRITICAL: Always Define CPU for Programs
```hopper
program MyApplet
{
    #define CPU_65C02S    // ALWAYS REQUIRED FOR PROGRAMS!
    
    uses "System/Definitions"
    // ... rest of program
}
```

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

## 🚨 CRITICAL: 16-bit Zero Page Naming Convention
**ALWAYS define both the uint AND the L/H bytes separately:**
```hopper
// CORRECT way to define 16-bit zero page locations:
const uint myPointer = 0x5A;   // For [myPointer] syntax
const byte myPointerL = 0x5A;  // For LDA myPointerL
const byte myPointerH = 0x5B;  // For LDA myPointerH

// This prevents dyslexic errors and allows both usage patterns:
LDA [myPointer], Y    // Indirect addressing
LDA myPointerL        // Direct low byte access
STA myPointerH        // Direct high byte access
```

## 🚨 CRITICAL: Loop Construct Rules
**Hopper Assembly has STRUCTURED loops with NO LABELS!**

### ❌ WRONG - This is NOT Hopper Assembly:
```hopper
rowLoop:        // NO! No labeled loops!
{
    // ...
    if (condition) { continue rowLoop; }  // WRONG!
}
```

### ✅ CORRECT - Hopper Assembly loops:
```hopper
loop
{
    // Loop body
    if (done) { break; }      // Exits current loop
    if (skip) { continue; }    // Goes to top of current loop
}
```

### Nested Loops - CORRECT Pattern:
```hopper
LDX #0  // Row counter
loop    // Outer loop
{
    PHX
    LDY #0  // Column counter
    loop    // Inner loop
    {
        // Do work with Y
        INY
        CPY width
        if (Z) { break; }  // Exits INNER loop only
    }
    PLX
    INX
    CPX height
    if (Z) { break; }  // Exits OUTER loop
}
```

### Common Loop Patterns:
```hopper
// Count up:
LDX #0
loop
{
    // work
    INX
    CPX limit
    if (Z) { break; }
}

// Count down:
LDX count
loop
{
    // work
    DEX
    if (Z) { break; }
}

// While-style:
loop
{
    // check condition
    if (done) { break; }
    // work
}
```

## 🚨 Register Preservation Rules - SIMPLIFIED
**CALLEE preserves what it modifies (except A):**

```hopper
PublicMethod()  // Uppercase = public
{
    // Only preserve registers THIS METHOD modifies:
    PHX         // ONLY if this method changes X
    PHY         // ONLY if this method changes Y
    // NEVER preserve A - caller's responsibility
    
    // Do work...
    
    PLY         // Only if we pushed Y
    PLX         // Only if we pushed X
    // Return with meaningful flag (usually C for success/failure)
}

privateHelper() // Lowercase = private  
{
    // Private methods called only internally
    // Check if caller already preserved - avoid double-preservation!
    // If caller saved X/Y, don't save again
}
```

### Stack Management Anti-Pattern to AVOID:
```hopper
// BAD - Double preservation:
Caller()
{
    PHY         // Caller saves Y
    callee();   // Callee ALSO saves Y - wasteful!
    PLY
}

// GOOD - Single preservation:
Caller()
{
    callee();   // Callee preserves Y if it modifies it
}
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

// Remember: NO labeled loops, NO goto!
```

## Program Template
```hopper
program MyApplet
{
    #define CPU_65C02S              // ALWAYS REQUIRED!
    
    uses "System/Definitions"       // Gets ZP, BIOS interface
    uses "System/Print"
    uses "System/Serial"
    uses "System/Memory"
    
    // Your zero page allocations (USE FREE ZONE!):
    const byte myCounter = 0x58;
    const byte myFlags = 0x59;
    
    // 16-bit values need both definitions:
    const uint myPointer = 0x5A;
    const byte myPointerL = 0x5A;
    const byte myPointerH = 0x5B;
    
    const string title = "My Applet v1.0\n";
    const string error = "Error occurred!\n";
    
    Hopper()  // Entry point
    {
        // Initialize
        STZ myCounter
        STZ myFlags
        STZ myPointerL
        STZ myPointerH
        
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
// 16-bit value loading (using proper naming):
LDA #(value % 256)
STA myPointerL      // Use the L suffix
LDA #(value / 256)
STA myPointerH      // Use the H suffix

// 16-bit increment:
INC myPointerL
if (Z)
{
    INC myPointerH
}

// Clear 16-bit value (65C02S):
STZ myPointerL
STZ myPointerH

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
4. **Preserving A** - Accumulator is never preserved by callee
5. **Double register saves** - Don't preserve in both caller and callee
6. **Missing parentheses** - `LDA #(value + 1)` not `LDA #value + 1`
7. **Using old 6502 patterns** - We have STZ, PHX/PLX, etc!
8. **Labeled loops** - Hopper uses structured `loop`, not labels!
9. **Missing L/H definitions** - Always define both uint and L/H bytes
10. **Missing CPU define** - Programs must have `#define CPU_65C02S`

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

## Working Example - Nested Loop Pattern
```hopper
program NestedLoopDemo
{
    #define CPU_65C02S    // REQUIRED!
    
    uses "System/Definitions"
    uses "System/Print"
    
    const byte rows = 0x58;
    const byte cols = 0x59;
    const byte currentRow = 0x5A;
    const byte currentCol = 0x5B;
    
    Hopper()
    {
        LDA #5
        STA rows
        LDA #10
        STA cols
        
        STZ currentRow
        loop  // Outer loop - rows
        {
            STZ currentCol
            loop  // Inner loop - columns
            {
                // Do work at [currentRow, currentCol]
                LDA #'*'
                Print.Char();
                
                INC currentCol
                LDA currentCol
                CMP cols
                if (Z) { break; }  // Exit inner loop
            }
            
            Print.NewLine();
            
            INC currentRow
            LDA currentRow
            CMP rows
            if (Z) { break; }  // Exit outer loop
        }
    }
}
```

Remember: 
- **NO labeled loops** - use structured `loop { }`
- **Define CPU** - `#define CPU_65C02S` in every program
- **L/H suffixes** - Define both uint and byte versions
- **Single preservation** - Callee saves what it modifies (except A)
- **Use enhanced 65C02S** - STZ, PHX/PLX, etc.