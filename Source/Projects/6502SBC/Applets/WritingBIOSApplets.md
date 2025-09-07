# Hopper BIOS Applet Complete Guidelines

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

---

# 📍 ZERO PAGE MANAGEMENT

## Critical Zero Page Map
```hopper
// BIOS RESERVED - NEVER TOUCH:
0x00-0x05   System flags, error, heap management
0x06-0x0D   Serial, I2C, file system  
0x0E-0x0F   Debug only
0x10-0x1F   Core registers (ACC, TOP, NEXT, IDX, IDY, STR)
0x20-0x29   System slots (jump table, BIOS dispatch, emulator)
0x24-0x27   Timer ticks
0x30-0x41   M0-M17 workspace (SPECIAL - see below!)
0x42-0x57   File system workspace

// FREE FOR APPLETS:
0x58-0xEB   YOUR SPACE (148 bytes available)

// HARDWARE I/O (immovable):
0xEC-0xFF   ACIA, VIA ports
```

## 🎯 Zero Page Best Practices

### 1. Use Single Base + Offsets Pattern
**Make your code relocatable by using a single const and offsets:**

```hopper
unit MyUnit
{
    // ALL zero page definitions at TOP of unit!
    
    // Single base constant - easy to relocate
    const byte mySlots = 0x58;
    
    // Public properties using offsets
    const byte Property1 = mySlots+0;
    const byte Property2 = mySlots+1;
    const byte Property3 = mySlots+2;
    
    // 16-bit values need all three definitions
    const uint Pointer   = mySlots+3;
    const byte PointerL  = mySlots+3;
    const byte PointerH  = mySlots+4;
    
    // Private workspace continues with offsets
    const byte workspace1 = mySlots+5;
    const byte workspace2 = mySlots+6;
    
    // Methods follow...
}
```

### 2. ALL Zero Page Definitions at Top
**No scattered definitions! All ZP allocations must be at the top of the unit for clarity:**

```hopper
unit BadExample
{
    const byte var1 = 0x58;
    
    SomeMethod()
    {
        const byte var2 = 0x59;  // NO! Don't hide ZP definitions in methods
    }
}

unit GoodExample  
{
    // ALL zero page definitions here
    const byte var1 = 0x58;
    const byte var2 = 0x59;
    
    SomeMethod()
    {
        // Use them, don't define them here
    }
}
```

### 3. 16-bit Zero Page Naming Convention
**ALWAYS define uint AND L/H bytes separately to prevent errors:**

```hopper
// CORRECT - Define all three:
const uint myPointer = 0x5A;   // For [myPointer] syntax
const byte myPointerL = 0x5A;  // For LDA myPointerL
const byte myPointerH = 0x5B;  // For LDA myPointerH

// Usage patterns:
LDA [myPointer], Y    // Indirect addressing
LDA myPointerL        // Direct low byte access
STA myPointerH        // Direct high byte access
```

## 🔧 M0-M17 Workspace - The Tricky Part

### What Are M0-M17?
These are **shared leaf function workspace** locations (0x30-0x41) that can be reused by different functions under strict conditions.

### Rules for Using M0-M17:

1. **Leaf Functions Only** - Can only be used by functions that don't call other API methods
2. **No Survival Expected** - Values don't survive beyond the current function
3. **Check Conflicts** - Never use if calling functions that also use them
4. **Document Usage** - Update ZeroPage.asm documentation when adding new uses
5. **Prefer for Temporary Heavy Use** - Ideal for functions needing many temporary variables

### Who Currently Uses M0-M17?
From ZeroPage.asm documentation:
- **Memory.Allocate()** and **Memory.Free()** - Never call these if using M0-M17!
- **Time.Delay()** - Uses M0-M3 as TARGET0-3
- **Time.Seconds()** - Uses M0-M7 as RESULT0-7
- **Long math operations** - Use M0-M7 for RESULT
- **Debug functions** - Use M0-M15 as DB0-DB15
- **ScreenBuffer.Update()** - Major leaf API using M0-M9

### Example: ScreenBuffer Using M0-M17
```hopper
unit ScreenBuffer
{
    // Regular allocations
    const byte zeroPageSlots = 0x58;
    
    const byte CursorCol  = zeroPageSlots+0;
    const byte CursorRow  = zeroPageSlots+1;
    const byte Foreground = zeroPageSlots+2;
    const byte Background = zeroPageSlots+3;
    const byte Attributes = zeroPageSlots+4;
    
    // ... more regular slots ...
    
    // LEAF FUNCTION workspace using M0-M17
    // Update() is a leaf - doesn't call Memory.Allocate or other APIs
    const byte sbSize    = ZP.M0;   // Temporary use in Update()
    const byte sbSizeL   = ZP.M0;
    const byte sbSizeH   = ZP.M1;
    
    const byte sbRow     = ZP.M2;   // Only during Update()
    const byte sbCol     = ZP.M3;
    
    const byte sbOffset  = ZP.M4;
    const byte sbOffsetL = ZP.M4;
    const byte sbOffsetH = ZP.M5;
    
    Update()  // LEAF function - safe to use M0-M17
    {
        // Can freely use M0-M9 here
        // But CANNOT call Memory.Allocate() or Memory.Free()!
    }
    
    Initialize()  // NOT a leaf - calls Memory.Allocate
    {
        // CANNOT use M0-M17 here!
        Memory.Allocate();  // This uses M0-M17
    }
}
```

### When to Use M0-M17 vs New Slots?

**USE M0-M17 when:**
- Function is a true leaf (no API calls)
- Need many temporary variables
- Function is performance-critical
- Variables are purely temporary

**ALLOCATE NEW SLOTS when:**
- Function calls other APIs
- Values must survive function calls
- Not sure about conflicts
- Need permanent storage

---

# 📚 CONTROL FLOW IN HOPPER ASSEMBLY

## 🔄 LOOPS - Structured, Not Labels!

### ❌ FORBIDDEN - Traditional Assembly Style
```hopper
// NEVER DO THIS IN HOPPER:
loop_start:         // NO! No labels!
    DEX
    BNE loop_start  // NO! No branch instructions!
    
rowLoop:            // NO! Named loops don't exist!
    // ...
    JMP rowLoop     // NO! No JMP for loops!
```

### ✅ CORRECT - Hopper Structured Loops
```hopper
loop
{
    // Loop body
    if (done) { break; }      // Exits current loop
    if (skip) { continue; }    // Goes to top of current loop
}
```

### Loop Control Instructions
- **`break`** - Exits the current loop immediately
- **`continue`** - Jumps to the top of the current loop
- **NO `goto`** - Doesn't exist in Hopper
- **NO labels** - Loops are anonymous blocks

### Nested Loops - The Right Way
```hopper
// CORRECT nested loop pattern:
STZ rowIndex
loop    // Outer loop (anonymous!)
{
    STZ colIndex
    loop    // Inner loop (also anonymous!)
    {
        // Work with current position
        processCell();
        
        INC colIndex
        LDA colIndex
        CMP width
        if (Z) { break; }  // Exits INNER loop only
    }
    
    INC rowIndex
    LDA rowIndex
    CMP height
    if (Z) { break; }  // Exits OUTER loop only
}
```

### Common Loop Patterns

#### Count from 0 to N-1:
```hopper
LDX #0
loop
{
    // Process element X
    INX
    CPX count
    if (Z) { break; }
}
```

#### Count down from N to 1:
```hopper
LDX count
loop
{
    // Process element X
    DEX
    if (Z) { break; }
}
```

#### Infinite loop with conditional exit:
```hopper
loop
{
    getInput();
    if (done) { break; }
    processInput();
}
```

#### Process null-terminated string:
```hopper
LDY #0
loop
{
    LDA [STR], Y
    if (Z) { break; }  // Null terminator
    
    processChar();
    INY
}
```

---

## 🔀 SWITCHES - No Fall-Through!

### Critical Difference from C
**Hopper switches NEVER fall through!** Each case is isolated.

### ❌ WRONG - C-Style Thinking
```c
// This is C, NOT Hopper:
switch(x) {
    case 1:
        doA();
        break;  // In C, break exits the switch
    case 2:
        doB();
        // Falls through to case 3 in C
    case 3:
        doC();
        break;
}
```

### ✅ CORRECT - Hopper Switch
```hopper
switch (A)
{
    case 1:
    {
        doA();
        // No break needed - cases don't fall through
    }
    case 2:
    {
        doB();
        // Isolated from other cases
    }
    case 3:
    {
        doC();
    }
    default:
    {
        handleUnknown();
    }
}
```

### 🚨 CRITICAL: Break in Switch Cases
**`break` inside a switch case exits the ENCLOSING LOOP, not the switch!**

```hopper
loop
{
    switch (A)
    {
        case 1:
        {
            if (error) 
            { 
                break;  // Exits the LOOP, not the switch!
            }
            processOne();
        }
        case 2:
        {
            processTwo();
            break;      // Exits the LOOP, not the switch!
        }
        default:
        {
            // Unknown
        }
    }
    // Only reached if no break was executed
    continueProcessing();
}
```

### Advanced Pattern: Switch with Dual Exit Paths
```hopper
// Elegant pattern using break semantics
processState()
{
    loop
    {
        switch (state)
        {
            case STATE_A:
            {
                handleA();
                SEC  // Success path
            }
            case STATE_B:
            {
                handleB();
                SEC  // Success path
            }
            default:
            {
                CLC  // Error path
                break;  // Exit loop with error
            }
        }
        // Success cases reach here
        break;  // Exit loop with success
    }
}
```

### Switch Optimization Rules
Switches can be optimized into jump tables when:
- Switch operates on **X or Y register** (not A)
- Each case contains **exactly one subroutine call**
- **Default case is present**
- **More than 8 cases** total
- Switch is followed immediately by **return** or end of method

```hopper
// Optimizable switch:
switch (X)
{
    case 0: { Routine0(); }
    case 1: { Routine1(); }
    case 2: { Routine2(); }
    // ... more cases ...
    case 9: { Routine9(); }
    default: { DefaultRoutine(); }
}
return;  // Required for optimization
```

---

## 🎯 SINGLE EXIT PATTERN

### Purpose
Single exit makes code more maintainable and predictable. All paths converge to one point where cleanup can occur.

### ❌ WRONG - Multiple Returns
```hopper
badMethod()
{
    if (error1)
    {
        return;  // Early exit
    }
    
    if (error2)
    {
        return;  // Another exit
    }
    
    doWork();
    return;      // Yet another exit
}
```

### ✅ CORRECT - Single Exit
```hopper
goodMethod()
{
    loop
    {
        if (error1)
        {
            CLC  // Set failure flag
            break;
        }
        
        if (error2)
        {
            CLC  // Set failure flag
            break;
        }
        
        doWork();
        SEC  // Set success flag
        break;
    }
    // Single exit point - cleanup here if needed
}
```

### Single Exit with Result Processing
```hopper
processData()
{
    loop
    {
        openFile();
        if (NC)  // Failed
        {
            LDA #Error.FileNotFound
            break;
        }
        
        readData();
        if (NC)  // Failed
        {
            LDA #Error.ReadError
            break;
        }
        
        processBuffer();
        LDA #Success
        break;
    }
    // Single exit - A contains result code
    // Any cleanup code goes here
}
```

---

## 🚫 NO BRANCH INSTRUCTIONS OR LABELS

### The Hopper Philosophy
Hopper Assembly uses **structured control flow** exclusively. Direct branches and labels are forbidden in normal code.

### ❌ FORBIDDEN Instructions
```hopper
// NEVER use these in regular Hopper code:
BEQ label    // NO! Use if (Z) { }
BNE label    // NO! Use if (NZ) { }
BCS label    // NO! Use if (C) { }
BCC label    // NO! Use if (NC) { }
BMI label    // NO! Use if (MI) { }
BPL label    // NO! Use if (PL) { }
JMP label    // NO! Use loop/break/continue
BRA label    // NO! Use structured flow
```

### ✅ CORRECT - Structured Equivalents
```hopper
// Instead of: BEQ skip_code
if (NZ)
{
    // Code to execute if not zero
}

// Instead of: BCS error_handler
if (NC)
{
    // Success path
}
else
{
    // Error path
}

// Instead of: JMP loop_start
loop
{
    // Loop body
    if (done) { break; }
}
```

### The ONLY Exception
Labels and JMP are allowed **ONLY** within a single method for optimization, and should be rare:

```hopper
optimizedMethod()
{
    // RARE EXCEPTION - optimization within single method
    LDX #8
    JMP entry  // Skip first iteration setup
    loop
    {
        INC counter
entry:
        ASL data
        DEX
        if (Z) { break; }
    }
}
```

---

## 65C02S Enhanced Instructions Always Available
```hopper
STZ address          // Store zero directly
PHX/PLX, PHY/PLY    // Direct stack ops for X,Y
BRA target          // Branch always (but don't use!)
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

## Register Preservation Rules
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
}
```

---

## 🎨 COMPLETE PATTERN EXAMPLES

### State Machine with Single Exit
```hopper
processEscapeSequence()
{
    loop
    {
        LDA escState
        switch (A)
        {
            case 0:  // Normal state
            {
                readChar();
                if (isEscape)
                {
                    LDA #1
                    STA escState
                    continue;  // Loop again
                }
                // Have regular char
                SEC
            }
            case 1:  // Got ESC
            {
                readChar();
                if (isBracket)
                {
                    LDA #2
                    STA escState
                    continue;  // Loop again
                }
                // Invalid sequence
                CLC
                break;  // Exit loop with error
            }
            default:
            {
                // Reset on error
                STZ escState
                CLC
                break;  // Exit loop with error
            }
        }
        break;  // Exit loop (success path reaches here)
    }
    // Single exit point
}
```

### Nested Processing with Error Handling
```hopper
processMatrix()
{
    STZ error
    STZ row
    loop  // Process rows
    {
        STZ col
        loop  // Process columns
        {
            processElement();
            if (NC)  // Error occurred
            {
                INC error
                break;  // Exit column loop
            }
            
            INC col
            LDA col
            CMP width
            if (Z) { break; }  // Done with columns
        }
        
        // Check if column loop had error
        LDA error
        if (NZ) { break; }  // Exit row loop on error
        
        INC row
        LDA row
        CMP height
        if (Z) { break; }  // Done with rows
    }
    
    // Single exit - check final status
    LDA error
    if (Z)
    {
        SEC  // Success
    }
    else
    {
        CLC  // Failure
    }
}
```

---

## 📋 Quick Reference Card

### Control Flow Rules
1. **NO labels** (except rare optimization within single method)
2. **NO branch instructions** (BEQ, BNE, JMP, etc.)
3. **Use `loop { }`** for all loops
4. **Use `if (condition) { }`** for all conditionals
5. **Switch cases don't fall through**
6. **`break` in switch exits the enclosing loop**
7. **Single exit pattern** for maintainability

### Zero Page Rules
1. **Single base + offsets** for easy relocation
2. **ALL definitions at top** of unit
3. **Define uint AND L/H bytes** for 16-bit values
4. **M0-M17 for leaf functions only**
5. **Document M0-M17 usage** in ZeroPage.asm

### Flag Conditions
- `if (Z)` - Zero flag set
- `if (NZ)` - Zero flag clear
- `if (C)` - Carry set (usually success)
- `if (NC)` - Carry clear (usually failure)
- `if (MI)` - Negative flag set
- `if (PL)` - Positive flag clear

### Remember
**Hopper Assembly is STRUCTURED ASSEMBLY** - it combines the power of assembly with the clarity of structured programming. Embrace the structure!