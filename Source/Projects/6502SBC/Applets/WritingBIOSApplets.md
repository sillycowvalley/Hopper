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
                break;  // Exit loop
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
                break;  // Exit loop
            }
            default:
            {
                // Reset on error
                STZ escState
                CLC
                break;  // Exit loop
            }
        }
        // If no break executed, loop continues
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

### Flag Conditions
- `if (Z)` - Zero flag set
- `if (NZ)` - Zero flag clear
- `if (C)` - Carry set (usually success)
- `if (NC)` - Carry clear (usually failure)
- `if (MI)` - Negative flag set
- `if (PL)` - Positive flag clear
- `if (V)` - Overflow set
- `if (NV)` - Overflow clear

### 65C02S Bit Tests
- `if (BBS0, address)` - Branch if bit 0 set
- `if (BBR7, address)` - Branch if bit 7 reset

### Remember
**Hopper Assembly is STRUCTURED ASSEMBLY** - it combines the power of assembly with the clarity of structured programming. Embrace the structure!