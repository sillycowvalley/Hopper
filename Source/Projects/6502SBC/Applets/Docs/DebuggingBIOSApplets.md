# Debugging Guide for Hopper BIOS Applets

## Core Debugging Principles

### 1. Zero Page Conflicts Are Always Suspect #1
When encountering inexplicable behavior, immediately analyze zero page usage across all modules in the call chain.

**Analysis Pattern:**
```hopper
// Document the call chain:
Test → Debug.String() → Screen.SaveCursor() → corrupts ZP.STR
```

### 2. What Changed? Not What's Broken
When previously working code fails, focus on recent changes. The bug is almost certainly in new/modified code, not in code that was working.

### 3. Analyze Before Generating Code
When debugging, resist the urge to immediately write fixes. Instead:
1. Identify what changed
2. Analyze where the issue could be
3. Add targeted debug output
4. Only then generate fixes

## Common Failure Patterns

### Register Corruption
**BIOS calls preserve A and Y but NOT X**

```hopper
// ✅ CORRECT - Save X around BIOS calls
LDX #10
loop
{
    PHX
    Serial.WriteChar();
    PLX
    DEX
    if (Z) { break; }
}

// ❌ WRONG - X gets corrupted
LDX #10
loop
{
    Serial.WriteChar();  // Trashes X!
    DEX                  // X is garbage
    if (Z) { break; }
}
```

### Zero Page Corruption
**Common culprits:**
- Screen functions that internally use Print.String() (corrupts ZP.STR)
- Overlapping module allocations
- M0-M17 workspace conflicts in non-leaf functions

### Switch Statement Gotcha
Hopper switch statements don't fall through. `break;` in a case exits the **enclosing loop**, not the switch!

```hopper
loop
{
    switch (A)
    {
        case State.Error:
        {
            break;  // Exits the LOOP, not the switch!
        }
    }
    // Code here may be unexpectedly skipped
}
```

## Debugging Strategies

### Strategic Debug Output
Place debug output at the left margin (no indentation) for easy removal:

```hopper
processData()
{
    LDA value
    STA ZP.ACCL
    
// Debug point 1
LDA #'1'
Serial.WriteChar();
LDA ZP.ACCL
Print.Hex();

    Memory.Allocate();
    
// Debug point 2  
LDA #'2'
Serial.WriteChar();
}
```

### Zero Page Inspection
```hopper
inspectAppZeroPage()
{
    LDA #0x58    // App zero page start
    STA ZP.IDXL
    STZ ZP.IDXH
    LDA #32      // Inspect 32 bytes
    Debug.DumpMemory();
}
```

### Call Chain Analysis
Document every function in the chain and what zero page it uses:

```
Module          Uses            Modifies
------          ----            --------
Main            ZP.STR          -
Debug           ZP.STR          debugRow, debugEnabled
Screen          -               ZP.M0, ZP.M1 (was ZP.STR!)
Serial          -               -
```

## Syntax and Style Rules

### Enum Usage
Use direct enum syntax without unit qualification:
```hopper
// ✅ CORRECT
LDA #SymbolType.Variable

// ❌ WRONG
LDA #Objects.SymbolType.Variable
```

### Flag Comments
Use clear flag descriptions:
```hopper
// ✅ GOOD
CMP #10
// Sets C if >= 10
// Sets Z if equal

// ❌ AVOID
// Sets 1 if >= 10
// Sets 0 if not equal
```

### Success/Failure Convention
Prefer C/NC for success/failure over Z/NZ:
```hopper
// ✅ PREFERRED
Memory.Allocate();
if (NC)  // Failed
{
    handleError();
}

// ❌ AVOID
checkSomething();
if (Z)   // Less clear what this means
{
    handleResult();
}
```

## Prevention Strategies

### Initialize Your Zero Page
```hopper
Initialize()
{
    // Clear all module slots on startup
    LDX #(mySlots + mySize - 1)
    LDA #0
    loop
    {
        STA 0x00, X
        DEX
        CPX #mySlots
        if (NC) { break; }
    }
}
```

### Document Side Effects
```hopper
// PrintString()
// Input:  ZP.STR = string pointer
// Output: None
// Modifies: A, X (NOT Y)
// Preserves: Y, ZP.STR
```

### Defensive Coding
```hopper
SafeFunction()
{
    // Save critical values
    LDA ZP.STRL
    PHA
    LDA ZP.STRH
    PHA
    
    SomeSystemCall();  // Might corrupt ZP.STR
    
    // Restore critical values
    PLA
    STA ZP.STRH
    PLA
    STA ZP.STRL
}
```

## Serial Output Analysis

Look for telltale signs in serial output:
- Duplicate save cursor sequences (`\x1B[s\x1B[s`)
- Missing expected strings
- Corrupted escape sequences
- Unexpected characters where strings should be

## The Debugging Checklist

When something breaks:

1. **What changed?** - Focus on recent modifications
2. **Check zero page conflicts** - List all modules, document their ZP usage
3. **Check X register preservation** - Any BIOS calls in loops?
4. **Check ZP.STR integrity** - Any Screen functions between set and use?
5. **Check M0-M17 usage** - Leaf functions only?
6. **Add strategic debug output** - At decision points and state changes
7. **Trace the call chain** - Document every function called
8. **Binary search the problem** - Comment out half the code to isolate

## Remember

In 6502 programming with limited zero page, conflicts are not just possible - they're probable. When you see mysterious behavior:

**First:** Check zero page allocation
**Second:** Check X register corruption  
**Third:** Check what changed recently
**Always:** Analyze before generating fixes

Silent failures are never acceptable. If something might fail, handle it explicitly or use BRK to make the failure visible during development.