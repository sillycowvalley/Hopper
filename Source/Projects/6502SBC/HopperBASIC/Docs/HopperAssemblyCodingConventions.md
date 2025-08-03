# Hopper Assembly Coding Conventions
**Document Type: Coding Standards**

## Core Principles

### 1. No Silent Failures (RULE #1)
- **Always report errors explicitly** before failing
- **BRK is acceptable** - it throws an exception in the emulator
- **Standard TODO/unimplemented pattern**:
```hopper
// TODO: Implement feature X / Show variables
LDA #(Messages.NotImplemented % 256)
STA ZP.LastErrorL
LDA #(Messages.NotImplemented / 256)
STA ZP.LastErrorH
BRK
```

### 2. Zero Page Management (RULE #2)
- **Document all zero page usage** in your project
- **Respect existing allocations** when integrating with other code
- **You may not use ZeroPage slots that are not part of the BASIC project without asking first**
- **Prefer stack for temporary storage** in general-purpose code:
```hopper
// Good - use stack for temporary values over short simple sections
PHA
PHX
PHY
// ... code ...
PLY
PLX
PLA

// Bad - assuming zero page locations are free
STA $90  // Is this allocated? Document it!
```

### 3. Accessibility-Friendly Comments (RULE #3)
- **Use descriptive flag comments** for better readability (easier for dyslexic developers):
```hopper
// Correct:
if (Z)   // Set Z (zero flag set)
if (NZ)  // Set NZ (zero flag clear) 
if (C)   // Set C (carry set)
if (NC)  // Set NC (carry clear)
if (MI)  // Set MI (minus/negative)
if (PL)  // Set PL (plus/positive)

// Avoid (hard for dyslexic developers):
if (Z)   // 1
if (NZ)  // 0
```

### 4. Complete Code Generation (RULE #4)
- **Always generate complete methods**
- **Never use placeholders** like:
```hopper
// FORBIDDEN:
// Rest of the function remains the same...
// ... existing code ...

// ALWAYS generate the complete implementation
```

### 5. Debug-First Methodology (RULE #5)
- **When debugging, instrument before speculating**
- **Use debug helpers that preserve flags** (Tools.Dump* and Tools.*Out() methods in preference to Serial.Out methods):
  - `Serial.HexOut()` for simple output
  - Create project-specific debug helpers
  - Document what registers/flags are preserved
- **Suggest instrumentation points** rather than rewriting code:
```hopper
// "Add debug output after the calculation at line X"
// "Check the value of ZP.IDX before the loop"
```
- **"Thoughts?" is shorthand for "Rule #5"** - don't generate code, just do some analysis, etc.

### 6. Avoid Error-Prone Addressing Modes (RULE #6)
- **X-Indexed Zero Page Indirect is forbidden for this project** (opcodes like A1 and 81):
```hopper
// FORBIDDEN (opcodes $A1, $81, etc.):
LDA ($80,X)
STA ($90,X)

// PREFER these alternatives:
LDA [ZP.IDX]      // Zero page indirect
LDA [ZP.IDX], Y   // Zero page indirect indexed by Y
```

### 7. Consistent Success/Failure Convention (RULE #7)
- **Use Carry flag for success/failure returns**:
```hopper
// Standard pattern:
MyMethod()
{
    // ... do work ...
    if (error_condition)
    {
        CLC  // Clear carry = failure
        return;
    }
    SEC  // Set carry = success
}

// Calling pattern:
MyMethod();
if (NC)  // Check for failure
{
    // handle error
}
```

### 8. Readable Naming Conventions (RULE #8)
- **Use camelCase for readability** - identifiers like NEXT_POINTER_OFFSET are ugly and loud:
```hopper
// Good:
const byte headerSize = 7;
const byte nextOffset = 0;
calculateNodeSize()

// Avoid:
const byte HEADER_SIZE = 7;
const byte NEXT_PTR_OFFSET = 0;
CALCULATE_NODE_SIZE()
```

### 9. Clean Enum Usage (RULE #9)
- **Use direct enum syntax** when enums are imported - don't qualify with unit namespace:
```hopper
// Correct (when unit is imported with 'uses'):
LDA #SymbolType.VARIABLE
LDA #TokenType.NUMBER
CMP #Flags.Ready

// Incorrect:
LDA #Objects.SymbolType.VARIABLE

// Only qualify when necessary for disambiguation:
LDA #Serial.Status.Ready  // If multiple Status enums exist
```

### 10. Stick to Real 6502 Instructions (RULE #10)
- **Use only actual 6502 assembly instructions** - no made-up addressing modes
- **No stack-relative addressing** - the 6502 doesn't support `LDA offset,S` syntax
- **No extended addressing modes** that don't exist on the real processor
- **Valid 6502 stack operations only**:
```hopper
// Correct - real 6502 instructions:
PHA              // Push accumulator
PLA              // Pull accumulator  
PHP              // Push processor status
PLP              // Pull processor status
TSX              // Transfer stack pointer to X
TXS              // Transfer X to stack pointer

// FORBIDDEN - not real 6502:
LDA 0x01, S      // Stack-relative addressing doesn't exist
LDA (offset,S)   // This addressing mode doesn't exist
```

- **Alternative approaches for parameter passing**:
```hopper
// Good - use zero page or registers:
STA ZP.TEMP      // Store parameter in zero page
// ... call function ...
LDA ZP.TEMP      // Retrieve parameter

// Good - use registers when possible:
LDX #operationMode  // Pass in X register
```

## Hopper Assembly Syntax Rules

### Constant Expressions in Operands
Constant expressions as operands need parentheses:

**Correct:**
```hopper
LDA (stmtValue + 1)
LDA (Address.BasicProcessBuffer2 + 4)
```

**Incorrect:**
```hopper
LDA stmtValue + 1     // Missing parentheses
LDA Address.BasicProcessBuffer2 + 4
```

### Addressing Mode Disambiguation
The assembler considers `LDA (constantName)` to be "dangerous" because it could be a mistake (perhaps `LDA [constantName]` was intended for indirect addressing).

**Recommended workaround:**
```hopper
LDA (stmtNamePtr + 0)    // Clean and unambiguous
```

This clearly indicates absolute addressing of a constant expression while avoiding the assembler warning.

### Indirect Addressing Syntax
Hopper Assembly uses `[ ]` for indirect addressing modes (that's one reason why):

**6502 Indirect:**
```hopper
LDA [ZP.IDX], Y      // Load from address pointed to by ZP.IDX + Y
STA [ZP.FREELIST], Y // Store to address pointed to by ZP.FREELIST + Y
```

**Direct Addressing:**
```hopper
LDA ZP.IDX           // Load from ZP.IDX directly
LDA (stmtValue + 0)  // Load from constant address
```

## Code Structure

### Brace Placement
```hopper
// Opening braces on their own line:
if (Z)
{
    // code block
}

loop
{
    // code block
}

MyFunction()
{
    // implementation
}
```

### Indentation
- Use consistent spacing (typically 4 spaces)
- Indent code blocks within control structures
- Align related code for readability

### Method Structure
```hopper
// Public method (starts with uppercase)
PublicMethod()
{
    // Save registers if modified
    PHA
    PHX
    PHY
    
    // Main logic
    
    // Restore registers
    PLY
    PLX
    PLA
    
    // Set success/failure
    SEC  // or CLC for failure
}

// Private method (starts with lowercase)
privateHelper()
{
    // Only accessible within this unit
}
```

## Architectural Conventions

### Layer Separation and Storage
Each processing layer should have dedicated storage to avoid coupling:

```hopper
// Private Statement layer storage - BasicProcessBuffer2
const uint stmtNamePtr     = Address.BasicProcessBuffer2;      // 2 bytes
const uint stmtValue       = Address.BasicProcessBuffer2 + 2;  // 2 bytes
const uint stmtTokensPtr   = Address.BasicProcessBuffer2 + 4;  // 2 bytes
// etc...

// Private Expression layer storage - BasicProcessBuffer3  
const uint exprTemp1       = Address.BasicProcessBuffer3;      // 2 bytes
const uint exprTemp2       = Address.BasicProcessBuffer3 + 2;  // 2 bytes
// etc...
```

**Benefits:**
- **Complete isolation** - Layer changes can't break other layers
- **No hidden coupling** - Symbol system changes can't break us
- **Future extensibility** - Room for growth in each layer
- **Clear ownership** - Each buffer section belongs to one layer

### Private Naming Convention
Use lowercase first letters for private/internal constants:
```hopper
const uint stmtNamePtr     = Address.BasicProcessBuffer2;  // Private
const uint StmtNamePtr     = Address.BasicProcessBuffer2;  // Public (avoid)
```

### Error Handling Patterns
Use single exit point with cleanup for complex methods:
```hopper
someComplexMethod()
{
    loop // Single exit point for cleanup
    {
        // Do work...
        if (error) { break; } // Error exit
        
        // More work...
        if (another_error) { break; } // Error exit
        
        SEC  // Success
        break;
    } // Single exit loop
    
    // Cleanup code always runs
    cleanupResources();
}
```

## Best Practices

### Namespace Usage
- **Include namespace for clarity** (prevents ambiguity):
```hopper
// Recommended (prevents ambiguity):
LDA ZP.TOPL
STA Address.Buffer

// Legal but can cause issues:
LDA TOPL    // What if another unit defines TOPL?
```

### Error Handling
```hopper
// Define error constants
const string ErrorMessage = "Operation failed";

// Standard error pattern:
LDA #(ErrorMessage % 256)
STA ZP.ERRORL  // Or project-specific error location
LDA #(ErrorMessage / 256)
STA ZP.ERRORH
CLC  // NC = error
return;
```

### 16-bit Operations
```hopper
// Consistent 16-bit value handling:
LDA #(value % 256)    // Low byte
STA ZP.ADDRL
LDA #(value / 256)    // High byte
STA ZP.ADDRH

// 16-bit increment:
INC ZP.ADDRL
if (Z)
{
    INC ZP.ADDRH
}
```

### Memory Allocation
```hopper
// Always check allocation results:
Memory.Allocate();
LDA ZP.IDXL
ORA ZP.IDXH
if (Z)  // Check for null (0x0000)
{
    // Handle allocation failure
    CLC
    return;
}
```

### Documentation
```hopper
// Document method contracts:
// Calculate widget size based on input parameters
// Input: ZP.WIDTHL/H = width (16-bit)
//        ZP.HEIGHTL/H = height (16-bit)
// Output: ZP.SIZEL/H = calculated size (16-bit)
//         C set if successful, NC if overflow
// Preserves: X, Y
// Munts: A, ZP.TEMP0-1
CalculateSize()
{
    // implementation
}
```

### Platform Independence
```hopper
// Use conditional compilation for platform-specific code:
#ifdef BENEATER_IO
    const uint ACIA = 0x5000;
#endif

#ifdef X16_IO
    const uint ACIA = 0x9F10;
#endif

// Use the constant in platform-independent code:
LDA ACIA
```

## Control Flow Best Practices

### Structured Programming
```hopper
// Prefer structured flow:
loop
{
    // Process item
    if (done_condition)
    {
        break;
    }
    // Continue processing
}

// Over goto-style:
process_loop:
    // Process item
    if (not_done)
    {
        JMP process_loop
    }
```

### Clear Exit Conditions
```hopper
// Make loop exit conditions obvious:
loop
{
    // Check end condition first
    LDA ZP.COUNTERL
    ORA ZP.COUNTERH
    if (Z) { break; }  // Counter reached zero
    
    // Process...
    
    // Decrement counter
    // ...
}
```

## Memory Management

### Buffer Usage Guidelines
- **BasicProcessBuffer1**: Already used by Tokenizer
- **BasicProcessBuffer2**: Statement layer (32 bytes)
- **BasicProcessBuffer3**: Expression layer (32 bytes)

### Zero Page Allocation
Current HopperBASIC zero page usage:
- **0x30-0x39**: BASIC-specific (10 bytes allocated)
- **0x3A-0x4F**: Available for future BASIC features (22 bytes)
- **0x70-0x7F**: Symbol table operations (16 bytes allocated)

### Unit Relationships
Use `friend` declarations for controlled access between related units:
```hopper
unit Objects
{
    friend Variables, Functions, Arguments;
    // ...
}
```

## Comments and Documentation

### Free Block Documentation
The large FREE block at the end of heap dumps is the remaining unallocated heap (remaining RAM). It is not corruption - this is normal and expected.

### Flag State Documentation
Always document expected flag states:
```hopper
// Returns C if successful, NC if error
// Munts: ZP.IDX, ZP.IDY
// Preserves: A, X, Y, all other ZP variables
someMethod()
{
    
}
```

## Performance Considerations

### Stack vs Zero Page
For short-term temporary storage, prefer CPU stack over zero page:
```hopper
PHA              // Save A temporarily
PHX              // Save X temporarily
// Do work...
PLX              // Restore X
PLA              // Restore A
```

### Avoid Stack Juggling
Don't use excessive PHA/PLA pairs. Use dedicated storage instead:

**Bad:**
```hopper
LDA value1
PHA
LDA value2  
PHA
// ... 12 more PHA/PLA pairs
PLA
STA value2
PLA
STA value1
```

**Good:**
```hopper
LDA value1
STA (stmtValue1 + 0)
LDA value2
STA (stmtValue2 + 0)
// Use dedicated storage, no juggling needed
```

This reduces complexity and eliminates the chance of stack misalignment errors.

## Summary

These conventions promote:
1. **Reliability** - No silent failures, explicit error handling
2. **Readability** - Clear naming, structured code, good comments
3. **Maintainability** - Complete code, documented interfaces
4. **Portability** - Platform-independent patterns
5. **Debuggability** - Instrumentation-first debugging
6. **Accessibility** - Dyslexia-friendly documentation

Following these conventions ensures that Hopper Assembly code is professional, maintainable, and accessible to all developers.