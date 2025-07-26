# Hopper Assembly Coding Conventions

## Core Principles

### 1. No Silent Failures
- **Always report errors explicitly** before failing
- **BRK is acceptable** - it throws an exception in the emulator
- **Standard TODO/unimplemented pattern**:
```hopper
// TODO: Implement feature X
LDA #(Messages.NotImplemented % 256)
STA ZP.LastErrorL
LDA #(Messages.NotImplemented / 256)
STA ZP.LastErrorH
BRK
```

### 2. Zero Page Management
- **Document all zero page usage** in your project
- **Respect existing allocations** when integrating with other code
- **Prefer stack for temporary storage** in general-purpose code:
```hopper
// Good - use stack for temporary values
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

### 3. Accessibility-Friendly Comments
- **Use descriptive flag comments** for better readability:
```hopper
// Correct:
if (Z)   // Set Z (zero flag set)
if (NZ)  // Set NZ (zero flag clear)
if (C)   // Set C (carry set)
if (NC)  // Set NC (carry clear)
if (MI)  // Set MI (minus/negative)
if (PL)  // Set PL (plus/positive)

// Avoid:
if (Z)   // 1
if (NZ)  // 0
```

### 4. Complete Code Generation
- **Always generate complete methods**
- **Never use placeholders** like:
```hopper
// FORBIDDEN:
// Rest of the function remains the same...
// ... existing code ...

// ALWAYS generate the complete implementation
```

### 5. Debug-First Methodology
- **When debugging, instrument before speculating**
- **Use debug helpers that preserve flags**:
  - `Serial.HexOut()` for simple output
  - Create project-specific debug helpers
  - Document what registers/flags are preserved
- **Suggest instrumentation points** rather than rewriting code:
```hopper
// "Add debug output after the calculation at line X"
// "Check the value of ZP.IDX before the loop"
```

### 6. Avoid Error-Prone Addressing Modes
- **X-Indexed Zero Page Indirect is discouraged**:
```hopper
// AVOID (opcodes $A1, $81, etc.):
LDA ($80,X)
STA ($90,X)

// PREFER these alternatives:
LDA [ZP.IDX]      // Zero page indirect
LDA [ZP.IDX], Y   // Zero page indirect indexed by Y
```

### 7. Consistent Success/Failure Convention
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

### 8. Readable Naming Conventions
- **Use camelCase for readability**:
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

### 9. Clean Enum Usage
- **Use direct enum syntax** when enums are imported:
```hopper
// When unit is imported with 'uses':
LDA #TokenType.NUMBER
CMP #Flags.Ready

// Only qualify when necessary for disambiguation:
LDA #Serial.Status.Ready  // If multiple Status enums exist
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

## Best Practices

### Namespace Usage
- **Include namespace for clarity**:
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

## Summary

These conventions promote:
1. **Reliability** - No silent failures, explicit error handling
2. **Readability** - Clear naming, structured code, good comments
3. **Maintainability** - Complete code, documented interfaces
4. **Portability** - Platform-independent patterns
5. **Debuggability** - Instrumentation-first debugging
6. **Accessibility** - Dyslexia-friendly documentation

Following these conventions ensures that Hopper Assembly code is professional, maintainable, and accessible to all developers.