# Hopper Assembly Coding Conventions
**Document Type: Coding Standards for Hopper 6502 Projects**

## Core Principles

### 1. No Silent Failures (RULE #1)
- **Always report errors explicitly** before failing
- **BRK is acceptable** - it throws an exception in the emulator
- **Standard error pattern**:
```hopper
// Error handling with message
LDA #ErrorID.NotImplemented
Error.ThrowError();
BRK
```

### 2. Zero Page Management (RULE #2)
- **Document all zero page usage** in your project
- **Respect existing allocations** when integrating with other code
- **Use ZP unit for named access** instead of raw addresses:
```hopper
// Good - use named ZP locations
LDA ZP.TOPL
STA ZP.IDXL

// Avoid - raw addresses without documentation
LDA $12  // What is this?
```

### 3. Accessibility-Friendly Comments (RULE #3)
- **Use descriptive flag comments** for better readability:
```hopper
// Correct - descriptive comments:
if (Z)   // Zero flag set
if (NZ)  // Zero flag clear
if (C)   // Carry set
if (NC)  // Carry clear
if (MI)  // Minus/negative
if (PL)  // Plus/positive

// Avoid - cryptic comments:
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
- **Use debug helpers that preserve state**:
```hopper
Debug.HexOut();    // Preserves flags
Debug.DumpHeap();  // Full state preservation
Debug.ValidateHeap();
```
- **"Thoughts?" is shorthand for "Rule #5"** - analyze before generating code

### 6. Consistent Success/Failure Convention (RULE #6)
- **Use Carry flag for success/failure returns**:
```hopper
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

### 7. Readable Naming Conventions (RULE #7)
- **Use camelCase for readability**:
```hopper
// Good:
const byte headerSize = 7;
calculateNodeSize()
printString()

// Avoid ALL_CAPS:
const byte HEADER_SIZE = 7;
CALCULATE_NODE_SIZE()
```

### 8. Clean Enum Usage (RULE #8)
- **Use direct enum syntax** when units are imported:
```hopper
// Correct (when unit is imported):
LDA #ErrorID.FileNotFound
CMP #StatusFlags.Ready

// Only qualify when necessary for disambiguation
```

### 9. Stick to Real 6502 Instructions (RULE #9)
- **Use only actual 6502 assembly instructions**
- **No made-up addressing modes**:
```hopper
// Correct - real 6502 instructions:
PHA              // Push accumulator
TSX              // Transfer stack pointer to X
LDA [ZP.IDX], Y  // Zero page indirect indexed

// FORBIDDEN - not real 6502:
LDA 0x01, S      // Stack-relative doesn't exist
```

### 10. Register Preservation Policy (RULE #10)
- **Accumulator (A) is NEVER preserved** - callers must save if needed
- **Public methods preserve X and Y** - only if they modify them
- **No blanket preservation** - save code space

```hopper
// Public method (uppercase first letter)
PublicMethod()
{
    // Only preserve X and Y if we modify them
    PHX          // Only if X is modified
    PHY          // Only if Y is modified
    
    // A is freely modified without preservation
    LDA #42
    
    // Restore only what was saved
    PLY          // Only if Y was saved
    PLX          // Only if X was saved
    
    SEC  // Set success
}

// Private method (lowercase first letter)
privateHelper()
{
    // No register preservation requirements
    // Caller is responsible for saving what they need
    LDX #0
    LDY #10
    LDA #255
}

// Example: Public method that doesn't modify X or Y
GetStatus()
{
    // No need to preserve X or Y since we don't touch them
    LDA ZP.STATUS
    SEC
}

// Example: Public method that only modifies Y
ProcessByte()
{
    PHY              // Only preserve Y
    
    LDY #0
    loop
    {
        // processing using Y...
        INY
        CPY #8
        if (NZ) { continue; }
        break;
    }
    
    PLY              // Restore Y
    SEC
}
```

## Hopper Assembly Syntax Rules

### Constant Expressions in Operands
Constant expressions need parentheses:

**Correct:**
```hopper
LDA (Address.Buffer + 4)
LDA (value + 1)
```

**Incorrect:**
```hopper
LDA Address.Buffer + 4     // Missing parentheses
```

### Indirect Addressing Syntax
Hopper Assembly uses `[ ]` for indirect addressing modes:

```hopper
LDA [ZP.IDX], Y      // Zero page indirect indexed by Y
STA [ZP.FREELIST]    // Zero page indirect
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

### Method Structure
```hopper
// Public method (starts with uppercase)
PublicMethod()
{
    // Save X and Y only if modified
    PHX     // Only include if X is modified
    PHY     // Only include if Y is modified
    
    // Main logic here
    // A can be freely modified
    
    // Restore only what was saved
    PLY     // Only if Y was saved
    PLX     // Only if X was saved
    
    // Set success/failure
    SEC  // or CLC for failure
}

// Private method (starts with lowercase)
privateHelper()
{
    // No register preservation requirements
    // Only accessible within this unit
}
```

## Best Practices

### Platform Configuration
Use conditional compilation for different platforms:
```hopper
#ifdef BENEATER_IO
    const uint ACIA = 0x5000;
#endif

#ifdef X16_IO
    const uint ACIA = 0x9F10;
#endif
```

### CPU Target Selection
```hopper
#ifdef CPU_65C02S
    STZ ZP.PORTA      // Use enhanced instructions
    PHX               // Enhanced stack operations
#else
    LDA #0           // Traditional 6502
    STA ZP.PORTA
#endif
```

### Error Handling
```hopper
// Use error system for user-facing errors
Error.FileNotFound();
if (NC)
{
    return;  // Propagate error
}

// For internal errors/assertions
LDA #ErrorID.InternalError
Error.ThrowError();
BRK
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

### Memory Management
```hopper
// Always check allocation results:
Memory.Allocate();
LDA ZP.IDXL
ORA ZP.IDXH
if (Z)  // Check for null (0x0000)
{
    // Handle allocation failure
    Error.OutOfMemory();
    CLC
    return;
}
```

### Documentation
```hopper
// Document method contracts:
// Calculate file size in sectors
// Input: ZP.FLENGTHL/H = file size in bytes (16-bit)
// Output: A = sectors needed (8-bit)
//         C set if successful, NC if overflow
// Preserves: X, Y (per Rule #10)
// Munts: A (always), ZP.TEMP0-1
calculateSectors()
{
    // implementation
}

// Document when X or Y are not preserved:
// Process buffer data
// Input: ZP.ADDRL/H = buffer address
// Output: A = checksum
//         C set if successful
// Preserves: None (modifies X and Y internally)
// Munts: A, X, Y, ZP.TEMP0
processBuffer()
{
    PHX  // Preserve X (public method)
    PHY  // Preserve Y (public method)
    
    // Use X and Y freely here
    LDX #0
    LDY #0
    
    PLY
    PLX
    SEC
}
```

### Single Exit Pattern
Use single exit pattern for complex methods:
```hopper
ComplexMethod()
{
    PHX  // Only if X is modified
    
    loop // Single exit for cleanup
    {
        // Do work...
        if (error) 
        { 
            CLC
            break; 
        }
        
        // More work...
        if (another_error) 
        { 
            CLC
            break; 
        }
        
        SEC  // Success
        break;
    } // Single exit
    
    // Cleanup always runs
    PLX  // Only if X was saved
}
```

### Friend Declarations
Use `friend` for controlled access between related units:
```hopper
unit Memory
{
    friend GC, Debug;  // These units can access private methods
    
    privateAllocate()  // Private method
    {
        // No register preservation required
        // implementation
    }
}
```

## ROM Size Configuration
Configure ROM size based on target hardware:
```hopper
#define ROM_32K     // Origin at 0x8000
#define ROM_16K     // Origin at 0xC000  
#define ROM_8K      // Origin at 0xE000
#define ROM_4K      // Origin at 0xF000
```

## Zero Page Allocation Guidelines

Current kernel zero page usage:
- **0x06-0x09**: Memory management (FREELIST, HEAPSTART, HEAPSIZE)
- **0x0E-0x1A**: Math units (ACC, TOP, NEXT with type bytes)
- **0x1B-0x1E**: General purpose (IDX, IDY pointers)
- **0x4F-0x60**: Shared workspace (memory allocator, math operations)
- **0x61-0x66**: File system (FSOURCE/FDEST/FLENGTH)
- **0x77-0x8A**: I2C communication
- **0xEC-0xFF**: Hardware mapped I/O (ACIA, VIA)

## Performance Considerations

### Stack vs Zero Page
For short-term storage, prefer stack but follow Rule #10:
```hopper
// Good - save A only when needed by caller
TXA      // Move X to A for processing
PHA      // Caller saves A if needed
// Do work...
PLA      // Restore A only if saved
TAX      // Restore X from A
```

### Avoid Excessive Stack Operations
Use dedicated storage for complex state:
```hopper
// Good - use allocated storage
LDA value1
STA tempStorage
LDA value2
STA tempStorage + 1

// Avoid - excessive stack juggling
PHA
PHA
PHA  // Many pushes make code hard to follow
```

### Register Preservation Optimization
```hopper
// Inefficient - preserving unnecessarily
ProcessData()
{
    PHX    // Unnecessary if X not modified
    PHY    // Unnecessary if Y not modified
    
    LDA #42
    JSR doSomething  // Only uses A
    
    PLY
    PLX
    SEC
}

// Efficient - only preserve what's modified
ProcessData()
{
    // No preservation needed if X and Y aren't touched
    LDA #42
    JSR doSomething
    SEC
}
```

## Module System Best Practices

### Uses Declarations
```hopper
uses "/Source/Runtime/6502/Serial"
uses "Memory.asm"
```

### Cross-Module Calls
```hopper
// Use qualified calls for clarity
Serial.Initialize();
Memory.Allocate();
Debug.DumpHeap();
```

### Module Initialization Order
```hopper
Hopper()
{
    // Initialize in dependency order
    Memory.Initialize();
    Serial.Initialize();
    File.Initialize();
    
    // Then application code
    applicationMain();
}
```

## Summary

These conventions promote:
1. **Reliability** - No silent failures, explicit error handling
2. **Readability** - Clear naming, structured code
3. **Maintainability** - Complete code, documented interfaces
4. **Portability** - Platform-independent patterns
5. **Debuggability** - Instrumentation-first debugging
6. **Performance** - Efficient use of zero page and stack
7. **Code Size** - Minimal register preservation overhead

Following these conventions ensures professional, maintainable Hopper Assembly code for kernel and system-level development. The register preservation policy (Rule #10) balances safety with code size efficiency.