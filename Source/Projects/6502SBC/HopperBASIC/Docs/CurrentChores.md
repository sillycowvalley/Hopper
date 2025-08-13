# HopperBASIC Array Implementation Plan
**Document Type: Work Plan / Subproject Specification**
**Status: In Progress - Array Write Support Needed**
**Last Updated: Current Session**

## Overview
Implementation of array support for HopperBASIC to enable running the Sieve of Eratosthenes and other benchmark programs. Arrays are dynamically allocated like strings, with Variables owning the memory through pointers.

**Current Status**: Arrays can be declared and read from, but assignment (writing) is not yet implemented.

## 🔴 Remaining Critical Work

### Must Complete for Sieve Benchmark:
1. **Add SETINDEX OpCode** (OpCodes.asm)
   - Define SETINDEX = 0x39 in OpCode enum
   
2. **Implement executeSetIndex()** (executor.asm)
   - Pop value, index, array reference from stack
   - Call BASICArray.SetItem() to write element
   - Handle type checking and bounds checking
   
3. **Compile Array Assignment** (compiler.asm/compilerflow.asm)
   - Detect array[index] on LHS of assignment
   - Generate SETINDEX instead of POPGLOBAL/POPLOCAL
   
4. **Add Emit.SetIndex()** (emit.asm)
   - Emit SETINDEX opcode when compiling array assignment

Without these, arrays can be created and read but not written to, making the Sieve benchmark impossible to run.

## Key Design Decisions

### Memory Architecture
- Arrays are dynamically allocated on the heap (like strings)
- Variables own a pointer to the array memory
- Array unit (array.asm) already implements the core functionality
- Memory layout: count (2 bytes) + type (1 byte) + elements

### Type System Redesign ✅ COMPLETED
Since functions live on their own list and never mix with variables/constants, we can reduce SymbolType to 2 bits and use bit 5 for the ARRAY flag:

```assembly
// BASICTypes.asm modifications:
flags BASICType
{
    VOID     = 0x00,   // Function return type (internal use)
    INT      = 0x02,   // Signed 16-bit integer
    BYTE     = 0x03,   // Unsigned 8-bit value
    WORD     = 0x04,   // Unsigned 16-bit value
    BIT      = 0x06,   // Boolean value (0 or 1)
    STRING   = 0x0F,   // String type
    
    VAR      = 0x10,   // Bit 4 - runtime-determined type
    ARRAY    = 0x20,   // Bit 5 - array flag (NEW)
    
    TYPEMASK = 0x0F,   // Bottom 4 bits for base type
    FLAGMASK = 0x30,   // Bits 4-5 for flags
    MASK     = 0x3F,   // Bottom 6 bits total
}

// Objects.asm modifications:
flags SymbolType
{
    VARIABLE = 0x40,   // Mutable values
    CONSTANT = 0x80,   // Immutable values  
    ARGUMENT = 0x40,   // Function parameters (reuse VARIABLE value)
    LOCAL    = 0x80,   // Local variables (reuse CONSTANT value)
    
    MASK     = 0xC0,   // Top 2 bits only
}
```

Example packed values:
- **INT variable**: `0x40 | 0x02 = 0x42`
- **Array of INT**: `0x40 | 0x20 | 0x02 = 0x62`
- **Array of BIT**: `0x40 | 0x20 | 0x06 = 0x66`

### OpCode Status
```assembly
// OpCodes.asm
GETITEM = 0x1E,  // ✅ IMPLEMENTED - String/array element access  
SETINDEX = 0x39, // 🔴 TODO - Add for array element assignment
```

## Implementation Tasks

### 1. Type System Update ✅ COMPLETED
**Files**: BASICTypes.asm, Objects.asm
- Reduce SymbolType to 2 bits (top bits 7-6)
- Add ARRAY flag as bit 5
- Update MASK constants
- Test type extraction/packing

### 2. Declaration Parsing ✅ COMPLETED
**Files**: statement.asm, tokenizer.asm
- Parse syntax: `BIT flags[8191]` or `INT scores[100]`
- After identifier, check for LBRACKET token
- Parse and evaluate constant size expression
- Set ARRAY flag in type byte
- Store size in ZP.NEXT for Variables.Declare

### 3. Variables.Declare Enhancement ✅ COMPLETED
**File**: variables.asm
- Detect ARRAY flag in type
- Use ZP.NEXT as array size (element count)
- Call BASICArray.New() to allocate
- Store returned pointer as variable value
- Add cleanup for array memory on variable deletion

### 4. Array Indexing (Read) - GETITEM ✅ COMPLETED
**File**: executor.asm
- Implemented as executeGetItem() which handles both string and array indexing
- Calls indexArray() which uses BASICArray.GetItem()
- OpCode.GETITEM = 0x1E defined and working
- Emit.Index() generates GETITEM opcode

### 5. Array Assignment (Write) - SETINDEX 🔴 TODO
**Files**: OpCodes.asm, executor.asm
- **First**: Add SETINDEX = 0x39 to OpCode enum in OpCodes.asm
- New executeSetIndex() method in executor.asm
- Stack order: [..., array_ref, index, value]
- Call BASICArray.SetItem()

```assembly
executeSetIndex()
{
    // Pop value to set
    Stacks.PopTop();  // New value in ZP.TOP, type in ZP.TOPT
    
    // Pop index
    Stacks.PopNext(); // Index in ZP.NEXT
    
    // Convert index to ACC for bounds checking
    LDA ZP.NEXTL
    STA ZP.ACCL
    LDA ZP.NEXTH
    STA ZP.ACCH
    
    // Check for negative index (INT type)
    LDA ZP.NEXTT
    AND #BASICType.TYPEMASK
    CMP #BASICType.INT
    if (Z)
    {
        LDA ZP.NEXTH
        if (MI)
        {
            Error.RangeError();
            States.SetFailure();
            break;
        }
    }
    
    // Pop array reference
    Stacks.PopNext();  // Array ptr in ZP.NEXT
    
    // Transfer to BASICArray format
    LDA ZP.NEXTL
    STA ZP.IDXL
    LDA ZP.NEXTH
    STA ZP.IDXH
    
    LDA ZP.ACCL
    STA ZP.IDYL
    LDA ZP.ACCH
    STA ZP.IDYH
    
    // TOP already has the value
    BASICArray.SetItem();
    if (NC)
    {
        States.SetFailure();
    }
    else
    {
        States.SetSuccess();
    }
}
```

### 6. Assignment Compilation 🔴 TODO
**Files**: compiler.asm, compilerflow.asm
- In compileAssignment(), detect array[index] on LHS
- Generate SETINDEX instead of POPGLOBAL/POPLOCAL
- Handle in compilePrimary() for array variable references

```assembly
// In compileAssignment after compiling LHS identifier:
LDA ZP.CurrentToken
CMP #Token.LBRACKET
if (Z)
{
    // Array element assignment
    // Identifier already pushed array pointer
    
    Tokenizer.NextToken();
    compileComparison(); // Index expression
    
    // Check RBRACKET
    LDA ZP.CurrentToken
    CMP #Token.RBRACKET
    if (NZ)
    {
        Error.ExpectedRightBracket();
        break;
    }
    
    Tokenizer.NextToken();
    
    // Check EQUALS
    LDA ZP.CurrentToken
    CMP #Token.EQUALS
    if (NZ)
    {
        Error.ExpectedEqual();
        break;
    }
    
    Tokenizer.NextToken();
    compileComparison(); // Value expression
    
    // Emit SETINDEX
    Emit.SetIndex();
}
```

### 7. Emit Functions 🔴 TODO
**File**: emit.asm
- Add Emit.SetIndex() for new SETINDEX opcode
- Index() already exists (emits GETITEM)

### 8. LIST Command Support ✅ COMPLETED
**File**: tokeniterator.asm
- Array declaration rendering
- Show: `BIT flags[100]`
- Handle LBRACKET/RBRACKET in renderToken()

### 9. VARS Command Enhancement ✅ COMPLETED
**File**: console.asm
- Display array type and size
- Show first few elements: `= TRUE, TRUE, FALSE, ...`
- Handle array display in ShowVariables()

### 10. Heap Validation ✅ COMPLETED
**File**: Debug.asm
- Add validateArray() similar to validateString()
- Check array header integrity
- Validate element count and type
- Verify memory bounds

### 11. Global Initialization ✅ COMPLETED
**File**: console.asm
- In initializeGlobals(), arrays are zero-initialized by BASICArray.New()
- No special handling needed (unlike strings)

## Testing Plan

### Phase 1: Basic Declaration ✅ WORKING
```basic
> BIT flags[10]
OK
> VARS
BIT flags[10] = FALSE, FALSE, FALSE, ...
```

### Phase 2: Element Access ✅ READING WORKS
```basic
> PRINT flags[0]     ! ✅ Works
FALSE
> PRINT flags[5]     ! ✅ Works
FALSE
```

### Phase 3: Loop Integration ✅ CAN TEST
```basic
> FOR i = 0 TO 9
*   PRINT flags[i]    ! ✅ Reading works
* NEXT i
```

### Phase 4: Assignment 🔴 BLOCKED
```basic
> flags[0] = TRUE     ! 🔴 Not working - SETINDEX not implemented
```

### Phase 5: Sieve Benchmark 🔴 BLOCKED
Run the complete Sieve of Eratosthenes benchmark program.

## Error Handling

### Compile-Time Errors
- Array size must be constant expression
- Array size must be positive
- Array size must fit in 16 bits
- Type checking for array elements

### Runtime Errors
- Index out of bounds
- Negative index
- Type mismatch on assignment
- Memory allocation failure

## Memory Management

### Allocation
- Variables.Declare calls BASICArray.New()
- Array memory owned by variable (pointer stored)
- Zero-initialized by default

### Deallocation
- When variable freed, array memory freed
- Similar to string handling
- Prevent memory leaks

## Implementation Order

1. **Type system changes** ✅ COMPLETED
2. **Declaration parsing** ✅ COMPLETED - `BIT flags[100]` working
3. **Variables.Declare updates** ✅ COMPLETED - Arrays allocated
4. **GETITEM integration** ✅ COMPLETED - Read array elements working
5. **SETINDEX implementation** 🔴 TODO - Write array elements
6. **Assignment compilation** 🔴 TODO - Handle array[i] = value
7. **Emit.SetIndex** 🔴 TODO - Emit SETINDEX opcode
8. **LIST/VARS rendering** ✅ COMPLETED - Arrays display nicely
9. **Heap validation** ✅ COMPLETED - Debug support working
10. **Testing** 🔴 TODO - Comprehensive test suite
11. **Benchmarks** 🔴 TODO - Run Sieve successfully

## Notes

- Array.asm is already complete and tested
- Follow STRING pattern for memory management
- Global arrays only (no local arrays initially)
- Single-dimensional only
- Zero-based indexing
- Bounds checking on all access
- Type safety enforced

## Success Criteria

Successfully run the Sieve of Eratosthenes benchmark:
```basic
CONST sizepl = 8191
BIT flags[sizepl]
BEGIN
    ! ... full sieve algorithm ...
END
```

## Design Decisions (Resolved)

- ✅ **Array parameters to functions**: YES - Works like strings (pass by reference, pointer immutable, elements mutable)
- ✅ **CONST arrays**: NO - Would require initializer syntax (future enhancement)
- ✅ **Maximum array size**: No artificial limits, fail gracefully on allocation
- ✅ **Multi-dimensional arrays**: Future enhancement