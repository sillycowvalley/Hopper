# HopperBASIC Array Implementation Plan
**Document Type: Work Plan / Subproject Specification**
**Status: In Progress**
**Last Updated: Current Session**

## Overview
Implementation of array support for HopperBASIC to enable running the Sieve of Eratosthenes and other benchmark programs. Arrays will be dynamically allocated like strings, with Variables owning the memory through pointers.

## Key Design Decisions

### Memory Architecture
- Arrays are dynamically allocated on the heap (like strings)
- Variables own a pointer to the array memory
- Array unit (array.asm) already implements the core functionality
- Memory layout: count (2 bytes) + type (1 byte) + elements

### Type System Redesign
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

### OpCode Changes
Rename and add opcodes for clarity:
```assembly
// OpCodes.asm
GETINDEX = 0x38,  // Renamed from INDEX (string/array element access)
SETINDEX = 0x39,  // New opcode for array element assignment
```

## Implementation Tasks

### 1. Type System Update ✅ PRIORITY
**Files**: BASICTypes.asm, Objects.asm
- Reduce SymbolType to 2 bits (top bits 7-6)
- Add ARRAY flag as bit 5
- Update MASK constants
- Test type extraction/packing

### 2. Declaration Parsing
**Files**: statement.asm, tokenizer.asm
- Parse syntax: `BIT flags[8191]` or `INT scores[100]`
- After identifier, check for LBRACKET token
- Parse and evaluate constant size expression
- Set ARRAY flag in type byte
- Store size in ZP.NEXT for Variables.Declare

```assembly
// In processSingleSymbolDeclaration after getting identifier:
LDA ZP.CurrentToken
CMP #Token.LBRACKET
if (Z)
{
    // Set ARRAY flag
    LDA stmtType
    ORA #BASICType.ARRAY
    STA stmtType
    
    Tokenizer.NextToken();
    // Parse size expression (must be constant)
    // Evaluate to get size in ZP.ACC
    
    // Check for RBRACKET
    LDA ZP.CurrentToken
    CMP #Token.RBRACKET
    if (NZ)
    {
        Error.ExpectedRightBracket();
        break;
    }
    
    // Move size to ZP.NEXT for Variables.Declare
    LDA ZP.ACCL
    STA ZP.NEXTL
    LDA ZP.ACCH
    STA ZP.NEXTH
    
    Tokenizer.NextToken(); // Move past RBRACKET
}
```

### 3. Variables.Declare Enhancement
**File**: variables.asm
- Detect ARRAY flag in type
- Use ZP.NEXT as array size (element count)
- Call BASICArray.New() to allocate
- Store returned pointer as variable value
- Add cleanup for array memory on variable deletion

```assembly
// In Variables.Declare:
LDA ZP.ACCT
AND #BASICType.ARRAY
if (NZ)
{
    // Array declaration
    // ZP.NEXT contains size, extract element type
    LDA ZP.ACCT
    AND #BASICType.TYPEMASK
    STA ZP.ACCT  // Element type for BASICArray.New
    
    // ZP.ACC = size (from ZP.NEXT)
    LDA ZP.NEXTL
    STA ZP.ACCL
    LDA ZP.NEXTH
    STA ZP.ACCH
    
    BASICArray.New();  // Returns pointer in ZP.IDX
    
    // Store array pointer as variable value
    LDA ZP.IDXL
    STA ZP.NEXTL
    LDA ZP.IDXH
    STA ZP.NEXTH
}
```

### 4. Array Indexing (Read) - GETINDEX
**File**: executor.asm
- Already implemented as indexArray()
- Wire up to renamed GETINDEX opcode
- Tested with existing BASICArray.GetItem()

### 5. Array Assignment (Write) - SETINDEX
**File**: executor.asm
- New executeSetIndex() method
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

### 6. Assignment Compilation
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

### 7. Emit Functions
**File**: emit.asm
- Add Emit.SetIndex() for new opcode
- Already have Emit.Index() (rename to GetIndex)

### 8. LIST Command Support
**File**: tokeniterator.asm
- Array declaration rendering
- Show: `BIT flags[100]`
- Handle LBRACKET/RBRACKET in renderToken()

### 9. VARS Command Enhancement
**File**: console.asm
- Display array type and size
- Show first few elements: `= TRUE, TRUE, FALSE, ...`
- Handle array display in ShowVariables()

### 10. Heap Validation
**File**: Debug.asm
- Add validateArray() similar to validateString()
- Check array header integrity
- Validate element count and type
- Verify memory bounds

### 11. Global Initialization
**File**: console.asm
- In initializeGlobals(), arrays are zero-initialized by BASICArray.New()
- No special handling needed (unlike strings)

## Testing Plan

### Phase 1: Basic Declaration
```basic
> BIT flags[10]
OK
> VARS
BIT flags[10] = FALSE, FALSE, FALSE, ...
```

### Phase 2: Element Access
```basic
> flags[0] = TRUE
OK
> PRINT flags[0]
TRUE
> PRINT flags[5]
FALSE
```

### Phase 3: Loop Integration
```basic
> FOR i = 0 TO 9
*   flags[i] = TRUE
* NEXT i
OK
```

### Phase 4: Sieve Benchmark
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

1. **Type system changes** ✅ FIRST
2. **Declaration parsing** - Get `BIT flags[100]` working
3. **Variables.Declare updates** - Allocate arrays
4. **GETINDEX integration** - Read array elements
5. **SETINDEX implementation** - Write array elements
6. **Assignment compilation** - Handle array[i] = value
7. **LIST/VARS rendering** - Display arrays nicely
8. **Heap validation** - Debug support
9. **Testing** - Comprehensive test suite
10. **Benchmarks** - Run Sieve successfully

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

## Open Questions

- ❓ Should CONST arrays be supported? (Probably not initially) -> Future
- ❓ Maximum array size limits? (Memory dependent) -> not an issue (we fail gracefully if not enough memory)
- ❓ Multi-dimensional arrays? (Future enhancement) -> future