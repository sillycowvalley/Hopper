# Hopper BASIC - Locals and Arrays Implementation Plan

## Overview
After completing Phase 1 (all core functionality including IF/THEN/ELSE/ENDIF, WHILE/WEND, DO/UNTIL, and PRINT variants), two major architectural components remain for benchmark compatibility:
1. **Function Locals and Arguments** - Required for Fibonacci benchmark and FOR/NEXT loops
2. **Arrays** - Required for Sieve of Eratosthenes benchmark

---

## Function Locals and Arguments

### Current State
- ✅ Functions can declare parameters (parsed and stored in symbol table)
- ✅ Function calls evaluate arguments and push them on stack
- ✅ Stack frame mechanism exists (Hopper VM with ZP.BP and ZP.SP)
- ❌ Functions cannot access their arguments (no PUSHLOCAL/POPLOCAL opcodes)
- ❌ Functions cannot declare local variables
- ❌ FOR/NEXT loops blocked (loop counter needs to be local)

### Stack Frame Architecture (Hopper VM)
**Value Stack** (data only):
```
Higher addresses
[Local Variable n]
...
[Local Variable 0]    <- BP (Base Pointer) points here
[Argument n]           
...
[Argument 0]
[Return Value placeholder]
Lower addresses
```

**Call Stack** (separate, control flow):
```
[Saved BP]
[Return Address]
```

- ZP.SP = Value Stack Pointer
- ZP.BP = Base Pointer (frame pointer)
- ZP.CSP = Call Stack Pointer
- Nesting depth = CSP/4 (4 bytes per call frame)

### Required Implementation

#### New/Modified Opcodes
- `ENTER n` - Allocate n locals (BP already set by Hopper VM)
- `PUSHLOCAL offset` - Push local/argument:
  - Positive offset: locals (BP + offset)
  - Negative offset: arguments (BP - offset - 1)
- `POPLOCAL offset` - Pop to local variable
- `RETURN n` - Clean up frame (handled by Hopper VM)

#### Symbol Table Changes
- Add scope tracking (global vs local context)
- Store frame-relative offsets for locals/arguments
- **Shadowing rules**:
  - Locals may not duplicate argument names
  - Search order: locals → arguments → globals
- Track "current function" context during compilation

#### Compilation Context Tracking
```asm
CompilerFunctionLocals   // Count of locals in current function
CompilerFunctionArgs     // Count of arguments in current function  
CompilerInFunction       // Flag: compiling function vs main program
CompilerFrameOffset      // Current offset for next local
```

---

## FOR/NEXT Implementation

### Loop Structure
FOR/NEXT requires **three local variables**:
1. **Counter** - Current loop value
2. **Limit** - TO value (evaluated once)
3. **Step** - STEP value (default 1, evaluated once)

### Compilation Strategy
```basic
FOR I = start TO end STEP increment
```
Compiles to:
```
PUSH start
POPLOCAL counter
PUSH end  
POPLOCAL limit
PUSH increment
POPLOCAL step

loop_start:
    ; Loop body
    
    ; NEXT processing:
    PUSHLOCAL counter
    PUSHLOCAL step
    ADD
    POPLOCAL counter
    
    ; Termination check (depends on sign of step)
    PUSHLOCAL counter
    PUSHLOCAL limit
    [LE or GE depending on step sign]
    JUMPNZW loop_start
```

### NEXT Matching
- Must verify NEXT variable matches FOR variable
- Error if NEXT without FOR
- Handle nested FOR loops correctly

---

## Arrays

### Current State
- ❌ No array declaration syntax
- ❌ No array indexing operations
- ❌ No array memory management

### Design Decisions
- **Global arrays only** (no local arrays)
- **Pass by reference** (arrays are pointers)
- **No special array opcodes** (use existing pointer operations)
- **Bounds checking enabled** (safety first, optimize later)

### Grammar (Missing from Spec)
```
array_decl := type_keyword identifier "[" expression "]" 

array_access := identifier "[" expression "]"

// In primary_expr, add:
primary_expr := ... | array_access | ...
```

### Symbol Table Storage
- `snType` field: Use `ARRAY` type marker
- `snValue` field: Pointer to dynamically allocated array data block

### Array Data Block Format
```
Offset  Size  Description
0       1     Element type (BIT, BYTE, INT, WORD)
1       2     Element count (max 65535 elements)
3+      n     Array data (size depends on type and count)
```

### Memory Management
- Allocate from heap during declaration (single linked list like other globals)
- **Must be zero-initialized on RUN command**
- Calculate sizes:
  - BIT: (count + 7) / 8 bytes
  - BYTE: count bytes
  - INT/WORD: count * 2 bytes

### Array Access Implementation

Since arrays are global only, we compile the base address directly (no resolve-and-replace needed):

**Index Calculation:**
```asm
; For BYTE: address = base + 3 + index
; For WORD/INT: address = base + 3 + (index * 2)
; For BIT: byte_offset = base + 3 + (index >> 3)
;          bit_mask from lookup table
```

**BIT Array Optimization:**
Use existing Hopper VM Array library with lookup tables:
```asm
BitMasks:    .byte $01,$02,$04,$08,$10,$20,$40,$80
BitInvMasks: .byte $FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F
```

### Compilation
Array access compiles to:
1. Push base address (compile-time constant)
2. Evaluate index expression
3. Calculate element address
4. PEEK/POKE for BYTE arrays
5. Memory operations for WORD/INT
6. Bit operations for BIT arrays

---

## Error Handling

### Stack Unwinding
Simple due to Hopper VM architecture:
- On error or RUN: Reset `CSP = 0`, `SP = 0`, `BP = 0`
- No dynamic allocation to clean up (except globals)
- Hardware stack remains untouched

### Break Handling (Ctrl-C)
Check `ZP.SerialBreakFlag` on each `DispatchOpCode` loop:
```asm
LDA ZP.SerialBreakFlag
if (NZ) 
{
    Error.SetBreak();  // "BREAK" error message
    States.SetFailure();
    break;
}
```
Future Phase 6: Add CONT functionality to resume

---

## Implementation Priority

### Phase 2A: Arguments Only
1. Implement PUSHLOCAL/POPLOCAL for arguments (negative offsets)
2. Modify function compilation to set up arguments
3. Test with Fibonacci (needs only arguments, not locals)

### Phase 2B: Full Locals
1. Extend PUSHLOCAL/POPLOCAL for positive offsets
2. Add local variable declarations in functions
3. Implement shadowing rules

### Phase 2C: FOR/NEXT Loops
1. Allocate three locals (counter, limit, step)
2. Implement initialization
3. Handle NEXT with increment and termination check
4. Test nested loops

### Phase 2D: Arrays
1. Add array declaration syntax to tokenizer/parser
2. Implement heap allocation with headers
3. Add zero-initialization on RUN
4. Implement BYTE arrays first (simplest)
5. Add WORD/INT arrays
6. Implement BIT arrays with bit manipulation
7. Add bounds checking
8. Test with Sieve benchmark

---

## Testing Strategy

### Incremental Tests
1. **Echo function**: `FUNC Echo(x) RETURN x ENDFUNC`
2. **Local variable**: `FUNC Test() INT x = 5 RETURN x ENDFUNC`
3. **Shadowing**: Local shadows global of same name
4. **Simple BYTE array**: 10 elements, set and get
5. **BIT array**: Test all 8 bit positions in a byte
6. **FOR/NEXT**: Simple count to 10
7. **Nested FOR**: Two-level nesting
8. **Fibonacci**: Recursive with arguments
9. **Sieve**: Full benchmark

---

## Memory Architecture

All heap-based with linked lists:
- **Global variables**: Single linked list, heap allocated
- **Functions**: Separate linked list
- **Arrays**: Part of global variable list
- **Strings**: String pool (existing)
- No separate symbol table - names stored in nodes

Stack frame cleanup is trivial since stacks are reset on each RUN.

---

## Success Criteria
1. Fibonacci benchmark runs correctly with recursive calls
2. Sieve benchmark runs with 8191-element BIT array
3. FOR/NEXT loops work with local counters
4. No memory leaks or stack corruption
5. Bounds checking prevents array overruns
6. Ctrl-C break works reliably
7. Performance acceptable on 6502