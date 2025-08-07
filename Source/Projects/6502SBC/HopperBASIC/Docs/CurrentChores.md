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
- ❌ Functions cannot access their arguments (no mechanism to read them)
- ❌ Functions cannot declare local variables
- ❌ FOR/NEXT loops blocked (loop counter needs to be local)

### Required Implementation

#### Value Stack Frame Structure
```
Higher addresses
[Local Variable n]
...
[Local Variable 0]    <- Frame Pointer (BP) points here
[Argument n]           
...
[Argument 0]
[Return Value]
Lower addresses
```

#### New/Modified Opcodes
- `ENTER n` - Create stack frame with n locals (currently just a stub)
- `PUSHLOCAL offset` - Push local/argument (negative offset for args, positive for locals)
- `POPLOCAL offset` - Pop to local variable
- `RETURN n` - Clean up frame (n = locals + arguments)
- Modify `CALL` to properly set up arguments

#### Symbol Table Changes
- Add scope tracking (global vs local context)
- Store frame-relative offsets for locals/arguments
- Implement shadowing (locals can shadow globals)
- Track "current function" context during compilation

#### Compilation Context Tracking
```asm
// New ZP variables needed:
CompilerFunctionLocals   // Count of locals in current function
CompilerFunctionArgs     // Count of arguments in current function  
CompilerInFunction       // Flag: compiling function vs main program
CompilerFrameOffset      // Current offset for next local
```

---

## Arrays

### Current State
- ❌ No array declaration syntax
- ❌ No array indexing operations
- ❌ No array memory management

### Required Implementation

#### Grammar (Missing from Spec)
```
array_decl := type_keyword identifier "[" expression "]" 
            | CONST type_keyword identifier "[" expression "]"

array_access := identifier "[" expression "]"

// In primary_expr, add:
primary_expr := ... | array_access | ...
```

#### Symbol Table Storage
- `snType` field: Use `ARRAY` type marker
- `snValue` field: Pointer to dynamically allocated array data block

#### Array Data Block Format
```
Offset  Size  Description
0       1     Element type (BIT, BYTE, INT, WORD)
1       2     Element count (max 65535 elements)
3+      n     Array data (size depends on type and count)
```

#### Memory Management
- Allocate from heap during declaration
- **Must be zero-initialized on RUN command**
- Track allocations for cleanup on NEW
- Calculate sizes:
  - BIT: (count + 7) / 8 bytes
  - BYTE: count bytes
  - INT/WORD: count * 2 bytes

#### Array Access Implementation

**BYTE Arrays:**
```asm
; Address = base + 3 (header) + index
LDA base
CLC
ADC #3
ADC index_low
STA ptr
; Handle high byte...
```

**WORD/INT Arrays:**
```asm
; Address = base + 3 + (index * 2)
; Shift index left once, then add
ASL index_low
ROL index_high
; Then add to base+3
```

**BIT Arrays:**
```asm
; Byte offset = (index >> 3) + base + 3
; Bit mask = 1 << (index & 7)
LDA index_low
AND #7          ; Bit position
TAX
LDA #1
loop: DEX
      BMI done
      ASL
      JMP loop
done: ; A now contains bit mask

; For byte offset:
LDA index_low
LSR
LSR  
LSR             ; Divide by 8
CLC
ADC base_low
ADC #3          ; Skip header
; Continue for high byte...
```

#### New Opcodes
- `PUSHARRAY` - Load array element onto stack
- `POPARRAY` - Store stack top to array element
- Both need type information for proper sizing

#### Type Checking
- Verify index is integer type (BYTE, INT, or WORD)
- Verify value type matches array element type
- Range checking (optional - might be too expensive)

---

## Implementation Priority

### Phase 2A: Locals and Arguments
1. Implement basic stack frame (ENTER/RETURN modifications)
2. Add PUSHLOCAL/POPLOCAL opcodes
3. Update symbol table for scope tracking
4. Modify function compilation to track locals
5. Test with recursive Fibonacci

### Phase 2B: FOR/NEXT Loops
1. Use local variable for loop counter
2. Implement STEP support
3. Handle loop termination correctly

### Phase 2C: Arrays
1. Add array declaration syntax to tokenizer/parser
2. Implement heap allocation with headers
3. Add zero-initialization on RUN
4. Implement BYTE arrays first (simplest)
5. Add WORD/INT arrays
6. Implement BIT arrays with bit manipulation
7. Test with Sieve benchmark

---

## Key Challenges

### Locals/Arguments:
- Keeping track of frame pointer manipulations
- Proper cleanup on early RETURN
- Nested function calls
- Error handling with partial frames

### Arrays:
- BIT array bit manipulation complexity
- Ensuring zero-initialization doesn't trash other data
- Memory fragmentation with dynamic allocation
- Array bounds checking (performance vs safety)

---

## Success Criteria
1. Fibonacci benchmark runs correctly with recursive calls
2. Sieve benchmark runs with 8191-element BIT array
3. FOR/NEXT loops work with local counters
4. No memory leaks or stack corruption
5. Performance remains acceptable on 6502