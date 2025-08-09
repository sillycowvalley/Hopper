# HopperBASIC Remaining Implementation Tasks

## FOR/NEXT Loops

### Design Decisions
- **Implicit local creation** - Loop variable automatically becomes a local if not already declared
- **No global iterators** - FOR/NEXT only allowed inside functions (FUNC or BEGIN)
- **Classic BASIC behavior** - Loop variable retains overflow value after loop completion
- **Expressions evaluated once** - FROM, TO, and STEP expressions captured at loop start

### Syntax
```basic
FOR i = <from> TO <to> [STEP <step>]
    ' Loop body
NEXT i
```

### Compilation Strategy
When encountering FOR:
1. Create implicit local for iterator if not exists (allocate slot, increment `compilerFuncLocals`)
2. Evaluate `<from>` expression → store in iterator variable
3. Evaluate `<to>` expression → store as hidden local (BP+n+1)
4. Evaluate `<step>` if present (default 1) → store as hidden local (BP+n+2)
5. Record loop start position for NEXT

When encountering NEXT:
1. Verify variable name matches most recent FOR
2. Add step to iterator: `iterator = iterator + step`
3. Compare with limit based on step sign:
   - Positive step: continue if `iterator <= limit`
   - Negative step: continue if `iterator >= limit`
4. Jump back to loop start if continuing

### Example Compilation
```basic
FOR i = 1 TO 10 STEP 2
    PRINT i
NEXT i
```

Compiles to approximately:
```
PUSH1                    ; Start value
POPLOCAL i              ; Store in iterator
PUSHBYTE 10             ; TO value
POPLOCAL hidden_limit   ; Store limit
PUSHBYTE 2              ; STEP value  
POPLOCAL hidden_step    ; Store step

loop_start:
PUSHLOCAL i
SYSCALL PRINT
PUSHBYTE 10
SYSCALL PRINTCHAR       ; Newline

; NEXT implementation:
PUSHLOCAL i
PUSHLOCAL hidden_step
ADD
POPLOCAL i              ; i = i + step

PUSHLOCAL i
PUSHLOCAL hidden_limit
LE                      ; i <= limit (for positive step)
JUMPNZW loop_start
```

### Implementation Notes
- Must track FOR context for proper NEXT matching
- Support nested FOR loops with separate contexts
- Iterator variable accessible after loop with overflow value
- All three values (iterator, limit, step) are locals in current function frame
- No cleanup needed - locals cleaned up with function exit

---

## Arrays

### Design Constraints (Established)
- **No VAR arrays** - Fixed element type only for efficient indexing
- **No array assignment** - Can't do `arr = otherArr`, only element updates  
- **Pass by reference** - Arrays are immutable pointers
- **Global arrays only** - No local arrays initially
- **No special array opcodes** - Use existing memory operations

### Array Declaration Syntax
```basic
INT ARRAY data[100]    ' 100 INTs, 200 bytes + header
BYTE ARRAY buffer[256] ' 256 BYTEs  
BIT ARRAY flags[32]    ' 32 bits = 4 bytes
STRING ARRAY names[10] ' 10 string pointers
```

### Memory Layout
```
[Header: 3 bytes]
  Byte 0: Element type (BIT, BYTE, INT, WORD)
  Byte 1-2: Element count (max 65535)
[Data: n bytes based on type and count]
```

### Implementation Steps
1. Add array declaration parsing
2. Allocate from heap with header
3. Zero-initialize on allocation
4. Implement indexing compilation
5. Add bounds checking

### Array Access Compilation
Since arrays are global only:
```asm
; For BYTE: address = base + 3 + index
; For WORD/INT: address = base + 3 + (index * 2)
; For BIT: byte_offset = base + 3 + (index >> 3)
;          bit_mask from lookup table
```

Use existing Hopper VM bit manipulation tables:
```asm
BitMasks:    .byte $01,$02,$04,$08,$10,$20,$40,$80
BitInvMasks: .byte $FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F
```

---

## Implementation Priority

### Phase 1: FOR/NEXT
- Create implicit local for iterator variable
- Allocate hidden locals for limit and step
- Implement FOR compilation with expression evaluation
- Implement NEXT with increment and comparison
- Handle nested loops correctly
- Test with benchmarks

### Phase 2: Arrays
- Parse array declarations
- Implement heap allocation with headers
- Add indexing operations
- Implement bounds checking
- Start with BYTE arrays (simplest)
- Add WORD/INT arrays
- Implement BIT arrays with bit manipulation
- Test with Sieve benchmark

---

## Testing Strategy

### FOR/NEXT
```basic
FUNC TestSimpleFor()
    ' Simple loop
    FOR I = 1 TO 10
        PRINT I
    NEXT I
    PRINT "After loop: "; I  ' Should print 11
ENDFUNC

FUNC TestNestedFor()
    ' Nested loops
    FOR I = 1 TO 3
        FOR J = 1 TO 3
            PRINT I * J
        NEXT J
    NEXT I
ENDFUNC

FUNC TestStepFor()
    ' With STEP
    FOR I = 10 TO 1 STEP -1
        PRINT I
    NEXT I
ENDFUNC

FUNC TestDynamicFor(n)
    ' Dynamic bounds
    FOR I = n/2 TO n*2
        PRINT I
        n = n + 1  ' Doesn't affect loop bounds
    NEXT I
ENDFUNC
```

### Arrays
```basic
' Sieve of Eratosthenes
BIT ARRAY prime[8191]

BEGIN
    ' Initialize all as prime (0)
    FOR I = 2 TO 90
        IF prime[I] = 0 THEN
            FOR J = I * 2 TO 8190 STEP I
                prime[J] = 1
            NEXT J
        ENDIF
    NEXT I
    
    ' Count primes
    INT count = 0
    FOR I = 2 TO 8190
        IF prime[I] = 0 THEN
            count = count + 1
        ENDIF
    NEXT I
    PRINT count; " primes found"
END
```

---

## Technical Notes

### FOR/NEXT Context Tracking
```asm
CompilerForContext       // Stack of FOR loop contexts
CompilerForDepth         // Current nesting depth
CompilerForVariable      // Current FOR variable for NEXT matching
CompilerForLoopStart     // Address to jump back to
```

### Error Conditions
- FOR without NEXT
- NEXT without FOR
- NEXT with wrong variable name
- FOR at global scope (not in function)
- Nested FOR with same variable name

### Memory Management
- FOR iterator and hidden locals cleaned up with function frame
- No special cleanup needed
- Arrays allocated on heap, persist until explicit removal

### Success Criteria
1. FOR/NEXT loops work with implicit locals
2. Loop variable retains overflow value after completion
3. Nested FOR loops work correctly
4. Dynamic bounds evaluated once at loop start
5. STEP works with positive and negative values
6. Fibonacci benchmark runs with FOR loops
7. Sieve benchmark runs with BIT array
8. No memory leaks or stack corruption
9. DASM clearly shows loop structure
10. Error detection for mismatched FOR/NEXT