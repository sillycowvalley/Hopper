# HopperBASIC Remaining Implementation Tasks

## FOR/NEXT Loops

### Design Decisions
- **Implicit local creation** - Loop variable automatically becomes a local if not already declared
- **No global iterators** - FOR/NEXT only allowed inside functions (FUNC or BEGIN)
- **Classic BASIC behavior** - Loop variable retains overflow value after loop completion
- **Expressions evaluated once** - FROM, TO, and STEP expressions captured at loop start
- **Stack-based temporaries** - TO and STEP values live on stack during loop execution

### Syntax
```basic
FOR i = <from> TO <to> [STEP <step>]
    ' Loop body
NEXT i
```

### Compilation Strategy
When encountering FOR:
1. Create implicit local for iterator if not exists (allocate slot, increment `compilerFuncLocals`)
2. Evaluate `<from>` expression → POPLOCAL to iterator variable
3. Evaluate `<to>` expression → leave on stack (becomes temporary at SP-2)
4. If STEP present: evaluate `<step>` expression → leave on stack (at SP-1)
   Otherwise: PUSH1 → leave on stack (at SP-1)
5. Save loop start position with PHA/PLA
6. Save iterator variable in ZP.IDX (preserved through compilation)

When encountering NEXT:
1. Verify variable name matches ZP.IDX (current FOR iterator)
2. Emit increment code:
   ```
   PUSHLOCAL iterator
   PUSHLOCAL [SP-1]    ; Get step from stack temporary
   ADD
   POPLOCAL iterator
   ```
3. Emit comparison:
   ```
   PUSHLOCAL iterator
   PUSHLOCAL [SP-2]    ; Get limit from stack temporary
   [LE or GE based on step sign]
   JUMPNZW loop_start
   ```
4. Emit cleanup: DECSP, DECSP (remove TO and STEP temporaries)
5. Restore parent FOR context if nested

### Stack Layout During Loop
```
[STEP value]      <- SP-1 (temporary)
[TO value]        <- SP-2 (temporary)
[Local n]         <- BP+n
...
[Iterator]        <- BP+k (actual local)
[Local 0]         <- BP
```

### Example Compilation
```basic
FOR i = 1 TO 10 STEP 2
    PRINT i
NEXT i
```

Compiles to:
```
PUSH1                   ; Start value
POPLOCAL i              ; Store in iterator
PUSHBYTE 10             ; TO value - stays on stack
PUSHBYTE 2              ; STEP value - stays on stack

loop_start:
PUSHLOCAL i
SYSCALL PRINT
PUSHBYTE 10
SYSCALL PRINTCHAR       ; Newline

; NEXT implementation:
PUSHLOCAL i
DUP                     ; Get step from SP-1 position
ADD
POPLOCAL i              

PUSHLOCAL i
[access SP-2 for limit] ; Need to access TO value
LE                      
JUMPNZW loop_start

DECSP                   ; Remove STEP
DECSP                   ; Remove TO
; Iterator i remains as local
```

### Implementation Notes
- Use PHA/PLA to manage jump-back address during compilation
- Use ZP.IDX to track current FOR iterator variable
- Preserve ZP.IDX around expression evaluations and statement compilation
- Stack temporaries accessed via relative addressing during execution
- No explicit hidden locals needed - stack provides temporary storage
- Nested FOR loops naturally handled by saving/restoring ZP.IDX

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
- Leave TO and STEP values on stack as temporaries
- Implement FOR compilation with expression evaluation
- Implement NEXT with stack-relative access to temporaries
- Use PHA/PLA for jump address, ZP.IDX for iterator tracking
- Handle nested loops via ZP.IDX save/restore
- Clean up with DECSP after loop
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
    ' Simple loop - i becomes implicit local
    FOR i = 1 TO 10
        PRINT i
    NEXT i
    PRINT "After loop: "; i  ' Should print 11
ENDFUNC

FUNC TestNestedFor()
    ' Nested loops - both become locals
    FOR i = 1 TO 3
        FOR j = 1 TO 3
            PRINT i * j
        NEXT j
    NEXT i
ENDFUNC

FUNC TestStepFor()
    ' With STEP
    FOR i = 10 TO 1 STEP -1
        PRINT i
    NEXT i
ENDFUNC

FUNC TestDynamicFor(n)
    ' Dynamic bounds - evaluated once
    FOR i = n/2 TO n*2
        PRINT i
        n = n + 1  ' Doesn't affect loop bounds
    NEXT i
ENDFUNC
```

### Arrays
```basic
' Sieve of Eratosthenes
BIT ARRAY prime[8191]

BEGIN
    ' Initialize all as prime (0)
    FOR i = 2 TO 90
        IF prime[i] = 0 THEN
            FOR j = i * 2 TO 8190 STEP i
                prime[j] = 1
            NEXT j
        ENDIF
    NEXT i
    
    ' Count primes
    INT count = 0
    FOR i = 2 TO 8190
        IF prime[i] = 0 THEN
            count = count + 1
        ENDIF
    NEXT i
    PRINT count; " primes found"
END
```

---

## Technical Notes

### FOR/NEXT Compilation State
```asm
; During compilation, manage with:
PHA/PLA                 ; Jump-back address
ZP.IDX                  ; Current FOR iterator variable
; Nested loops: save/restore ZP.IDX around inner FOR
```

### Stack Access During Execution
Need mechanism to access stack temporaries at known offsets:
- SP-1: STEP value
- SP-2: TO value
- May need new opcodes or use existing DUP/indexed access

### Error Conditions
- FOR without NEXT
- NEXT without FOR
- NEXT with wrong variable name
- FOR at global scope (not in function)
- Nested FOR with same variable name (allowed but iterator shadowed)

### Success Criteria
1. FOR/NEXT loops work with implicit locals
2. Loop variable retains overflow value after completion
3. Stack temporaries properly managed
4. Nested FOR loops work correctly
5. Dynamic bounds evaluated once at loop start
6. STEP works with positive and negative values
7. No stack corruption from temporaries
8. DECSP cleanup after loop completion
9. DASM clearly shows loop structure
10. Error detection for mismatched FOR/NEXT