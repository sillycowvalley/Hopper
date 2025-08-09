# HopperBASIC Remaining Implementation Tasks

## FOR/NEXT Loops

### Design Decisions
- **Two-opcode approach** - FORCHK for initial check, FORIT for increment-and-check
- **Implicit local creation** - Loop variable automatically becomes a local if not already declared
- **Stack-based iterator** - Iterator is a proper local variable (BP-relative)
- **No global iterators** - FOR/NEXT only allowed inside functions (FUNC or BEGIN)
- **Classic BASIC behavior** - Loop variable retains overflow value after loop completion
- **Expressions evaluated once** - FROM, TO, and STEP expressions captured at loop start
- **Stack temporaries** - TO and STEP values naturally placed on stack at SP-2 and SP-1
- **Zero-iteration support** - FORCHK ensures loops can skip entirely if initial conditions not met

### Syntax
```basic
FOR i = <from> TO <to> [STEP <step>]
    ' Loop body
NEXT i
```

### Compilation Strategy

#### New Opcodes Required
```asm
// In OpCodes.asm enum OpCode:

// === THREE-OPERAND OPCODES (0xC0-0xFF) ===
// Bits 7-6: 11 (three operand bytes)
// All opcodes in this range have exactly 3 operand bytes

FORCHK = 0xC0,  // FOR initial check [iterator_offset] [forward_offset_lsb] [forward_offset_msb]
                // Compares iterator with limit at SP-2
                // Jumps forward if out of range (zero-iteration case)
                // Falls through if loop should execute
                
FORIT  = 0xC1,  // FOR iterate [iterator_offset] [backward_offset_lsb] [backward_offset_msb]  
                // Increments iterator by step at SP-1
                // Compares with limit at SP-2
                // Jumps back to loop body if continuing
                // Falls through when loop completes
```

#### Compiler Workspace
```asm
const uint compilerForIteratorOffset = Address.BasicCompilerWorkspace + 11; 
// 1 byte - signed BP offset of current FOR iterator
```

#### When encountering FOR:
1. PHA current `compilerForIteratorOffset` (for nested loops)
2. Parse iterator name
3. Create implicit local if not exists:
   - Allocate local slot (increment `compilerFuncLocals`)
   - Calculate BP offset for iterator
   - Store offset in `compilerForIteratorOffset`
4. Evaluate `<from>` expression → POPLOCAL to iterator
5. Evaluate `<to>` expression → leave on stack (becomes SP-2)
6. If STEP present: evaluate `<step>` → leave on stack (becomes SP-1)
   Otherwise: PUSH1 → leave on stack (becomes SP-1)
7. PHA current PC (location after FORCHK for backward jump target)
8. Emit FORCHK opcode with placeholder forward offset:
   ```
   FORCHK [iterator_offset] [0x00] [0x00]  ; To be patched
   ```
9. PHA location of FORCHK's offset operands (for later patching)

#### For each statement in the loop body:
10. Compile statements normally

#### When encountering NEXT:
11. Verify variable name matches current iterator (same BP offset as `compilerForIteratorOffset`)
12. PLA to get FORCHK patch location
13. PLA to get loop body start position (target for FORIT's backward jump)
14. Calculate and emit FORIT with backward jump offset:
    ```
    FORIT [iterator_offset] [backward_offset_lsb] [backward_offset_msb]
    ```
15. Patch FORCHK with forward jump offset (current PC + 4 for the DECSPs)
16. Emit cleanup: DECSP, DECSP (remove TO and STEP temporaries)
17. PLA to restore parent `compilerForIteratorOffset` (if nested)

### Stack Layout During Loop
```
[STEP value]      <- SP-1 (temporary)
[TO value]        <- SP-2 (temporary)
[Local n]         <- BP+n
...
[Iterator]        <- BP+k (actual local variable)
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
POPLOCAL k              ; Store in iterator at BP+k
PUSHBYTE 10             ; TO value - naturally on stack at SP-2
PUSHBYTE 2              ; STEP value - naturally on stack at SP-1

FORCHK k exit           ; Check: is 1 <= 10? Yes, fall through
                        ; Would jump to exit if out of range

loop_body:              ; (e.g., PC = 0x1004)
PUSHLOCAL k             ; Load iterator (correctly starts at 1)
SYSCALL PRINT
PUSHBYTE 10
SYSCALL PRINTCHAR       ; Newline

FORIT k loop_body       ; Increment i by 2, check if 3 <= 10
                        ; If yes, jump back to loop_body
                        ; If no, fall through

exit:
DECSP                   ; Remove STEP
DECSP                   ; Remove TO
; Iterator remains as local variable with final value (11 in this case)
```

### FORCHK Opcode Implementation (in Executor.asm)
```asm
executeFORCHK()
{
    // Fetch operands
    FetchOperandByte();     // Iterator BP offset in A
    STA executorOperandL    
    FetchOperandWord();     // Forward jump offset in executorOperand1/2
    
    // Load iterator value (initial value, no increment)
    LDA executorOperandL
    CLC
    ADC ZP.BP
    TAY
    LDA Address.ValueStackLSB, Y
    STA ZP.TOPL
    LDA Address.ValueStackMSB, Y
    STA ZP.TOPH
    
    // Check step sign (at SP-1) to determine comparison direction
    LDY ZP.SP
    DEY
    LDA Address.ValueStackMSB, Y
    BPL positiveStep
    
negativeStep:
    // For negative step: check if iterator >= limit
    DEY                     ; SP-2 (limit)
    LDA ZP.TOPH
    CMP Address.ValueStackMSB, Y
    if (C) { return; }      ; Iterator high >= limit high, continue loop
    if (NZ) { doJump(); }   ; Iterator high < limit high, exit loop
    LDA ZP.TOPL
    CMP Address.ValueStackLSB, Y
    if (C) { return; }      ; Iterator low >= limit low, continue loop
    doJump();               ; Iterator < limit, exit loop
    
positiveStep:
    // For positive step: check if iterator <= limit
    DEY                     ; SP-2 (limit)
    LDA Address.ValueStackMSB, Y
    CMP ZP.TOPH
    if (C) { return; }      ; Limit high > iterator high, continue loop
    if (NZ) { doJump(); }   ; Limit high < iterator high, exit loop
    LDA Address.ValueStackLSB, Y
    CMP ZP.TOPL
    if (NC) { return; }     ; Limit low >= iterator low, continue loop
    
doJump:
    // Take the forward jump (exit loop)
    CLC
    LDA ZP.PCL
    ADC executorOperand1
    STA ZP.PCL
    LDA ZP.PCH
    ADC executorOperand2
    STA ZP.PCH
}
```

### FORIT Opcode Implementation (in Executor.asm)
```asm
executeFORIT()
{
    // Fetch operands
    FetchOperandByte();     // Iterator BP offset in A
    STA executorOperandL    
    FetchOperandWord();     // Backward jump offset in executorOperand1/2
    
    // Load iterator value
    LDA executorOperandL
    CLC
    ADC ZP.BP
    TAY
    LDA Address.ValueStackLSB, Y
    STA ZP.TOPL
    LDA Address.ValueStackMSB, Y
    STA ZP.TOPH
    
    // Add step (at SP-1)
    LDY ZP.SP
    DEY
    CLC
    LDA Address.ValueStackLSB, Y
    ADC ZP.TOPL
    STA ZP.TOPL
    LDA Address.ValueStackMSB, Y
    ADC ZP.TOPH
    STA ZP.TOPH
    
    // Store updated iterator
    LDA executorOperandL
    CLC
    ADC ZP.BP
    TAY
    LDA ZP.TOPL
    STA Address.ValueStackLSB, Y
    LDA ZP.TOPH
    STA Address.ValueStackMSB, Y
    
    // Check step sign (at SP-1) to determine comparison direction
    LDY ZP.SP
    DEY
    LDA Address.ValueStackMSB, Y
    BPL positiveStep
    
negativeStep:
    // For negative step: check if iterator >= limit
    DEY                     ; SP-2 (limit)
    LDA ZP.TOPH
    CMP Address.ValueStackMSB, Y
    if (C) { doJump(); }    ; Iterator high >= limit high, continue loop
    if (NZ) { return; }     ; Iterator high < limit high, exit loop
    LDA ZP.TOPL
    CMP Address.ValueStackLSB, Y
    if (C) { doJump(); }    ; Iterator low >= limit low, continue loop
    return;                 ; Iterator < limit, exit loop
    
positiveStep:
    // For positive step: check if iterator <= limit
    DEY                     ; SP-2 (limit)
    LDA Address.ValueStackMSB, Y
    CMP ZP.TOPH
    if (C) { doJump(); }    ; Limit high > iterator high, continue loop
    if (NZ) { return; }     ; Limit high < iterator high, exit loop
    LDA Address.ValueStackLSB, Y
    CMP ZP.TOPL
    if (NC) { doJump(); }   ; Limit low >= iterator low, continue loop
    return;                 ; Limit > iterator, exit loop
    
doJump:
    // Take the backward jump (continue loop)
    CLC
    LDA ZP.PCL
    ADC executorOperand1
    STA ZP.PCL
    LDA ZP.PCH
    ADC executorOperand2
    STA ZP.PCH
}
```

### Implementation Notes
- **Two-opcode design**: FORCHK for initial check, FORIT for iterate-and-check
- **Zero-iteration support**: FORCHK ensures loops can execute zero times
- **Natural stack usage**: TO and STEP values placed naturally by existing push opcodes
- **BP offset tracking**: Single byte in compiler workspace tracks current iterator offset
- **Nested loops**: PHA/PLA pattern preserves parent loop context
- **Expression preservation**: PHA/PLA around expression evaluation preserves offsets
- **Type checking**: Iterator must be numeric type (INT, WORD, BYTE)
- **Signed offsets**: Both opcodes use signed 16-bit offsets for flexible jumping
- **Three-operand format**: Both opcodes in 0xC0-0xFF range with 3 operand bytes

### Benefits of Two-Opcode Approach
1. **Correctness**: Zero-iteration loops work properly
2. **Performance**: Only two opcode dispatches per loop (initial + per iteration)
3. **Code size**: Compact - 4 bytes for FORCHK, 4 bytes for FORIT
4. **Simplicity**: Clear separation between initial check and iteration
5. **No patching complexity**: Forward offset known when FORIT is emitted
6. **Debugging**: Clear loop structure with distinct entry and iteration points
7. **Memory efficiency**: No additional zero page usage beyond compiler workspace

### Challenges Addressed
1. **Zero-iteration loops**: ✅ Solved - FORCHK handles initial check
2. **Stack-relative access**: ✅ Solved - Both opcodes directly access SP-1 and SP-2
3. **Step sign detection**: ✅ Solved - Both opcodes check sign at runtime
4. **Type promotion**: Handle mixed types in FROM/TO/STEP expressions during compilation

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
- Add FORCHK and FORIT opcodes to OpCodes.asm (0xC0 and 0xC1)
- Update executor dispatch to handle 0xC0-0xFF as 3-operand opcodes
- Implement executeFORCHK and executeFORIT in Executor.asm
- Add compilerForIteratorOffset to compiler workspace
- Implement FOR parsing and compilation
- Implement NEXT parsing with FORIT emission
- Handle nested loops via PHA/PLA pattern
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

FUNC TestZeroIteration()
    ' Zero-iteration loop
    FOR i = 10 TO 1 STEP 1
        PRINT "Should not print"
    NEXT i
    PRINT "Correctly skipped"
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

### Stack Management
- **SP-relative access**: FORCHK and FORIT directly access SP-1 and SP-2
- **BP-relative access**: Iterator stored as regular local variable
- **Compiler state**: Single byte tracks current iterator offset
- **Nested context**: PHA/PLA preserves parent loop state

### Error Conditions
- FOR without NEXT
- NEXT without FOR  
- NEXT with wrong variable name
- FOR at global scope (not in function)
- Type mismatches in loop expressions

### Success Criteria
1. FOR/NEXT loops work with implicit locals
2. Zero-iteration loops execute correctly
3. Loop variable retains overflow value after completion
4. Stack temporaries (TO/STEP) properly managed
5. Nested FOR loops work correctly
6. Dynamic bounds evaluated once at loop start
7. STEP works with positive and negative values
8. No stack corruption from temporaries
9. DECSP cleanup after loop completion
10. FORCHK and FORIT opcodes handle all edge cases
11. Signed jump offsets work for all loop sizes