# HopperBASIC Remaining Implementation Tasks

## Local Variables

### Current State
- PUSHLOCAL/POPLOCAL opcodes implemented and working for arguments
- Arguments use negative BP offsets (BP-1, BP-2, etc.)  
- ENTER opcode exists but doesn't allocate space for locals
- Functions can access arguments but cannot declare local variables

### Required Implementation

#### 1. Parse Local Variable Declarations
Detect variable declarations inside function bodies after FUNC header:
```basic
FUNC TEST(A)              ' A is ARGUMENT at BP-1
    INT X = 10            ' X is LOCAL at BP+1
    WORD Y = A + 5        ' Y is LOCAL at BP+2
    STRING S = "Hello"    ' S is LOCAL at BP+3
    RETURN X + Y
ENDFUNC
```

**Changes needed:**
- Create nodes with `SymbolType.LOCAL | dataType`
- Assign positive BP offsets starting at BP+1
- Track count in `compilerFuncLocals`
- Add to same linked list as arguments

#### 2. Modify ENTER Opcode
Currently ENTER just sets up BP. Must extend to allocate local space:
```asm
executeEnter()
{
    ; Existing: Save BP, set new BP
    
    ; NEW: Reserve space for locals
    FetchOperandByte()  ; A = local count
    if (NZ)
    {
        TAX
        loop
        {
            STZ ZP.TOPL
            STZ ZP.TOPH
            LDA #BASICType.VOID
            STA ZP.TOPT
            Stacks.PushTop()
            DEX
            if (Z) { break; }
        }
    }
}
```

#### 3. STRING Cleanup in RETURN/RETURNVAL
Future consideration when string operations are added - locals in token buffer won't need cleanup, but heap-allocated strings will.

---

## VAR Type Locals

### Design
```basic
FUNC VARTEST(V)
    VAR LOCAL = V    ' Type determined at runtime
    LOCAL = LOCAL + 1
    RETURN LOCAL
ENDFUNC
```

### Implementation Notes
- Store as `SymbolType.LOCAL | BASICType.VAR`
- Type checking happens at assignment time
- May need STRING cleanup if assigned STRING value (future)
- Same BP offset mechanism as typed locals

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

## FOR/NEXT Loops

### Design
FOR/NEXT requires three implicit locals:
1. **Counter** - Current loop value (BP+n)
2. **Limit** - TO value evaluated once (BP+n+1)
3. **Step** - STEP value, default 1 (BP+n+2)

### Compilation Strategy
```basic
FOR I = start TO end STEP increment
```
Compiles to:
```
PUSH start → POPLOCAL counter
PUSH end → POPLOCAL limit  
PUSH increment → POPLOCAL step

loop_start:
    ; Body
    
    ; NEXT:
    PUSHLOCAL counter
    PUSHLOCAL step
    ADD
    POPLOCAL counter
    
    PUSHLOCAL counter
    PUSHLOCAL limit
    [LE or GE based on step sign]
    JUMPNZW loop_start
```

### Implementation Notes
- Must verify NEXT variable matches FOR variable
- Handle nested FOR loops correctly  
- Step sign determines termination comparison (LE vs GE)
- All three loop variables are implicit locals

---

## Implementation Priority

### Phase 1: Basic Locals
- Parse INT, WORD, BYTE, BIT local declarations
- Modify ENTER to allocate space
- Assign positive BP offsets
- Test with simple functions

### Phase 2: VAR Locals  
- Add VAR type local support
- Runtime type determination
- Test with mixed-type operations

### Phase 3: FOR/NEXT
- Allocate three implicit locals
- Implement initialization and increment
- Handle NEXT matching and nesting
- Test with benchmarks

### Phase 4: Arrays
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

### Local Variables
```basic
' Basic locals
FUNC LOCALS1(A, B)
    INT X = A + B
    WORD Y = 100
    RETURN X + Y
ENDFUNC

' VAR locals
FUNC VARLOCAL(V)
    VAR X = V
    VAR Y = 10
    RETURN X + Y
ENDFUNC

' Recursion with locals
FUNC FACTORIAL(N)
    INT RESULT = 1
    IF N > 1 THEN
        RESULT = N * FACTORIAL(N - 1)
    ENDIF
    RETURN RESULT
ENDFUNC
```

### FOR/NEXT
```basic
' Simple loop
FOR I = 1 TO 10
    PRINT I
NEXT I

' Nested loops
FOR I = 1 TO 3
    FOR J = 1 TO 3
        PRINT I * J
    NEXT J
NEXT I

' With STEP
FOR I = 10 TO 1 STEP -1
    PRINT I
NEXT I
```

### Arrays
```basic
' Sieve of Eratosthenes
BIT ARRAY prime[8191]
FOR I = 2 TO 90
    IF prime[I] = 0 THEN
        FOR J = I * 2 TO 8190 STEP I
            prime[J] = 1
        NEXT J
    ENDIF
NEXT I
```

---

## Technical Notes and Caveats

### Stack Frame Architecture
**Value Stack** (data only):
```
[Local Variable n]       <- Higher addresses
...
[Local Variable 0]       <- BP points here
[Argument n]           
...
[Argument 0]
[Return Value placeholder] <- Lower addresses
```

**Call Stack** (separate, control flow):
```
[Saved BP]
[Return Address]
```

### Memory Management
- All heap-based with linked lists
- Global variables: Single linked list
- Functions: Separate linked list  
- Arrays: Part of global variable list
- String literals in functions live in token buffer (no cleanup)
- Stack frame cleanup trivial - stacks reset on RUN

### Shadowing Rules
- Locals may not duplicate argument names
- Search order: locals → arguments → globals
- Local shadows global of same name

### Compilation Context Tracking
```asm
CompilerFunctionLocals   // Count of locals in current function
CompilerFunctionArgs     // Count of arguments  
CompilerInFunction       // Flag: compiling function vs main
CompilerFrameOffset      // Current offset for next local
```

### Error Handling
- Stack unwinding simple: Reset CSP = 0, SP = 0, BP = 0
- No dynamic allocation to clean up (except globals)
- Hardware stack remains untouched
- Ctrl-C break checks `ZP.SerialBreakFlag` in dispatch loop

---

## Success Criteria
1. Local variables can be declared in functions
2. Locals properly scoped (not visible outside function)
3. VAR locals work with runtime typing
4. FOR/NEXT loops work with implicit locals
5. Arrays work with fixed element types
6. Nested FOR loops work correctly
7. Fibonacci benchmark runs with locals
8. Sieve benchmark runs with BIT array
9. No memory leaks or stack corruption
10. DASM clearly shows [BP+n] for locals