# Hopper BASIC Programming GURU Guidelines and Common Pitfalls

## Language Constraints and Rules

### Variable and Array Declaration Rules
- **ALL local variables MUST be declared at function start** - no VAR declarations after any control flow statements (FOR, WHILE, DO, UNTIL, IF, THEN, ELSE, RETURN)
- **Arrays can ONLY be declared at global scope** - never attempt to declare arrays within functions
- **Exception:** Implicit FOR loop iterators are automatically declared
- **Correct pattern:**
  ```basic
  FUNC ProcessData()
      VAR i, j, cnt, result  ! All locals declared first
      ! Now control flow is allowed
      FOR k = 0 TO 10  ! k is implicitly declared
          result = result + k
      NEXT k
  ENDFUNC
  ```
- **Incorrect:** Declaring variables inside loops or conditionals
- **Example Fix:** Change `VAR nL = PEEK(c)` inside WHILE to declaring `VAR nL, nH` at function start, then `nL = PEEK(c)` inside loop

### Identifier Naming Rules
- **NEVER USE UNDERSCORES** - They are illegal in HopperBASIC identifiers and will cause syntax errors
- **NEVER USE RESERVED WORDS** - Cannot use built-in function names, keywords, or data types as variables
- **Reserved words include:** LEN, ABS, CHR, ASC, PEEK, POKE, RND, IF, THEN, ELSE, WHILE, FOR, NEXT, FUNC, VAR, CONST, BIT, CHAR, BYTE, WORD, INT, LONG, STRING, TRUE, FALSE, AND, OR, NOT, READ, WRITE, PINMODE, I2CFIND, I2CBEGIN, etc.
- **Use camelCase or simple concatenation** for multi-word identifiers
- **Keep names short** for token efficiency
- **Common mistake:** `VAR len = LEN(s)` fails because LEN is reserved
- **Solution:** Use alternatives like `sz`, `size`, `cnt`, `num`, `val`, `pos`, etc.

### Correct Naming Examples:
```basic
! Good naming patterns
VAR i, j, k                    ! Single letters
VAR cnt, pos, sz, val          ! Short abbreviations  
VAR gameOver, playerScore      ! camelCase
VAR maxItems, totalCount       ! camelCase
CONST PI = 3                   ! Simple constants
CONST MAXSIZE = 100           ! ALL_CAPS constants

! Module namespacing
CONST hpcBlockSize = 16        ! Heap manager constant
VAR hpFreeList                 ! Heap manager variable
FUNC hpAlloc(size)            ! Heap manager function
```

### Incorrect Naming Examples (ILLEGAL):
```basic
! These will cause syntax errors
VAR player_score         ! ILLEGAL - underscore
VAR max_items           ! ILLEGAL - underscore  
CONST BLOCK_SIZE = 16   ! ILLEGAL - underscore
FUNC hp_alloc(size)     ! ILLEGAL - underscore
VAR len = 10            ! ILLEGAL - reserved word
VAR next = 5            ! ILLEGAL - reserved word
VAR if = TRUE           ! ILLEGAL - reserved word
```

### Code Organization and Size Optimization

#### Comment Placement
- **Place comments BEFORE function headers, not within functions** - comments in the token stream increase program size
- **Correct:**
  ```basic
  ! Calculate factorial recursively
  FUNC Factorial(n)
      VAR result
      IF n <= 1 THEN
          RETURN 1
      ENDIF
      RETURN n * Factorial(n - 1)
  ENDFUNC
  ```
- **Incorrect:**
  ```basic
  FUNC Factorial(n)
      ! This comment wastes token space
      VAR result
      RETURN n * Factorial(n - 1)
  ENDFUNC
  ```

#### Identifier Naming for Size Efficiency
- **Use short identifier names** - longer names increase program size in the token buffer
- **Prefer single letters for locals:** `i`, `j`, `k`, `n`, `c`, `x`, `y`, `z`
- **Use abbreviations for globals:** `cnt` not `counter`, `sz` not `size`, `pos` not `position`

#### Module Organization and Namespacing
- **Use lowercase prefixes for module namespacing** to avoid identifier conflicts
- **Function/Variable naming:** `moduleXXX` (e.g., `hpAlloc`, `hpFree` for heap manager)
- **Constant naming:** `modulecXXX` (e.g., `hpcBlockSize`, `hpcMaxHeap`)

### String and Array Access
- **Always check bounds before array/string access** - HopperBASIC enforces bounds checking
- **Runtime error:** "VALUE OUT OF RANGE" when accessing beyond string/array length
- **Safe pattern:** `IF i < LEN(str) THEN c = ASC(str[i]) ELSE c = 0 ENDIF`

### Buffer and Memory Constraints
- **Keep functions concise** - limited token buffer space per function (~100-150 lines max)
- **Use short identifiers** - prefer `c`, `i`, `n` over `current`, `index`, `number`
- **Minimize comments** - only essential ones, keep them brief
- **Avoid deeply nested structures** - can overflow buffers
- **Split large functions** - many IF statements (20+) can exceed token buffer
- **Watch for misleading errors** - "BUFFER OVERFLOW" often means function too long, not memory issue

### Control Flow Constraints
- **Single-line IF/THEN/RETURN broken** - `IF x THEN RETURN ENDIF` fails on one line
- **Use multi-line form instead:**
  ```basic
  IF x THEN
      RETURN
  ENDIF
  ```
- **Applies to all control statements with RETURN** - always use multi-line form

### Function Return Values
- **Functions can only return ONE value** - no multiple returns or tuples
- **Workarounds:** Use global variables, modify passed arrays, or combine operations
- **Example:** Can't have `GetOps()` return opc, op1, op2, op3 - must use different approach

### Program Structure
- **BEGIN...END defines main program** - replaces existing main when loaded
- **Utility functions should avoid BEGIN...END** - allows coexistence with programs being debugged
- **SAVE without BEGIN...END** - creates reusable function libraries

## Common Error Messages and Fixes

| Error | Actual Cause | Fix |
|-------|-------------|-----|
| `?SYNTAX ERROR` | Using reserved word as variable OR underscore in name OR single-line IF/THEN/RETURN OR array declared in function | Choose different name OR remove underscore OR use multi-line form OR move array to global scope |
| `?NO MORE LOCALS` | VAR after control flow OR trying to declare array locally | Move all VARs to function start OR move arrays to global scope |
| `?VALUE OUT OF RANGE` | Array/string bounds exceeded | Check length before access |
| `?TYPE MISMATCH` | Incompatible types in operation | Ensure type compatibility |
| `?BUFFER OVERFLOW` | Function too long (NOT memory issue!) | Split into smaller functions, use shorter names, fewer comments |

## Known Bugs and Annoyances

### Parser Issues
- **Single-line IF/THEN/RETURN fails** - Parser doesn't handle `IF condition THEN RETURN ENDIF` on one line
- **Inconsistent syntax acceptance** - Some constructs work multi-line but not single-line
- **No else-if chain** - Must nest IF statements, no ELSEIF keyword

### Error Message Problems
- **"BUFFER OVERFLOW" is misleading** - Usually means token buffer exceeded (function too long), not runtime memory issue
- **No line numbers in errors** - Hard to locate syntax errors in large functions
- **Generic "SYNTAX ERROR"** - Doesn't specify what's wrong (reserved word? bad structure?)

### Language Limitations
- **No multiple return values** - Functions limited to single return, complicates many algorithms
- **Token buffer too small** - ~100-150 line practical limit per function
- **No computed GOTO/GOSUB** - Can't jump based on calculated values
- **No SELECT/CASE statement** - Must use nested IFs for multi-way branches
- **No string concatenation operator** - Must use multiple PRINT statements

### Development Friction
- **Can't test partial functions** - Must complete entire function before testing
- **No #include or libraries** - Can't easily share code between programs
- **Limited debugging** - No breakpoints, watch variables, or step-through in BASIC mode

## Best Practices for Utility Functions

### Function Design Patterns
```basic
! Good pattern - all guidelines followed
! Process array data with bounds checking
FUNC ProcessArray()
    VAR i, sz, val, sum     ! All locals declared first, short names
    sz = LEN(dataArray)     
    FOR i = 0 TO sz - 1     ! Implicit iterator OK
        val = dataArray[i]
        sum = sum + val
    NEXT i
    RETURN sum
ENDFUNC

! Bad pattern - multiple violations
FUNC ProcessArrayData()     ! Name too long
    FOR index = 0 TO LEN(dataArray) - 1  ! Don't declare locals after this
        VAR value = dataArray[index]      ! ILLEGAL - VAR after FOR
        ! This comment wastes token space
        summation = summation + value     ! Long names waste space
    NEXT index
ENDFUNC
```

### Module Template
```basic
! ============================================
! MyModule - Brief description
! ============================================

! Module constants (no underscores!)
CONST mycMaxItems = 100
CONST mycVersion = 1

! Module globals (arrays must be global)
VAR myItems, myCount, myStatus
INT myScores[100]           ! Arrays only at global scope
CHAR myBuffer[256]

! Initialize module
FUNC myInit()
    VAR i
    myCount = 0
    myStatus = TRUE
    FOR i = 0 TO mycMaxItems - 1
        myScores[i] = 0
    NEXT i
ENDFUNC

! Add item to collection
FUNC myAddItem(item)
    VAR pos
    IF myCount >= mycMaxItems THEN
        RETURN FALSE
    ENDIF
    pos = myCount
    myScores[pos] = item
    myCount = myCount + 1
    RETURN TRUE
ENDFUNC
```

### Array Declaration Examples
```basic
! Global arrays - correct placement (no underscores in names!)
BIT flags[256]
CHAR buffer[128]  
INT scores[10]
WORD gameData[50]

FUNC ProcessData()
    ! Cannot declare arrays here - syntax error
    ! INT localArray[10]  ! ILLEGAL
    
    VAR i, j, val  ! Only simple variables allowed as locals
    FOR i = 0 TO LEN(gameData) - 1
        val = gameData[i]
        ! process val
    NEXT i
ENDFUNC
```

### Memory Inspection Functions
- Use PEEK to read memory directly
- Zero page locations are documented in ZeroPage.asm
- Symbol structures have fixed offsets (ptr:0-1, type:2, tokens:3-4, etc.)

### String Comparison from Memory
```basic
! Compare string with memory contents
FUNC StrEq(addr, str)
    VAR o, i, c1, c2, sz  ! All locals declared first
    o = 11  ! Name offset in symbol
    i = 0
    sz = LEN(str)
    ! Compare byte by byte with bounds checking
ENDFUNC
```

### Hex Formatting
- Keep hex functions minimal - single byte or word
- Use compact math: `h = b / 16`, `l = b & 15`

### Working Around Token Buffer Limits
- Split large IF/THEN chains into multiple functions
- Use helper functions liberally
- Consider table-driven approaches over many IFs
- Keep each function under ~50 lines when possible

## Memory Layout Knowledge

### Key Zero Page Locations
- `0x08`: HEAPSTART - heap start page
- `0x09`: HEAPSIZE - heap size in pages  
- `0x3C-0x3D`: VariablesList pointer
- `0x3E-0x3F`: FunctionsList pointer

### Symbol Node Structure (Functions/Variables)
```
Offset 0-1:  ptr pointer  (not "next" - reserved word!)
Offset 2:    type|datatype (packed)
Offset 3-4:  tokens pointer
Offset 5-6:  value/args pointer
Offset 7-8:  extended value
Offset 9-10: opcode pointer (0000 = not compiled yet)
Offset 11+:  null-terminated name
```

## Development Workflow Tips

1. **Test incrementally** - HopperBASIC gives immediate feedback
2. **Use FORGET** to remove test functions
3. **SAVE utilities separately** from main programs
4. **Keep REPL-friendly versions** without BEGIN...END
5. **Test with edge cases** - empty strings, zero values, boundaries
6. **Watch function size** - Split before hitting token limit
7. **Use multi-line IF/THEN/RETURN** - Avoid parser bug
8. **Expect JIT compilation** - Functions compile on first call

### Size Optimization Checklist
1. **Short identifiers** - `i` not `index`, `sz` not `size`
2. **No underscores** - use camelCase instead
3. **No reserved words** - check against built-in functions and keywords
4. **Comments before functions** - not inside token stream
5. **Minimal nesting** - avoid deep IF/FOR structures
6. **Split large functions** - keep under 50 lines when possible
7. **Use abbreviations** - `cnt`, `pos`, `val`, `res`, `tmp`
8. **Module prefixes** - prevent name collisions

### Development Workflow Updates
1. **Design module interfaces first** - plan your `xxxInit()`, `xxxProcess()`, etc.
2. **Declare all arrays globally** - even if only used by one function
3. **Use consistent prefixes** - makes code easier to navigate
4. **Test functions individually** - build up from simple to complex
5. **Monitor token usage** - shorter names = more functionality fits
6. **Keep utility libraries** - save commonly used modules separately

## Examples of Good Practice

### Heap Manager Module Example
```basic
! ============================================
! Heap Manager - Dynamic memory allocation
! ============================================

! Heap constants
CONST hpcBlockSize = 16
CONST hpcMinAlloc = 4
CONST hpcMaxHeap = 8192

! Heap globals
VAR hpStart, hpEnd, hpFree, hpUsed

! Initialize heap system
FUNC hpInit()
    VAR p
    hpStart = PEEK(0x08) * 256
    hpEnd = hpStart + PEEK(0x09) * 256
    hpFree = hpStart
    hpUsed = 0
ENDFUNC

! Allocate memory block
FUNC hpAlloc(sz)
    VAR p, blkSz
    IF sz < hpcMinAlloc THEN
        sz = hpcMinAlloc
    ENDIF
    blkSz = (sz + hpcBlockSize - 1) & (~(hpcBlockSize - 1))
    p = hpFree
    hpFree = hpFree + blkSz
    hpUsed = hpUsed + blkSz
    RETURN p
ENDFUNC

! Get heap statistics
FUNC hpStats()
    VAR total
    total = hpEnd - hpStart
    PRINT "Heap: "; hpUsed; "/"; total; " bytes used"
ENDFUNC
```

### I2C Display Module Example
```basic
! ============================================
! OLED Display Driver - SSD1306 via I2C
! ============================================

! Display constants
CONST dpcAddr = 0x3C
CONST dpcWidth = 128
CONST dpcHeight = 64

! Display globals
VAR dpReady, dpX, dpY

! Initialize display
FUNC dpInit()
    VAR ok
    IF NOT I2CFIND(dpcAddr) THEN
        RETURN FALSE
    ENDIF
    
    I2CBEGIN(dpcAddr)
    I2CPUT(0)       ! Command mode
    I2CPUT(174)     ! Display off
    I2CPUT(213)     ! Set clock
    I2CPUT(128)     ! Ratio
    I2CPUT(175)     ! Display on
    ok = I2CEND()
    
    IF ok THEN
        dpReady = TRUE
        dpX = 0
        dpY = 0
    ENDIF
    RETURN ok
ENDFUNC

! Set cursor position
FUNC dpSetPos(x, y)
    IF x < dpcWidth AND y < dpcHeight THEN
        dpX = x
        dpY = y
        RETURN TRUE
    ENDIF
    RETURN FALSE
ENDFUNC
```

## Keywords for Search
HopperBASIC, 6502 BASIC, variable declaration, local variables, reserved words, built-in functions, array bounds, string access, buffer overflow, token buffer, function size limits, BEGIN END, main program, utility functions, PEEK POKE, memory inspection, zero page, symbol table, type mismatch, syntax error, debugging, REPL, single-line IF THEN RETURN bug, multiple return values, parser issues, token limit, JIT compilation, no underscores, camelCase, module prefixes