# HopperBASIC Programming Guidelines and Common Pitfalls

## Language Constraints and Rules

### Variable Declaration Rules
- **ALL local variables MUST be declared at function start** - no VAR declarations after any control flow statements (FOR, WHILE, DO, UNTIL, IF, THEN, ELSE)
- **Incorrect:** Declaring variables inside loops or conditionals
- **Correct:** Declare all variables at the top, then use assignment later
- **Example Fix:** Change `VAR nL = PEEK(c)` inside WHILE to declaring `VAR nL, nH` at function start, then `nL = PEEK(c)` inside loop

### Reserved Words and Built-in Functions
- **Cannot use built-in function names as variables**: LEN, ABS, CHR, ASC, PEEK, POKE, RND, etc.
- **Common mistake:** `VAR len = LEN(s)` fails because LEN is reserved
- **Solution:** Use alternatives like `sz`, `size`, `cnt`, `num`, etc.

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
| `?SYNTAX ERROR` | Using reserved word as variable OR single-line IF/THEN/RETURN | Choose different name OR use multi-line form |
| `?NO MORE LOCALS` | VAR after control flow | Move all VARs to function start |
| `?VALUE OUT OF RANGE` | Array/string bounds exceeded | Check length before access |
| `?TYPE MISMATCH` | Incompatible types in operation | Ensure type compatibility |
| `?BUFFER OVERFLOW` | Function too long (NOT memory issue!) | Split into smaller functions |

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

### Memory Inspection Functions
- Use PEEK to read memory directly
- Zero page locations are documented in ZeroPage.asm
- Symbol structures have fixed offsets (next:0-1, type:2, tokens:3-4, etc.)

### String Comparison from Memory
```basic
FUNC StrEq(addr, str)
    VAR o = 11  ! Name offset in symbol
    VAR i = 0
    VAR c1, c2
    VAR sz = LEN(str)
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
Offset 0-1:  next pointer
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

## Keywords for Search
HopperBASIC, 6502 BASIC, variable declaration, local variables, reserved words, built-in functions, array bounds, string access, buffer overflow, token buffer, function size limits, BEGIN END, main program, utility functions, PEEK POKE, memory inspection, zero page, symbol table, type mismatch, syntax error, debugging, REPL, single-line IF THEN RETURN bug, multiple return values, parser issues, token limit, JIT compilation