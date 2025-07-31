# Resolve-and-Replace Architecture for HopperBASIC

## Core Concept: Runtime Symbol Resolution with Opcode Patching

Instead of runtime symbol lookups on every access, HopperBASIC uses a "resolve-and-replace" system:

1. **Initial compilation**: Emit unresolved opcodes with token buffer offsets to symbol names
2. **First execution**: Resolve names to node addresses, patch opcodes to fast versions
3. **Subsequent executions**: Use direct node address access (very fast)

This provides excellent performance while maintaining type safety through node-based access.

---

## Opcode Architecture: Unresolved → Resolved Pairs

### Function Calls
```hopper
// Unresolved (first execution)
CALL     functionNameOffset    // Look up function by name, patch to CALLF
// Resolved (subsequent executions)  
CALLF    functionNodeAddress   // Direct function call via node address
```

### Global Variable Access  
```hopper
// Unresolved (first execution)
PUSHGLOBAL variableNameOffset  // Look up variable by name, patch to PUSHGLOBALF
POPGLOBAL  variableNameOffset  // Look up variable by name, patch to POPGLOBALF

// Resolved (subsequent executions)
PUSHGLOBALF variableNodeAddress // Direct variable access via node address
POPGLOBALF  variableNodeAddress // Direct variable assignment via node address
```

### String Literals (Always Resolved)
```hopper
PUSHCSTRING stringAddress      // Direct pointer to string in token buffer
```

---

## Implementation Roadmap

### Phase 1: Function Call Resolution (Priority 1)
**Goal**: Enable `RUN` command and function calls for benchmarks

**Required Changes:**
1. **OpCodes.asm**: Add `CALL = 0x47` and `CALLF = 0x48`
2. **Compiler.asm**: Emit `CALL functionNameOffset` for function calls
3. **Executor.asm**: Implement resolution logic in `executeCall()`
4. **RUN command**: Find "BEGIN" function and execute it

**Test Case:**
```basic
FUNC Greet()
    PRINT "Hello"
ENDFUNC

BEGIN
    Greet()  // Function call
END

RUN  // Execute main program
```

### Phase 2: Global Variable Resolution (Priority 2)  
**Goal**: Fast global variable access with full type safety

**Required Changes:**
1. **OpCodes.asm**: Add `PUSHGLOBALF = 0x86`, `POPGLOBALF = 0x87`
2. **Compiler.asm**: Update `EmitPushGlobal()` to use name offsets
3. **Executor.asm**: Implement resolution logic in `executePushGlobal()`/`executePopGlobal()`

**Test Case:**
```basic
INT counter = 0
FUNC Increment()
    counter = counter + 1  // Global access
ENDFUNC
```

### Phase 3: CONSTSTRING Support (Priority 3)
**Goal**: String literals with full expression integration

**Required Changes:**
1. **BasicTypes.asm**: Add `CONSTSTRING = 0x13`
2. **Tokenizer.asm**: Enable string literal scanning (`"text"`)
3. **Compiler.asm**: Add `PUSHCSTRING` emission for string literals
4. **ComparisonInstructions.asm**: Smart string equality (pointer + content)
5. **Statement.asm**: Update `executePrint()` for CONSTSTRING handling

**Syntax Support:**
```basic
// Allowed (Phase 3)
CONST STRING greeting = "Hello World"    // Immutable string pointer
CONST STRING empty                       // Empty string (points to null terminator)
PRINT greeting                           // Direct string printing
IF message = "OK" THEN PRINT "Success"   // String comparison

// Not allowed (future)
STRING message = "Hello"                 // Mutable strings (Phase 4+)
```

---

## Technical Implementation Details

### Resolution Process (Functions and Variables)

**Step 1: Name Resolution**
```hopper
// At executeCall() first encounter:
// 1. Extract name offset from operand
// 2. Calculate absolute address: tokenBuffer + compilationStart + offset
// 3. Look up function via Functions.Find()
// 4. Get function node address
```

**Step 2: Opcode Patching**
```hopper
// 5. Patch opcode stream in place:
//    - Change CALL (0x47) to CALLF (0x48)  
//    - Replace name offset with node address
// 6. Execute CALLF immediately (don't defer to next run)
```

**Step 3: Fast Execution**
```hopper
// Subsequent executions hit CALLF directly:
// 1. Fetch node address from operand
// 2. Execute function immediately (no lookup)
```

### Node-Based Access Benefits

**Type Safety:**
- Push operations get type from node: `Variables.GetValue()` → `ZP.TOP + ZP.TOPT`
- Pop operations validate compatibility: `Variables.GetType()` + `CheckRHSTypeCompatibility()`

**Performance:**
- Resolved opcodes are 2-3x faster (no symbol table traversal)
- Type information cached in node structure

**Memory Efficiency:**
- Function token streams persist with resolved opcodes
- No runtime symbol lookup overhead

---

## Recompilation Triggers

### Structural Changes (Require Recompilation)
- Adding/removing global variables → affects PUSHGLOBAL/POPGLOBAL resolution
- Adding/removing functions → affects CALL resolution  
- Renaming symbols → invalidates name-based resolution

### Value Changes (No Recompilation)
- Assigning new values to existing variables → resolved opcodes remain valid
- Calling existing functions → CALLF opcodes remain valid

**Implementation Note:** For MVP, trigger full recompilation on any structural change. Future optimization could track which functions need recompilation.

---

## String Syntax Constraints (Phase 3)

### Supported String Operations
```basic
CONST STRING msg = "Hello"       // String literal assignment
PRINT msg                        // String printing
PRINT "Direct literal"           // Direct string printing  
IF name = "admin" THEN ...       // String equality (smart: pointer + content)
```

### Deliberately Unsupported (This Phase)
```basic
STRING buffer = "Hello"          // Mutable strings (compile error)
msg = msg + " World"             // String concatenation (compile error)
INPUT msg                        // String input (compile error)
```

**Rationale:** Immutable strings via `CONST STRING` provide 90% of functionality needed for benchmarks while avoiding complex string memory management.

---

## Success Metrics

### Phase 1 Complete: Function Calls
```basic
FUNC Fibo(n)
    IF n <= 1 THEN RETURN n
    RETURN Fibo(n-1) + Fibo(n-2)  // Recursive calls work
ENDFUNC

RUN  // Executes BEGIN block
```

### Phase 2 Complete: Fast Globals
```basic
INT counter = 0
FUNC Test()
    counter = counter + 1  // Fast global access after first call
ENDFUNC
```

### Phase 3 Complete: String Literals
```basic
CONST STRING name = "Fibonacci"
FUNC Benchmark(arg, loops)
    PRINT name, "(", arg, ") = ", result
ENDFUNC
```

---

## Architecture Benefits

1. **Performance**: Resolved opcodes eliminate symbol lookup overhead
2. **Type Safety**: Node-based access preserves full type information
3. **Simplicity**: Same pattern for functions, variables, and strings
4. **Extensibility**: Easy to add new symbol types (arrays, etc.)
5. **Debugging**: Unresolved opcodes clearly show first-time execution

This resolve-and-replace architecture positions HopperBASIC for excellent performance while maintaining the simplicity and type safety required for a robust BASIC interpreter.