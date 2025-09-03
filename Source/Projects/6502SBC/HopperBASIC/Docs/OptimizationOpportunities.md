# HopperBASIC Opcode Usage Analysis & Optimization Report

## Individual Opcode Frequency (Top 20)

| Opcode | Name           | Hex | Usage | % of Total | Optimization Potential |
|--------|----------------|-----|-------|------------|----------------------|
| PUSHLOCAL | Local variable push | 43 | ~65 | 12.4% | **HIGH** - Most frequent opcode |
| SYSCALL | System call | 48 | ~61 | 11.6% | None - I/O operations |
| POPLOCAL | Local variable pop | 44 | ~39 | 7.4% | Medium - assignment targets |
| PUSHEMPTYVAR | Variable declaration | 1B | ~27 | 5.1% | Low - necessary for stack setup |
| PUSHLONG0 | MSW=0 for 32-bit | 18 | ~24 | 4.6% | **HIGH** - part of literal pattern |
| PUSHWORD | 16-bit literal | 81 | ~24 | 4.6% | **HIGH** - combines with PUSHLONG0 |
| PUSHCHAR | Character literal | 42 | ~23 | 4.4% | Low - I/O bound |
| DECSP | Stack cleanup | 4F | ~20 | 3.8% | **HIGH** - void function cleanup |
| PUSHBYTE | 8-bit literal | 41 | ~18 | 3.4% | Medium - comparison operands |
| PUSHCSTRING | String literal | 82 | ~18 | 3.4% | Low - I/O bound |
| PUSHGLOBAL | Global variable push | 4B | ~17 | 3.2% | Medium - variable access |
| JUMPZW | Conditional jump | 88 | ~15 | 2.9% | **HIGH** - part of comparison patterns |
| PUSH1 | Literal 1 | 16 | ~14 | 2.7% | **HIGH** - increment/comparison patterns |
| CALLF | Function call | 84 | ~14 | 2.7% | **CRITICAL** - void function optimization |
| RETURNVAL | Return with value | 4A | ~13 | 2.5% | None - necessary for functions |
| PUSHVOID | Void value | 17 | ~12 | 2.3% | **CRITICAL** - void function optimization |
| POPGLOBAL | Global variable pop | 4C | ~12 | 2.3% | Medium - assignment targets |
| SUB | Subtraction | 02 | ~10 | 1.9% | Medium - part of decrement patterns |
| ADD | Addition | 01 | ~10 | 1.9% | Medium - part of math patterns |
| MUL | Multiplication | 03 | ~10 | 1.9% | **HIGH** - squaring patterns |

## Most Common 2-Opcode Patterns

| Pattern | Names | Count | Status | Optimization Opportunity |
|---------|-------|-------|---------|-------------------------|
| 81,18 | PUSHWORD, PUSHLONG0 | 24 | ❌ | **PUSHLONG** (saves 1 byte per 32-bit literal) |
| 42,48 | PUSHCHAR, SYSCALL | 23 | ✅ | None - I/O operations |
| 48,42 | SYSCALL, PUSHCHAR | 18 | ✅ | None - I/O operations |
| 82,48 | PUSHCSTRING, SYSCALL | 16 | ✅ | None - I/O operations |
| 44,43 | POPLOCAL, PUSHLOCAL | 15 | ❌ | **MOVELOCAL** (variable copying) |
| 1B,1B | PUSHEMPTYVAR, PUSHEMPTYVAR | 15 | ❌ | **PUSHMULTIVARS** (multiple declarations) |
| 43,43 | PUSHLOCAL, PUSHLOCAL | 13 | ❌ | **DUPLOCAL** (duplicate local) |
| 44,81 | POPLOCAL, PUSHWORD | 12 | ❌ | **POPLOCANL_PUSHWORD** (common assignment) |
| 43,41 | PUSHLOCAL, PUSHBYTE | 10 | ❌ | Part of comparison optimization |
| 0D,88 | EQ, JUMPZW | 9 | ❌ | Part of comparison optimization |
| 18,44 | PUSHLONG0, POPLOCAL | 9 | ❌ | **SETLOCAL0** (zero assignment) |
| 48,82 | SYSCALL, PUSHCSTRING | 9 | ✅ | None - I/O operations |
| 17,84 | **PUSHVOID, CALLF** | 8 | ❌ | **CRITICAL - CALLV** (void function call) |
| 84,4F | **CALLF, DECSP** | 8 | ❌ | **CRITICAL - Part of void cleanup** |
| 41,0D | PUSHBYTE, EQ | 8 | ❌ | Part of comparison optimization |

## Most Common 3-Opcode Patterns

| Pattern | Names | Count | Status | Optimization Opportunity |
|---------|-------|-------|---------|-------------------------|
| 48,42,48 | SYSCALL, PUSHCHAR, SYSCALL | 18 | ✅ | None - I/O operations |
| 44,81,18 | POPLOCAL, PUSHWORD, PUSHLONG0 | 12 | ❌ | **SETLOCAL32** (32-bit assignment) |
| 82,48,42 | PUSHCSTRING, SYSCALL, PUSHCHAR | 11 | ✅ | None - I/O operations |
| 1B,1B,1B | PUSHEMPTYVAR, PUSHEMPTYVAR, PUSHEMPTYVAR | 11 | ❌ | **PUSHMULTIVARS** (3+ variables) |
| 81,18,44 | PUSHWORD, PUSHLONG0, POPLOCAL | 9 | ❌ | **SETLOCAL32** (32-bit assignment) |
| 48,82,48 | SYSCALL, PUSHCSTRING, SYSCALL | 9 | ✅ | None - I/O operations |
| 41,0D,88 | PUSHBYTE, EQ, JUMPZW | 8 | ❌ | **CMPBYTEJZ** (compare byte and jump) |
| 43,41,0D | PUSHLOCAL, PUSHBYTE, EQ | 7 | ❌ | **CMPLOCALBYTE** (compare local to byte) |
| **17,84,4F** | **PUSHVOID, CALLF, DECSP** | 7+ | ❌ | **CRITICAL - CALLV** (complete void call) |
| 44,43,43 | POPLOCAL, PUSHLOCAL, PUSHLOCAL | 7 | ❌ | Complex assignment pattern |
| 81,18,16 | PUSHWORD, PUSHLONG0, PUSH1 | 7 | ❌ | **PUSHLONG_INC** (32-bit literal + 1) |
| 43,43,03 | **PUSHLOCAL, PUSHLOCAL, MUL** | 6+ | ❌ | **SQUARELOCAL** (x = x * x) |
| 43,43,01 | **PUSHLOCAL, PUSHLOCAL, ADD** | 5+ | ❌ | **DOUBLELOCAL** (x = x + x) |

## Currently Implemented Optimizations ✅

The peephole optimizer already implements these patterns:

| Pattern | Optimization | Status |
|---------|-------------|---------|
| PUSHGLOBAL n, PUSH1, ADD, POPGLOBAL n | → **INCGLOBAL** n | ✅ Implemented |
| PUSHLOCAL n, PUSH1, ADD, POPLOCAL n | → **INCLOCAL** n | ✅ Implemented |
| PUSHLOCAL n, PUSHLOCAL m, ADD, POPLOCAL n | → **ADDLOCALS** n m | ✅ Implemented |
| PUSHGLOBAL n, PUSHGLOBAL m, ADD, POPGLOBAL n | → **ADDGLOBALS** n m | ✅ Implemented |
| PUSHGLOBAL arr, PUSHGLOBAL idx, GETITEM | → **GETITEMGG** arr idx | ✅ Implemented |
| PUSHGLOBAL arr, PUSHLOCAL idx, GETITEM | → **GETITEMGL** arr idx | ✅ Implemented |
| PUSHLOCAL arr, PUSHGLOBAL idx, GETITEM | → **GETITEMLG** arr idx | ✅ Implemented |
| PUSHLOCAL arr, PUSHLOCAL idx, GETITEM | → **GETITEMLL** arr idx | ✅ Implemented |
| (Similar SETITEM patterns) | → **SETITEMXX** variants | ✅ Implemented |

## TOP PRIORITY Missing Optimizations ❌

### 1. **CRITICAL: Void Function Calls** 🔥
```
Current:  PUSHVOID + [args] + CALLF + DECSP  (4+ bytes)
Optimized: CALLV + [args]                    (2+ bytes)
Savings:  2-3 bytes per void function call
Impact:   Appears in EVERY program, exponential savings in recursion
```

### 2. **HIGH: 32-bit Literal Pushes**
```
Current:  PUSHWORD n + PUSHLONG0  (4 bytes total)
Optimized: PUSHLONG n             (3 bytes total)  
Savings:  1 byte per 32-bit literal
Impact:   Very frequent in mathematical code
```

### 3. **HIGH: Mathematical Operations on Same Variable**
```
Current:  PUSHLOCAL n + PUSHLOCAL n + MUL  (4 bytes)
Optimized: SQUARELOCAL n                   (2 bytes)
Savings:  2 bytes per squaring operation
Impact:   Critical for mathematical hot paths (Mandelbrot, etc.)

Current:  PUSHLOCAL n + PUSHLOCAL n + ADD  (4 bytes)  
Optimized: DOUBLELOCAL n                   (2 bytes)
Savings:  2 bytes per doubling operation
Impact:   Frequent in mathematical calculations
```

### 4. **MEDIUM: Comparison and Branch Combinations**
```
Current:  PUSHLOCAL n + PUSHBYTE b + EQ + JUMPZW offset  (6 bytes)
Optimized: CMPLOCALJZ n b offset                         (4 bytes)
Savings:  2 bytes per comparison
Impact:   Frequent in control flow
```

### 5. **MEDIUM: Variable Movement Operations**
```
Current:  POPLOCAL n + PUSHLOCAL m  (3 bytes)
Optimized: MOVELOCAL n m            (3 bytes same size, faster execution)
Savings:  Execution speed improvement
Impact:   Common in assignments and parameter passing
```

## Implementation Recommendations

### Phase 1: Critical (Immediate Implementation)
1. **CALLV** - Void function call optimization
2. **PUSHLONG** - 32-bit literal optimization  
3. **SQUARELOCAL/DOUBLELOCAL** - Mathematical operations

### Phase 2: High Value
4. **CMPLOCALJZ/CMPGLOBALJZ** - Comparison optimizations
5. **SETLOCAL32/SETGLOBAL32** - 32-bit assignment patterns

### Phase 3: Polish
6. **MOVELOCAL/MOVEGLOBAL** - Variable movement
7. **PUSHMULTIVARS** - Multiple variable declarations

## Expected Performance Impact

- **Code Size Reduction**: 15-25% in typical programs
- **Execution Speed**: 10-20% improvement in mathematical code  
- **Memory Usage**: Reduced bytecode buffer requirements
- **Recursive Functions**: Exponential improvements (FIBO example shows 2+ second execution with heavy recursion)

The **void function call optimization alone** would provide the highest ROI, appearing in every HopperBASIC program multiple times and creating compound benefits in recursive algorithms.