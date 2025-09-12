# C Compiler for Hopper BIOS - Project Specification v1.0

## Overview
A minimal C compiler that runs natively on 6502 under Hopper BIOS, capable of compiling simple C programs to executable binaries.

## Target Programs
The compiler must successfully compile these two benchmark programs:

### Program 1: BENCH.C
```c
// Noel's RetroLab Benchmark
void main() {
    long s;
    long st = seconds();
    long start = millis();
    
    int i, j;
    for (i = 1; i <= 10; i++) {
        s = 0;
        for (j = 1; j <= 1000; j++) {
            s = s + j;
        }
        putchar('.');
    }
    
    printf("%ld\n", s);
    printf("%ld ms\n", millis() - start);
    printf("%ld seconds\n", seconds() - st);
}
```

### Program 2: FIBO.C
```c
// Recursive Fibonacci benchmark
int fibo(int n) {
    if (n <= 1) {
        return n;
    }
    return fibo(n-1) + fibo(n-2);
}

void benchmark(char* name, int arg, int loops) {
    long start;
    int result;
    int count;
    long elapsed;
    long avgS;
    
    start = seconds();
    
    for (count = 0; count < loops; count++) {
        result = fibo(arg);
    }
    
    elapsed = seconds() - start;
    avgS = elapsed / loops;
    
    printf("%s(%d) = %d in %ld seconds average\n", 
           name, arg, result, avgS);
}

void main() {
    benchmark("Fibo", 10, 5);
}
```

## Language Specification

### Supported Types
- `void` - No value
- `char` - 8-bit value
- `int` - 16-bit signed integer  
- `long` - 32-bit signed integer
- `char*` - 16-bit pointer to char (strings only)

### Type Conversion Rules
- **Implicit promotion**: Narrower types promote to wider in binary operations
- **Assignment truncation**: Wide to narrow assignments truncate without warning
- **Function arguments**: Promote to match parameter type
- **Return values**: Convert to declared return type

### Grammar (Minimal Subset)
```
<program> ::= <function>+

<function> ::= <type> <identifier> "(" <param-list>? ")" <compound-stmt>

<statement> ::= <var-decl>
              | <expr-stmt>
              | <if-stmt>
              | <for-stmt>
              | <return-stmt>
              | <compound-stmt>

<if-stmt> ::= "if" "(" <expression> ")" <statement>

<for-stmt> ::= "for" "(" <expr-stmt> <expression> ";" <expression>? ")" <statement>

<expression> ::= <assignment>
<assignment> ::= <relational> ("=" <assignment>)?
<relational> ::= <additive> (("<" | ">" | "<=" | ">=") <additive>)*
<additive> ::= <multiplicative> (("+" | "-") <multiplicative>)*
<multiplicative> ::= <postfix> (("*" | "/" | "%") <postfix>)*
<postfix> ::= <primary> ("(" <arg-list>? ")")*
<primary> ::= <identifier> | <integer> | <string> | "(" <expression> ")"
            
```

### Built-in Functions
```c
void putchar(char c);      // Maps to Serial.WriteChar
long millis(void);         // Maps to Time.Millis  
long seconds(void);        // Maps to Time.Seconds
void printf(char* fmt, ...); // Special handling, supports %d, %ld, %s
```

## Runtime Architecture

### Memory Layout
```
0x0200-0x02FF: Stack page 0 (LSB of 32-bit values)
0x0300-0x03FF: Stack page 1 (byte 1 of 32-bit values)
0x0400-0x04FF: Stack page 2 (byte 2 of 32-bit values)  
0x0500-0x05FF: Stack page 3 (MSB of 32-bit values)
0x0058-0x005F: C runtime zero page
0x0800:        Program entry point & code start
After code:    String literals
```

### Zero Page Allocation (C Runtime)
```
0x58: cBP      - Base pointer (frame pointer)
0x59: cTemp1   - Expression evaluation temp
0x5A: cTemp2   - Expression evaluation temp
0x5B: cTemp3   - Expression evaluation temp
0x5C: cTemp4   - Expression evaluation temp
0x5D: (reserved)
0x5E: cReturnL - Return value low
0x5F: cReturnH - Return value high
```

### Calling Convention

#### Stack Frame Layout
```
SP+n: [return slot]      ← Always allocated by caller
SP+n-1: [arg1]          ← Arguments pushed left-to-right
SP+n-2: [arg2]          
...
SP+3: [ret addr H]      ← Pushed by JSR
SP+2: [ret addr L]      
SP+1: [saved BP]        ← Function prologue
SP:   [local1]          ← Local variables
SP-1: [local2]
```

#### Calling Sequence
```hopper
// Caller:
TSX
DEX                 // Reserve return slot
TXS
// Push arguments left-to-right
// (evaluate and store to parallel pages)
JSR function
// Clean up arguments
TSX
INX                 // One per argument
TXS
// Return value now at SP+1

// Callee prologue:
LDA cBP
PHA                 // Save old BP
TSX
STX cBP             // New BP = SP
// Allocate locals
TSX
DEX                 // One per local
TXS

// Callee epilogue:
// Store return value at BP+argcount+3
LDX cBP
TXS                 // Restore SP
PLA
STA cBP             // Restore BP
RTS
```

#### Register Usage
- **A**: Never preserved, freely used
- **X**: Never preserved, used for indexing, munted by BIOS calls
- **Y**: Preserved by functions that modify it (PHY/PLY)

## Compiler Architecture

### Components
1. **Lexer** - Streams tokens from source file using File.NextStream
2. **Parser** - Builds AST using heap allocation
3. **Type Checker** - Walks AST, validates types
4. **Code Generator** - Emits to code buffer
5. **Linker** - Patches addresses, appends literals, writes executable

### Data Structures

#### Symbol Table (Heap Allocated)
```
Per symbol:
- Name pointer (to heap string)
- Type (char/int/long/void/pointer)
- Storage (BP offset for locals/params)
- Scope level
- Next pointer (linked list)
```

#### AST Nodes (Heap Allocated)
```
Node types:
- FunctionDef, VarDecl, If, For, Return, Block
- BinOp, UnaryOp, Call, Assign, Ident, IntLit, StringLit

Node structure varies by type:
BinOp:  [type][op][left_ptr:2][right_ptr:2]
IntLit: [type][value:4]
Ident:  [type][symbol_ptr:2]
```

### Code Generation

#### Code Buffer
- Initially allocate 8KB via Memory.Allocate()
- Can grow if needed
- Tracks current position for emission

#### String Literal Buffer
- Separate buffer for collecting literals
- Appended to code at end
- References patched from relative to absolute

#### Printf Handling
Parse format string at compile time and generate specific calls:
```hopper
// printf("%d %ld", intval, longval) becomes:
LDA #(format % 256)
STA ZP.STRL
LDA #(format / 256)
STA ZP.STRH
JSR printf_start

// Push int argument
LDA intval_low
LDX intval_high
JSR printf_int

// Push long argument  
// (load 4 bytes to ZP.TOP)
JSR printf_long

JSR printf_end
```

### Built-in Function Marshalling
Example for `seconds()`:
```hopper
builtin_seconds:
    TSX
    DEX             // Reserve return slot
    TXS
    
    LDX #SysCall.TimeSeconds
    JSR [ZP.BIOSDISPATCH]  // Returns in ZP.TOP0-3
    
    TSX
    LDA ZP.TOP0     // Copy to stack return slot
    STA 0x0200,X
    LDA ZP.TOP1
    STA 0x0300,X
    LDA ZP.TOP2
    STA 0x0400,X
    LDA ZP.TOP3
    STA 0x0500,X
    RTS
```

## Command Line Interface
```
CC BENCH
```
- Reads: BENCH (source file, assumes .C extension conceptually)
- Writes: BENCHX (executable, marked as such in filesystem)

## Error Handling
- Stop on first error
- Report line number (counted during streaming parse)
- Display error message with context if possible

## Implementation Priority
1. Lexer with File.NextStream integration
2. Symbol table management
3. Parser building minimal AST
4. Code generator for expressions
5. Code generator for statements
6. Built-in function marshalling
7. String literal handling
8. Final linking and output

## Future Enhancements (Not in v1)
- Global variables
- Arrays
- While/do-while loops
- Switch statements
- Pointer arithmetic
- Optimization passes
- Better error recovery
- Debugging information