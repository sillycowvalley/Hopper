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


## Disassembly Analysis Guide

### Understanding C Compiler Output

When debugging the compiler's generated code, use these patterns to verify correct code generation.

#### Runtime Stack Pointer Initialization
The compiler initializes four zero-page pointer pairs that point to consecutive bytes in the hardware stack:
```assembly
; Expected pattern at function start:
0814: A5 1B       LDA ZP.IDXH              ; Usually 0x01 for stack page
0816: 85 62       STA runtimeStack0H       ; Set 0x62 = 0x01
0818: 1A          INC A                    ; A = 0x02
0819: 85 64       STA runtimeStack1H       ; Set 0x64 = 0x02
081B: 1A          INC A                    ; A = 0x03
081C: 85 66       STA runtimeStack2H       ; Set 0x66 = 0x03
081E: 1A          INC A                    ; A = 0x04
081F: 85 68       STA runtimeStack3H       ; Set 0x68 = 0x04
```

This creates pointers:
- `[runtimeStack0]` → 0x0100,Y (LSB of 32-bit values)
- `[runtimeStack1]` → 0x0200,Y (byte 1 of 32-bit values)
- `[runtimeStack2]` → 0x0300,Y (byte 2 of 32-bit values)
- `[runtimeStack3]` → 0x0400,Y (MSB of 32-bit values)

#### Correct Addressing Mode Patterns

**✓ CORRECT - Indirect Indexed (opcode 0x91 for STA, 0xB1 for LDA):**
```assembly
8A          TXA                      ; Transfer X to A
A8          TAY                      ; Transfer to Y for indexing
B1 62       LDA [runtimeStack0],Y    ; Load through pointer (CORRECT!)
91 62       STA [runtimeStack0],Y    ; Store through pointer (CORRECT!)
```

**✗ WRONG - Absolute Indexed (opcode 0x9D for STA, 0xBD for LDA):**
```assembly
BD 00 62    LDA 0x6200,X    ; WRONG! Treats 0x62 as high byte of address
9D 00 62    STA 0x6200,X    ; WRONG! Would access 0x6200+X, not stack
```

#### Function Prologue Pattern
```assembly
; Save old base pointer and establish new frame
A5 60       LDA runtimeBP            ; Load old BP
48          PHA                      ; Push to hardware stack
BA          TSX                      ; Get stack pointer
86 60       STX runtimeBP            ; New BP = current SP
48          PHA                      ; Reserve space (optional)
```

#### Function Epilogue Pattern
```assembly
; Restore stack and base pointer
A6 60       LDX runtimeBP            ; Load saved BP position
9A          TXS                      ; Restore stack pointer
68          PLA                      ; Pop old BP
85 60       STA runtimeBP            ; Restore BP
60          RTS                      ; Return
```

#### Local Variable Access Pattern
For a local 32-bit variable at BP-4:
```assembly
; Calculate BP + offset in Y
A5 60       LDA runtimeBP            ; Load base pointer
18          CLC                      ; Clear carry
69 FC       ADC #0xFC                ; Add -4 (0xFC = -4 in two's complement)
A8          TAY                      ; Transfer to Y

; Access all 4 bytes of the long
B1 62       LDA [runtimeStack0],Y    ; Load byte 0
B1 64       LDA [runtimeStack1],Y    ; Load byte 1
B1 66       LDA [runtimeStack2],Y    ; Load byte 2
B1 68       LDA [runtimeStack3],Y    ; Load byte 3
```

#### Parameter Access Pattern
For a parameter at BP+4 (after return address and saved BP):
```assembly
; Parameters need +3 adjustment to skip frame overhead
A5 60       LDA runtimeBP            ; Load base pointer
18          CLC                      ; Clear carry
69 07       ADC #0x07                ; Add 4+3 = 7 (skip overhead)
A8          TAY                      ; Transfer to Y
; Then use same indirect indexed access
```

#### Stack Push/Pop Patterns

**Pushing 32-bit value:**
```assembly
BA          TSX                      ; Get stack pointer
8A          TXA                      ; Transfer to A
A8          TAY                      ; Transfer to Y
A5 16       LDA ZP.NEXT0            ; Load value byte 0
91 62       STA [runtimeStack0],Y    ; Store to stack
A5 17       LDA ZP.NEXT1            ; Load value byte 1
91 64       STA [runtimeStack1],Y    ; Store to stack
; ... repeat for bytes 2 and 3
CA          DEX                      ; Adjust stack pointer
9A          TXS                      ; Update stack
```

**Popping 32-bit value:**
```assembly
BA          TSX                      ; Get stack pointer
E8          INX                      ; Point to data
8A          TXA                      ; Transfer to A
A8          TAY                      ; Transfer to Y
B1 62       LDA [runtimeStack0],Y    ; Load byte 0
85 16       STA ZP.NEXT0            ; Store to NEXT
B1 64       LDA [runtimeStack1],Y    ; Load byte 1
85 17       STA ZP.NEXT1            ; Store to NEXT
; ... repeat for bytes 2 and 3
9A          TXS                      ; Update stack
```


# Disassembly Task Description

## Definition
**Disassemble**: Convert raw hexadecimal machine code bytes back into human-readable 6502 assembly language instructions with addresses and mnemonics.

## Purpose
Disassembly is essential for debugging the CC compiler's code generation by allowing inspection of the actual machine code produced to identify bugs in the emitted instruction sequences.

## Format Requirements

### Input
Raw hex dump with addresses, typically from `dumpCodeBuffer()`:
```
0800: 4C 18 08 6C 22 00 60 25 6C 64 0A 00 25 6C 64 20
0810: 73 65 63 6F 6E 64 73 00 A2 00 20 03 08 64 61 64
```

### Output
Formatted assembly listing using Hopper syntax conventions:
```hopper
0800: JMP 0x0818           ; Jump to main
0803: JMP [0x0022]         ; BIOS dispatcher (indirect)
0806: RTS
0818: LDX #0x00            ; SysCall.MemAllocate  
081A: JSR 0x0803           ; Call BIOS
081D: STZ 0x61             ; runtimeStack0L = 0
```

## Conventions
- Use Hopper hex notation: `0x` prefix, not `$`
- Show addresses on the left as `XXXX:`
- Include meaningful comments for:
  - Jump/call targets
  - Zero page variable names when known
  - System call numbers
  - String literal contents
- Group related instruction sequences
- Identify bugs with "BUG:" annotations

## Common Patterns to Recognize
- Stack operations (TSX/TXS sequences)
- BIOS calls (LDX #syscall, JSR dispatcher)
- BP-relative addressing (LDA BP, CLC, ADC #offset, TAY)
- Parallel stack access (STA [0x61],Y through [0x67],Y)

## Example Annotations
- `; Reserve return slot` for TSX/DEX/TXS
- `; Marshal result to stack` for TOP→stack transfers
- `; BUG: Should be #0xFF` for incorrect values
- `; Function prologue/epilogue` for standard sequences



### Common Issues to Watch For

1. **Wrong Addressing Mode**: Look for opcodes 0xBD/0x9D (absolute,X) when you should see 0xB1/0x91 (indirect,Y)

2. **Missing X→Y Transfer**: Indirect indexed uses Y register, not X. Watch for missing TXA/TAY sequence.

3. **Incorrect Offset Calculation**: 
   - Locals: BP + negative offset (no adjustment)
   - Parameters: BP + positive offset + 3 (skip return address and saved BP)

4. **Stack Pointer Confusion**: The hardware stack pointer (SP) in X vs. the base pointer (BP) in zero page

5. **Uninitialized Runtime Stack Pointers**: The runtimeStack0H-3H bytes must be initialized to point to different stack pages (0x01, 0x02, 0x03, 0x04)

### Debugging Checklist

When analyzing disassembly:
- [ ] Are runtimeStack pointers initialized correctly?
- [ ] Is indirect indexed addressing (0x91/0xB1) used for stack access?
- [ ] Is X transferred to Y before indirect indexed operations?
- [ ] Are BP offsets calculated correctly (with +3 for parameters)?
- [ ] Does function prologue save/restore BP properly?
- [ ] Are 32-bit values accessed through all four pointer pairs?

### Zero Page Map for Reference
```
0x16-0x19: ZP.NEXT0-3 (32-bit accumulator)
0x22-0x23: ZP.BIOSDISPATCH (BIOS vector)
0x60: runtimeBP (base pointer)
0x61-0x62: runtimeStack0/runtimeStack0H (→ stack page 0x01)
0x63-0x64: runtimeStack1/runtimeStack1H (→ stack page 0x02)
0x65-0x66: runtimeStack2/runtimeStack2H (→ stack page 0x03)
0x67-0x68: runtimeStack3/runtimeStack3H (→ stack page 0x04)
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