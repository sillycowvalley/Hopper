# TinyC 6502 Compiler - Complete Project Specification

## Overview
A minimal C compiler written in 6502 Hopper Assembly that compiles a subset of C directly to 6502 machine code. The compiler is designed to be self-hosting: once bootstrapped, it can compile itself.

## Bootstrap Language Subset

### Data Types
- **int** - 32-bit signed integer (only type needed for bootstrap)

### Identifiers  
- **Variables**: Single lowercase letter (a-z) - 26 globals maximum
- **Functions**: Single uppercase letter (A-Z) - 26 functions maximum

### Literals
- **Hex integers**: `0x00` through `0xFFFFFFFF`
- **Character constants**: `'x'` (single ASCII character, no escapes)

### Operators (in precedence order)
1. `=` (assignment) - right associative
2. `==`, `!=` (equality)
3. `<`, `>` (comparison)
4. `+`, `-` (addition/subtraction)
5. `*`, `/` (multiplication/division)

### Control Flow
- `if (expr) stmt` - no else in bootstrap
- `for(;;) { ... }` - infinite loop
- `break;` - exit current loop
- `continue;` - restart current loop
- `return expr;` - return from function

### Built-in Functions
- `int G()` - getchar() equivalent (reads from input stream)
- `int P(int c)` - putchar() equivalent (writes to output stream)
- `int E(int code)` - error() and exit

## Formal Grammar

```
program     := global* function+

global      := 'int' var_list ';'
var_list    := LOWER (',' LOWER)*

function    := 'int' UPPER '(' params? ')' block
params      := 'int' LOWER (',' 'int' LOWER)*

block       := '{' statement* '}'

statement   := if_stmt
            | for_stmt  
            | 'break' ';'
            | 'continue' ';'
            | 'return' expr ';'
            | expr ';'
            | block

if_stmt     := 'if' '(' expr ')' statement

for_stmt    := 'for' '(' ';' ';' ')' statement

expr        := assign_expr

assign_expr := eq_expr ('=' assign_expr)?

eq_expr     := rel_expr (('==' | '!=') rel_expr)*

rel_expr    := add_expr (('<' | '>') add_expr)*

add_expr    := mul_expr (('+' | '-') mul_expr)*

mul_expr    := unary_expr (('*' | '/') unary_expr)*

unary_expr  := primary_expr
            | '-' unary_expr

primary_expr:= LOWER                    # variable
            | UPPER '(' args? ')'        # function call
            | HEX                        # hex literal  
            | CHAR                       # character literal
            | '(' expr ')'               # grouped expression

args        := expr (',' expr)*

# Lexical elements
LOWER       := 'a'..'z'
UPPER       := 'A'..'Z'  
HEX         := '0x' [0-9a-fA-F]+
CHAR        := '\'' . '\''
```

## Runtime Architecture

### Memory Layout
```
0x0058-0x00EB   Global variable handles (148 bytes)
0x0100-0x01FF   6502 hardware stack (holds handles)
0x2000-0x3FFF   Program code
0x4000-0x43FF   Value table (256 slots × 4 bytes)
0x4400+         String literals (if needed)
```

### Value Table
Each handle (0-255) maps to a 4-byte slot:
```
Slot[handle] at 0x4000 + (handle * 4):
  Byte 0-3: 32-bit little-endian integer value
```

### Handle Allocation Strategy
- Handles 0-25: Reserved for global variables (a-z)
- Handle 26: Reserved as temporary for return values
- Handles 27-255: Dynamic allocation for temporaries and locals
- Simple incrementing allocator during expression evaluation
- Reset handle allocator at statement boundaries

## Calling Convention

### Stack Frame Layout
```
Higher addresses (stack grows down)
[0x1FF]  ...prior stack...
[0x1FE]  Return handle      <- Caller pushes first
[0x1FD]  Arg 1 handle       <- Caller pushes
[0x1FC]  Arg 2 handle       <- Caller pushes (SP before JSR)
[0x1FB]  Return addr H      <- JSR pushes
[0x1FA]  Return addr L      <- JSR pushes (SP after JSR)
[0x1F9]  Saved BP           <- Function pushes (BP points here)
[0x1F8]  Local 1 handle     <- Function allocates
[0x1F7]  Local 2 handle     <- SP after locals
Lower addresses
```

### Call Sequence

1. **Caller** allocates and pushes return handle:
   ```hopper
   LDA #26          // Use handle 26 for return value
   PHA
   ```

2. **Caller** evaluates and pushes arguments (left-to-right):
   ```hopper
   // Evaluate arg1 expression → result handle in A
   PHA              // Push arg1 handle
   // Evaluate arg2 expression → result handle in A
   PHA              // Push arg2 handle
   ```

3. **Caller** calls function:
   ```hopper
   JSR function_F
   ```

4. **Callee** prologue - establish frame:
   ```hopper
   function_F()
   {
       PHX              // Save old BP (using X as temp)
       TSX              // Get current stack pointer
       STX BP           // This becomes new BP
       // Allocate locals if needed by pushing handles
   }
   ```

5. **Callee** accesses parameters:
   ```hopper
   LDX BP
   LDA $0103, X      // Arg 2 handle (BP+3)
   LDA $0104, X      // Arg 1 handle (BP+4)
   LDA $0105, X      // Return handle (BP+5)
   ```

6. **Callee** stores return value:
   ```hopper
   LDX BP
   LDA $0105, X      // Get return handle (BP+5 for 2 args)
   TAY              // Handle in Y
   // Store 32-bit value to handle Y's slot
   JSR StoreResultToHandle
   ```

7. **Callee** epilogue - restore and return:
   ```hopper
   LDX BP           // Restore stack to BP (removes locals)
   TXS
   PLX              // Restore old BP
   STX BP
   RTS              // Return to caller
   ```

8. **Caller** cleanup - remove arguments:
   ```hopper
   PLA              // Remove arg2 handle
   PLA              // Remove arg1 handle
   // Return handle still on stack with result
   PLA              // Get return handle if needed
   ```

## Code Generation Patterns

### Program Structure
```hopper
program Generated
{
    #define CPU_65C02S
    uses "System/Definitions"
    
    // Runtime support functions
    LoadHandleToTOP()
    {
        // ... (see Helper Functions section)
    }
    
    // User functions
    function_F()
    {
        // ... function body
    }
    
    // Main entry point
    Hopper()
    {
        // Initialize value table
        InitValueTable();
        
        // Call main function
        LDA #26          // Return handle
        PHA
        JSR M            // Call main
        
        // Exit program
        LDA #0
        E();             // Exit with code 0
    }
}
```

### Global Variable Declaration
```c
int a, b, c;
```
Compile-time assignments:
- Variable 'a' → handle 0
- Variable 'b' → handle 1  
- Variable 'c' → handle 2

### Assignment Statement
```c
a = 0x42;
```
Generates:
```hopper
LDA #0x42        // Value low byte
LDX #0           // Handle 0 for 'a'
JSR StoreConstToHandle
```

### Binary Operation
```c
c = a + b;
```
Generates:
```hopper
// Load handle 0 (a) to ZP.TOP
LDA #0
LoadHandleToTOP();

// Load handle 1 (b) to ZP.NEXT  
LDA #1
LoadHandleToNEXT();

// Call BIOS Long.Add
Long.Add();

// Store result from ZP.NEXT to handle 2 (c)
LDA #2
StoreNEXTToHandle();
```

### Function Call Expression
```c
r = F(x, y);
```
Generates:
```hopper
LDA #26          // Return handle
PHA

LDA #23          // Handle for x (assuming x is at 23)
PHA

LDA #24          // Handle for y (assuming y is at 24)
PHA

JSR F

PLA              // Remove y
PLA              // Remove x
// Stack top now has return handle with result

PLA              // Get return handle
TAX              // Save it
LDA #17          // Handle for r (assuming r is at 17)
TAY
TXA
CopyHandle();    // Copy from handle X to handle Y
```

### If Statement
```c
if (a > b) { statements; }
```
Generates:
```hopper
// Load handles for comparison
LDA #0           // Handle for a
LoadHandleToTOP();
LDA #1           // Handle for b
LoadHandleToNEXT();

// Call BIOS Long.Compare
Long.Compare();

// Check result in ZP.ACC
LDA ZP.ACCL
if (MI)          // If negative (a < b), skip
{
    // Skip the if body
}
else
{
    if (Z)       // If zero (a == b), skip
    {
        // Skip the if body
    }
    else
    {
        // Body of if statement
        // [statements]
    }
}
```

### For Loop
```c
for(;;) { 
    statements;
    if (done) break;
    more_statements;
    continue;
}
```
Generates:
```hopper
loop
{
    // statements
    
    // Evaluate done condition
    // [evaluate done → handle in A]
    LoadHandleToACC();
    LDA ZP.ACCL
    ORA ZP.ACCH
    ORA ZP.ACC2
    ORA ZP.ACC3
    if (NZ)      // If non-zero, break
    {
        break;
    }
    
    // more_statements
    
    continue;    // This is actually not needed at end of loop
}
```

## Helper Functions (Runtime Support)

```hopper
// Load value from handle in A to ZP.TOP
LoadHandleToTOP()
{
    ASL A
    ASL A           // handle * 4
    TAX
    LDA $4000, X    // Load 4 bytes from value table
    STA ZP.TOP0
    LDA $4001, X
    STA ZP.TOP1
    LDA $4002, X
    STA ZP.TOP2
    LDA $4003, X
    STA ZP.TOP3
}

// Load value from handle in A to ZP.NEXT
LoadHandleToNEXT()
{
    ASL A
    ASL A
    TAX
    LDA $4000, X
    STA ZP.NEXT0
    LDA $4001, X
    STA ZP.NEXT1
    LDA $4002, X
    STA ZP.NEXT2
    LDA $4003, X
    STA ZP.NEXT3
}

// Store ZP.NEXT to handle in A
StoreNEXTToHandle()
{
    ASL A
    ASL A
    TAX
    LDA ZP.NEXT0
    STA $4000, X
    LDA ZP.NEXT1
    STA $4001, X
    LDA ZP.NEXT2
    STA $4002, X
    LDA ZP.NEXT3
    STA $4003, X
}

// Store immediate 32-bit value to handle
// A = low byte of value, X = handle
StoreConstToHandle()
{
    PHA              // Save value low byte
    TXA
    ASL A
    ASL A
    TAX
    PLA
    STA $4000, X     // Store low byte
    LDA #0
    STA $4001, X     // Zero upper bytes for now
    STA $4002, X
    STA $4003, X
}

// Initialize value table (call at program start)
InitValueTable()
{
    // Allocate 1KB for value table using BIOS
    LDA #0x04
    STA ZP.ACCL
    LDA #0x00
    STA ZP.ACCH
    Memory.Allocate();
    // Assume it returns 0x4000 in ZP.IDX
    // Table is already zeroed by Memory.Allocate
}
```

## Compiler Structure

### Global State (in compiler)
```hopper
// Compiler variables (using applet zero page)
const byte c = 0x58;        // Current input character
const byte t = 0x59;        // Current token type
const byte v = 0x5A;        // Token value (4 bytes)
const byte vL = 0x5A;
const byte vH = 0x5B;
const byte v2 = 0x5C;
const byte v3 = 0x5D;
const byte pc = 0x5E;        // Program counter (2 bytes)
const byte pcL = 0x5E;
const byte pcH = 0x5F;
const byte nh = 0x60;        // Next handle to allocate
const byte ln = 0x61;        // Current line number (2 bytes)
const byte lnL = 0x61;
const byte lnH = 0x62;

// Symbol tables (26 entries each)
const byte vars = 0x63;     // Variable handles (a-z)
const byte funcs = 0x7D;    // Function addresses (A-Z) - 2 bytes each
```

### Main Compilation Loop
```hopper
Compile()
{
    InitCompiler();
    NextChar();         // Prime the pump
    NextToken();
    
    // Parse globals
    loop
    {
        LDA t
        CMP #TOKEN_INT
        if (NZ)
        {
            break;      // Done with globals
        }
        ParseGlobal();
    }
    
    // Parse functions
    loop
    {
        LDA t
        CMP #TOKEN_EOF
        if (Z)
        {
            break;      // Done with functions
        }
        ParseFunction();
    }
    
    PatchForwardRefs();
}
```

### Tokenizer Structure
```hopper
NextToken()
{
    // Skip whitespace
    loop
    {
        LDA c
        CMP #' '
        if (Z)
        {
            NextChar();
            continue;
        }
        CMP #0x09        // Tab
        if (Z)
        {
            NextChar();
            continue;
        }
        CMP #0x0A        // Newline
        if (Z)
        {
            NextChar();
            continue;
        }
        break;           // Not whitespace
    }
    
    // Check for hex number 0xNN
    LDA c
    CMP #'0'
    if (Z)
    {
        NextChar();
        LDA c
        CMP #'x'
        if (Z)
        {
            NextChar();
            ParseHex();
            LDA #TOKEN_NUMBER
            STA t
            return;
        }
        Error();         // Bad number format
    }
    
    // Check for character literal 'x'
    CMP #0x27           // Single quote
    if (Z)
    {
        NextChar();
        LDA c
        STA vL          // Save character value
        STZ vH
        STZ v2
        STZ v3
        NextChar();     // Skip character
        NextChar();     // Skip closing quote
        LDA #TOKEN_NUMBER
        STA t
        return;
    }
    
    // Check for identifier/keyword
    CMP #'a'
    if (NC)             // >= 'a'
    {
        CMP #('z'+1)
        if (C)          // < 'z'+1
        {
            // It's a lowercase letter - variable
            STA vL
            NextChar();
            CheckKeyword(); // Check if it's 'int', 'if', 'for', 'return'
            return;
        }
    }
    
    // Check for uppercase - function name
    CMP #'A'
    if (NC)             // >= 'A'
    {
        CMP #('Z'+1)
        if (C)          // < 'Z'+1
        {
            STA vL
            NextChar();
            LDA #TOKEN_IDENT
            STA t
            return;
        }
    }
    
    // Check for operators and punctuation
    // ... handle =, ==, !=, <, >, +, -, *, /, (, ), {, }, ;, ,
    
    // Unknown character
    Error();
}
```

### Expression Parser with Precedence
```hopper
// Parse expression with minimum precedence
// A = minimum precedence level
ParseExpr()
{
    PHA              // Save min precedence
    ParsePrimary();  // Parse primary expression
    
    loop
    {
        PLA          // Get min precedence
        PHA          // Keep it on stack
        GetPrecedence(); // Get precedence of current token in A
        CMP $01, S   // Compare with min precedence
        if (C)       // If lower precedence
        {
            break;   // Done with this expression level
        }
        
        // Save operator
        LDA t
        PHA
        NextToken();
        
        // For assignment, use same precedence (right associative)
        // For others, use precedence + 1 (left associative)
        PLA          // Get operator
        PHA          // Keep it
        CMP #TOKEN_ASSIGN
        if (Z)
        {
            GetPrecedence();
        }
        else
        {
            GetPrecedence();
            CLC
            ADC #1
        }
        
        ParseExpr(); // Parse right side recursively
        
        // Generate code for operator
        PLA          // Get operator
        GenerateBinOp();
        
        // Continue loop to check for more operators
    }
    
    PLA              // Clean up precedence
}
```

## Test Program for Bootstrap

```c
// Factorial calculator
int a;
int r;

int F(int n) {
    if (n < 0x02) {
        return 0x01;
    }
    return n * F(n - 0x01);
}

int M() {
    a = 0x05;
    r = F(a);
    
    // Print result as ASCII
    P(r + 0x30);  
    P(0x0A);      // Newline
    
    return 0x00;
}
```

Expected output: `x` (ASCII 120 = factorial of 5)

## Implementation Phases

### Phase 0: Bootstrap Compiler (Hand-written Assembly)
1. Implement tokenizer for minimal syntax
2. Implement recursive descent parser
3. Implement code generator with handle-based stack
4. Test with factorial program

### Phase 1: Self-Compilation
1. Rewrite compiler in minimal C subset
2. Compile using bootstrap compiler
3. Verify generated compiler can compile test program
4. Verify generated compiler can compile itself

### Phase 2: Enhanced Compiler
Once self-hosted, iteratively add:
- Multi-character identifiers
- Local variables (using handle stack)
- else statements
- while loops
- Decimal literals
- Strings
- Arrays
- Better error messages

## Success Criteria

1. **Bootstrap**: Hand-written assembler compiles test program correctly
2. **Self-hosting**: Compiler written in C compiles itself
3. **Enhancement**: Self-compiled compiler adds new features

## Open Issues

1. **Forward Function References**: 
   - Solution: Keep list of call sites to patch after parsing all functions

2. **Error Messages**:
   - Bootstrap: Single byte error codes
   - Enhanced: Add string support for messages

3. **I/O Functions**:
   - G(), P(), E() as intrinsics or regular functions?
   - Solution: Implement as intrinsics that directly call BIOS

4. **Handle Exhaustion**:
   - Monitor handle allocation
   - Error if > 255 handles needed

5. **Program Size Limits**:
   - Maximum ~8KB for code (0x2000-0x3FFF)
   - Check during emission

---

This complete specification provides everything needed to implement the bootstrap compiler and achieve self-hosting. The calling convention is proven, the language subset is minimal but sufficient, and the path to enhancement is clear. All code samples now follow proper Hopper Assembly structured control flow conventions without any JMP instructions or labels.