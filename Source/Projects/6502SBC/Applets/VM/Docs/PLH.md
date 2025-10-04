# PL/H Language Specification
## Minimal Bootstrap Language for Hopper 6502 VM

### Version 1.1

---

## 1. Introduction

### 1.1 Purpose
PL/H (Programming Language for Hopper) is a minimal systems programming language designed specifically for bootstrapping a self-hosting compiler on the Hopper 6502 VM. The language prioritizes simplicity of implementation over features, requiring approximately 500-800 lines of PL/H code to implement its own compiler, or 2000-3000 lines of VM assembly for the initial bootstrap.

### 1.2 Design Goals
- **Minimal keyword count** (8 total)
- **Single-character tokens** where possible  
- **Direct mapping** to stack machine operations
- **Minimal local variables** for recursion support
- **No complex features** (no multiplication, no nested procedures, no parameters)
- **Self-hosting capability** with practical compiler implementation

### 1.3 Target Architecture
The Hopper 6502 VM with:
- Stack-based computation model
- 256-byte function size limit
- 256-byte global variable space
- 256-byte hardware stack (shared between calls and locals)
- Direct BIOS system calls

---

## 2. Lexical Structure

### 2.1 Character Set
- Letters: `a-z`, `A-Z`, `_`
- Digits: `0-9`
- Operators: `+` `-` `=` `<` `>` `!` `?` `:`
- Delimiters: `{` `}` `(` `)` `[` `]` `;`
- Special: `'` `"` `/`

### 2.2 Keywords (8 total)
```
const       - Constant declaration
var         - Variable declaration
proc        - Procedure declaration
main        - Program entry point
loop        - Loop construct
break       - Exit loop
continue    - Next iteration
return      - Exit procedure
```

### 2.3 Identifiers
```ebnf
ident = letter { letter | digit }
letter = "a".."z" | "A".."Z" | "_"
```

### 2.4 Literals

#### 2.4.1 Numeric Literals
```ebnf
number = digit { digit }
digit = "0".."9"
```
- Decimal only
- Range: 0-65535 (word) or 0-255 (byte context)

#### 2.4.2 Character Literals
```ebnf
char_literal = "'" printable_char "'"
```
- Single ASCII character
- Examples: `'A'`, `'0'`, `' '`

#### 2.4.3 String Literals
```ebnf
string_literal = '"' { printable_char } '"'
```
- Used only for constants and error messages
- No escape sequences in bootstrap version

### 2.5 Comments
```ebnf
comment = "//" { any_char } newline
```
- Single-line comments only
- Everything after `//` until end of line is ignored

---

## 3. Types and Variables

### 3.1 Data Types
- **byte**: 8-bit unsigned (0-255)
- **word**: 16-bit unsigned (0-65535)
- **arrays**: Fixed-size byte arrays only

Type is inferred from context:
- Array elements are bytes
- Numeric literals ≤255 are bytes
- Numeric literals >255 are words

### 3.2 Global Variable Declarations
```ebnf
var_decl = "var" ident [ "[" number "]" ] ";"
```

Global variables are allocated in the 256-byte global page:
```c
var ch;             // Global byte/word variable
var buffer[32];     // Global 32-byte array  
var count;          // Global counter
```

### 3.3 Local Variable Declarations
Local variables are declared within procedures and allocated on the stack:
```c
proc scan_number {
    var digit;      // Local byte variable
    var value;      // Local word variable
    var temp[8];    // Local 8-byte array
    
    // Procedure body
}
```

### 3.4 Constant Declarations
```ebnf
const_decl = "const" ident "=" ( number | char_literal | string_literal ) ";"
```

Examples:
```c
const EOF = 255;
const NEWLINE = 10;
const ERROR_MSG = "Syntax error\n";
```

### 3.5 Scope Rules
- **Constants**: Global scope only
- **Global variables**: Accessible from all procedures
- **Local variables**: Accessible only within declaring procedure
- **Shadowing**: Local variables shadow globals with same name
- No nested procedures (flat namespace)

### 3.6 Stack Frame Layout
```
High Memory
    [Local N]       <- BP-N
    ...
    [Local 2]       <- BP-2  
    [Local 1]       <- BP-1
    [Local 0]       <- BP+0 (first local)
    [Saved BP]      <- BP+1
    [Return Address (3 bytes)]
    [Caller's locals...]
Low Memory
```

---

## 4. Expressions

### 4.1 Grammar
```ebnf
expr = term { ("+" | "-") term }

term = factor { compare_op factor }

compare_op = "==" | "!=" | "<" | "<=" | ">" | ">="

factor = ident
       | ident "[" expr "]"
       | number
       | char_literal
       | "(" expr ")"
       | call

call = "getc" "(" ")"
     | "emit" "(" expr ")"
     | "print" "(" string_literal ")"
     | ident "(" ")"
```

### 4.2 Operators

#### 4.2.1 Arithmetic
- `+` Addition
- `-` Subtraction
- No multiplication or division in bootstrap

#### 4.2.2 Comparison
- `==` Equal
- `!=` Not equal
- `<` Less than
- `<=` Less than or equal
- `>` Greater than
- `>=` Greater than or equal

#### 4.2.3 Assignment
- `=` Assignment (not an expression)

### 4.3 Operator Precedence
1. Primary (literals, identifiers, calls)
2. Comparison operators  
3. Addition and subtraction

### 4.4 Variable Resolution
1. Check local variables first
2. Check global variables
3. Report undefined error

---

## 5. Statements

### 5.1 Grammar
```ebnf
statement = assignment
          | conditional
          | loop_stmt
          | "break" ";"
          | "continue" ";"
          | "return" [ expr ] ";"
          | expr ";"

assignment = ident [ "[" expr "]" ] "=" expr ";"

conditional = expr "?" statement [ ":" statement ]

loop_stmt = "loop" block

block = "{" { statement } "}"
```

### 5.2 Assignment Statement
```c
x = 10;             // Assign to local or global
buffer[i] = ch;     // Array element assignment
```

### 5.3 Conditional Statement
Statement-based ternary operator:
```c
x == 5 ? emit(10);              // No else clause
x == 5 ? emit(10) : emit(20);   // With else clause

// With blocks
x == 5 ? {
    emit(10);
    count = count + 1;
} : {
    emit(20);
}
```

### 5.4 Loop Statement
Single loop construct with explicit break conditions:
```c
loop {
    ch = getc();
    ch == EOF ? break;
    ch == ' ' ? continue;
    emit(ch);
}
```

### 5.5 Control Flow
- `break` - Exit innermost loop
- `continue` - Next iteration of innermost loop
- `return` - Exit procedure (optional expression)

---

## 6. Procedures

### 6.1 Declaration
```ebnf
proc_decl = "proc" ident "{" { local_var_decl } { statement } "}"

local_var_decl = "var" ident [ "[" number "]" ] ";"
```

### 6.2 Local Variables
- Allocated on stack frame
- Initialized to undefined values
- Scope limited to procedure
- Shadow global variables with same name

### 6.3 Restrictions
- No parameters (use globals for communication)
- No nested procedures
- No forward declarations (must be defined before use)
- Maximum size: 256 bytes of compiled code
- Stack usage must fit within ~256 byte hardware stack

### 6.4 Example
```c
proc parse_number {
    var digit;
    var value;
    
    value = 0;
    loop {
        digit = ch - '0';
        digit > 9 ? break;
        value = value * 10 + digit;  // Would need multiply
        ch = getc();
    }
    return value;
}
```

---

## 7. Built-in Functions

### 7.1 I/O Functions

#### 7.1.1 `getc()`
- Returns: Next byte from input file
- Returns 255 (EOF) at end of file
- Input file is command line argument 1

#### 7.1.2 `emit(byte)`
- Parameters: Byte value to output
- Effect: Writes byte to output file  
- Output file is command line argument 2

#### 7.1.3 `print(string)`
- Parameters: String literal
- Effect: Writes string to console (for errors/debugging)

### 7.2 Usage Example
```c
proc copy_file {
    var ch;
    
    loop {
        ch = getc();
        ch == 255 ? break;  // EOF
        emit(ch);
    }
}
```

---

## 8. Program Structure

### 8.1 Grammar
```ebnf
program = { global_declaration } { proc_decl } "main" block

global_declaration = const_decl | var_decl
```

### 8.2 Execution Model
1. Runtime opens input file (arg 1) and output file (arg 2)
2. Global variables initialized to zero
3. Execution begins at `main`
4. Return value from main is program exit code
5. Files automatically closed on exit

### 8.3 Complete Example: Expression Compiler
```c
// Minimal expression compiler with locals
const EOF = 255;
const TK_NUMBER = 1;
const TK_PLUS = 2;
const OP_PUSHB = 0x04;
const OP_ADDB = 0x24;

var token;          // Current token (global)
var value;          // Token value (global)  
var ch;             // Current character (global)

proc next_token {
    var start;
    
    // Skip whitespace
    loop {
        ch == ' ' ? { ch = getc(); continue; }
        ch == '\n' ? { ch = getc(); continue; }
        break;
    }
    
    // Check for number
    ch >= '0' ? {
        ch <= '9' ? {
            value = 0;
            loop {
                ch < '0' ? break;
                ch > '9' ? break;
                value = value * 10;  // Would need multiply
                value = value + (ch - '0');
                ch = getc();
            }
            token = TK_NUMBER;
            return;
        }
    }
    
    // Single character tokens
    ch == '+' ? {
        token = TK_PLUS;
        ch = getc();
        return;
    }
    
    token = ch;
    ch = getc();
}

proc compile_factor {
    var saved_value;
    
    token == TK_NUMBER ? {
        saved_value = value;
        emit(OP_PUSHB);
        emit(saved_value);
        next_token();
        return;
    }
    
    token == '(' ? {
        next_token();
        compile_expr();
        token != ')' ? {
            print("Expected )\n");
            return 1;
        }
        next_token();
    }
}

proc compile_expr {
    var op;
    
    compile_factor();
    loop {
        token != TK_PLUS ? break;
        op = token;
        next_token();
        compile_factor();
        emit(OP_ADDB);
    }
}

main {
    ch = getc();
    next_token();
    
    loop {
        token == EOF ? break;
        compile_expr();
    }
    
    return 0;
}
```

---

## 9. Code Generation

### 9.1 Stack Frame Instructions
Procedures with locals generate frame management:
```
ENTER n     (0x90)  - Allocate n bytes for locals
LEAVE       (0x92)  - Restore stack frame
PUSHLB      (0x64)  - Push local byte
PUSHLW      (0x66)  - Push local word  
POPLB       (0x6A)  - Pop to local byte
POPLW       (0x6C)  - Pop to local word
```

### 9.2 Target Instructions
Primary opcodes for code generation:
```
PUSHB (0x04)    - Push byte immediate
PUSHW (0x0A)    - Push word immediate
PUSHGB (0x70)   - Push global byte
PUSHGW (0x72)   - Push global word
POPGB (0x74)    - Pop to global byte
POPGW (0x76)    - Pop to global word
PUSHLB (0x64)   - Push local byte (BP+offset)
POPLB (0x6A)    - Pop to local byte (BP+offset)
ADDB (0x24)     - Add bytes
ADDW (0x2A)     - Add words
SUBB (0x26)     - Subtract bytes
SUBW (0x2C)     - Subtract words
EQB (0x34)      - Compare equal (bytes)
BZF (0x80)      - Branch if zero flag clear
BNZF (0x84)     - Branch if zero flag set
BRAR (0x7E)     - Branch relative backward
BRAF (0x7C)     - Branch relative forward
CALL (0x88)     - Call procedure
RET (0x8A)      - Return from procedure
HALT (0x02)     - End program
```

### 9.3 Code Patterns

#### Procedure with Locals
```c
proc example {
    var x;
    var y;
    x = 10;
    y = x + 5;
}
```
Generates:
```asm
example:
ENTER 2         ; 2 bytes for locals
PUSHB 10
POPLB 0         ; x is at BP+0
PUSHLB 0        ; Load x
PUSHB 5
ADDB
POPLB -1        ; y is at BP-1
LEAVE
RET
```

#### Local vs Global Access
```c
var global_x;

proc test {
    var local_x;
    
    local_x = 10;   // Uses POPLB
    global_x = 20;  // Uses POPGB
}
```

#### Loop with Local Counter
```c
proc count_chars {
    var count;
    var ch;
    
    count = 0;
    loop {
        ch = getc();
        ch == EOF ? break;
        count = count + 1;
    }
    return count;
}
```

---

## 10. Memory Model

### 10.1 Memory Layout
```
Global Page (256 bytes):
  0x00-0xFF:  Global variables

Stack (256 bytes):
  - Function return addresses (3 bytes each)
  - Saved base pointers (1 byte each)
  - Local variables
  - Expression evaluation stack
  
Heap: 
  - Not used in bootstrap version
```

### 10.2 Stack Usage
Each procedure call uses:
- 3 bytes: return address
- 1 byte: saved BP
- N bytes: local variables
- Variable: expression evaluation

Maximum practical call depth: 8-10 levels

### 10.3 Local Variable Offsets
```
First local:  BP+0
Second local: BP-1
Third local:  BP-2
Array local:  BP+0 to BP-(size-1)
```

---

## 11. Symbol Table

### 11.1 Structure
The compiler maintains a symbol table with:
```c
// Compiler's internal structure (conceptual)
struct symbol {
    name[16];       // Identifier name
    type;           // CONST, VAR, PROC, LOCAL
    scope;          // GLOBAL or procedure name
    offset;         // Memory offset or value
    size;           // For arrays
}
```

### 11.2 Resolution Order
1. Search local variables of current procedure
2. Search global variables
3. Search constants
4. Search procedures
5. Report undefined

---

## 12. Limitations and Constraints

### 12.1 Memory Constraints
- Global variables: 256 bytes total
- Stack depth: ~256 bytes (shared with calls and locals)
- Function size: 256 bytes maximum
- Local variables: Limited by stack space
- No dynamic memory allocation

### 12.2 Language Limitations
- No multiplication or division operators (in bootstrap)
- No floating point
- No strings (except literals)
- No structures or unions
- No pointers
- No procedure parameters
- Limited recursion (stack depth)
- No type declarations

### 12.3 Implementation Notes
- Two-pass compilation may be needed for forward references
- Symbol table can be fixed-size array
- No optimization required
- Direct code emission (no intermediate representation)

---

## 13. Error Handling

### 13.1 Compile-Time Errors
The compiler reports:
- Syntax errors with line numbers
- Undefined identifiers
- Duplicate definitions
- Stack overflow (too many locals)

### 13.2 Runtime Errors
No runtime error checking in bootstrap version:
- Stack overflow is not detected
- Array bounds not checked

### 13.3 Error Reporting Example
```c
proc error_undefined {
    var name_len;
    
    print("Undefined: ");
    // Print identifier name
    print("\n");
    return 1;
}

proc expect {
    var expected;
    
    expected = expected_token;  // From global
    token != expected ? {
        print("Expected token ");
        print_number(expected);
        print(" but got ");
        print_number(token);
        print("\n");
        error_count = error_count + 1;
    }
}
```

---

## 14. Bootstrap Plan

### 14.1 Stage 0: VM Assembly Implementation
Write initial PL/H compiler in VM assembly (~2500-3500 lines)
- Hand-coded recursive descent parser
- Manual stack frame management
- Fixed-size symbol table

### 14.2 Stage 1: Self-Hosting
Rewrite compiler in PL/H itself (~600-900 lines)
- Cleaner with local variables
- Natural recursion for parsing
- Same algorithms as assembly version

### 14.3 Stage 2: Extended Language
Add features once self-hosting:
- Multiplication/division operators
- Procedure parameters
- Larger local variable space
- Better string support
- Optimization passes

---

## Appendix A: Reserved Words

The following identifiers are reserved:
```
const, var, proc, main, loop, break, continue, return,
getc, emit, print
```

## Appendix B: ASCII Values

Common ASCII values for the bootstrap compiler:
```
'\n' = 10    ' ' = 32     '0' = 48     'A' = 65     'a' = 97
'\t' = 9     '!' = 33     '9' = 57     'Z' = 90     'z' = 122
EOF = 255    '=' = 61     '<' = 60     '>' = 62     '?' = 63
             '(' = 40     ')' = 41     '{' = 123    '}' = 125
             '[' = 91     ']' = 93     ';' = 59     ':' = 58
             '/' = 47     '+' = 43     '-' = 45     '"' = 34
             '\'' = 39
```

## Appendix C: Complete Grammar Summary

```ebnf
program = { global_declaration } { proc_decl } "main" block

global_declaration = const_decl | var_decl

const_decl = "const" ident "=" ( number | char_literal | string_literal ) ";"

var_decl = "var" ident [ "[" number "]" ] ";"

proc_decl = "proc" ident "{" { local_var_decl } { statement } "}"

local_var_decl = "var" ident [ "[" number "]" ] ";"

block = "{" { statement } "}"

statement = assignment
          | conditional
          | loop_stmt
          | "break" ";"
          | "continue" ";"
          | "return" [ expr ] ";"
          | expr ";"

assignment = ident [ "[" expr "]" ] "=" expr ";"

conditional = expr "?" statement [ ":" statement ]

loop_stmt = "loop" block

expr = term { ("+" | "-") term }

term = factor { compare_op factor }

compare_op = "==" | "!=" | "<" | "<=" | ">" | ">="

factor = ident
       | ident "[" expr "]"
       | number
       | char_literal
       | "(" expr ")"
       | call

call = "getc" "(" ")"
     | "emit" "(" expr ")"
     | "print" "(" string_literal ")"
     | ident "(" ")"
```