# PL/H Language Specification
## Minimal Bootstrap Language for Hopper 6502 VM

### Version 1.2

---

## 1. Introduction

### 1.1 Purpose
PL/H (Programming Language for Hopper) is a minimal systems programming language designed specifically for bootstrapping a self-hosting compiler on the Hopper 6502 VM. The language prioritizes simplicity of implementation over features, requiring approximately 500-800 lines of PL/H code to implement its own compiler, or 2000-3000 lines of VM assembly for the initial bootstrap.

### 1.2 Design Goals
- **Minimal keyword count** (8 total)
- **Single-character tokens** where possible
- **Direct mapping** to stack machine operations
- **Simplified parsing** (hex literals only, parenthesized conditionals)
- **Minimal local variables** for recursion support
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
- Hex digits: `0-9`, `a-f`, `A-F`
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
digit = "0".."9"
```

### 2.4 Literals

#### 2.4.1 Numeric Literals (Hexadecimal Only)
```ebnf
number = "0x" hex_digit { hex_digit }
hex_digit = "0".."9" | "a".."f" | "A".."F"
```
- Hexadecimal only in bootstrap version
- Range: 0x00-0xFFFF (0-65535)
- Examples: `0xFF`, `0x0A`, `0x1234`

#### 2.4.2 Character Literals
```ebnf
char_literal = "'" printable_char "'"
```
- Single ASCII character
- Examples: `'A'`, `'0'`, `' '`, `'\n'`

#### 2.4.3 String Literals
```ebnf
string_literal = '"' { printable_char } '"'
```
- Used for constants and print statements
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
- **byte**: 8-bit value (0x00-0xFF)
- **word**: 16-bit value (0x0000-0xFFFF)
- **string**: Immutable string literals only
- **arrays**: Fixed-size byte arrays

Type is inferred from context:
- Array elements are bytes
- Numeric literals ≤0xFF are bytes
- Numeric literals >0xFF are words
- Function returns are unchecked

### 3.2 Global Variable Declarations
```ebnf
var_decl = "var" ident [ "[" number "]" ] ";"
```

Global variables are allocated in the 256-byte global page:
```c
var ch;             // Global byte/word variable
var buffer[0x20];   // Global 32-byte array
var count;          // Global counter
```

### 3.3 Local Variable Declarations
Local variables are declared within procedures and allocated on the stack:
```c
proc scan_number {
    var digit;      // Local byte variable
    var value;      // Local word variable
    var temp[0x08]; // Local 8-byte array
    
    // Procedure body
}
```

### 3.4 Constant Declarations
```ebnf
const_decl = "const" ident "=" ( number | char_literal | string_literal ) ";"
```

Examples:
```c
const EOF = 0xFF;           // -1 as unsigned byte
const SPACE = 0x20;         // Space character
const NEWLINE = 0x0A;       // Line feed
const ERROR_MSG = "Syntax error\n";
const OP_PUSHB = 0x04;      // VM opcode
```

### 3.5 Scope Rules
- **Constants**: Global scope only
- **Global variables**: Accessible from all procedures
- **Local variables**: Accessible only within declaring procedure
- **Shadowing**: Local variables shadow globals with same name
- No nested procedures (flat namespace)

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
     | "print" "(" expr | string_literal ")"
     | "in" "(" ")"
     | "out" "(" ")"
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
- `=` Assignment (statement, not expression)

### 4.3 Operator Precedence
1. Primary (literals, identifiers, calls, parentheses)
2. Comparison operators
3. Addition and subtraction

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

conditional = "(" expr ")" "?" statement [ ":" statement ]

loop_stmt = "loop" block

block = "{" { statement } "}"
```

### 5.2 Assignment Statement
```c
x = 0x0A;           // Assign to variable
buffer[i] = ch;     // Array element assignment
```

### 5.3 Conditional Statement
Parenthesized condition with optional else:
```c
(x == 0x05) ? emit(0x0A);              // No else clause
(x == 0x05) ? emit(0x0A) : emit(0x14); // With else clause

// With blocks
(x > 0x0A) ? {
    print("Greater\n");
    count = count + 0x01;
} : {
    print("Less or equal\n");
}

// Chained conditionals
(x == 0x01) ? handle_one() :
(x == 0x02) ? handle_two() :
(x == 0x03) ? handle_three() :
              handle_default();
```

### 5.4 Loop Statement
Single loop construct with explicit conditions:
```c
loop {
    ch = getc();
    (ch == 0xFF) ? break;      // EOF
    (ch == 0x20) ? continue;    // Skip spaces
    emit(ch);
}
```

### 5.5 Control Flow
- `break` - Exit innermost loop
- `continue` - Next iteration of innermost loop
- `return` - Exit procedure with optional value

---

## 6. Procedures

### 6.1 Declaration
```ebnf
proc_decl = "proc" ident "{" { local_var_decl } { statement } "}"

local_var_decl = "var" ident [ "[" number "]" ] ";"
```

### 6.2 Local Variables
- Allocated on stack frame
- Not initialized (contain garbage)
- Scope limited to procedure
- Shadow global variables with same name

### 6.3 Restrictions
- No parameters (use globals for communication)
- No nested procedures
- No forward declarations (define before use)
- Maximum size: 256 bytes of compiled code
- Stack usage must fit within ~256 byte limit

### 6.4 Example
```c
proc skip_whitespace {
    var ch;
    
    loop {
        ch = getc();
        (ch != 0x20) ? {            // Not space
            (ch != 0x09) ? {        // Not tab
                (ch != 0x0A) ? {    // Not newline
                    pushback = ch;   // Save for later
                    break;
                }
            }
        }
    }
}
```

---

## 7. Built-in Functions

### 7.1 File Argument Functions

#### 7.1.1 `in()`
- Returns: Input filename as string, or "" if no input file specified
- Use: Check if input file was provided

#### 7.1.2 `out()`
- Returns: Output filename as string, or "" if no output file specified
- Use: Check if output file was provided

### 7.2 I/O Functions

#### 7.2.1 `getc()`
- Returns: Next byte from input file (0x00-0xFF)
- Returns: -1 (0xFFFFFFFF) if no input file or EOF
- Input file specified by command line argument

#### 7.2.2 `emit(byte)`
- Parameters: Byte value to output
- Effect: Writes byte to output file
- Effect: Does nothing if no output file specified

#### 7.2.3 `print(value)`
- Parameters: Integer expression, character, or string literal
- Effect: Prints to console (stderr) for debugging
- Behavior:
  - Integer: Printed as decimal number
  - Character: Printed as ASCII character
  - String: Printed as-is

### 7.3 Usage Examples
```c
// Check for required files
main {
    (in() == "") ? {
        print("Error: No input file specified\n");
        return 0x01;
    }
    
    (out() == "") ? {
        print("Error: No output file specified\n");
        return 0x01;
    }
    
    // Safe to proceed
    compile_file();
    return 0x00;
}

// Polymorphic print examples
print(0x41);        // Prints: 65 (decimal)
print('A');         // Prints: A
print("Hello\n");   // Prints: Hello (with newline)

// Safe reading
proc read_char {
    var ch;
    ch = getc();
    (ch == -0x01) ? return 0xFF;  // Convert to byte EOF
    return ch;
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
1. Parse command line arguments
2. If files specified, open them
3. Global variables initialized to zero
4. Execution begins at `main`
5. Return value from main is program exit code (0 = success)
6. Files automatically closed on exit

### 8.3 Complete Example: Hex Tokenizer
```c
// Minimal hex number tokenizer
const EOF = 0xFF;
const TK_NUMBER = 0x01;
const TK_IDENT = 0x02;
const TK_ERROR = 0xFF;

var token;
var value;
var ch;

proc is_hex_digit {
    (ch >= '0') ? {
        (ch <= '9') ? return 0x01;
    }
    (ch >= 'a') ? {
        (ch <= 'f') ? return 0x01;
    }
    (ch >= 'A') ? {
        (ch <= 'F') ? return 0x01;
    }
    return 0x00;
}

proc hex_digit_value {
    (ch >= '0') ? {
        (ch <= '9') ? return ch - '0';
    }
    (ch >= 'a') ? {
        (ch <= 'f') ? return ch - 'a' + 0x0A;
    }
    return ch - 'A' + 0x0A;
}

proc scan_hex {
    var digit;
    
    value = 0x00;
    loop {
        (is_hex_digit() == 0x00) ? break;
        digit = hex_digit_value();
        // Note: Need shift operator in real implementation
        // value = (value << 4) | digit;
        value = value * 0x10 + digit;  // Temporary workaround
        ch = getc();
    }
    token = TK_NUMBER;
}

proc next_token {
    // Skip whitespace
    loop {
        (ch == 0x20) ? { ch = getc(); continue; }  // Space
        (ch == 0x0A) ? { ch = getc(); continue; }  // Newline
        (ch == 0x09) ? { ch = getc(); continue; }  // Tab
        break;
    }
    
    // Check for hex number
    (ch == '0') ? {
        ch = getc();
        (ch == 'x') ? {
            ch = getc();
            scan_hex();
            return;
        }
        print("Error: Only hex literals (0x...) supported\n");
        token = TK_ERROR;
        return;
    }
    
    // Check EOF
    (ch == -0x01) ? {
        token = EOF;
        return;
    }
    
    // Single character token
    token = ch;
    ch = getc();
}

main {
    // Validate arguments
    (in() == "") ? {
        print("Usage: tokenizer input.plh output.tok\n");
        return 0x01;
    }
    
    // Initialize
    ch = getc();
    (ch == -0x01) ? {
        print("Error: Cannot read input file\n");
        return 0x01;
    }
    
    // Process tokens
    loop {
        next_token();
        (token == EOF) ? break;
        
        (token == TK_NUMBER) ? {
            (out() != "") ? {
                emit(TK_NUMBER);
                emit(value);  // Emit value bytes
            }
            continue;
        }
        
        print("Unknown token: ");
        print(token);
        print("\n");
    }
    
    return 0x00;
}
```

---

## 9. Code Generation

### 9.1 Stack Frame Instructions
```
ENTER n     (0x90)  - Allocate n bytes for locals
LEAVE       (0x92)  - Restore stack frame
PUSHLB      (0x64)  - Push local byte (BP+offset)
PUSHLW      (0x66)  - Push local word (BP+offset)
POPLB       (0x6A)  - Pop to local byte
POPLW       (0x6C)  - Pop to local word
```

### 9.2 Primary VM Opcodes
```
PUSHB  (0x04)   - Push byte immediate
PUSHW  (0x0A)   - Push word immediate
PUSHGB (0x70)   - Push global byte
PUSHGW (0x72)   - Push global word
POPGB  (0x74)   - Pop to global byte
POPGW  (0x76)   - Pop to global word
ADDB   (0x24)   - Add bytes
ADDW   (0x2A)   - Add words
SUBB   (0x26)   - Subtract bytes
SUBW   (0x2C)   - Subtract words
EQB    (0x34)   - Compare equal (bytes)
EQW    (0x3C)   - Compare equal (words)
NEB    (0x36)   - Compare not equal (bytes)
NEW    (0x3E)   - Compare not equal (words)
LTB    (0x38)   - Less than (bytes)
LTW    (0x40)   - Less than (words)
LEB    (0x3A)   - Less or equal (bytes)
LEW    (0x42)   - Less or equal (words)
BZF    (0x80)   - Branch forward if Z clear
BZR    (0x82)   - Branch backward if Z clear
BNZF   (0x84)   - Branch forward if Z set
BNZR   (0x86)   - Branch backward if Z set
BRAF   (0x7C)   - Branch relative forward
BRAR   (0x7E)   - Branch relative backward
CALL   (0x88)   - Call procedure
RET    (0x8A)   - Return from procedure
HALT   (0x02)   - End program
```

### 9.3 Code Generation Patterns

#### Parenthesized Conditional
```c
(x == 0x05) ? emit(0x0A) : emit(0x14);
```
Generates:
```asm
PUSHGB x_offset
PUSHB 0x05
EQB
BZF else_label      ; Branch if not equal
PUSHB 0x0A
CALL emit
BRAF end_label
else_label:
PUSHB 0x14
CALL emit
end_label:
```

#### Loop with Break
```c
loop {
    ch = getc();
    (ch == 0xFF) ? break;
    emit(ch);
}
```
Generates:
```asm
loop_start:
CALL getc
POPGB ch_offset
PUSHGB ch_offset
PUSHB 0xFF
EQB
BNZF loop_end      ; Branch if equal
PUSHGB ch_offset
CALL emit
BRAR loop_start
loop_end:
```

---

## 10. Memory Model

### 10.1 Memory Layout
```
Global Page (256 bytes):
  0x00-0xFF:  Global variables and arrays

Stack (256 bytes):
  - Function return addresses (3 bytes each)
  - Saved base pointers (1 byte each)
  - Local variables
  - Expression evaluation stack
  
Code Pages:
  - One 256-byte page per procedure
  - Main procedure gets its own page
```

### 10.2 Stack Frame Layout
```
High Memory
    [Saved BP]      <- BP+1
    [Local 0]       <- BP+0 (first local)
    [Local 1]       <- BP-1
    [Local 2]       <- BP-2
    ...
Low Memory
```

### 10.3 Stack Usage
- Each call: 4 bytes overhead (3 return address, 1 saved BP)
- Plus local variables
- Plus expression evaluation
- Maximum practical depth: 8-10 nested calls

---

## 11. Symbol Table

### 11.1 Structure
The compiler maintains symbols for:
- Global variables with offsets (0x00-0xFF)
- Local variables with stack offsets (BP+0, BP-1, etc.)
- Constants with values
- Procedures with code addresses
- Arrays with sizes

### 11.2 Resolution Order
1. Search local variables of current procedure
2. Search global variables
3. Search constants
4. Search procedures
5. Report undefined error

---

## 12. Limitations and Constraints

### 12.1 Bootstrap Version Limitations
- **Hex literals only** (no decimal in bootstrap)
- **No multiplication/division** (shifts can substitute)
- **No bitwise operators** (except in extensions)
- **No string variables** (only literals)
- **No type checking** (bytes and words intermixed)

### 12.2 Memory Constraints
- Global variables: 256 bytes maximum
- Stack: ~256 bytes total (shared)
- Procedure size: 256 bytes of code maximum
- No dynamic allocation

### 12.3 Language Constraints
- No procedure parameters
- No nested procedures
- No forward declarations
- Limited recursion depth
- No floating point
- No pointers
- No structures

---

## 13. Error Handling

### 13.1 Compile-Time Errors
```c
proc error {
    print("Error at line ");
    print(line_number);
    print(": ");
    print(error_message);
    print("\n");
    error_count = error_count + 0x01;
    (error_count > 0x0A) ? {
        print("Too many errors, aborting\n");
        halt();
    }
}
```

### 13.2 Runtime Behavior
- No bounds checking
- Stack overflow unchecked
- Invalid memory access undefined
- Division by zero not applicable

---

## 14. Complete Grammar

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

conditional = "(" expr ")" "?" statement [ ":" statement ]

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
     | "print" "(" expr | string_literal ")"
     | "in" "(" ")"
     | "out" "(" ")"
     | ident "(" ")"

number = "0x" hex_digit { hex_digit }

hex_digit = "0".."9" | "a".."f" | "A".."F"

char_literal = "'" printable_char "'"

string_literal = '"' { printable_char } '"'

ident = letter { letter | digit }

letter = "a".."z" | "A".."Z" | "_"

digit = "0".."9"

comment = "//" { any_char } newline
```

---

## 15. Bootstrap Plan

### 15.1 Stage 0: VM Assembly Bootstrap
- Write PL/H compiler in VM assembly (~2500-3000 lines)
- Supports hex literals only
- Minimal error checking
- Direct code emission

### 15.2 Stage 1: Self-Hosting
- Rewrite compiler in PL/H (~600-800 lines)
- Same features as Stage 0
- Cleaner implementation with locals

### 15.3 Stage 2: Enhanced Compiler
Once self-hosting, add:
- Decimal literals
- Shift operators (`<<`, `>>`)
- Multiplication/division
- Procedure parameters
- Better error messages

---

## Appendix A: Reserved Words

```
const, var, proc, main, loop, break, continue, return,
getc, emit, print, in, out
```

## Appendix B: ASCII Values (Hex)

```
'\n' = 0x0A    ' '  = 0x20    '0' = 0x30    'A' = 0x41    'a' = 0x61
'\t' = 0x09    '!'  = 0x21    '9' = 0x39    'Z' = 0x5A    'z' = 0x7A
'\r' = 0x0D    '"'  = 0x22    ':' = 0x3A    '[' = 0x5B    '{' = 0x7B
EOF  = 0xFF    '#'  = 0x23    ';' = 0x3B    ']' = 0x5D    '}' = 0x7D
               '\'' = 0x27    '<' = 0x3C    '_' = 0x5F    
               '('  = 0x28    '=' = 0x3D
               ')'  = 0x29    '>' = 0x3E
               '+'  = 0x2B    '?' = 0x3F
               '-'  = 0x2D    
               '/'  = 0x2F
```

## Appendix C: VM Opcode Quick Reference

```
0x02  HALT        0x24  ADDB        0x3C  EQW         0x70  PUSHGB
0x04  PUSHB       0x26  SUBB        0x3E  NEW         0x72  PUSHGW
0x06  PUSHB0      0x2A  ADDW        0x40  LTW         0x74  POPGB
0x08  PUSHB1      0x2C  SUBW        0x42  LEW         0x76  POPGW
0x0A  PUSHW       0x34  EQB         0x64  PUSHLB      0x7C  BRAF
0x0C  PUSHW0      0x36  NEB         0x66  PUSHLW      0x7E  BRAR
0x0E  PUSHW1      0x38  LTB         0x6A  POPLB       0x80  BZF
0x1A  DROPB       0x3A  LEB         0x6C  POPLW       0x82  BZR
0x1C  DROPW                                           0x84  BNZF
0x90  ENTER                                           0x86  BNZR
0x92  LEAVE                                           0x88  CALL
0x8A  RET                                            0x8C  SYSCALL
```