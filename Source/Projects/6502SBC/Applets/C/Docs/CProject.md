# C Compiler for Hopper BIOS - Project Specification v2.3

## Overview
A native C compiler that runs on 6502 under Hopper BIOS, capable of compiling C programs with pointer operations, file I/O, dynamic memory management, logical/bitwise operators, and global variables/constants to executable binaries.

## Language Specification

### Supported Types
- `void` - No value
- `char` - 8-bit value  
- `int` - 16-bit signed integer
- `long` - 32-bit signed integer
- `char*` - 16-bit pointer to char (strings and buffers)

### Type Conversion Rules
- **Implicit promotion**: Narrower types promote to wider in binary operations
- **Assignment truncation**: Wide to narrow assignments truncate without warning
- **Function arguments**: Promote to match parameter type
- **Return values**: Convert to declared return type

### Literals

#### Integer Literals
Integer literals can be specified in decimal or hexadecimal format:
- **Decimal**: `42`, `100`, `65535`
- **Hexadecimal**: `0x2A`, `0xFF`, `0x1000` (case-insensitive: `0X2a` also valid)

#### Character Literals
Single characters in single quotes: `'A'`, `'\n'`, `'\0'`

#### String Literals
Null-terminated strings in double quotes: `"Hello World\n"`

### Global Declarations

#### Global Constants
Global constants must be initialized with literal values only:
```c
const int MAX_SIZE = 100;
const char DELIMITER = ',';
const long BIG_NUMBER = 1000000;
const int HEX_VALUE = 0xFF;
```

**Special Case: const char* Initialization**

Constant string pointers can be initialized using either traditional string literals or byte array syntax:

```c
// Traditional string literal
const char* message = "Hello World!";

// Byte array initializer (new)
const char* data = { 'H', 'e', 'l', 'l', 'o', 
                     0x20,  // space character
                     'W', 'o', 'r', 'l', 'd', '!',
                     0x00   // null terminator
                   };

// Mixing character literals and hex values
const char* buffer = { 0x01, 0x02, 'A', 'B', 0x00 };
```

**Restrictions:**
- Must be initialized at declaration
- Initializer must be a literal value (no expressions)
- Array initializers for const char* can mix character literals and integer literals (decimal or hex)
- Stored in program memory as compile-time constants

#### Global Variables
Global variables are declared without initialization:
```c
int counter;
char* buffer;
long total;
```

**Restrictions:**
- Cannot be initialized at declaration
- Must be explicitly initialized in code before use
- Allocated in zero page (0x69-0xDF range)
- Maximum space for globals limited by zero page availability

### Operators

#### Arithmetic Operators
- Binary: `+`, `-`, `*`, `/`, `%`
- Unary: `-` (negation)

#### Logical Operators
- `&&` (logical AND with short-circuit evaluation)
- `||` (logical OR with short-circuit evaluation)

#### Bitwise Operators
- `&` (bitwise AND)
- `|` (bitwise OR)

#### Comparison Operators
- `<`, `>`, `<=`, `>=`, `==`, `!=`

#### Assignment Operators
- Simple assignment: `=`
- Compound assignment: `+=`, `-=`

#### Increment/Decrement Operators
- Postfix: `++`, `--` (e.g., `i++`, `count--`)

#### Pointer Operators
- Dereference: `*ptr` (read value at pointer)
- Address-of: `&var` (get address of variable) [planned]
- Array indexing: `buffer[n]` (equivalent to `*(buffer + n)`)

### Control Structures
- `if` / `else` statements
- `for` loops
- `while` loops
- `break` statement (exit from nearest enclosing loop)
- `continue` statement (skip to next iteration - while loops only)
- `return` statements

### Grammar (Extended)
```
<program> ::= <global-decl>* <function>+

<global-decl> ::= <const-decl> | <var-decl>

<const-decl> ::= "const" <type> <identifier> "=" <initializer> ";"

<initializer> ::= <literal> | <array-initializer>

<array-initializer> ::= "{" <array-element> ("," <array-element>)* ","? "}"

<array-element> ::= <char-literal> | <integer>

<var-decl> ::= <type> <identifier> ";"

<function> ::= <type> <identifier> "(" <param-list>? ")" <compound-stmt>

<statement> ::= <var-decl>
              | <expr-stmt>
              | <if-stmt>
              | <while-stmt>
              | <for-stmt>
              | <return-stmt>
              | <break-stmt>
              | <continue-stmt>
              | <compound-stmt>

<if-stmt> ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?

<while-stmt> ::= "while" "(" <expression> ")" <statement>

<for-stmt> ::= "for" "(" <expr-stmt> <expression> ";" <expression>? ")" <statement>

<break-stmt> ::= "break" ";"

<continue-stmt> ::= "continue" ";"

<expression> ::= <assignment>
<assignment> ::= <logical-or> (("=" | "+=" | "-=") <assignment>)?
<logical-or> ::= <logical-and> ("||" <logical-and>)*
<logical-and> ::= <bitwise-or> ("&&" <bitwise-or>)*
<bitwise-or> ::= <bitwise-and> ("|" <bitwise-and>)*
<bitwise-and> ::= <equality> ("&" <equality>)*
<equality> ::= <relational> (("==" | "!=") <relational>)*
<relational> ::= <additive> (("<" | ">" | "<=" | ">=") <additive>)*
<additive> ::= <multiplicative> (("+" | "-") <multiplicative>)*
<multiplicative> ::= <unary> (("*" | "/" | "%") <unary>)*
<unary> ::= ("*" | "-")? <postfix>
<postfix> ::= <primary> ("++" | "--" | "[" <expression> "]" | "(" <arg-list>? ")")*
<primary> ::= <identifier> | <literal> | "(" <expression> ")"
<literal> ::= <integer> | <string> | <char-literal>
<integer> ::= <decimal-integer> | <hex-integer>
<decimal-integer> ::= [0-9]+
<hex-integer> ::= "0" ("x" | "X") [0-9A-Fa-f]+
<char-literal> ::= "'" <character> "'"
<string> ::= '"' <character>* '"'
```

**Operator Precedence** (from highest to lowest):
1. Postfix: `++`, `--`, `[]`, `()`
2. Unary: `*`, `-`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-`
5. Relational: `<`, `>`, `<=`, `>=`
6. Equality: `==`, `!=`
7. Bitwise AND: `&`
8. Bitwise OR: `|`
9. Logical AND: `&&`
10. Logical OR: `||`
11. Assignment: `=`, `+=`, `-=`

## Built-in Functions

[Unchanged sections omitted for brevity]

## Example Programs

### Using Hex Literals and Array Initializers
```c
// Hex literals in various contexts
const int MASK = 0xFF;
const long ADDRESS = 0x8000;

// Array initializer for const char*
const char* protocol = { 0x01, 0x02, 0x03, 0x04, 0x00 };
const char* mixed = { 'C', 'M', 'D', ':', 0x20, 0x00 };

void test_hex() {
    int value = 0xABCD;
    char byte = 0xFF;
    
    printf("Value: 0x%04X\n", value);
    printf("Byte: 0x%02X\n", byte);
    
    // Using hex in comparisons
    if (byte == 0xFF) {
        printf("Byte is all ones\n");
    }
    
    // Hex in expressions
    int masked = value & 0xFF00;
    printf("Masked: 0x%04X\n", masked);
}

void test_array_init() {
    // Traditional string
    const char* str1 = "Hello";
    
    // Array initializer - equivalent
    const char* str2 = { 'H', 'e', 'l', 'l', 'o', 0x00 };
    
    // Binary protocol data
    const char* header = { 
        0xAA, 0x55,      // Magic number
        0x01, 0x00,      // Version 1.0
        0x10, 0x00,      // Length = 16
        'D', 'A', 'T', 'A',  // Tag
        0x00
    };
    
    printf("str1: %s\n", str1);
    printf("str2: %s\n", str2);
    printf("Header magic: 0x%02X%02X\n", header[0], header[1]);
}
```

### Using Logical and Bitwise Operators
```c
void test_operators() {
    int a = 5;      // 0101 in binary
    int b = 3;      // 0011 in binary
    
    // Bitwise operations
    printf("5 & 3 = %d\n", a & b);   // 1 (0001)
    printf("5 | 3 = %d\n", a | b);   // 7 (0111)
    
    // Logical operations with short-circuit
    if (a > 0 && b > 0) {
        printf("Both positive\n");
    }
    
    if (a == 0 || check_condition()) {
        // check_condition() only called if a != 0
        printf("At least one true\n");
    }
    
    // Mixed operations showing precedence
    int c = 2 & 3 | 4;    // (2&3)|4 = 2|4 = 6
    int d = 5 & 7 == 7;   // 5&(7==7) = 5&1 = 1
    printf("c=%d d=%d\n", c, d);
}
```

### Loop Control with Break and Continue
```c
void find_prime() {
    int n, i, is_prime;
    
    for (n = 2; n < 100; n++) {
        is_prime = 1;
        
        for (i = 2; i < n; i++) {
            if (n % i == 0) {
                is_prime = 0;
                break;  // Exit inner loop early
            }
        }
        
        if (is_prime) {
            printf("%d ", n);
        }
    }
    printf("\n");
}

void skip_vowels(char* text) {
    int i = 0;
    
    while (text[i]) {
        char c = text[i++];
        
        // Skip vowels
        if (c == 'a' || c == 'e' || c == 'i' || 
            c == 'o' || c == 'u') {
            continue;  // Skip to next iteration
        }
        
        putchar(c);
    }
    putchar('\n');
}
```

### Bit Manipulation Example
```c
void print_bits(int value) {
    int mask = 0x8000;  // Start with MSB
    int i;
    
    for (i = 0; i < 16; i++) {
        if (value & mask) {
            putchar('1');
        } else {
            putchar('0');
        }
        mask = mask >> 1;  // [Note: shift operators not yet implemented]
    }
    putchar('\n');
}

void set_flags() {
    int flags = 0;
    const int FLAG_A = 0x01;
    const int FLAG_B = 0x02;
    const int FLAG_C = 0x04;
    const int FLAG_D = 0x08;
    
    // Set flags using hex values
    flags = flags | FLAG_A;  // Set bit 0
    flags = flags | FLAG_C;  // Set bit 2
    
    // Test flags
    if (flags & FLAG_A) {
        printf("FLAG_A is set\n");
    }
    
    // Clear a flag
    flags = flags & ~FLAG_B;  // [Note: ~ operator not yet implemented]
}
```

## Implementation Status
- ✅ Basic language features (v1.0)
- ✅ While loops
- ✅ Compound assignment operators (`+=`, `-=`)
- ✅ Increment/decrement operators (`++`, `--`)
- ✅ Multiplication, division, modulo operators (`*`, `/`, `%`)
- ✅ Pointer dereference (read/write)
- ✅ Array indexing via pointer arithmetic
- ✅ Memory management (malloc/free)
- ✅ File I/O operations (fopen/fclose/fgetc/fputc/fread/fwrite)
- ✅ Keyboard input functions (kbhit/getch)
- ✅ Extended printf formatters
- ✅ File extension handling (.C/.EXE)
- ✅ Global constants (literal initialization only)
- ✅ Global variables (no initialization at declaration)
- ✅ Logical operators (`&&`, `||`) with short-circuit evaluation
- ✅ Bitwise operators (`&`, `|`)
- ✅ Break statement (exits nearest enclosing loop)
- ✅ Continue statement (while loops only)
- ✅ Hexadecimal integer literals (`0x` prefix)
- ✅ Array initializer syntax for const char* declarations

## Future Enhancements
- Continue statement for for loops
- Bitwise XOR (`^`) and NOT (`~`) operators
- Shift operators (`<<`, `>>`)
- Global variable initialization
- Multi-dimensional arrays
- Do-while loops
- Switch statements
- Advanced pointer arithmetic
- Struct support
- Typedef
- Preprocessor directives
- Optimization passes
- Better error recovery
- Debugging information
- Multiple assignment operators (`*=`, `/=`, `%=`, `&=`, `|=`, `^=`)
- Address-of operator (`&`)
- Octal literals (`0` prefix)

## Limitations
- Only one file can be open at a time (requires buffering for file operations)
- No floating-point support
- Stack-based architecture limits recursion depth
- Maximum 65535 bytes per file (also limits file copy size due to buffer allocation)
- Filenames limited to 13 characters (uppercase)
- Global variables limited to ~29 (119 bytes of zero page)
- Global constants must be initialized with literals only
- Continue statement only works in while loops, not for loops
- Array initializers for const char* are converted to string literals at parse time

## Target Benchmark Programs

### BENCH.C - Performance Test
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

### FIBO.C - Recursion Test
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