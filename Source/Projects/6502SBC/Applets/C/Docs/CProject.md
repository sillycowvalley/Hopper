Here's the updated specification with the new operators and control flow statements:

# C Compiler for Hopper BIOS - Project Specification v2.2

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

### Global Declarations

#### Global Constants
Global constants must be initialized with literal values only:
```c
const int MAX_SIZE = 100;
const char DELIMITER = ',';
const long BIG_NUMBER = 1000000;
```

**Restrictions:**
- Must be initialized at declaration
- Initializer must be a literal value (no expressions)
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

<const-decl> ::= "const" <type> <identifier> "=" <literal> ";"

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
<literal> ::= <integer> | <string>
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
    const int FLAG_A = 1;
    const int FLAG_B = 2;
    const int FLAG_C = 4;
    
    // Set flags
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

## Limitations
- Only one file can be open at a time (requires buffering for file operations)
- No floating-point support
- Stack-based architecture limits recursion depth
- Maximum 65535 bytes per file (also limits file copy size due to buffer allocation)
- Filenames limited to 13 characters (uppercase)
- Global variables limited to ~29 (119 bytes of zero page)
- Global constants must be initialized with literals only
- Continue statement only works in while loops, not for loops

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