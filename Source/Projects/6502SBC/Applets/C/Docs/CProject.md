# C Compiler for Hopper BIOS - Project Specification v2.1

## Overview
A native C compiler that runs on 6502 under Hopper BIOS, capable of compiling C programs with pointer operations, file I/O, dynamic memory management, and global variables/constants to executable binaries.

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
              | <compound-stmt>

<if-stmt> ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?

<while-stmt> ::= "while" "(" <expression> ")" <statement>

<for-stmt> ::= "for" "(" <expr-stmt> <expression> ";" <expression>? ")" <statement>

<expression> ::= <assignment>
<assignment> ::= <logical-or> (("=" | "+=" | "-=") <assignment>)?
<logical-or> ::= <logical-and> ("||" <logical-and>)*
<logical-and> ::= <relational> ("&&" <relational>)*
<relational> ::= <additive> (("<" | ">" | "<=" | ">=" | "==" | "!=") <additive>)*
<additive> ::= <multiplicative> (("+" | "-") <multiplicative>)*
<multiplicative> ::= <unary> (("*" | "/" | "%") <unary>)*
<unary> ::= ("*" | "-")? <postfix>
<postfix> ::= <primary> ("++" | "--" | "[" <expression> "]" | "(" <arg-list>? ")")*
<primary> ::= <identifier> | <literal> | "(" <expression> ")"
<literal> ::= <integer> | <string>
```

## Built-in Functions

### Console I/O
```c
void putchar(char c);        // Maps to Serial.WriteChar
void printf(char* fmt, ...); // Supports %d, %ld, %x, %s, %c formatters
int kbhit(void);            // Maps to Serial.IsAvailable (non-blocking key check)
int getch(void);            // Maps to Serial.WaitForChar (blocking key read)
```

### Time Functions
```c
long millis(void);    // Maps to Time.Millis (milliseconds since boot)
long seconds(void);   // Maps to Time.Seconds (seconds since boot)
```

### Memory Management
```c
void* malloc(int size);  // Maps to SysCall.MemAllocate
void free(void* ptr);    // Maps to SysCall.MemFree
```

### File I/O
```c
FILE* fopen(char* filename, char* mode);     // Maps to SysCall.FOpen
int fclose(FILE* fp);                        // Maps to SysCall.FClose
int fgetc(FILE* fp);                         // Maps to SysCall.FGetC
int fputc(int c, FILE* fp);                  // Maps to SysCall.FPutC
int fread(void* ptr, int size, int n, FILE* fp);  // Maps to SysCall.FRead
int fwrite(void* ptr, int size, int n, FILE* fp); // Maps to SysCall.FWrite
```

**Note**: Only one file can be open at a time in the Hopper BIOS file system. This means file operations must be carefully sequenced - you cannot have both a source and destination file open simultaneously. File copying requires reading the entire source into memory first, then writing to the destination.

## Runtime Architecture

### Memory Layout
```
0x0200-0x02FF: Stack page 0 (LSB of 32-bit values)
0x0300-0x03FF: Stack page 1 (byte 1 of 32-bit values)
0x0400-0x04FF: Stack page 2 (byte 2 of 32-bit values)  
0x0500-0x05FF: Stack page 3 (MSB of 32-bit values)
0x0058-0x005F: C runtime zero page (see below)
0x0060-0x0068: Runtime stack pointers
0x0069-0x00DF: Global variables (119 bytes available)
0x00C0-0x00C3: Library work space
0x0800:        Program entry point & code start
After code:    String literals
```

### Zero Page Allocation

#### C Runtime (0x58-0x5F)
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

#### Runtime Stack Pointers (0x60-0x68)
```
0x60: runtimeBP         - Runtime base pointer
0x61-0x62: runtimeStack0L/H - Pointer to stack page 0x01
0x63-0x64: runtimeStack1L/H - Pointer to stack page 0x02
0x65-0x66: runtimeStack2L/H - Pointer to stack page 0x03
0x67-0x68: runtimeStack3L/H - Pointer to stack page 0x04
```

#### Global Variables (0x69-0xDF)
```
0x69-0xDF: Global variable space
           - Allocated sequentially from 0x69
           - Each global uses 4 bytes (32-bit values)
           - Maximum ~29 global variables
```

#### Library Work Space (0xC0-0xC3)
```
0xC0-0xC1: libArgL/H   - Library argument passing
0xC2-0xC3: libStrL/H   - String pointer for library functions
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
2. **Parser** - Builds AST using heap allocation, supports all new operators and global declarations
3. **Type Checker** - Walks AST, validates types and pointer operations, verifies globals
4. **Code Generator** - Emits to code buffer with support for:
   - Global constant and variable access
   - Compound assignments
   - Increment/decrement operations
   - Pointer dereference (read/write)
   - Array indexing
   - While loops
   - All arithmetic operations including *, /, %
5. **Linker** - Patches addresses, appends literals, writes executable

### Data Structures

#### Symbol Table (Heap Allocated)
```
Per symbol:
- Name pointer (to heap string)
- Type (char/int/long/void/pointer)
- Storage (BP offset for locals/params, ZP address for globals)
- Scope level (global/local)
- Next pointer (linked list)
```

#### AST Nodes (Extended)
```
Node types:
- FunctionDef, VarDecl, ConstDecl, If, While, For, Return, Block
- BinOp, UnaryOp, Call, Assign, Ident, IntLit, StringLit
- PostfixOp (for ++ and --)

Node structure examples:
BinOp:     [type][op][left_ptr:2][right_ptr:2]
UnaryOp:   [type][op][child_ptr:2]
PostfixOp: [type][op][child_ptr:2]
IntLit:    [type][value:4]
Ident:     [type][symbol_ptr:2]
ConstDecl: [type][ident_ptr:2][value_ptr:2]
```

### Code Generation Features

#### Global Variable Access
```c
int counter;  // Allocated at 0x69

void increment() {
    counter++;  // Direct zero page access
}
```

#### Global Constant Usage
```c
const int MAX = 100;

void check(int value) {
    if (value > MAX) {  // Constant folded at compile time
        // ...
    }
}
```

#### Compound Assignments
The compiler transforms compound assignments during parsing:
```c
x += 5;  // Becomes: x = x + 5
y -= 3;  // Becomes: y = y - 3
```

#### Increment/Decrement
Postfix operators are handled specially:
```c
i++;  // Load i, increment, store back, original value on stack
j--;  // Load j, decrement, store back, original value on stack
```

#### Array Indexing
Arrays are implemented via pointer arithmetic:
```c
buffer[i]     // Becomes: *(buffer + i)
buffer[i] = x // Becomes: *(buffer + i) = x
```

#### Pointer Operations
Direct pointer manipulation:
```c
char* ptr = malloc(100);  // Allocate buffer
*ptr = 'A';               // Write through pointer
char c = *ptr;            // Read through pointer
```

#### File Function Marshalling
File operations use specific system calls starting at 0xF0:
```
FOpen:  0xF0
FClose: 0xF1
FGetC:  0xF2
FPutC:  0xF3
FRead:  0xF4
FWrite: 0xF5
```

#### Keyboard Input Functions
Keyboard operations map to BIOS serial I/O:
```
kbhit:  SysCall.SerialIsAvailable (0x87)
getch:  SysCall.SerialWaitForChar (0x86)
```

## Example Programs

### Using Global Variables and Constants
```c
const int MAX_COUNT = 100;
const char NEWLINE = '\n';

int count;
char* message;

void init() {
    count = 0;
    message = "Counter: ";
}

void increment() {
    count++;
    if (count > MAX_COUNT) {
        count = 0;
    }
}

void display() {
    printf("%s%d", message, count);
    putchar(NEWLINE);
}

void main() {
    init();
    while (1) {
        if (kbhit()) {
            getch();
            increment();
            display();
        }
    }
}
```

### Memory and String Manipulation
```c
void strcpy(char* dst, char* src) {
    while (*src) {
        *dst++ = *src++;
    }
    *dst = 0;
}

void main() {
    char* buffer = malloc(100);
    char* msg = "Hello, World!";
    strcpy(buffer, msg);
    printf("%s\n", buffer);
    free(buffer);
}
```

### File Processing Example
```c
void process_file(char* filename) {
    FILE* fp = fopen(filename, "r");
    int c;
    int count = 0;
    
    if (!fp) {
        printf("Cannot open file %s\n", filename);
        return;
    }
    
    while ((c = fgetc(fp)) != -1) {
        if (c == '\n') {
            count++;
        }
    }
    
    printf("File has %d lines\n", count);
    fclose(fp);
}

void main() {
    process_file("README.TXT");
}
```

### Interactive Console Program
```c
void echo_until_escape() {
    int c;
    int done;
    
    printf("Type characters (ESC to quit):\n");
    
    done = 0;
    while (!done) {
        if (kbhit()) {           // Check if key available
            c = getch();          // Get key without echo
            if (c == 27) {        // ESC key
                done = 1;
            }
            else {
                putchar(c);       // Echo the character
                if (c == '\r') {  // Add newline after CR
                    putchar('\n');
                }
            }
        }
    }
    
    printf("\nGoodbye!\n");
}

void main() {
    echo_until_escape();
}
```

### Array Processing with Division/Modulo
```c
const int ARRAY_SIZE = 10;

void analyze_array(int* arr, int n) {
    int sum = 0;
    int i;
    
    for (i = 0; i < n; i++) {
        sum = sum + arr[i];
    }
    
    printf("Sum: %d\n", sum);
    printf("Average: %d\n", sum / n);
    printf("Remainder: %d\n", sum % n);
}

void main() {
    int* numbers = malloc(ARRAY_SIZE * sizeof(int));
    int i;
    
    // Initialize with multiples
    for (i = 0; i < ARRAY_SIZE; i++) {
        numbers[i] = i * 3;
    }
    
    analyze_array(numbers, ARRAY_SIZE);
    free(numbers);
}
```

### Recursive Fibonacci with Timing
```c
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

### Text Processing with Dynamic Memory
```c
void reverse_string(char* str) {
    int len = 0;
    int i;
    char temp;
    
    // Find length
    while (str[len]) {
        len++;
    }
    
    // Reverse in place
    for (i = 0; i < len/2; i++) {
        temp = str[i];
        str[i] = str[len-1-i];
        str[len-1-i] = temp;
    }
}

void main() {
    char* text = malloc(50);
    int i = 0;
    int c;
    
    printf("Enter text: ");
    while ((c = getch()) != '\n' && i < 49) {
        putchar(c);  // Echo character
        text[i++] = c;
    }
    text[i] = 0;
    putchar('\n');
    
    reverse_string(text);
    printf("Reversed: %s\n", text);
    
    free(text);
}
```

### File Copy Utility
```c
void copy_file(char* src, char* dst) {
    FILE* fp;
    char* buffer;
    int c;
    int size = 0;
    int i;
    
    // First pass: determine file size
    fp = fopen(src, "r");
    if (!fp) {
        printf("Cannot open source: %s\n", src);
        return;
    }
    
    while ((c = fgetc(fp)) != -1) {
        size++;
    }
    fclose(fp);
    
    if (size == 0) {
        printf("Source file is empty\n");
        return;
    }
    
    // Allocate buffer for entire file
    buffer = malloc(size);
    if (!buffer) {
        printf("Cannot allocate %d bytes\n", size);
        return;
    }
    
    // Second pass: read file into buffer
    fp = fopen(src, "r");
    if (!fp) {
        printf("Cannot reopen source: %s\n", src);
        free(buffer);
        return;
    }
    
    for (i = 0; i < size; i++) {
        c = fgetc(fp);
        if (c == -1) {
            printf("Read error at byte %d\n", i);
            fclose(fp);
            free(buffer);
            return;
        }
        buffer[i] = c;
    }
    fclose(fp);
    
    // Write buffer to destination file
    fp = fopen(dst, "w");
    if (!fp) {
        printf("Cannot create destination: %s\n", dst);
        free(buffer);
        return;
    }
    
    for (i = 0; i < size; i++) {
        fputc(buffer[i], fp);
    }
    fclose(fp);
    
    free(buffer);
    printf("Copied %d bytes from %s to %s\n", size, src, dst);
}

void main() {
    copy_file("SOURCE.TXT", "DEST.TXT");
}
```

## Command Line Interface
```
CC PROGRAM[.C]
```
- **Input**: Source file name. If no extension is provided, `.C` is appended
- **Output**: Executable file with `.EXE` extension

Examples:
- `CC HELLO` - Compiles `HELLO.C` to `HELLO.EXE`
- `CC HELLO.C` - Compiles `HELLO.C` to `HELLO.EXE`
- `CC TEST` - Compiles `TEST.C` to `TEST.EXE`

## Error Handling
- Stop on first error
- Report line number (counted during streaming parse)
- Display error message with context when possible
- Support for error types:
  - Syntax errors
  - Type mismatches
  - Undefined identifiers
  - Invalid pointer operations
  - Unsupported formatters in printf
  - File I/O errors
  - Global variable allocation failures (out of zero page)

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

## Future Enhancements
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
- Multiple assignment operators (`*=`, `/=`, `%=`)
- Address-of operator (`&`)

## Limitations
- Only one file can be open at a time (requires buffering for file operations)
- No floating-point support
- Stack-based architecture limits recursion depth
- Maximum 65535 bytes per file (also limits file copy size due to buffer allocation)
- Filenames limited to 13 characters (uppercase)
- Global variables limited to ~29 (119 bytes of zero page)
- Global constants must be initialized with literals only

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