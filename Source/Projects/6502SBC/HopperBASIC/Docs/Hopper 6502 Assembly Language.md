# Hopper Assembly Syntax Reference
**Document Type: Language Specification for Hopper Assembly**
## Overview

Hopper Assembly is a unique hybrid syntax that combines structured programming constructs with 6502 assembly language. It maintains the power of assembly while providing higher-level control flow structures similar to C or other structured languages.

## Key Differences from Conventional Assembly

### 1. **Structured Control Flow**
Unlike conventional assembly with labels and jumps, Hopper Assembly uses structured blocks:

**Conventional Assembly:**
```assembly
CMP #$05
BNE skip_code
LDA #$FF
STA $1000
skip_code:
```

**Hopper Assembly:**
```hopper
CMP #5
if (NZ)
{
    LDA #0xFF
    STA 0x1000
}
```

### 2. **No Labels Required for Loops and Conditionals**
Control structures are self-contained with automatic branching:

**Conventional Assembly:**
```assembly
LDX #10
loop_start:
    DEX
    BNE loop_start
```

**Hopper Assembly:**
```hopper
LDX #10
loop
{
    DEX
    if (Z) { break; }
}
```

### 3. **C-style Comments and Formatting**
Uses `//` for comments instead of `;`, and C-style brace formatting:

**Conventional Assembly:**
```assembly
; This is a comment
    LDA #$10    ; Load accumulator
```

**Hopper Assembly:**
```hopper
// This is a comment
LDA #0x10       // Load accumulator
```

### 4. **Condition Syntax Uses Processor Flags**
Conditions directly reference 6502 status register flags:
- `Z` - Zero flag set
- `NZ` - Zero flag clear  
- `C` - Carry flag set
- `NC` - Carry flag clear
- `MI` - Negative flag set (minus)
- `PL` - Negative flag clear (plus)
- `V` - Overflow flag set
- `NV` - Overflow flag clear

## Formatting and Style Conventions

### Brace Placement
Opening curly braces should be on their own line:

```hopper
loop
{
    // code block
}

if (Z)
{
    // code block
}
else
{
    // alternative code
}
```

### Indentation
Use consistent indentation (typically 2 or 4 spaces) for code blocks:

```hopper
MyFunction()
{
    LDA #0x10
    if (Z)
    {
        STA ZP.TOPL    // Indented within if block
    }
}
```

### Comments
Use `//` for all comments, not the traditional assembly `;`:

```hopper
// This is a comment
LDA #0xFF       // End-of-line comment
```

## Control Flow Structures

### If Statements
```hopper
if (condition)
{
    // code
}

// If-else structure
if (Z)
{
    // zero flag set
}
else
{
    // zero flag clear
}
```

### Switch Statements
Switch statements have special optimization rules and formatting requirements:

```hopper
switch (A)  // Can switch on A, X, or Y registers
{
    case 0x00:
    {
        // Each case requires curly braces
        SomeFunction();
    }
    case 0x01:
    {
        // Cases never fall through (no break needed)
        AnotherFunction();
    }
    default:
    {
        // Default case required for jump table optimization
        DefaultFunction();
    }
}
```

**Jump Table Optimization**: Switch statements can be optimized into jump tables when:
- Switch operates on X or Y register (not A)
- Each case contains exactly one subroutine call
- Default case is present
- More than 8 cases total
- Switch is followed immediately by `return` or end of method

### Loops
```hopper
loop
{
    // infinite loop
    if (condition) { break; }
    if (other_condition) { continue; }
}
```

### Return Statement
```hopper
return;  // equivalent to RTS
```

### Continue and Break
```hopper
loop
{
    if (condition) { continue; }  // jump to loop start
    if (other_condition) { break; }  // exit loop
}
```

## Preprocessor Directives and ROM Configuration

### Conditional Compilation
```hopper
#define SYMBOL_NAME

#ifdef SYMBOL_NAME
    // Code here
#endif

#if defined(SYMBOL1) && (defined(SYMBOL2) || !defined(SYMBOL3))
    // Complex boolean expressions supported
#endif
```

### ROM Size Configuration
These symbols control the 6502 start vector address and Intel IHex layout:

```hopper
#define ROM_32K     // Origin at 0x8000 (default)
#define ROM_16K     // Origin at 0xC000  
#define ROM_8K      // Origin at 0xE000
#define ROM_4K      // Origin at 0xF000
#define ROM_1K      // Origin at 0xFC00
```

### CPU Target Selection
```hopper
#define CPU_6502        // Original MOS 6502 instruction set
#define CPU_65C02S      // Expanded 65C02S instruction set
#define CPU_65UINO      // Similar to ROM_4K and CPU_6502
```

The compiler automatically generates different code based on CPU capabilities:

```hopper
#ifdef CPU_65C02S
    STZ ZP.PORTA      // Use enhanced zero instruction
    PHX               // Enhanced stack operations
    if (BBS0, ZP.FLAGS) { /* bit test branch */ }
#else
    LDA #0           // Traditional approach
    STA ZP.PORTA
    TXA
    PHA              // Traditional stack save
    LDA ZP.FLAGS
    AND #0b00000001
    if (NZ) { /* equivalent bit test */ }
#endif
```

### Hardware Platform Support

Hopper supports multiple 6502-based hardware platforms through conditional compilation:

```hopper
#define BENEATER_IO     // Ben Eater's 6502 computer  
#define X16_IO          // Commander X16
#define MECB6502_IO     // Minimal 6502 Computer Board
#define ZEROPAGE_IO     // Default zero-page mapped I/O (default)
```

Each platform defines different memory mappings for the same logical devices:

```hopper
// BENEATER_IO: VIA at 0x6000, ACIA at 0x5000
// X16_IO: VIA at 0x9F20, ACIA at 0x9F10  
// MECB6502_IO: VIA at 0xF000, ACIA at 0xF008
// ZEROPAGE_IO: VIA at 0xF0, ACIA at 0xEC

STA ZP.PORTA    // Automatically maps to correct address for platform
```

This allows the same code to run on different hardware platforms by simply changing the platform define.

### Module Imports
```hopper
uses "moduleName"           // Import a module
uses "/path/to/module"      // Import with path
```

### Cross-Module Communication

The Hopper compiler provides sophisticated module linking:

```hopper
// In main program
program HopperEcho
{
    uses "/Source/Runtime/6502/Serial"
    
    Hopper()
    {
        Serial.Initialize();    // Automatic function resolution
        Serial.WriteChar();     // Cross-module method calls
    }
}
```

**Module System Features:**
- Automatic function resolution and linking
- Only needed functions are included in final binary
- Proper JSR instruction generation for method calls
- Link-time optimization across modules

## Numeric Literals and Data Types

Hopper Assembly uses C-style numeric literals instead of traditional assembly syntax:

**Conventional Assembly:**
```assembly
LDA #$FF        ; Hexadecimal
LDA #%11111111  ; Binary
```

**Hopper Assembly:**
```hopper
LDA #0xFF       // Hexadecimal (C-style)
LDA #0b11111111 // Binary (C-style)
LDA #255        // Decimal
LDA #'A'        // ASCII character
```

### String and Data Literals

Hopper supports various data literal types:

```hopper
const string message = "Hello\nWorld!";   // Null-terminated strings
const byte[] data = { 0x01, 0x02, 0x03 }; // Byte arrays
const uint address = 0x1000;              // 16-bit constants

// String usage with indexed addressing
LDX #0
loop
{
    LDA message, X    // Access string characters
    if (Z) { break; } // Null terminator check
    Serial.WriteChar();
    INX
}
```

**String Features:**
- Escape sequences (`\n`, `\r`, etc.) are automatically converted
- Strings are stored in ROM and null-terminated
- Can be accessed using indexed addressing modes

## Addressing Modes

### Immediate Mode
```hopper
LDA #5          // Load immediate value 5
LDA #0xFF       // Hexadecimal immediate
LDA #0b00000001 // Binary immediate
CMP #'A'        // Compare with ASCII 'A'
```

### Zero Page and Absolute
```hopper
LDA ZP.TOPL     // Zero page variable
STA 0x1000      // Absolute address (no $ prefix)
```

### Indexed Addressing
```hopper
LDA 0x1000, X   // Absolute indexed by X (no $ prefix)
STA ZP.TOPL, Y  // Zero page indexed by Y
```

### Indirect Addressing (65C02)
```hopper
LDA [ZP.IDX]        // Zero page indirect
STA [ZP.IDX], Y     // Zero page indirect indexed
```

## Special 65C02S Instructions

When `CPU_65C02S` is defined, additional bit manipulation instructions are available:

```hopper
SMB0 ZP.DDRA       // Set Memory Bit 0
RMB1 ZP.DDRA       // Reset Memory Bit 1
BBS2 ZP.DDRA       // Branch if Bit Set 2
BBR3 ZP.DDRA       // Branch if Bit Reset 3
```

These are often used in conditional contexts:
```hopper
if (BBS0, ZP.DDRA)  // if bit 0 is set
{
    // code
}
```

## Memory Operations

### Zero Page Indirect Operations
```hopper
LDA [ZP.IDX]        // Load from address in ZP.IDX
STA [ZP.IDY], Y     // Store to address in ZP.IDY + Y offset
```

### 16-bit Operations
```hopper
// Increment 16-bit value
INC ZP.TOPL
if (Z)
{
    INC ZP.TOPH
}

// Or using utility functions
IncIDX();           // Increment IDX register pair
DecACCx2();         // Decrement ACC by 2
```

## Program Structure and Entry Points

### Program Unit
The main program is defined using the `program` keyword:

```hopper
program MyProgram
{
    uses "/Source/Runtime/6502/Serial"
    uses "I2C.asm"
    
    // Optional interrupt handlers (if absent, vectors remain 0x0000)
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
        // NMI handler - this is optional
        // If omitted, NMI vector remains 0x0000
    }
    
    Hopper()  // Main entry point (like main() in C)
    {
        Serial.Initialize();
        loop
        {
            // main program loop
        }
    }
}
```

### Unit Structure and Encapsulation
Units provide encapsulation with visibility rules:

```hopper
unit MyUnit
{
    uses "OtherUnit"
    
    // Private function (lowercase first letter)
    privateHelper()
    {
        // only accessible within this unit
    }
    
    // Public function (uppercase first letter)
    PublicFunction()
    {
        privateHelper();  // can call private functions
    }
    
    // Another public function
    Initialize()
    {
        // public initialization function
    }
}
```

### Visibility Rules
- **Private**: Methods starting with lowercase letters are private to the unit
- **Public**: Methods starting with uppercase letters are accessible from other units
- **Qualified Access**: Use dot notation to resolve ambiguous method names

```hopper
program Example
{
    uses "Serial"
    uses "I2C" 
    
    Hopper()
    {
        Serial.Initialize();    // Qualified call to Serial unit
        I2C.Start();           // Qualified call to I2C unit
        privateMethod();       // Private method within current unit
    }
    
    privateMethod()  // Private to this program unit
    {
        // implementation
    }
}
```

## Enums and Named Constants

### Enum Declaration
Enums provide named constants for better code readability:

```hopper
enum LibCalls
{
    WireBeginTx     = 0x05,
    WireEndTx       = 0x06,
    WireWrite       = 0x07,
    WireRead        = 0x09,
    WireRequestFrom = 0x0A,
}

enum StatusFlags
{
    Running = 0x01,
    Paused  = 0x02,
    Error   = 0x04,
}
```

### Using Enums
```hopper
switch (X)
{
    case LibCalls.WireBeginTx:
    {
        I2C.BeginTx();
    }
    case LibCalls.WireEndTx:
    {
        I2C.EndTx();
    }
    default:
    {
        missing();
    }
}

// Enum values can be used like constants
LDA #StatusFlags.Running
STA ZP.W0
```

## Zero Page Variables and Constants

### Zero Page Memory Management

Hopper uses a sophisticated zero page allocation system through the `ZP` unit, which maps the entire zero page (0x00-0xFF) into functional regions:

```hopper
uses "/Source/Runtime/6502/ZeroPage"

// Runtime system variables (16-bit values use L/H suffix pattern)
LDA #(1000 % 256)
STA ZP.TOPL        // Low byte of 16-bit value
LDA #(1000 / 256)
STA ZP.TOPH        // High byte of 16-bit value

// Hardware I/O (platform-dependent)
LDA #0b00000001
STA ZP.DDRA        // Data direction register A
STA ZP.PORTA       // Port A output

// Working registers for different subsystems
LDA #0x3C
STA ZP.I2CADDR     // I2C device address
LDA ZP.TICK0       // Timer system access
```

### ZP Unit Organization

The ZP unit provides several categories of zero page constants:

**System Variables:**
- `PC`, `FLAGS`, `SP`, `BP` - Runtime system state
- `ACC`, `TOP`, `NEXT`, `IDX`, `IDY` - Virtual machine registers with L/H pairs

**Hardware I/O (Platform-Dependent):**
- `PORTA`, `PORTB`, `DDRA`, `DDRB` - GPIO control
- `ACIASTATUS`, `ACIADATA` - Serial communication
- `T1CL`, `T1CH`, `IFR`, `IER` - Timer and interrupt control

**Working Variables by Subsystem:**
- `W0`-`W7` - General workspace (used by Serial, I2C)
- `TICK0`-`TICK3`, `TARGET0`-`TARGET3` - Time system
- `M0`-`M15` - Memory management
- `F0`-`F15` - General function parameters
- `U0`-`U7` - UInt operations

### Common Zero Page Patterns

**16-bit Value Handling:**
```hopper
// Loading 16-bit constants
LDA #(1000 % 256)     // Low byte
STA ZP.TOPL
LDA #(1000 / 256)     // High byte  
STA ZP.TOPH

// 16-bit increment
INC ZP.TOPL
if (Z)
{
    INC ZP.TOPH
}
```

### Alternative: Enum-Based Constants

Instead of the ZP unit approach, you could define zero page locations using enums:

```hopper
enum ZeroPage
{
    PORTA = 0xF1,
    DDRA  = 0xF3,
    TOPL  = 0x12,
    TOPH  = 0x13,
}

// Usage
LDA #0xFF
STA ZeroPage.PORTA
```

However, the ZP unit approach is preferred as it provides better organization and platform-specific mappings.

### Constant Arrays
```hopper
const byte[] lookup_table = {
    0x00, 0x01, 0x02, 0x03,
    0x04, 0x05, 0x06, 0x07
};

// Usage
LDY #2
LDA lookup_table, Y
```

### Memory Layout and ROM Configuration

ROM size configuration affects memory layout and vector placement:

```hopper
#define ROM_4K      // Code at 0xF000-0xFFFF, vectors at 0xFFFA-0xFFFF
#define ROM_16K     // Code at 0xC000-0xFFFF, vectors at 0xFFFA-0xFFFF  
#define ROM_32K     // Code at 0x8000-0xFFFF, vectors at 0xFFFA-0xFFFF
```

**Automatic Features:**
- Interrupt vectors automatically generated and placed
- String and constant data stored in ROM
- Reset vector points to compiler-generated startup code
- Stack pointer automatically initialized to 0xFF

## Friend Classes and Access Control

### Friend Declaration
Units can declare other units as "friends" to allow access to private methods:

```hopper
unit List
{
    friend GC, Memory;  // GC and Memory units can access private methods
    
    privateHelper()     // Private method
    {
        // Only accessible by List, GC, and Memory units
    }
    
    PublicMethod()      // Public method
    {
        privateHelper();
    }
}

unit GC
{
    CleanupList()
    {
        List.privateHelper();  // Allowed because GC is a friend of List
    }
}
```

## Advanced Features

### Inline Assembly Optimization
The compiler can optimize certain patterns:
```hopper
#ifdef FASTINTS
    // Optimized multiplication by powers of 2
    if (Z)
    {
        ASL ZP.NEXTL
        ROL ZP.NEXTH
        LSR
    }
#endif
```

### Error Handling
```hopper
#ifdef CHECKED
    BIT ZP.TOPH
    if (MI) // value > 32767
    {
        LDA #0x0D // Numeric type out of range
        Diagnostics.die();
    }
#endif
```

### Complex Conditional Logic
```hopper
LDA ZP.PLUGNPLAY
AND #0b00000010
if (NZ) // EEPROM present?
{
    // EEPROM-specific code
}
```

## Labels and Traditional Assembly Support

While structured control flow is preferred, traditional assembly labels are still supported within method scope:

```hopper
MyFunction()
{
    LDX #8
    JMP first     // Jump to label (traditional assembly style)
    loop
    {
        INC ZP.DDRB
first:
        ASL ZP.OutB
        if (C)
        {
            // structured code
        }
        DEC ZP.DDRB
        DEX
        if (Z) { break; }
    }
}
```

**Important**: Labels are only valid within the current method scope and cannot be accessed from other methods or units.

## Best Practices

1. **Use structured control flow** over traditional labels and jumps
2. **Leverage conditional compilation** for different CPU targets
3. **Use meaningful zero page variable names** (ZP.TOPL vs raw addresses)
4. **Group related functionality in units** for code organization
5. **Use bit manipulation instructions** on 65C02S when available
6. **Prefer high-level constructs** while maintaining assembly-level control

## Example: Complete I2C Implementation

```hopper
unit I2C
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    const byte SCL = 0b00000001;        // DRB0 bitmask
    const byte SCL_INV = 0b11111110;    // Inverted for easy clear bit
    const byte SDA = 0b00000010;        // DRB1 bitmask
    const byte SDA_INV = 0b11111101;    // Inverted for easy clear bit
    
    // Public method - starts I2C communication
    Start()
    {
        LDA ZP.I2CADDR
        ROL                // Shift in carry
        STA ZP.OutB        // Save addr + r/w bit

        LDA #SCL_INV
        AND ZP.DDRB
        STA ZP.DDRB      // Start with SCL as input HIGH
        
        LDA #SDA          // Ensure SDA is output low before SCL is LOW
        ORA ZP.DDRB
        STA ZP.DDRB
        LDA #SDA_INV
        AND ZP.PORTB
        STA ZP.PORTB
        
        LDA #SCL_INV      // Ensure SCL is low when it turns to output
        AND ZP.PORTB
        STA ZP.PORTB
        INC ZP.DDRB      // Set to output == OUT, LOW
        
        byteOut();        // Call private method
    }
    
    // Public method - stops I2C communication
    Stop()
    {
        LDA ZP.DDRB // SDA low
        ORA #SDA
        STA ZP.DDRB
        DEC ZP.DDRB // SCL HIGH
        LDA ZP.DDRB // Set SDA high after SCL == Stop condition
        AND #SDA_INV
        STA ZP.DDRB
    }
    
    // Private method - sends byte in ZP.OutB
    byteOut() // clears ZP.OutB
    {
#ifdef CPU_65C02S
        PHX
#else
        TXA 
        PHA
#endif
        
        LDA #SDA_INV // Set SDA LOW for data byte
        AND ZP.PORTB
        STA ZP.PORTB
        LDX #8
        JMP first     // Traditional label usage (within method scope)
        loop
        {
            INC ZP.DDRB  // SCL out, low
first:
            ASL ZP.OutB    // MSB to carry
            if (C)
            {
                LDA ZP.DDRB
                AND #SDA_INV
                STA ZP.DDRB
            }
            else
            {
                LDA ZP.DDRB
                ORA #SDA
                STA ZP.DDRB
            }
            DEC ZP.DDRB
            DEX
            if (Z) { break; }
        }

        INC ZP.DDRB
        LDA ZP.DDRB // Set SDA to INPUT (HIGH)
        AND #SDA_INV
        STA ZP.DDRB

        DEC ZP.DDRB // Clock high
        LDA ZP.PORTB  // Check ACK bit
        SEC
        AND #SDA
        if (Z)
        {
            CLC       // Clear carry on ACK
        }
        INC ZP.DDRB // SCL low
        
#ifdef CPU_65C02S
        PLX
#else
        PLA 
        TAX
#endif
    }
}

program I2CExample
{
    uses "I2C.asm"
    uses "/Source/Runtime/6502/Serial"
    
    Hopper()  // Main entry point
    {
        Serial.Initialize();
        
        LDA #0x3C      // I2C device address
        STA ZP.I2CADDR
        
        I2C.Start();   // Public method call with qualification
        LDA #0x00
        STA ZP.OutB
        // I2C.byteOut(); // This would be an ERROR - byteOut is private!
        I2C.Stop();    // Public method call
    }
}
```

This syntax provides the precision of assembly language with the readability and maintainability of structured programming, making it particularly suitable for system-level programming on 6502-based microcontrollers.