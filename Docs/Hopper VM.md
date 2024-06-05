## Hopper Virtual Machine Documentation

### Overview
The Hopper Virtual Machine (VM) is designed to efficiently execute the Hopper programming language on small devices. It supports a range of instructions, some of which are optimized for compactness and higher code density.

### Types

#### Value Types
Value types in Hopper fit within the 16-bit stack slots on the Hopper value stack. These include:
- `Char` (0x01)
- `Int` (0x02)
- `Byte` (0x03)
- `UInt` (0x04)
- `Bool` (0x06)

#### Reference Types
Reference types are represented on the value stack by a 16-bit 'reference' (a pointer in the heap). The 8-bit type stack provides additional context for these references:
- `Float` (0x0D)
- `Long` (0x0E)
- `String` (0x0F)
- `Pair` (0x10)
- `Array` (0x12)
- `Dictionary` (0x13)
- `Variant` (0x14)
- `File` (0x15)
- `Directory` (0x16)
- `List` (0x19)
- `ListItem` (0x1A)

#### Type Enumeration
```hopper
unit Types
{
    enum Type
    {
        Undefined  = 0x00,
        Char       = 0x01,
        Int        = 0x02,
        Byte       = 0x03,
        UInt       = 0x04,
        Reference  = 0x05,
        Bool       = 0x06,
        Type       = 0x0C,
        Float      = 0x0D,
        Long       = 0x0E,
        String     = 0x0F,
        Pair       = 0x10,
        Array      = 0x12,
        Dictionary = 0x13,
        Variant    = 0x14,
        File       = 0x15,
        Directory  = 0x16,
        List       = 0x19,
        ListItem   = 0x1A,
    }
    
    bool IsReferenceType(Type htype)
    {
        return (byte(htype) >= 0x0D);
    }
}
```

### Stack and Registers

#### Value Stack
- **16-bit slots**: For storing values.
- **8-bit type slots**: Parallel to the value stack, specifying the type of each value.

#### Call Stack
- Stores return addresses for function calls.
- Stack frames for managing local variables and method arguments.

#### Registers
- **PC (16 bits)**: Program Counter.
- **SP (8 bits)**: Stack Pointer.
- **BP (8 bits)**: Base Pointer for stack frames.

### Instructions

#### Minimal Runtime Instructions
These are the core set of instructions required for the minimal implementation of the Hopper VM.

```hopper
enum Instructions
{
    NOP    = 0x00,
    DUP0   = 0x01,       // push [top]
    PUSHR0 = 0x02,       // R0 -> [top]
    POPR0  = 0x03,       // [top] -> R0
    BITSHL8  = 0x04,
    BITSHR8  = 0x05,
    BITANDFF = 0x06,
    LIBCALL  = 0x08,
    ENTER    = 0x49,
    CALL     = 0x34,
    CALLI    = 0x6A,
    SYSCALL  = 0x26,
    DECSP    = 0x28,
    RET      = 0x35,
    RETRES   = 0x36,
    JZ       = 0x31,
    JNZ      = 0x32,
    JW       = 0x33,
    PUSHI    = 0x37,
    POPLOCAL = 0x38,
    PUSHLOCAL = 0x39,
    POPREL   = 0x3A,
    PUSHREL  = 0x3B,
    POPGLOBAL = 0x3C,
    PUSHGLOBAL = 0x3D,
    PUSHSTACKADDR = 0x3E,
    BOOLNOT  = 0x41,
    BITNOT   = 0x42,
    SWAP     = 0x43,
    PUSHGP   = 0x47,
    COPYNEXTPOP = 0x48,
    CALLREL  = 0x4B,
    CAST     = 0x51,
    PUSHD    = 0x60,
    JIX      = 0x69,
    ADD      = 0x80,
    ADDI     = 0x81,
    SUB      = 0x82,
    SUBI     = 0x83,
    DIV      = 0x84,
    DIVI     = 0x85,
    MUL      = 0x86,
    MULI     = 0x87,
    MOD      = 0x88,
    MODI     = 0x89,
    GT       = 0x8A,
    GTI      = 0x8B,
    LT       = 0x8C,
    LTI      = 0x8D,
    GE       = 0x8E,
    GEI      = 0x8F,
    LE       = 0x90,
    LEI      = 0x91,
    EQ       = 0x92,
    NE       = 0x94,
    BOOLOR   = 0x96,
    BOOLAND  = 0x98,
    BITAND   = 0x9A,
    BITOR    = 0x9C,
    BITXOR   = 0x9E,
    BITSHR   = 0xA0,
    BITSHL   = 0xA2,
}
```

#### Packed Instructions
Packed instructions provide a more compact representation of operations, combining multiple steps into single instructions for higher code density.

```hopper
enum PackedInstructions
{
    PUSHIB       = 0x1A,
    POPLOCALB    = 0x1B,
    PUSHLOCALB   = 0x1C,
    POPRELB      = 0x1D,
    PUSHRELB     = 0x1E,
    POPGLOBALB   = 0x1F,
    PUSHGLOBALB  = 0x20,
    PUSHSTACKADDRB = 0x21,
    INCLOCALB    = 0x22,
    DECLOCALB    = 0x23,
    DECLOCALIB   = 0xA6,
    INCLOCALIB   = 0xA4,
    INCGLOBALB   = 0x53,
    INCGLOBALIB  = 0xA5,
    DECGLOBALB   = 0x54,
    DECGLOBALIB  = 0xA7,
    SYSCALL0     = 0x24,
    SYSCALL1     = 0x25,
    SYSCALL2     = 0x0B,
    LIBCALL0     = 0x09,
    LIBCALL1     = 0x0A,
    RETB         = 0x2A,
    RETRESB      = 0x2B,
    JZB          = 0x2E,
    JNZB         = 0x2F,
    JB           = 0x30,
    PUSHILT      = 0x55,
    ENTERB       = 0x5F,
    RETFAST      = 0x61,
    PUSHILEI     = 0x65,
    PUSHIBLE     = 0x6B,
    PUSHIBEQ     = 0x6C,
    ADDB         = 0x6D,
    SUBB         = 0x6E,
    BITSHLB      = 0x0C,
    BITSHRB      = 0x0D,
    BITANDB      = 0x0E,
    BITORB       = 0x0F,
    INCLOCALBB   = 0x3F,
    INCLOCALIBB  = 0xA3,
    PUSHGLOBALBB = 0x52,
    PUSHLOCALBB  = 0x56,
    SYSCALLB0    = 0xA8,
    SYSCALL00    = 0xA9,
    PUSHIBB      = 0xAA,
    SYSCALLB1    = 0xAB,
    SYSCALL01    = 0xAC,
    SYSCALL10    = 0xAD,
    PUSHI0       = 0x44,
    PUSHI1       = 0x45,
    PUSHIM1      = 0x46,
    RET0         = 0x4A,
    POPLOCALB00  = 0x4C,
    POPLOCALB01  = 0x4D,
    PUSHLOCALB00 = 0x4E,
    PUSHLOCALB01 = 0x4F,
    POPCOPYLOCALB = 0x57,
    POPCOPYRELB   = 0x58,
    POPCOPYGLOBALB = 0x59,
    POPCOPYLOCALB00 = 0x5D,
    POPCOPYLOCALB

01 = 0x5E,
}
```

### Example Usage

#### Adding 1 to the Current Value on the Stack
```hopper
// Using standard instructions
PUSHI 0x01 0x00
ADD

// Using packed instructions
PUSHI1
ADD

// Using combined packed instruction
ADDB 0x01
```

#### Pushing the First Local Variable in a Method
```hopper
// Using standard instructions
PUSHLOCAL 0x00 0x00

// Using packed instructions
PUSHLOCALB 0x00

// Using combined packed instruction
PUSHLOCALB00
```

# Hopper Virtual Machine Opcode Reference

This section provides detailed descriptions of each opcode in the Hopper Virtual Machine (VM). It covers the function, usage, and any relevant details for each opcode, including both minimal runtime instructions and packed instructions.

## Opcode Descriptions

### 0x00 - NOP
**Function**: No operation.
**Description**: This instruction performs no action. It is used to create a delay or to align instructions in memory.
**Usage**: 
```assembly
NOP
```

### 0x01 - DUP0
**Function**: Duplicate the top value on the stack.
**Description**: Pushes a copy of the value at the top of the stack onto the stack.
**Usage**: 
```assembly
DUP0
```
**Stack Operation**:
- Before: `[top, ...]`
- After: `[top, top, ...]`

### 0x02 - PUSHR0
**Function**: Push the value of register R0 onto the stack.
**Description**: Places the current value of register R0 onto the top of the stack.
**Usage**: 
```assembly
PUSHR0
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[R0, ... ]`

### 0x03 - POPR0
**Function**: Pop the top value from the stack into register R0.
**Description**: Removes the top value from the stack and stores it in register R0.
**Usage**: 
```assembly
POPR0
```
**Stack Operation**:
- Before: `[ top, ... ]`
- After: `[ ... ]`
- Register: `R0 = top`

### 0x04 - BITSHL8
**Function**: Shift the top value on the stack left by 8 bits.
**Description**: Performs a bitwise shift left by 8 bits on the value at the top of the stack.
**Usage**: 
```assembly
BITSHL8
```
**Stack Operation**:
- Before: `[value, ...]`
- After: `[(value << 8), ...]`

### 0x05 - BITSHR8
**Function**: Shift the top value on the stack right by 8 bits.
**Description**: Performs a bitwise shift right by 8 bits on the value at the top of the stack.
**Usage**: 
```assembly
BITSHR8
```
**Stack Operation**:
- Before: `[value, ...]`
- After: `[(value >> 8), ...]`

### 0x06 - BITANDFF
**Function**: Perform a bitwise AND with 0xFF on the top value of the stack.
**Description**: Applies a bitwise AND operation with the constant 0xFF to the value at the top of the stack.
**Usage**: 
```assembly
BITANDFF
```
**Stack Operation**:
- Before: `[value, ...]`
- After: `[(value & 0xFF), ...]`

### 0x08 - LIBCALL
**Function**: Call a library function.
**Description**: Calls a function from the linked library. The function to be called is determined by the operand.
**Usage**: 
```assembly
LIBCALL function_id
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: The stack operation depends on the specific library function being called.

### 0x49 - ENTER
**Function**: Enter a new stack frame.
**Description**: Sets up a new stack frame for a function call by adjusting the base pointer (BP) and stack pointer (SP).
**Usage**: 
```assembly
ENTER frame_size
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: This instruction affects the BP and SP registers.

### 0x34 - CALL
**Function**: Call a function.
**Description**: Calls a function at the specified address.
**Usage**: 
```assembly
CALL address
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: This instruction pushes the return address onto the call stack.

### 0x6A - CALLI
**Function**: Call a function indirectly.
**Description**: Calls a function whose address is stored at the top of the stack.
**Usage**: 
```assembly
CALLI
```
**Stack Operation**:
- Before: `[address, ...]`
- After: `[ ... ]`
- Note: This instruction pushes the return address onto the call stack.

### 0x26 - SYSCALL
**Function**: Call a system function.
**Description**: Calls a function provided by the system or runtime environment.
**Usage**: 
```assembly
SYSCALL function_id
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: The stack operation depends on the specific system function being called.

### 0x28 - DECSP
**Function**: Decrement the stack pointer.
**Description**: Decreases the stack pointer (SP) by 1, effectively popping a value from the stack.
**Usage**: 
```assembly
DECSP
```
**Stack Operation**:
- Before: `[top, ...]`
- After: `[ ... ]`

### 0x35 - RET
**Function**: Return from a function.
**Description**: Returns from the current function, restoring the previous stack frame.
**Usage**: 
```assembly
RET
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: This instruction affects the BP and SP registers.

### 0x36 - RETRES
**Function**: Return from a function with a result.
**Description**: Returns from the current function, leaving a result on the stack and restoring the previous stack frame.
**Usage**: 
```assembly
RETRES
```
**Stack Operation**:
- Before: `[result, ... ]`
- After: `[result, ... ]`
- Note: This instruction affects the BP and SP registers.

### 0x31 - JZ
**Function**: Jump if zero.
**Description**: Jumps to the specified address if the top value of the stack is zero.
**Usage**: 
```assembly
JZ address
```
**Stack Operation**:
- Before: `[condition, ...]`
- After: `[ ... ]`
- Note: The condition value is consumed by this instruction.

### 0x32 - JNZ
**Function**: Jump if not zero.
**Description**: Jumps to the specified address if the top value of the stack is not zero.
**Usage**: 
```assembly
JNZ address
```
**Stack Operation**:
- Before: `[condition, ...]`
- After: `[ ... ]`
- Note: The condition value is consumed by this instruction.

### 0x33 - JW
**Function**: Jump unconditionally (wide).
**Description**: Jumps to the specified address unconditionally.
**Usage**: 
```assembly
JW address
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`

### 0x37 - PUSHI
**Function**: Push an immediate value onto the stack.
**Description**: Pushes a 16-bit immediate value onto the stack.
**Usage**: 
```assembly
PUSHI value
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[value, ... ]`

### 0x38 - POPLOCAL
**Function**: Pop a value from the stack into a local variable.
**Description**: Pops the top value from the stack and stores it in a local variable.
**Usage**: 
```assembly
POPLOCAL offset
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[ ... ]`
- Note: The local variable is located at the specified offset from the BP.

### 0x39 - PUSHLOCAL
**Function**: Push a local variable onto the stack.
**Description**: Pushes the value of a local variable onto the stack.
**Usage**: 
```assembly
PUSHLOCAL offset
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[value, ... ]`
- Note: The local variable is located at the specified offset from the BP.

### 0x3A - POPREL
**Function**: Pop a value from the stack into a relative address.
**Description**: Pops the top value from the stack and stores it in a memory location relative to the BP.
**Usage**: 
```assembly
POPREL offset
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[ ... ]`
- Note: The memory location is specified relative to the BP.

### 0x3B - PUSHREL
**Function**: Push a value from a relative address onto the stack.
**Description**: Pushes the value from a memory location relative to the BP onto the stack.
**Usage**: 
```assembly
PUSHREL offset
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[value, ... ]`
- Note: The memory location is specified relative to the BP.

### 0x3C - POPGLOBAL
**Function**: Pop a value from the stack into a

 global variable.
**Description**: Pops the top value from the stack and stores it in a global variable.
**Usage**: 
```assembly
POPGLOBAL address
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[ ... ]`
- Note: The global variable is located at the specified address.

### 0x3D - PUSHGLOBAL
**Function**: Push a global variable onto the stack.
**Description**: Pushes the value of a global variable onto the stack.
**Usage**: 
```assembly
PUSHGLOBAL address
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[value, ... ]`
- Note: The global variable is located at the specified address.

### 0x3E - PUSHSTACKADDR
**Function**: Push the address of the top stack value onto the stack.
**Description**: Pushes the address of the top value on the stack onto the stack itself.
**Usage**: 
```assembly
PUSHSTACKADDR
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[address, ... ]`

### 0x41 - BOOLNOT
**Function**: Perform a logical NOT on the top value of the stack.
**Description**: Inverts the boolean value at the top of the stack.
**Usage**: 
```assembly
BOOLNOT
```
**Stack Operation**:
- Before: `[bool_value, ... ]`
- After: `[!bool_value, ... ]`

### 0x42 - BITNOT
**Function**: Perform a bitwise NOT on the top value of the stack.
**Description**: Inverts all bits of the value at the top of the stack.
**Usage**: 
```assembly
BITNOT
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[~value, ... ]`

### 0x43 - SWAP
**Function**: Swap the top two values on the stack.
**Description**: Exchanges the positions of the top two values on the stack.
**Usage**: 
```assembly
SWAP
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[value2, value1, ... ]`

### 0x47 - PUSHGP
**Function**: Push the global pointer onto the stack.
**Description**: Pushes the value of the global pointer (GP) onto the stack.
**Usage**: 
```assembly
PUSHGP
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[GP, ... ]`

### 0x48 - COPYNEXTPOP
**Function**: Copy the next value and pop it from the stack.
**Description**: Copies the value below the top value on the stack and then pops the top value.
**Usage**: 
```assembly
COPYNEXTPOP
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[value1, ... ]`

### 0x4B - CALLREL
**Function**: Call a function relative to the current address.
**Description**: Calls a function at an address relative to the current instruction pointer (IP).
**Usage**: 
```assembly
CALLREL offset
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: This instruction pushes the return address onto the call stack.

### 0x51 - CAST
**Function**: Cast the top value on the stack to a different type.
**Description**: Changes the type of the value at the top of the stack.
**Usage**: 
```assembly
CAST type_id
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[casted_value, ... ]`
- Note: The specific cast operation depends on the `type_id`.

### 0x60 - PUSHD
**Function**: Push a double value onto the stack.
**Description**: Pushes a double-precision floating-point value onto the stack.
**Usage**: 
```assembly
PUSHD double_value
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[double_value, ... ]`

### 0x69 - JIX
**Function**: Jump if index.
**Description**: Jumps to the specified address if the index value on the stack meets a condition.
**Usage**: 
```assembly
JIX index_condition address
```
**Stack Operation**:
- Before: `[index, ... ]`
- After: `[ ... ]`
- Note: The condition and the index value determine whether the jump is performed.

### 0x80 - ADD
**Function**: Add the top two values on the stack.
**Description**: Pops the top two values from the stack, adds them, and pushes the result onto the stack.
**Usage**: 
```assembly
ADD
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = value1 + value2`

### 0x81 - ADDI
**Function**: Add an immediate value to the top value on the stack.
**Description**: Pops the top value from the stack, adds an immediate value, and pushes the result onto the stack.
**Usage**: 
```assembly
ADDI immediate_value
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[result, ... ]`
- Note: `result = value + immediate_value`

### 0x82 - SUB
**Function**: Subtract the top value on the stack from the next value.
**Description**: Pops the top two values from the stack, subtracts the first from the second, and pushes the result onto the stack.
**Usage**: 
```assembly
SUB
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = value2 - value1`

### 0x83 - SUBI
**Function**: Subtract an immediate value from the top value on the stack.
**Description**: Pops the top value from the stack, subtracts an immediate value, and pushes the result onto the stack.
**Usage**: 
```assembly
SUBI immediate_value
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[result, ... ]`
- Note: `result = value - immediate_value`

### 0x84 - DIV
**Function**: Divide the next value on the stack by the top value.
**Description**: Pops the top two values from the stack, divides the second by the first, and pushes the result onto the stack.
**Usage**: 
```assembly
DIV
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = value2 / value1`

### 0x85 - DIVI
**Function**: Divide the top value on the stack by an immediate value.
**Description**: Pops the top value from the stack, divides it by an immediate value, and pushes the result onto the stack.
**Usage**: 
```assembly
DIVI immediate_value
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[result, ... ]`
- Note: `result = value / immediate_value`

### 0x86 - MUL
**Function**: Multiply the top two values on the stack.
**Description**: Pops the top two values from the stack, multiplies them, and pushes the result onto the stack.
**Usage**: 
```assembly
MUL
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = value1 * value2`

### 0x87 - MULI
**Function**: Multiply the top value on the stack by an immediate value.
**Description**: Pops the top value from the stack, multiplies it by an immediate value, and pushes the result onto the stack.
**Usage**: 
```assembly
MULI immediate_value
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[result, ... ]`
- Note: `result = value * immediate_value`

### 0x88 - MOD
**Function**: Compute the modulus of the next value on the stack by the top value.
**Description**: Pops the top two values from the stack, computes the modulus, and pushes the result onto the stack.
**Usage**: 
```assembly
MOD
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = value2 % value1`

### 0x89 - MODI
**Function**: Compute the modulus of the top value on the stack by an immediate value.
**Description**: Pops the top value from the stack, computes the modulus by an immediate value, and pushes the result onto the stack.
**Usage**: 
```assembly
MODI immediate_value
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[result, ... ]`
- Note: `result = value % immediate_value`


### 0x8C - LT
**Function**: Less than comparison for unsigned 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as unsigned integers, and pushes 1 if the second value is less than the first, otherwise pushes 0.
**Usage**:
```assembly
LT
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 < value1) ? 1 : 0`

### 0x8D - LTI
**Function**: Less than comparison for signed 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as signed integers, and pushes 1 if the second value is less than the first, otherwise pushes 0.
**Usage**:
```assembly
LTI
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 < value1) ? 1 : 0`

### 0x8E - GE
**Function**: Greater than or equal comparison for unsigned 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as unsigned integers, and pushes 1 if the second value is greater than or equal to the first, otherwise pushes 0.
**Usage**:
```assembly
GE
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 >= value1) ? 1 : 0`

### 0x8F - GEI
**Function**: Greater than or equal comparison for signed 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as signed integers, and pushes 1 if the second value is greater than or equal to the first, otherwise pushes 0.
**Usage**:
```assembly
GEI
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 >= value1) ? 1 : 0`

### 0x90 - LE
**Function**: Less than or equal comparison for unsigned 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as unsigned integers, and pushes 1 if the second value is less than or equal to the first, otherwise pushes 0.
**Usage**:
```assembly
LE
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 <= value1) ? 1 : 0`

### 0x91 - LEI
**Function**: Less than or equal comparison for signed 16-bit integers.
**Description**: Pops the top two values from the stack, compares them as signed integers, and pushes 1 if the second value is less than or equal to the first, otherwise pushes 0.
**Usage**:
```assembly
LEI
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 <= value1) ? 1 : 0`

### 0x92 - EQ
**Function**: Equality comparison.
**Description**: Pops the top two values from the stack, compares them, and pushes 1 if they are equal, otherwise pushes 0.
**Usage**:
```assembly
EQ
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 == value1) ? 1 : 0`

### 0x94 - NE
**Function**: Inequality comparison.
**Description**: Pops the top two values from the stack, compares them, and pushes 1 if they are not equal, otherwise pushes 0.
**Usage**:
```assembly
NE
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 != value1) ? 1 : 0`

### 0x96 - BOOLOR
**Function**: Boolean OR.
**Description**: Pops the top two values from the stack, performs a logical OR, and pushes the result.
**Usage**:
```assembly
BOOLOR
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 || value1)`

### 0x98 - BOOLAND
**Function**: Boolean AND.
**Description**: Pops the top two values from the stack, performs a logical AND, and pushes the result.
**Usage**:
```assembly
BOOLAND
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 && value1)`

### 0x9A - BITAND
**Function**: Bitwise AND.
**Description**: Pops the top two values from the stack, performs a bitwise AND, and pushes the result.
**Usage**:
```assembly
BITAND
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 & value1)`

### 0x9C - BITOR
**Function**: Bitwise OR.
**Description**: Pops the top two values from the stack, performs a bitwise OR, and pushes the result.
**Usage**:
```assembly
BITOR
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 | value1)`

### 0x9E - BITXOR
**Function**: Bitwise XOR.
**Description**: Pops the top two values from the stack, performs a bitwise XOR, and pushes the result.
**Usage**:
```assembly
BITXOR
```
**Stack Operation**:
- Before: `[value1, value2, ... ]`
- After: `[result, ... ]`
- Note: `result = (value2 ^ value1)`

### 0xA0 - BITSHR
**Function**: Bitwise shift right.
**Description**: Pops the top two values from the stack, shifts the second value to the right by the number of bits specified by the first value, and pushes the result.
**Usage**:
```assembly
BITSHR
```
**Stack Operation**:
- Before: `[shift_amount, value, ... ]`
- After: `[result, ... ]`
- Note: `result = (value >> shift_amount)`

### 0xA2 - BITSHL
**Function**: Bitwise shift left.
**Description**: Pops the top two values from the stack, shifts the second value to the left by the number of bits specified by the first value, and pushes the result.
**Usage**:
```assembly
BITSHL
```
**Stack Operation**:
- Before: `[shift_amount, value, ... ]`
- After: `[result, ... ]`
- Note: `result = (value << shift_amount)`

### 0xAA - PUSHIBB
**Function**: Push two immediate bytes.
**Description**: Pushes the next two bytes from the instruction stream onto the stack.
**Usage**:
```assembly
PUSHIBB byte1, byte2
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[byte1, byte2, ... ]`
- Note: `byte1` and `byte2` are the immediate bytes from the instruction stream.

### 0xAB - PUSHLOCALBB
**Function**: Push two local bytes.
**Description**: Pushes the values of two local variables at the specified byte offsets onto the stack.
**Usage**:
```assembly
PUSHLOCALBB offset1, offset2
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[local_value1, local_value2, ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the base pointer (BP).

### 0xAC - PUSHGLOBALBB
**Function**: Push two global bytes.
**Description**: Pushes the values of two global variables at the specified byte offsets onto the stack.
**Usage**:
```assembly
PUSHGLOBALBB offset1, offset2
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[global_value1, global_value2, ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the global pointer (GP).

### 0xAD - INCLOCALB
**Function**: Increment local byte.
**Description**: Increments the local variable at the specified byte offset by 1.
**Usage**:
```assembly
INCLOCALB offset
```
**Operation**:
- Before: `[local_value, ... ]`
- After: `[local_value + 1, ... ]`
- Note: `offset` is the byte offset from the base pointer (BP).

### 0xAE - INCLOCALBB
**Function**: Increment local byte by another local byte.
**Description**: Increments the local variable at the first specified byte offset by the value of the local variable at the second specified byte offset.
**Usage**:
```assembly
INCLOCALBB offset1, offset2
```
**Operation**:
- Before: `[local_value1, local_value2, ... ]`
- After: `[local_value1 + local_value2, ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the base pointer (BP).

### 0xAF - INCLOCALIB
**Function**: Increment local signed byte.
**Description**: Increments the local variable at the specified byte offset by 1, treating it as a signed value.
**Usage**:
```assembly
INCLOCALIB offset
```
**Operation**:
- Before: `[local_value, ... ]`
- After: `[local_value + 1, ... ]`
- Note: `offset` is the byte offset from the base pointer (BP).

### 0xB0 - INCGLOBALB
**Function**: Increment global byte.
**Description**: Increments the global variable at the specified byte offset by 1.
**Usage**:
```assembly
INCGLOBALB offset
```
**Operation**:
- Before: `[global_value, ... ]`
- After: `[global_value + 1, ... ]`
- Note: `offset` is the byte offset from the global pointer (GP).

### 0xB1 - INCGLOBALBB
**Function**: Increment global byte by another global byte.
**Description**: Increments the global variable at the first specified byte offset by the value of the global variable at the second specified byte offset.
**Usage**:
```assembly
INCGLOBALBB offset1, offset2
```
**Operation**:
- Before: `[global_value1, global_value2, ... ]`
- After: `[global_value1 + global_value2, ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the global pointer (GP).

### 0xB2 - DECGLOBALB
**Function**: Decrement global byte.
**Description**: Decrements the global variable at the specified byte offset by 1.
**Usage**:
```assembly
DECGLOBALB offset
```
**Operation**:
- Before: `[global_value, ... ]`
- After: `[global_value - 1, ... ]`
- Note: `offset` is the byte offset from the global pointer (GP).

### 0xB3 - DECGLOBALIB
**Function**: Decrement global signed byte.
**Description**: Decrements the global variable at the specified byte offset by 1, treating it as a signed value.
**Usage**:
```assembly
DECGLOBALIB offset
```
**Operation**:
- Before: `[global_value, ... ]`
- After: `[global_value - 1, ... ]`
- Note: `offset` is the byte offset from the global pointer (GP).

### PUSHIBB
**Function**: Push two immediate bytes.
**Description**: Pushes two immediate byte values onto the stack.
**Usage**:
```assembly
PUSHIBB X Y
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[Y, X, ... ]`

### PUSHLOCALBB
**Function**: Push two local bytes.
**Description**: Pushes two local variable values onto the stack, specified by the byte offsets.
**Usage**:
```assembly
PUSHLOCALBB offset1, offset2
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[local[offset2], local[offset1], ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the base pointer (BP).

### PUSHGLOBALBB
**Function**: Push two global bytes.
**Description**: Pushes two global variable values onto the stack, specified by the byte offsets.
**Usage**:
```assembly
PUSHGLOBALBB offset1, offset2
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[global[offset2], global[offset1], ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the global pointer (GP).

### INCLOCALBB
**Function**: Increment local byte by another local byte.
**Description**: Increments the local variable at the first byte offset by the value of the local variable at the second byte offset.
**Usage**:
```assembly
INCLOCALBB offset1, offset2
```
**Operation**:
- Before: `[local[offset1], local[offset2], ... ]`
- After: `[local[offset1] + local[offset2], local[offset2], ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the base pointer (BP).

### INCGLOBALBB
**Function**: Increment global byte by another global byte.
**Description**: Increments the global variable at the first byte offset by the value of the global variable at the second byte offset.
**Usage**:
```assembly
INCGLOBALBB offset1, offset2
```
**Operation**:
- Before: `[global[offset1], global[offset2], ... ]`
- After: `[global[offset1] + global[offset2], global[offset2], ... ]`
- Note: `offset1` and `offset2` are the byte offsets from the global pointer (GP).

### JIXB
**Function**: Jump indexed byte.
**Description**: Performs an indexed jump using byte offsets. 
**Usage**:
```assembly
JIXB
```
**Stack Operation**:
- Before: `[index, ... ]`
- After: `[ ... ]`
- Note: The actual jump address is computed using the index and a jump table.

### RETB
**Function**: Return with byte operand.
**Description**: Returns from a subroutine, adjusting the stack pointer by the byte operand.
**Usage**:
```assembly
RETB operand
```
**Stack Operation**:
- Before: `[ ... ]`
- After: `[ ... ]`
- Note: Adjusts the stack pointer by the value of `operand`.

### RETRESB
**Function**: Return with result and byte operand.
**Description**: Returns from a subroutine with a result, adjusting the stack pointer by the byte operand.
**Usage**:
```assembly
RETRESB operand
```
**Stack Operation**:
- Before: `[result, ... ]`
- After: `[result, ... ]`
- Note: Adjusts the stack pointer by the value of `operand`.

### JZB
**Function**: Jump if zero byte.
**Description**: Jumps to the specified address if the value on the top of the stack is zero, using a byte offset.
**Usage**:
```assembly
JZB offset
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[ ... ]`
- Note: `offset` is the byte offset to jump to if `value` is zero.

### JNZB
**Function**: Jump if not zero byte.
**Description**: Jumps to the specified address if the value on the top of the stack is not zero, using a byte offset.
**Usage**:
```assembly
JNZB offset
```
**Stack Operation**:
- Before: `[value, ... ]`
- After: `[ ... ]`
- Note: `offset` is the byte offset to jump to if `value` is not zero.

