# GTPFORTH Interpreter Documentation

## Introduction

GTPFORTH is a FORTH interpreter implemented in Hopper. It supports various stack operations, arithmetic operations, logical operations, and memory operations. This document provides a detailed reference guide and a user guide with examples.

## Reference Guide

### Stack Operations

- **`dup`** ( n -- n n )
  - Duplicate the top value on the stack.

- **`drop`** ( n -- )
  - Remove the top value from the stack.

- **`swap`** ( n1 n2 -- n2 n1 )
  - Swap the top two values on the stack.

- **`over`** ( n1 n2 -- n1 n2 n1 )
  - Copy the second value on the stack to the top.

- **`rot`** ( n1 n2 n3 -- n2 n3 n1 )
  - Rotate the top three values on the stack.

- **`-rot`** ( n1 n2 n3 -- n3 n1 n2 )
  - Rotate the top three values on the stack in the opposite direction.

### Arithmetic Operations

- **`+`** ( n1 n2 -- n1+n2 )
  - Add the top two values on the stack.

- **`-`** ( n1 n2 -- n1-n2 )
  - Subtract the top value from the second value on the stack.

- **`*`** ( n1 n2 -- n1*n2 )
  - Multiply the top two values on the stack.

- **`/`** ( n1 n2 -- n1/n2 )
  - Divide the second value by the top value on the stack.

- **`mod`** ( n1 n2 -- n1%n2 )
  - Compute the remainder of the division of the second value by the top value on the stack.

- **`negate`** ( n -- -n )
  - Negate the top value on the stack.

- **`abs`** ( n -- |n| )
  - Compute the absolute value of the top value on the stack.

### Logical Operations

- **`and`** ( n1 n2 -- n1&n2 )
  - Perform a bitwise AND on the top two values on the stack.

- **`or`** ( n1 n2 -- n1|n2 )
  - Perform a bitwise OR on the top two values on the stack.

- **`xor`** ( n1 n2 -- n1^n2 )
  - Perform a bitwise XOR on the top two values on the stack.

- **`invert`** ( n -- ~n )
  - Perform a bitwise NOT on the top value on the stack.

### Comparison Operations

- **`=`** ( n1 n2 -- flag )
  - Compare the top two values for equality. Push -1 if equal, 0 otherwise.

- **`<`** ( n1 n2 -- flag )
  - Compare the second value with the top value. Push -1 if the second value is less, 0 otherwise.

- **`>`** ( n1 n2 -- flag )
  - Compare the second value with the top value. Push -1 if the second value is greater, 0 otherwise.

### Memory Operations

- **`!`** ( n addr -- )
  - Store a value in memory at the given address.

- **`@`** ( addr -- n )
  - Fetch a value from memory at the given address.

- **`c!`** ( byte addr -- )
  - Store a byte in memory at the given address.

- **`c@`** ( addr -- byte )
  - Fetch a byte from memory at the given address.

### Input/Output Operations

- **`emit`** ( n -- )
  - Output the character represented by the top value on the stack.

- **`cr`** ( -- )
  - Output a carriage return and line feed.

- **`key`** ( -- n )
  - Read a single character from the input and push its ASCII value onto the stack.

- **`.`** ( n -- )
  - Print the top value on the stack.

### Control Structures

- **`:`** ( -- )
  - Start defining a new word.

- **`;`** ( -- )
  - End defining a new word.

### System Control

- **`bye`** ( -- )
  - Exit the interpreter.

## User Guide

### Starting the Interpreter

To start the GTPFORTH interpreter, run the `GTPFORTH` program. You will see the prompt `>>>`.

### Defining and Using Words

#### Example: Defining a Word

Define a word `squared` that squares the top value on the stack.

```forth
>>> : squared dup * ;
Defined word: squared
```

#### Example: Using a Defined Word

Push a number onto the stack and use the `squared` word.

```forth
>>> 4 squared .
16
```

### Arithmetic Operations

#### Example: Basic Arithmetic

Perform basic arithmetic operations.

```forth
>>> 5 3 + .
8
>>> 10 2 - .
8
>>> 6 7 * .
42
>>> 20 4 / .
5
>>> 10 3 mod .
1
```

### Logical Operations

#### Example: Logical AND, OR, XOR, and NOT

Perform logical operations on the stack values.

```forth
>>> 6 3 and .
2
>>> 6 3 or .
7
>>> 6 3 xor .
5
>>> 6 invert .
-7
```

### Comparison Operations

#### Example: Equality, Less Than, Greater Than

Compare stack values.

```forth
>>> 5 5 = .
-1
>>> 5 3 = .
0
>>> 3 5 < .
-1
>>> 5 3 < .
0
>>> 5 3 > .
-1
>>> 3 5 > .
0
```

### Stack Manipulation

#### Example: Stack Operations

Manipulate stack values.

```forth
>>> 1 2 dup .
2
>>> 1 2 drop .
1
>>> 1 2 swap .
2 1
>>> 1 2 over .
1 2 1
>>> 1 2 3 rot .
2 3 1
>>> 1 2 3 -rot .
3 1 2
```

### Memory Operations

#### Example: Memory Store and Fetch

Store and fetch values from memory.

```forth
>>> 42 10 ! ( Store 42 at address 10 )
>>> 10 @ .
42
>>> 255 20 c! ( Store byte 255 at address 20 )
>>> 20 c@ .
255
```

### Input/Output Operations

#### Example: Character Output and Input

Output characters and read input.

```forth
>>> 65 emit ( Outputs 'A' )
A
>>> cr ( Outputs a new line )
>>> key . ( Waits for a key press and outputs its ASCII value )
65
```

### Exiting the Interpreter

#### Example: Exit

To exit the GTPFORTH interpreter, use the `bye` word.

```forth
>>> bye
Exiting GTPFORTH interpreter.
```

### Putting It All Together

#### Example: Calculating the Square of a Number

Define a word `square` and use it to calculate the square of a number.

```forth
>>> : square dup * ;
Defined word: square
>>> 9 square .
81
```

## Conclusion

This document provides a comprehensive reference and user guide for the GTPFORTH interpreter. With this guide, you should be able to effectively use GTPFORTH for various stack operations, arithmetic operations, logical operations, memory operations, and input/output operations. Happy coding!