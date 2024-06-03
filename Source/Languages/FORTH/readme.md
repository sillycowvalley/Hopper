# HopperFORTH Interpreter Documentation

## Introduction

HopperFORTH is a FORTH interpreter implemented in Hopper. It supports various stack operations, arithmetic operations, logical operations, and memory operations. This document provides a detailed reference guide and a user guide with examples.

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

- **`pick`** ( n -- n' )
  - Fetch the nth item from the stack.

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

- **`1+`** ( n -- n+1 )
  - Increment the top value on the stack by 1.

- **`1-`** ( n -- n-1 )
  - Decrement the top value on the stack by 1.

- **`2+`** ( n -- n+2 )
  - Increment the top value on the stack by 2.

- **`2-`** ( n -- n-2 )
  - Decrement the top value on the stack by 2.

- **`2*`** ( n -- n*2 )
  - Multiply the top value on the stack by 2.

- **`2/`** ( n -- n/2 )
  - Divide the top value on the stack by 2.

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

- **`<>`** ( n1 n2 -- flag )
  - Compare the top two values for inequality. Push -1 if not equal, 0 otherwise.

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

- **`."`** ( -- )
  - Print a string defined within double quotes.

### Control Structures

- **`:`** ( -- )
  - Start defining a new word.

- **`;`** ( -- )
  - End defining a new word.

- **`if ... else ... then`** ( flag -- )
  - Conditional branch with else.

- **`begin ... until`** ( -- )
  - Begin-Until loop.

- **`begin ... again`** ( -- )
  - Begin-Again loop.

- **`do ... loop`** ( limit index -- )
  - Set up a finite loop with the given index and limit.

- **`+loop`** ( n -- )
  - Add n to the loop index. Continue or exit the loop based on the condition.

- **`i`** ( -- index )
  - Push the current loop index onto the stack.

- **`j`** ( -- index )
  - Push the outer loop index onto the stack in a nested loop.

- **`leave`** ( -- )
  - Exit the current DO loop immediately, continuing execution after the loop.

- **`exit`** ( -- )
  - Exit the current word prematurely.

- **`begin ... while ... repeat`** ( -- )
  - Begin-While-Repeat loop.

### System Control

- **`bye`** ( -- )
  - Exit the interpreter.

### Arduino Specific Operations

- **`seconds`** ( -- n )
  - Fetch the current number of seconds since the program started.

- **`delay`** ( n -- )
  - Wait for n milliseconds.

- **`pin`** ( pin mode -- )
  - Set the mode of the given pin.

- **`in`** ( pin -- n )
  - Read the digital value from the given pin.

- **`out`** ( n pin -- )
  - Write the digital value to the given pin.

- **`led`** ( -- n )
  - Push the built-in LED pin number onto the stack.

- **`output`** ( pin -- )
  - Set the given pin to OUTPUT mode.

- **`input`** ( pin -- )
  - Set the given pin to INPUT mode.

### Additional Words

- **`.s`** ( -- )
  - Display the current contents of the stack.

- **`words`** ( -- )
  - List all defined words.

- **`nip`** ( n1 n2 -- n2 )
  - Drop the second item on the stack.

- **`tuck`** ( n1 n2 -- n2 n1 n2 )
  - Copy the second item to the top of the stack.

- **`2dup`** ( n1 n2 -- n1 n2 n1 n2 )
  - Duplicate the top two items on the stack.

- **`2drop`** ( n1 n2 -- )
  - Drop the top two items from the stack.

- **`2swap`** ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
  - Swap the top two pairs of items on the stack.

- **`2over`** ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  - Copy the second pair of items to the top of the stack.

- **`0=`** ( n -- flag )
  - Check if the top item is zero. Push -1 if zero, 0 otherwise.

- **`0<`** ( n -- flag )
  - Check if the top item is less than zero. Push -1 if true, 0 otherwise.

- **`0>`** ( n -- flag )
  - Check if the top item is greater than zero. Push -1 if true, 0 otherwise.

- **`max`** ( n1 n2 -- max )
  - Push the maximum value of the top two items on the stack.

- **`min`** ( n1 n2 -- min )
  - Push the minimum value of the top two items on the stack.

- **`depth`** ( -- n )
  - Push the current stack depth.

- **`sp`** ( -- n )
  - Push the current stack pointer value.

## User Guide

### Starting the Interpreter

To start the HopperFORTH interpreter, run the `HopperFORTH` program. You will see the prompt `>>>`.

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
>>> 5 3 <> .
-1
>>> 5 5 <> .
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
>>> 1 2 3 1 pick .
2
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

### Control Structures

#### Example: Conditional Branches

Use `if ... else ... then` for conditional branches.

```forth
>>> 5 3 < if ." Less" else ." Greater" then cr
Greater
>>> 5 3 > if ." Greater" else ." Less" then cr
Greater
```

#### Example: Loops

Use `begin ... until` and `begin ... again` for loops.

```forth
>>> : countdown 10 begin dup . 1- dup 0= until drop ;
>>> countdown
10 9 8 7 6 5 4 3 2 1 0
>>> : infinite 10 begin dup . 1- dup 0= if exit then again ;
>>> infinite
10 9 8 7 6 5 4 3 2 1 0
```

Use `do ... loop` and `i` for counted loops.

```forth
>>> : countup 0 10 do i . loop ;
>>> countup
0 1 2 3 4 5 6 7 8 9
```

Use `j` to access the outer loop index in nested loops.

```forth
>>> : nested-countup 3 0 do 3 0 do j . i . loop loop ;
>>> nested-countup
0 0 0 1 0 2 1 0 1 1 1 2 2 0 2 1 2 2
```

Use `leave` to exit a loop conditionally.

```forth
>>> : leave-example 5 0 do i 3 > if leave then i . loop ;
>>> leave-example
0 1 2 3
```

#### Example: Begin-While-Repeat Loop

Use `begin ... while ... repeat` for loops with conditions.

```forth
>>> : test-sum
  0 1                     ( Initialize the sum and the starting number )
  begin
    over .                ( Print the current sum )
    over over +           ( Add the top two values and leave the result on the stack )
    -rot                  ( Move the sum to the third position )
    swap
    drop                  ( Drop the duplicated sum )
    dup 10 <.             ( Check if the current number is less than 10 )
  while
    1+                    ( Increment the current number )
  repeat
  drop                    ( Remove the extra value )
  ." Sum: " .             ( Print the final sum )
;
>>> test-sum
```

Expected output:
```
0 1 3 6 10 15 21 28 36 45 55 Sum: 66
```

### Arduino Specific Operations

#### Example: Arduino Specific Operations

Use `seconds`, `delay`, `pin`, `in`, `out`, `led`, `output`, and `input` for Arduino-specific tasks.

```forth
>>> : blink
    led output         ( Set the built-in LED pin as OUTPUT )
    begin
        led -1 out     ( Turn the built-in LED on )
        250 delay      ( Wait for 0.25 second )
        led 0 out      ( Turn the built-in LED off )
        250 delay      ( Wait for 0.25 second )
    again 
;
>>> blink
```

#### Example: Benchmark

Use `seconds` to measure elapsed time for a simple loop.

```forth
>>> : benchmark
    seconds           ( Record start time )
    1000              ( Initialize loop counter )
    begin
        1-              ( Decrement counter )
        dup 0=          ( Check if counter is zero )
    until               ( Repeat until counter is zero )
    drop                ( Drop the loop counter )
    seconds
    swap - .            ( Calculate and print elapsed time )
    ." seconds"
;
>>> benchmark
```

### Exiting the Interpreter

#### Example: Exit

To exit the HopperFORTH interpreter, use the `bye` word.

```forth
>>> bye
Exiting HopperFORTH interpreter.
```

## Conclusion

This document provides a comprehensive reference and user guide for the HopperFORTH interpreter. With this guide, you should be able to effectively use HopperFORTH for various stack operations, arithmetic operations, logical operations, memory operations, and input/output operations. Happy coding!