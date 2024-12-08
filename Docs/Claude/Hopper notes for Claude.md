# Hopper Programming Language Documentation

## Introduction

Hopper is a modern programming language designed to run efficiently on small devices like microcontrollers and 8-bit microprocessors. Hopper programs are single-threaded, run on a stack-based virtual machine, and have a garbage collector that collects objects not reachable from the stack.

## Key Language Features

1. **No `void` Keyword:**
   - Methods with no return type simply omit the return type specification
   - Applies to both regular methods and delegate declarations
   ```hopper
   ProcessData(uint value)  // Procedure (no return type)
   {
       // Process the data
   }
   
   delegate MyCallback(uint value);  // Delegate with no return type
   ```

2. **No `null` Keyword:**
   - For delegate initialization checks, declare an uninitialized delegate at global level
   ```hopper
   delegate MyCallback(uint value);
   MyCallback nullCallback;  // Global uninitialized delegate
   
   ProcessWithCallback(MyCallback callback)
   {
       if (callback != nullCallback)  // Check if callback is initialized
       {
           callback(42);
       }
   }
   ```

3. **List Operations:**
   - No direct indexing with square brackets
   - Use methods: `Append`, `Remove`, `GetItem`, `SetItem`, `Insert`, `Clear`, `Contains`
   - Example:
     ```hopper
     <string> list;
     list.Append("test");
     string item = list.GetItem(0);  // Not list[0]
     ```

4. **Switch Statements:**
   - Never fall through, no `break` required
   - Each case block terminates automatically
   - Always use curly braces `{}` even for single statements

5. **Loop Keyword:**
   - Use `loop` for infinite loops (shorthand for `while(true)`)

6. **Entry Point:**
   - Named `Hopper`, not `main`
   - Each program and unit resides in a single file

7. **Zero Initialization:**
   - Variables are always zero initialized:
     ```hopper
     string myString;  // ""
     uint a;          // 0
     float bob;       // 0.0
     bool ok;         // false
     ```

8. **Type Safety:**
   - `char` and `byte` are not the same type
   - `uint` and `byte` are not the same
   - Casting required between different types

9. **Array Definitions:**
   - Format is `type[size] name`, not `type name[size]`
   - Only `byte` or `char` arrays can be directly initialized
   - No objects, use `byte[PieceSize * PieceSize] shape;`

10. **Time and Random Numbers:**
    - Use `Time.Millis` instead of `System.TickCount`
    - For random numbers: `((Time.Millis).GetByte(0) % ShapeCount)`

11. **Error Handling:**
    - No exceptions
    - Use `Diagnostics.Die(byte reason)`:
      - 0x01: list index out of range
      - 0x02: array index out of range
      - 0x03: no entry for key in dictionary
      - 0x04: division by zero
      - 0x05: string index out of range
      - 0x06: call stack overflow
      - 0x07: argument stack overflow
      - 0x08: failed dynamic cast
      - 0x09: invalid variant type
      - 0x0A: feature not implemented
      - 0x0B: system failure
      - 0x0C: memory allocation failure
      - 0x0D: numeric overflow
      - 0x0E: child exe error
      
12. **System and Library Methods:**
    - `system` keyword marks methods implemented by the runtime
    - `library` keyword marks methods implemented only in MCU/6502 runtimes
    - Example:
      ```hopper
      uint Read(file this, byte[] data, uint bufferSize) library;  // MCU/6502 only
      bool IsValid(file this) system;                             // All runtimes
      ```

## Program Structure Example

```hopper
program Example
{
    uses "/Source/Library/Boards/CytronMakerPiRP2040"
    
    Setup()  // No void keyword
    {
        MCU.PinMode(Board.BuiltInLED, MCU.PinModeOption.Output);
    }
    
    Hopper()  // Entry point
    {
        Setup();
        loop  // Infinite loop
        {
            MCU.DigitalWrite(Board.BuiltInLED, true);
            Time.Delay(1000);
            MCU.DigitalWrite(Board.BuiltInLED, false);
            Time.Delay(1000);
        }
    }
}
```

## Unit Structure Example

```hopper
unit MCU
{
    uses "/Source/System/System"
    
    enum PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }

    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;  // No void return type
}
```

## Records Example

```hopper
record Word
{
    string Name;
    <string> Definition;
}

// Usage
Word newWord;  // Zero initialized
<Word> words;  // Empty list
words.Append(newWord);
```

## Conclusion

This document focuses on Hopper's unique features and common pitfalls. For general programming concepts and standard language features, refer to Pascal or C-like language documentation.
