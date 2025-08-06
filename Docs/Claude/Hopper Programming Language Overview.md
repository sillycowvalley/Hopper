# Hopper Programming Language Overview

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

10. **System and Library Methods:**
    - `system` keyword marks methods implemented by the runtime
    - `library` keyword marks methods implemented only in MCU/6502 runtimes
    - Example:
      ```hopper
      uint Read(file this, byte[] data, uint bufferSize) library;  // MCU/6502 only
      bool IsValid(file this) system;                             // All runtimes
      ```

11. **Nested Type Declarations:**
   - When declaring nested generic types, separate closing angle brackets with spaces
   - The tokenizer interprets `>>` as the right-shift operator, not as two closing brackets
   ```hopper
   <uint, <byte> > methodCode;     // Correct - space between > >
   <uint, <byte>> methodCode;      // Error - tokenizer sees >> as shift operator
   
   <string, <uint, string> > nestedMap;  // Correct for deeply nested types
   <string, <uint, string>> nestedMap;   // Error - same tokenizer issue
   ```

12. **Common Method Patterns:**
    - Try methods return bool with ref output:
      ```hopper
      bool TryParse(string content, ref uint returnValue)
      ```
    - Default parameter overloads:
      ```hopper
      Setup(uint baud, byte txPin, byte rxPin)
      Setup()
      Setup(uint baud)
      ```
    - Property backing fields:
      ```hopper
      uint defaultForeColour = Colour.MatrixGreen;
      uint ForeColour { get { return defaultForeColour; } set { defaultForeColour = value; }}
      ```

13. **Platform Abstractions:**
    - Hardware capabilities defined by conditional compilation:
      ```hopper
      #if defined(BOARD_HAS_LED)
          bool LED { get; set }
      #endif
      ```
    - Different implementations for different platforms:
      ```hopper
      #if defined(MCU)
          Delay(uint ms) system;
      #else
          Delay(uint ms)
          {
              long endTime = Millis + ms;
              loop
              {
                  if (Millis >= endTime) { break; }
              }
          }
      #endif
      ```

14. **Common Syntax Pitfalls to Avoid:**
   - Don't use `fn` or `func` keywords - methods are declared directly with their return type (or no return type for procedures)
   - Don't use `var` except as the implied iterator type in `foreach` loops
   - Always use C-style `for` loops: `for (uint i = 0; i < max; i++)`
   - Always use parentheses in `while` and `if` conditions: `while (condition)`, `if (condition)`
   - Use `byte` for pin numbers and small counts (0-255)
   - Remember Time.SampleMicros is a resolution setting (default 1000), not a counter
   - Time.Millis returns a `long` timestamp

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

This document focuses on Hopper's unique features and common patterns. For detailed information about standard library units and board support, refer to the Hopper Library Documentation.