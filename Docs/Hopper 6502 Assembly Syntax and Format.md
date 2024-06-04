### Hopper 6502 Assembly Syntax and Format

This document provides a comprehensive guide to the Hopper 6502 Assembly flavor. It is tailored to help future iterations of GPT understand and generate code that adheres to the Hopper 6502 Assembly conventions. The primary focus is on ensuring clarity, maintainability, and consistency in code generation.

#### General Formatting Rules

1. **Curly Braces:**
   - Opening curly braces `{` should typically be on their own line.
   - Example:
     ```assembly
     loop
     {
         // code block
     }
     ```

2. **Comments:**
   - Use `//` for comments, not `;`.
   - Example:
     ```assembly
     // This is a comment
     ```

3. **Indentation:**
   - Use consistent indentation for code blocks inside braces. Typically, a single level of indentation is two spaces.

4. **Line Breaks:**
   - Use line breaks to separate logical sections of code for better readability.

#### Instruction Format

1. **Labels and Functions:**
   - Define labels or function names followed by a colon `:`. However, labels within functions should be avoided in favor of structured control flow constructs like loops and conditionals.
   - Example:
     ```assembly
     MyFunction:
     ```

2. **Control Flow Constructs:**
   - Use structured control flow constructs (`if`, `loop`, `break`, `continue`) instead of jump instructions (`JMP`, `JZ`, etc.).

   - **If Statements:**
     - The supported flags for `if` include:
       - **C**: Carry flag is set.
       - **Z**: Zero flag is set.
       - **V**: Overflow flag is set.
       - **NC**: Carry flag is clear.
       - **NZ**: Zero flag is clear.
       - **NV**: Overflow flag is clear.
       - **PL**: Positive (negative flag is clear).
       - **MI**: Negative flag is set.
     - Example:
       ```assembly
       if (Z)
       {
           // code to execute if zero flag is set
       }
       else
       {
           // code to execute if zero flag is not set
       }
       if (NC)
       {
           // code to execute if carry flag is clear
       }
       ```

   - **Loop Statements:**
     ```assembly
     loop
     {
         // code block
         if (condition) { break; }
         if (condition) { continue; }
     }
     ```

#### Switch Statements
- Switch statements require curly braces for each case, even for single statements.
- Cases never fall through, meaning there is no need for a `break` statement within cases.
- Switch can operate on registers A, X, and Y. If X or Y is chosen, it can be optimized into a jump table under the following conditions:
  - **X or Y must be used** (not A).
  - Each switch case may only have a single subroutine call in it (no more, no less).
  - There must be a default case.
  - Number of cases, including default, must be greater than 8.
  - The switch must be followed immediately by `return` or the end of the method.
- **Note:** These strict conditions are required because the optimization effectively replaces subroutine calls with `JMP <subroutine>`, meaning all possible paths must resolve to a simple `JMP <subroutine>`.

```assembly
switch (X)
{
    case 0x00:
    {
        Subroutine1();
    }
    case 0x01:
    {
        Subroutine2();
    }
    default:
    {
        DefaultSubroutine();
    }
}
return;
```

#### Zero Page Variables

1. **Variable Declaration:**
   - Declare zero page variables at the beginning of the program using the `const` keyword.
   - Example:
     ```assembly
     const byte VAR1 = 0x00;
     const byte VAR2 = 0x01;
     ```

2. **Usage:**
   - Use zero page variables for frequently accessed data to optimize performance.
   - Example:
     ```assembly
     LDA VAR1
     STA VAR2
     ```

#### Memory and Stack Operations

1. **Load and Store:**
   - Use `LDA`, `STA`, `LDX`, `STX`, `LDY`, and `STY` for loading and storing data.
   - Example:
     ```assembly
     LDA #0x10
     STA VAR1
     ```

2. **Stack Operations:**
   - Use `PHA`, `PLA`, `PHP`, and `PLP` for pushing and pulling data to/from the stack.
   - Example:
     ```assembly
     PHA
     PLA
     ```

#### Function Calls and Returns

1. **Function Calls:**
   - Use `JSR` to call a subroutine. However, for clarity and maintainability, encapsulate function calls within meaningful method names.
   - Example:
     ```assembly
     Append();
     ```

2. **Function Returns:**
   - Use `RTS` to return from a subroutine. This is usually implied and not explicitly written in structured methods.
   - A synonym for `RTS` in Hopper 6502 Assembly is `return;`.
   - Example:
     ```assembly
     return;
     ```

#### Encapsulation and Friend Classes

1. **Friend Classes:**
   - Use the `friend` keyword to allow specific units to access private methods of another unit.
   - Example:
     ```assembly
     unit List
     {
         friend GC;
     }
     ```

#### Preprocessor Directives

1. **Define Symbols**:
   ```assembly
   #define SYMBOL_NAME
   ```

2. **Conditional Compilation**:
   ```assembly
   #ifdef SYMBOL_NAME
   // Code here
   #endif
   ```

3. **Boolean Expressions in Preprocessor Directives**:
   - Supports `#if defined(symbol)`, `||`, `&&`, `!`, and parentheses for boolean expressions.
   - Example:
     ```assembly
     #if defined(SYMBOL1) && (defined(SYMBOL2) || !defined(SYMBOL3))
     // Code here
     #endif
     ```

#### Program Definition

1. **Program Structure**:
   ```assembly
   program ProgramName
   {
       // Code here
   }
   ```

#### Module Imports

1. **Module Imports**:
   ```assembly
   uses "moduleName"
   ```

#### Predefined Macros and Symbols

1. **ROM_xxx Symbols**:
   - These symbols affect the 6502 start vector address and the layout of the generated Intel IHex file.
   - `ROM_16K`: Sets the origin to 0xC000.
   - `ROM_8K`: Sets the origin to 0xE000.
   - `ROM_4K`: Sets the origin to 0xF000.
   - `ROM_1K`: Sets the origin to 0xFC00.
   - If none of these are defined, it defaults to a 32K ROM with an origin of 0x8000.

2. **CPU_xxx Symbols**:
   - These symbols target different 6502 flavors.
   - `CPU_6502`: Original MOS 6502 instruction set.
   - `CPU_65C02S`: Expanded instruction set of the 65C02S.
   - `CPU_65UINO`: Similar to ROM_4K and CPU_6502.

### Sample Code Structure

Here is an example of a properly formatted Hopper 6502 Assembly code block:

```assembly
unit MyUnit
{
    // Declare zero page variables
    const byte VAR1 = 0x00;
    const byte VAR2 = 0x01;

    MyFunction()
    {
        // Load data into VAR1
        LDA #0x10
        STA VAR1

        // Conditional check
        if (Z)
        {
            // Code block
            LDA VAR1
            STA VAR2
        }
        else
        {
            // Code block
            LDA VAR2
            STA VAR1
        }

        // Loop example
        loop
        {
            LDA VAR1
            if (Z) { break; }

            INC VAR1
            if (C) { continue; }

            LDA VAR1
            STA VAR2
        }
    }

    MySwitchFunction()
    {
        // Switch example
        switch (X)
        {
            case 0x00:
            {
                Subroutine1();
            }
            case 0x01:
            {
                Subroutine2();
            }
            default:
            {
                DefaultSubroutine();
            }
        }
    }
}
```

### Key Takeaways

- **Consistency**: Always follow the same style for braces, comments, and indentation.
- **Structure**: Use structured control flow constructs instead of labels and jumps for better readability and maintainability.
- **Encapsulation**: Use the `friend` keyword to manage access to private methods across units.
- **Optimization**: Utilize zero page variables and stack operations for efficient memory management.
- **Method Calls**: Use method calls rather than direct `JSR` instructions for better readability and maintainability.
- **Switch Statements**: Require curly braces for each case, do not fall through, and can switch on A, X, and Y. Optimize into a jump table under specific conditions.
- **Predefined Symbols**: Understand and use predefined symbols for ROM sizes and CPU types to control the start vector address and the layout of the generated

 Intel IHex file.

This updated guide ensures that future iterations of GPT can generate and understand Hopper 6502 Assembly code that is clean, efficient, and maintainable.