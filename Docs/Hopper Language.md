**Note:**  - this is a work in progress between ChatGPT and me. It will improve as ChatGPT's knowledge of the language improves.

## Loops in Hopper

Here's a summary of the loop constructs in Hopper, including the use of `break` and `continue`:

### `loop`:
- The `loop` construct in Hopper is a simple infinite loop that repeats until explicitly exited using `break`.
- It doesn't have any built-in conditions or initialization, making it suitable for cases where you need to repeatedly execute a block of code.
- Example:
  ```hopper
  loop
  {
      // Code to repeat indefinitely
      if (condition)
      {
          break; // Exit the loop based on a condition
      }
  }
  ```

### `for`:
- The `for` loop in Hopper is similar to traditional `for` loops in other languages, allowing you to iterate over a range of values.
- It consists of an initialization, a condition, and an iteration statement, separated by semicolons.
- Example:
  ```hopper
  for (uint i = 0; i < 10; i++)
  {
      // Code to repeat 10 times
  }
  ```

### `foreach`:
- The `foreach` loop in Hopper is used to iterate over elements of a collection, such as a list or dictionary.
- It automatically iterates over each element, assigning it to a variable for processing.
- Example:
  ```hopper
  foreach (var element in myList)
  {
      // Process each element of the list
  }
  ```

### `while`:
- The `while` loop in Hopper repeats a block of code as long as a specified condition is true.
- It evaluates the condition before each iteration, so it may not execute at all if the condition is false initially.
- Example:
  ```hopper
  uint i = 0;
  while (i < 10)
  {
      // Code to repeat as long as i is less than 10
      i++;
  }
  ```

### `break`:
- The `break` statement is used to exit a loop prematurely, regardless of the loop's condition.
- It can be used in `for`, `foreach`, `while`, and `loop` constructs to immediately exit the innermost loop.
- Example:
  ```hopper
  loop
  {
      // Some condition to exit the loop
      if (condition)
      {
          break; // Exit the loop
      }
  }
  ```

### `continue`:
- The `continue` statement is used to skip the rest of the current iteration and start the next iteration of the loop.
- It can be used in `for`, `foreach`, `while`, and `loop` constructs to jump to the next iteration without executing the remaining code in the loop's body.
- Example:
  ```hopper
  for (uint i = 0; i < 10; i++)
  {
      // Skip even numbers
      if (i % 2 == 0)
      {
          continue; // Skip the rest of the loop body for even numbers
      }
      // Code here will only be executed for odd numbers
  }
  ```


### Hopper Switch Statement

The switch statement in Hopper allows you to conditionally execute code based on the value of a variable. It has several unique features
compared to other languages like C, C++, and C#.

#### Basic Syntax
The basic syntax of the switch statement in Hopper is as follows:

```hopper
switch (variable)
{
    case value1:
    {
        // code to execute if variable equals value1
    }
    case value2:
    {
        // code to execute if variable equals value2
    }
    // more cases...
    default:
    {
        // code to execute if variable does not match any case
    }
}
```

#### No Need for Break
Unlike C and its derivatives, Hopper **does not alow** the use of `break` statements after each case.
The code execution will **never** fall through to the next case. `break` is reserved for loop constructs.

#### Supported Variable Types
In Hopper, switch cases can be of various types including bool, char, byte, uint, int, and string. 

```hopper
string fruit = "Apple";
switch (fruit.ToLower())
{
    case "apple":
    {
        Screen.PrintLn("It's an apple!");
    }
    case "banana":
    {
        Screen.PrintLn("It's a banana!");
    }
    case "orange":
    {
        Screen.PrintLn("It's an orange!");
    }
    default:
    {
        Screen.PrintLn("Unknown fruit!");
    }
}
```

It also allows for flexible matching if your switch case `variable` is of `variant` type.

```hopper
switch (variable)
{
    case 'A':
    {
        Print("Uppercase A");
    }
    case 'a':
    {
        Print("Lowercase a");
    }
    case true:
    {
        Print("Boolean true");
    }
    case 42:
    {
        Print("The answer");
    }
    case "hello":
    {
        Print("The greeting");
    }
    default:
    {
        Print("Default");
    }
}
```

#### Stacking Case Labels
Hopper allows you to stack multiple case labels, making it more convenient to execute the same code for multiple values.

```hopper
switch (variable)
{
    case '1':
    case '2':
    case '3':
    {
        Print("One, Two, or Three");
    }
    default:
    {
        Print("Default");
    }
}
```

#### Integer Range Syntax
You can use range syntax in cases to match a range of values when those values are byte or char type.

```hopper
switch (variable)
{
    case 'a'..'z', 'A'..'Z':
    {
        Print("Alphabetic character");
    }
    case '0'..'9':
    {
        Print("Numeric character");
    }
    default:
    {
        Print("Default");
    }
}
```

#### Conclusion
The switch statement in Hopper offers flexibility and readability with its unique features like no fall-through,
support for various variable types in cases, stacking case labels, and integer range syntax. Understanding these
features can help you write more expressive and concise code in Hopper.





## Language Comparison

This table summarizes the key differences in syntax and features between Hopper, C, C#, and Java.


| Feature            | Hopper                                | C                          | C#                              | Java                            |
|----------------------|--------------------------------------|----------------------------|---------------------------------|---------------------------------|
| Type System          | Value types: int, uint, char<br>Reference types: list, string, dictionary | Primitive types: int, char, etc.<br>Structs, enums, unions, pointers | Primitive types: int, char, etc.<br>Reference types: classes, interfaces, delegates | Primitive types: int, char, etc.<br>Reference types: classes, interfaces, enums |
| Method Definition    | Methods defined with a code block between curly braces, no 'system' keyword | Functions and methods defined with curly braces, no special keyword | Methods defined with a code block between curly braces, 'system' keyword for system methods | Methods defined with a code block between curly braces, no special keyword |
| Variable Declaration | Requires type name, `var` only for loop iterators | Requires type name, no `var` keyword | `var` keyword for implicit type inference | `var` keyword for implicit type inference |
| Loop Statements      | Requires curly braces even for single statements | Requires curly braces even for single statements | Requires curly braces even for single statements | Requires curly braces even for single statements |
| Conditional Statements | Uses `while` for loops | Uses `while` for loops | Uses `while` for loops | Uses `while` for loops |
| Passing Arguments    | Uses `ref` keyword for passing arguments as references | Uses pointers for passing arguments by reference | Uses `ref` keyword for passing arguments as references | Uses `ref` keyword for passing arguments as references |
| String Concatenation | Uses `Build` method for efficiency, can also use `+` | Uses `strcat` function or `+` operator | Uses `StringBuilder` class or `+` operator | Uses `StringBuilder` class or `+` operator |
| Switch Statements    | No fall-through, no `break` needed | Fall-through unless `break` used | No fall-through, `break` needed | Fall-through unless `break` used |
| Expression vs Statement | Cannot use expression where statement is expected | Can use expression where statement is expected | Can use expression where statement is expected | Can use expression where statement is expected |

