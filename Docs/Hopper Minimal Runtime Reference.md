# Introduction to the Minimal Runtime API Reference

Welcome to the Minimal Runtime API Reference for the Hopper programming language. This document provides comprehensive documentation for each of the runtime units included in the Minimal Runtime environment, designed to support embedded and systems programming on resource-constrained devices.

## Overview of Units

The Minimal Runtime comprises several core units, each tailored to perform specific functionalities efficiently under tight resource constraints. Below is an overview of each unit and its primary responsibilities:

### System Unit
The `System` unit forms the backbone of the Minimal Runtime, providing fundamental system operations and utilities that are crucial for the runtime's operation. This unit includes essential services such as memory management, basic configuration, and system initialization procedures.

### IO Unit
The `IO` unit manages all input/output operations within the runtime. It provides the necessary tools for handling serial communication, including data transmission and reception, which are vital for interacting with external hardware and human-machine interfaces.

### Serial Unit
The `Serial` unit facilitates serial communication protocols. It includes functions for sending and receiving characters over serial interfaces, making it an essential component for data exchange in embedded systems.

### Diagnostics Unit
The `Diagnostics` unit offers tools for error detection and system diagnostics. It is designed to aid in debugging and ensures that the system maintains reliable operation, providing functionalities like error logging and system asserts.

### Char Unit
The `Char` unit includes utilities for character manipulation and examination. This unit is crucial for parsing text, handling strings, and performing character-based operations.

### String Unit
The `String` unit provides a rich set of string operations, including manipulation, conversion, and formatting capabilities. It supports essential string functions needed for creating, processing, and managing strings efficiently.

### Array Unit
The `Array` unit manages collections of elements, providing functionalities for element access, manipulation, and dynamic array operations. This unit is key to handling structured data efficiently.

### UInt Unit
The `UInt` unit contains functions for handling 16-bit unsigned integers. It includes mathematical operations, conversions, and other utilities specific to unsigned integer manipulation.

### Int Unit
The `Int` unit manages 16-bit signed integers and provides similar functionalities to the `UInt` unit but tailored for signed numbers. It includes operations for arithmetic, conversion, and bounds-checked manipulations.

### Byte Unit
The `Byte` unit offers utilities for byte manipulation and conversion operations. It is essential for data handling at the byte level, supporting operations necessary for low-level programming and interfacing with hardware.

### Bool Unit
The `Bool` unit includes functions for boolean value manipulation and conversions. It provides fundamental tools for boolean logic operations, decision making, and flow control within programs.

### Time Unit
The `Time` unit provides timing and delay functions, crucial for managing time-related operations like timeouts, delays, and measuring intervals.

## Structure of This Manual

This manual is structured into chapters, with each chapter dedicated to one of the units described above. Each chapter will detail the functions and usage of the respective unit, providing examples and best practices to help you effectively utilize the Minimal Runtime in your Hopper applications.

# Chapter 1: System Unit

The `System` unit in the Hopper Minimal Runtime serves as a foundational hub, designed to include and provide access to all other system-level units within the runtime environment. This central unit ensures that the essential components are accessible to any part of a Hopper application.

## Overview

Rather than offering direct functionalities, the `System` unit primarily aggregates all the core system units necessary for the operation of applications in Hopper. This aggregation simplifies the management and usage of the Hopper environment, promoting modularity and ease of access to crucial functionalities.

## Included Units

The `System` unit includes the following system units by default:

### Core Types and Utilities
- **`Char`**: Manages character-related operations and utilities.
- **`Bool`**: Provides basic boolean operations and conversions.
- **`Byte`**: Handles byte manipulations and conversions.
- **`Int`**: Supports operations on 16-bit signed integers.
- **`UInt`**: Supports operations on 16-bit unsigned integers.
- **`String`**: Facilitates string manipulation and operations.
- **`Array`**: Manages collections of elements, offering essential array functionalities.
- **`Time`**: Provides basic timing and delay functions.
- **`Type`**: Offers type-related utilities and functionalities, crucial for type safety and runtime type information.

### Communication and Diagnostic
- **`Serial`**: Enables serial communication, critical for data exchange and peripheral management.
- **`Diagnostics`**: Provides diagnostic tools and functionalities, aiding in debugging and system monitoring.

## Conclusion

The `System` unit does not directly provide system functionalities but acts as a crucial organizational framework that includes all necessary system units. This design allows for a cleaner and more structured approach to accessing system functionalities in Hopper applications, ensuring that developers can conveniently utilize the wide range of capabilities provided by the runtime.


# Chapter 2: IO Unit

The `IO` unit in the Hopper Minimal Runtime is designed to manage all input and output operations. This unit provides the necessary tools for handling serial communications, and potentially other forms of IO like displays.

## Overview

The `IO` unit simplifies interaction with hardware and external systems through a series of functions and utilities that abstract the complexities of direct device manipulation. This abstraction allows developers to write cleaner and more maintainable code while interacting with different input and output devices.

## Key Functions

### Serial Output

- **`Write(char c)`**
  - **Description**: Sends a character over the serial interface.
  - **Parameters**:
    - `c` (char): The character to send.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    Write('A');  // Send 'A' over the serial port
    ```

- **`Write(string s)`**
  - **Description**: Sends a string over the serial interface.
  - **Parameters**:
    - `s` (string): The string to send.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    Write("Hello, World!");  // Send "Hello, World!" over the serial port
    ```

- **`WriteLn()`**
  - **Description**: Sends a newline character over the serial interface, typically used to terminate lines in a transmission.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    WriteLn();  // Send a newline character
    ```

- **`WriteLn(string s)`**
  - **Description**: Sends a string followed by a newline character over the serial interface.
  - **Parameters**:
    - `s` (string): The string to send.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    WriteLn("Hello, World!");  // Send "Hello, World!" followed by a newline
    ```

### Serial Input

- **`ReadLn(ref string str)`**
  - **Description**: Reads a line of text from the serial interface until a newline character is encountered. This method handles user input interactively, allowing backspace for corrections and echoing each character to confirm input. It builds the string up character by character and returns it via the reference parameter.
  - **Parameters**:
    - `str` (ref string): A reference to a string where the input will be stored.
  - **Returns**: 
    - (bool): Returns `true` if the line was successfully read, indicating the presence of a newline character. Returns `false` if the reading was interrupted or failed.
  - **Usage Example**:
    ```hopper
    string userInput;
    if (ReadLn(ref userInput)) {
        WriteLn("You entered: " + userInput);
    } else {
        WriteLn("Input error or interruption.");
    }
    ```
  - **Special Handling**:
    - **Backspace**: Allows the user to correct mistakes by handling backspace characters. If the backspace key is pressed, the last character entered is removed from both the screen and the input buffer.
    - **Escape**: Clears the current input line if the escape key is pressed, allowing the user to start over. The method will also handle escape sequences by clearing the visible line on the display.


## Conclusion

The `IO` unit provides essential functionalities for managing input and output operations within Hopper applications, facilitating communication with external devices and systems. By abstracting hardware interactions, the `IO` unit allows developers to focus on the logic of their applications without worrying about the specifics of device communication protocols.

# Chapter 3: Serial Unit

The `Serial` unit in the Hopper Minimal Runtime provides essential functionalities for serial communication, enabling data transmission and reception over serial ports. This capability is fundamental for applications that require interaction with other computers, microcontrollers, sensors, or any serially communicable device.

## Overview

Serial communication is a common method for low-cost, reliable data exchange in embedded systems. The `Serial` unit abstracts the complexities of serial port operations, offering a simplified interface for sending and receiving data, which enhances portability and maintainability of the application code.

## Key Functions

### Data Transmission

- **`WriteChar(char c)`**
  - **Description**: Sends a single character over the serial port.
  - **Parameters**:
    - `c` (char): The character to send.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    Serial.WriteChar('H');  // Send the character 'H' over the serial port
    ```

### Data Reception

- **`ReadChar()`**
  - **Description**: Reads a single character from the serial port.
  - **Returns**: (char) The character received from the serial port.
  - **Usage Example**:
    ```hopper
    char received = Serial.ReadChar();  // Read a character from the serial port
    ```

### Availability Checking

- **`IsAvailable`**
  - **Description**: Checks if there is data available to read from the serial port.
  - **Returns**: (bool) True if data is available, otherwise false.
  - **Usage Example**:
    ```hopper
    if (Serial.IsAvailable)
    {
        char received = Serial.ReadChar();
        // Process received character
    }
    ```

## Conclusion

The `Serial` unit is an integral part of the Hopper Minimal Runtime, facilitating crucial communication capabilities for embedded systems. By providing easy-to-use functions for sending and receiving data, the `Serial` unit helps developers implement reliable and effective serial communication protocols without delving into the low-level intricacies of hardware interfaces. This unit ensures that applications can communicate seamlessly over serial interfaces, essential for a wide range of embedded applications.

# Chapter 4: Diagnostics Unit

The `Diagnostics` unit in the Hopper Minimal Runtime is designed to handle critical error management and system diagnostics. Given the minimalistic approach of the runtime, this unit includes a straightforward but essential function to manage system failures.

## Overview

The primary role of the `Diagnostics` unit is to provide a means for the system to halt operation in a controlled manner when unrecoverable errors occur. This functionality is crucial for preventing further error propagation and for maintaining system integrity in the face of critical failures.

## Key Function

### System Termination

- **`Die(byte error)`**
  - **Description**: Immediately halts system operations and provides an error code for diagnostic purposes. This function is intended to be called in scenarios where the system encounters an unrecoverable error and must cease operation to avoid corrupting data or causing additional errors.
  - **Parameters**:
    - `error` (byte): An error code that can be used to identify the cause of the termination. This could be logged or reported in a way that is accessible post-mortem for debugging and diagnostic analysis.
  - **Returns**: None
  - **Usage Example**:
    ```hopper
    if (criticalErrorDetected)
    {
        Diagnostics.Die(0x01);  // Halt the system and indicate the error type
    }
    ```

## Conclusion

While the `Diagnostics` unit is succinct, its functionality is essential for maintaining the reliability and safety of applications running within the Hopper Minimal Runtime. The ability to halt system operations in response to critical errors is a vital part of robust system design, ensuring that failures can be handled in a manner that minimizes negative

# Chapter 5: Char Unit

The `Char` unit in the Hopper Minimal Runtime provides a collection of functions dedicated to character manipulation and examination. This unit is vital for handling individual characters effectively within applications, supporting a range of operations from character classification to conversion.

## Overview

Character manipulation is a fundamental aspect of many programming tasks, including parsing user inputs, processing textual data, and interfacing with hardware that requires specific character protocols. The `Char` unit simplifies these tasks by offering a robust set of utilities that operate on the char data type.

## Key Functions

### Character Properties

- **`IsUpper(char this)`**
  - **Description**: Determines if the specified character is an uppercase letter.
  - **Parameters**:
    - `this` (char): The character to check.
  - **Returns**: (bool) True if the character is uppercase, false otherwise.
  - **Usage Example**:
    ```hopper
    if (Char.IsUpper('A'))
    {
        // Perform action if 'A' is uppercase
    }
    ```

- **`IsLower(char this)`**
  - **Description**: Determines if the specified character is a lowercase letter.
  - **Parameters**:
    - `this` (char): The character to check.
  - **Returns**: (bool) True if the character is lowercase, false otherwise.
  - **Usage Example**:
    ```hopper
    if (Char.IsLower('a'))
    {
        // Perform action if 'a' is lowercase
    }
    ```

### Conversion Functions

- **`ToUpper(char this)`**
  - **Description**: Converts the specified character to its uppercase equivalent.
  - **Parameters**:
    - `this` (char): The character to convert.
  - **Returns**: (char) The uppercase version of the character.
  - **Usage Example**:
    ```hopper
    char upper = Char.ToUpper('b');  // Convert 'b' to 'B'
    ```

- **`ToLower(char this)`**
  - **Description**: Converts the specified character to its lowercase equivalent.
  - **Parameters**:
    - `this` (char): The character to convert.
  - **Returns**: (char) The lowercase version of the character.
  - **Usage Example**:
    ```hopper
    char lower = Char.ToLower('G');  // Convert 'G' to 'g'
    ```

### Numeric and Special Character Checks

- **`IsDigit(char this)`**
  - **Description**: Determines if the specified character is a numeric digit (0-9).
  - **Parameters**:
    - `this` (char): The character to check.
  - **Returns**: (bool) True if the character is a digit, false otherwise.
  - **Usage Example**:
    ```hopper
    if (Char.IsDigit('5'))
    {
        // Perform action if '5' is a digit
    }
    ```

- **`IsWhitespace(char this)`**
  - **Description**: Determines if the specified character is a whitespace character, such as space, tab, or newline.
  - **Parameters**:
    - `this` (char): The character to check.
  - **Returns**: (bool) True if the character is whitespace, false otherwise.
  - **Usage Example**:
    ```hopper
    if (Char.IsWhitespace(' '))
    {
        // Perform action if character is a space
    }
    ```

## Conclusion

The `Char` unit is a critical component of the Hopper Minimal Runtime, facilitating detailed and efficient character handling. By providing a comprehensive set of functions for character classification, conversion, and checking, the `Char` unit enables developers to implement complex textual data manipulation and validation routines with ease and reliability.


# Chapter 5: Char Unit

The `Char` unit in the Hopper Minimal Runtime provides an extensive suite of functions dedicated to character manipulation and examination. This unit is essential for handling individual characters in a variety of contexts, including parsing, text processing, and data validation.

## Overview

Handling individual characters effectively is crucial in many programming scenarios. The `Char` unit simplifies tasks involving character analysis, conversion, and classification, thereby supporting a wide range of operations from user input validation to text formatting.

## Constants

Several constants are predefined in the `Char` unit for common character types, enhancing code readability and maintainability:

- **`EOL`**: Represents the end-of-line character (`\n`).
- **`Escape`**: The escape character (`\e`).
- **`Slash`**: The backslash character (`\`).
- **`Formfeed`**: The form feed page break character.
- **`Backspace`**: The backspace character.
- **`Break`**: The break character, often used as a stop signal.
- **`Tab`**: The horizontal tab character.

## Key Functions

### Character Classification

- **`IsUpper(char this)`**
  - **Description**: Determines if the specified character is uppercase.
- **`IsLower(char this)`**
  - **Description**: Determines if the specified character is lowercase.
- **`IsDigit(char this)`**
  - **Description**: Checks if the character is a numeric digit (0-9).
- **`IsLetter(char this)`**
  - **Description**: Determines if the character is an alphabetic letter.
- **`IsWhitespace(char this)`**
  - **Description**: Checks if the character is a whitespace (space, tab, newline, etc.).
- **`IsHexDigit(char this)`**
  - **Description**: Determines if the character is a hexadecimal digit (0-9, A-F, a-f).

### Character Conversion

- **`ToUpper(char this)`**
  - **Description**: Converts the character to its uppercase form.
- **`ToLower(char this)`**
  - **Description**: Converts the character to its lowercase form.

### Parsing and Utility Functions

- **`FromHex(char this)`**
  - **Description**: Converts a hexadecimal character to its numeric equivalent.
- **`ToString(char this)`**
  - **Description**: Converts the character to its string representation.

## Example Usage

Here's how you might use some of the functions in the `Char` unit:

```hopper
// Check if the character is uppercase
if (Char.IsUpper('A'))
{
    // Perform action if 'A' is uppercase
}

// Convert a hexadecimal character to its numeric value
uint num = Char.FromHex('B');  // Returns 11
```



# Chapter 7: Array Unit

The `Array` unit in the Hopper Minimal Runtime provides robust functionalities for managing collections of elements, enabling efficient operations on structured data. Arrays are indispensable in numerous programming scenarios, from data storage to complex algorithm implementations.

## Overview

Arrays in Hopper allow for the storage and manipulation of multiple items of the same type collectively. They are essential for efficiently handling data, allowing for batch operations on sets of elements and simplifying the management of data collections.

## Array Declaration

Arrays are declared with a specific type and size, and all elements are automatically initialized to zero:

    byte[1024] myArray;  // Declares an array of 1024 bytes

This statement initializes `myArray` with 1024 elements of type `byte`.

## Key Functions

### Element Access

Elements within an array are accessed by their index, starting from zero:

```hopper
    myArray[0] = 255;  // Set the first element to 255
    byte firstElement = myArray[0];  // Retrieve the first element
```

### Array Length

The size of an array is defined at the time of declaration but can be accessed using the `Count` property:

    uint arraySize = myArray.Count;  // Returns 1024 for myArray

### Iterating Over Arrays

Hopper supports iterating over arrays using the `foreach` loop, which simplifies accessing each element:

```hopper
    foreach (var element in myArray)
    {
        WriteLn(element.ToString());  // Outputs the string representation of each element
    }
```

The `foreach` loop uses `var` for the loop variable, which automatically infers the type from the array being iterated over. This feature enhances code readability and reduces the likelihood of type-related errors.

### Example Usage

Here is a comprehensive example demonstrating array declaration, manipulation, and iteration:

```hopper
    program ExampleArrayUsage
    {
        main()
        {
            // Declare an array of integers
            int[10] numbers;

            // Populate the array with square values
            for (int i = 0; i < numbers.Count; i++)
            {
                numbers[i] = i * i;  // Store the square of the index
            }

            // Iterate over the array and print each value
            foreach (var num in numbers)
            {
                WriteLn(num.ToString());
            }
        }
    }
```

## Conclusion

The `Array` unit provides essential functionalities for managing collections of data within the Hopper Minimal Runtime. With features such as automatic initialization, straightforward access by index, and efficient iteration capabilities, arrays are indispensable for handling structured data in applications.






# Chapter 8: UInt Unit

# Chapter 9: Int Unit

# Chapter 10: Byte Unit

# Chapter 11: Bool Unit

# Chapter 12: Time Unit





