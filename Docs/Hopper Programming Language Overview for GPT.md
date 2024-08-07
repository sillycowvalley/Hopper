### Hopper Programming Language Overview for GPT

#### Introduction
- **Purpose**: Hopper is a modern programming language optimized for small devices like microcontrollers and 8-bit microprocessors. It provides a structured alternative to Python.
- **Execution**: Single-threaded, stack-based virtual machine with garbage collection.

#### Key Hopper Language Features
- **Switch Statements**: No fall-through, automatic case termination.
- **Loop Keyword**: `loop` creates an infinite loop.
- **No `void` Keyword**: Omit return type for void methods.
- **Array Definitions**: `type[size] name`.
- **Type System**: Strict, no implicit type inference.
- **Single File Programs**: Each program/unit in a single file.
- **Delegates**: Typed function pointers.
- **Entry Point**: Named `Hopper`.
- **Garbage Collection**: Stack-based determination of live objects.
- **Boolean Expressions**: Only boolean expressions for conditionals.
- **No Objects**: Direct array allocation syntax.
- **Zero Initialization**: Variables are zero-initialized automatically.
- **Type Safety**: Strict type distinctions.
- **Diagnostics.Die Method**: Handles critical failures with specific error codes.
- **Scope Restrictions**: Methods, functions, enumerations, and constant declarations can only be made at the unit/program level. Function-local declarations of these are not supported.
- **Curly Braces Requirement**: All control structures must use curly braces, even for single statement blocks.

### Programs and Units
- **Program**: Main entry point `Hopper` method.
  ```hopper
  program Blink
  {
      uses "/Source/Library/Boards/PiPico"
      
      Hopper()
      {
          // code
      }
  }
  ```
- **Unit**: Reusable components or libraries.
  ```hopper
  unit MCU
  {
      uses "/Source/System/System"
      uses "/Source/System/Runtime"
      uses "/Source/System/IO"

      flags PinModeOption { Input = 0x00, Output = 0x01, InputPullup = 0x02, InputPulldown = 0x03 }
      enum PinStatus { Low = 0, High = 1, Change = 2, Falling = 3, Rising = 4 }
      delegate PinISRDelegate(byte pin, PinStatus status);

      PinMode(byte pin, PinModeOption pinMode) library;
      bool DigitalRead(byte pin) library;
      DigitalWrite(byte pin, bool value) library;
      uint AnalogRead(byte pin) library;
      AnalogWrite(byte pin, uint value) library;
      AnalogWriteResolution(byte bits) library;
      bool AttachToPin(byte pin, PinISRDelegate gpioISR, PinStatus status) library;
      bool InterruptsEnabled { get library; set library; }
  }
  ```

### Loop Constructs
- **`loop`**: Infinite loop.
  ```hopper
  loop { if (condition) { break; } }
  ```
- **`for`**: Standard for loop.
  ```hopper
  for (uint i = 0; i < 10; i++) { // code }
  ```
- **`foreach`**: Iterates over collections.
  ```hopper
  foreach (var element in myList) { // code }
  ```
- **`while`**: Repeats while condition is true.
  ```hopper
  uint i = 0; while (i < 10) { i++; }
  ```
- **`break`**: Exits the loop.
- **`continue`**: Skips to the next iteration.

### Switch Statements
- No fall-through, automatic case termination.
- Supports types: `bool`, `char`, `byte`, `uint`, `int`, `string`.
  ```hopper
  switch (variable) {
      case value1: { // code }
      case value2: { // code }
      default: { // code }
  }
  ```

### Delegates
- Typed function pointers for callbacks and event handling.
  ```hopper
  delegate void ButtonEventHandler(byte pin, PinStatus status);
  void ButtonPressedHandler(byte pin, PinStatus status) { // code }
  ButtonEventHandler buttonHandler = ButtonPressedHandler;
  ```

### Language Comparison
- **Type System**: Value types (int, uint, char), reference types (list, string, dictionary). Hopper enforces strict type distinctions.
- **Method Definition**: Code blocks are defined without a `void` keyword. Omit return type for methods that do not return a value.
- **Variable Declaration**: Explicit type name required for all variables, except for loop iterators where `var` is used.
- **Loop Statements**: Curly braces are mandatory for all control structures, including single-statement blocks, ensuring clear and structured code.
- **Switch Statements**: No fall-through, no `break` needed, eliminating the common fall-through bug in C/C++/C#.
- **Expression vs Statement**: Hopper differentiates strictly between expressions and statements; expressions cannot be used where statements are expected.
- **Scope Restrictions**: Methods, functions, enumerations, and constants can only be declared at the unit/program level, not within other methods or functions.

### Best Practices and Common Mistakes
- **Constants**: Declare constants at the program or unit level, not within functions.
  ```hopper
  // Correct
  program Blink
  {
      const byte ledPin = 13;

      Hopper()
      {
          // use ledPin
      }
  }

  // Incorrect
  program Blink
  {
      Hopper()
      {
          const byte ledPin = 13; // This is not allowed
      }
  }
  ```
- **Namespace Qualification**: Ensure functions and properties are used correctly within their context. If a `uses` statement is provided, members can be accessed directly without prefixing.
  ```hopper
  // Correct
  program Example
  {
      uses "/Source/Library/Boards/PiPico"

      Hopper()
      {
          LED = true;
          Delay(500);
      }
  }

  // Unnecessary qualification
  program Example
  {
      uses "/Source/Library/Boards/PiPico"

      Hopper()
      {
          GPIO.LED = true; // Unnecessary qualification
          Time.Delay(500); // Unnecessary qualification
      }
  }
  ```

### Minimal Runtime API

#### System Unit
- Aggregates core system units: `Char`, `Bool`, `Byte`, `Int`, `UInt`, `String`, `Array`, `Time`, `Type`, `Serial`, `Diagnostics`.

#### IO Unit
- Manages input/output operations, serial communication.
  - **Write(char c)**: Sends a character.
  - **Write(string s)**: Sends a string.
  - **WriteLn()**: Sends a newline.
  - **WriteLn(string s)**: Sends a string with a newline.
  - **ReadLn(ref string str)**: Reads a line of text interactively.

#### Serial Unit
- Facilitates serial communication.
  - **WriteChar(char c)**: Sends a character.
  - **ReadChar()**: Reads a character.
  - **IsAvailable**: Checks if data is available.

#### Diagnostics Unit
- Manages error detection and system diagnostics.
  - **Die(byte error)**: Halts system operations with an error code.

#### Char Unit
- Character manipulation and examination.
  - **IsUpper(char this)**: Checks if uppercase.
  - **IsLower(char this)**: Checks if lowercase.
  - **ToUpper(char this)**: Converts to uppercase.
  - **ToLower(char this)**: Converts to lowercase.
  - **IsDigit(char this)**: Checks if digit.
  - **IsWhitespace(char this)**: Checks if whitespace.

#### String Unit
- String operations and manipulation.
  - **Create(char[] chars)**: Creates a string from characters.
  - **Concat(string s1, string s2)**: Concatenates strings.
  - **Length(string s)**: Returns string length.
  - **Substring(string s, uint start, uint length)**: Extracts a substring.
  - **ToUpper(string s)**: Converts to uppercase.
  - **ToLower(string s)**: Converts to lowercase.
  - **IndexOf(string s, char c)**: Finds character index.
  - **Contains(string s, string substr)**: Checks if contains substring.

#### Array Unit
- Manages collections of elements.
  - **Element Access**: `myArray[0] = value`.
  - **Array Length**: `myArray.Count`.
  - **Iteration**: `foreach (var element in myArray) { // code }`.

#### UInt Unit
- 16-bit unsigned integer operations.
  - **Add(uint a, uint b)**: Adds two integers.
  - **Subtract(uint a, uint b)**: Subtracts integers.
  - **Max(uint a, uint b)**: Returns maximum.
  - **Min(uint a, uint b)**: Returns minimum.
  - **ToString(uint value)**: Converts to string.

#### Int Unit
- 16-bit signed integer operations.
  - **Add(int a, int b)**: Adds two integers.
  - **Subtract(int a, int b)**: Subtracts integers.
  - **Max(int a, int b)**: Returns maximum.
  - **Min(int a, int b)**: Returns minimum.
  - **ToString(int value)**: Converts to string.

#### Byte Unit
- Byte manipulation and conversion.
  - **And(byte a, byte b)**: Bitwise AND.
  - **Or(byte a, byte b)**: Bitwise OR.
  - **Xor(byte a, byte b)**: Bitwise XOR.
  - **Not(byte a)**: Bitwise NOT.
  - **ToHexString(byte value)**: Converts to hex string.

#### Bool Unit
- Boolean value manipulation and conversions.
  - **ToString(bool value)**: Converts to string.
  - **Not(bool value)**: Logical NOT.

#### Time Unit
- Timing and delay functions.
  - **Delay(uint milliseconds)**: Delays execution.
  - **Millis()**: Returns milliseconds since start.

#### GPIO Unit
- Handles general-purpose input/output operations for microcontrollers.
  - **Properties**:
    - **LED**: Controls the built-in LED.
      - **Set**: Configures pin mode and writes the value.
      - **Get**: Returns the current state.
    - **LEDR**, **LEDG**, **LEDB**: Control red, green, and blue LEDs if available.
    - **A0**, **A1**, **A2**, **A3**, **A4**, **A5**: Reads the value from analog pins.
  - **Usage**:
    ```hopper
    GPIO.LED = true;  // Turns on the built-in LED
    GPIO.LED = false; // Turns off the built-in LED
    GPIO.LED = !GPIO.LED; // Toggles the LED state
    ```

### Lists and Dictionaries
- **Lists**: Anonymous types that can store a collection of elements.
  - Example of list declaration and usage:
    ```hopper
    <uint> myList;
    myList.Append(1);
    myList.Append(2);
    myList.Append(3);

    foreach (var element in myList)
    {
        Write(element.ToString() + " ");
    }
    ```
- **Dictionaries**: Anonymous types that map keys to values.
  - Example of dictionary declaration and usage:
    ```hopper
    <string, bool> stringCases;
    stringCases["example"] = true;
    
    <uint, string> nameIndices;
    nameIndices[1] = "First";
    nameIndices[2] = "Second";
    
    if (stringCases["example"] == true)
    {
        WriteLn(nameIndices[1]);  // Outputs "First"
    }
    ```

### Typical Board Unit

```hopper
unit Board // PiPico
{
    #define RPIPICO
    #define MCU_BOARD_DEFINED
    #define MCU_BOARD_RP2040
    #define FORMFACTOR_PICO
    #define BOARD_HAS_LED
    #define BOARD_HAS_I2C
    #define BOARD_HAS_SPI
    #define BOARD_HAS_A0
    #define BOARD_HAS_A1
    #define BOARD_HAS_A2
    #define BOARD_HAS_A3
    const byte BuiltInLED = 25;
    const byte BuiltInA0 = 26;
    const byte BuiltInA1 = 27;
    const byte BuiltInA2 = 28;
    const byte BuiltInA3 = 29;
    // ... (other constants for pins)
    uses "/Source/Library/MCU"
    string BoardName { get { return "PiPico"; } }
}
```

### MCU Unit

```hopper
unit MCU
{
#if !defined(MCU_BOARD_DEFINED)
    #error "Find your board under /Source/Library/Boards, add 'uses' for it in your program"
#endif
#if !defined(SERIAL_CONSOLE)
  #define SERIAL_CONSOLE // for IO.hs if there is no alternative screen (like an LCD for example)
#endif
#if !defined(MCU)
  #define MCU            // for correct versions of System APIs (like Time.Delay(..) for example)
#endif
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/IO"
    uses "GPIO"
    uses "SPI"
#ifdef BITBANG_I2C
    uses "/Source/Minimal/I2C"
#else  
  #if defined(BOARD_HAS_I2C)
    uses "Wire"
  #endif
#endif
    uses "SD"
    uses "Timer"
#if defined(BOARD_HAS_WIFI)
    uses "/Source/System/WiFi"
    uses "WebClient"
    uses "WebServer"
#endif
#if defined(BOARD_HAS_NEOPIXEL)
    uses "NeoPixel"
#endif
    flags PinModeOption { Input = 0x00, Output = 0x01, InputPullup = 0x02, InputPulldown = 0x03 }
    enum PinStatus { Low = 0, High = 1, Change = 2, Falling = 3, Rising = 4 }
    delegate PinISRDelegate(byte pin, PinStatus status);
    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
    uint AnalogRead(byte pin) library;
    AnalogWrite(byte pin, uint value) library;
    AnalogWriteResolution(byte bits) library;
    Tone(byte pin, uint frequency, uint duration) library;
    Tone(byte pin, uint frequency) { Tone(pin, frequency, 0); }
    NoTone(byte pin) library;
    bool AttachToPin(byte pin, PinISRDelegate gpioISR, PinStatus status) library;
    bool InterruptsEnabled { get library; set library; }
#if defined(MCU_BOARD_RP2040)
    Reboot(bool bootsel) library;
    long HeapFree() library;  // Checks for memory leaks.
    long StackFree() library; // Checks for stack limit proximity.
    enum RP2040ClockSpeed { Slow48 = 48, Slow50 = 50, Default133 = 133, Overclock240 = 240, Overclock250 = 250, Overclock270 = 270 }
    RP2040ClockSpeed ClockSpeed { get library; set library; }
#endif
}
```