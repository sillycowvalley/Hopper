# Hopper for GPT

## Introduction

Hopper is a modern programming language designed to run efficiently on small devices like microcontrollers and 8-bit microprocessors. It aims to provide a structured alternative to Python in the microcontroller domain, much like Turbo Pascal did for BASIC forty years ago. Hopper programs are single-threaded, run on a stack-based virtual machine, and have a garbage collector that collects objects not reachable from the stack.

## Key Differences from Other C-Like Languages

Hopper has several notable differences from other C-like languages such as C, C++, Java, and C#:

1. **Switch Statements:**
   - Hopper switch cases never fall through, so a `break` statement is not required.
   - Each case block is terminated automatically.
   - Always use curly braces `{}` even for single statements within a switch case.

2. **Loop Keyword:**
   - Hopper uses the `loop` keyword as a shorthand for `while(true)` or `for(;;)`, creating an infinite loop.

3. **No `void` Keyword:**
   - Hopper does not use the `void` keyword. Methods with no return type simply omit the return type specification.

4. **No Statement as Expression:**
   - Hopper enforces a clear distinction between statements and expressions. Statements, which have no return value, cannot be used where expressions are expected.

5. **Array Definitions:**
   - Arrays in Hopper are defined with the syntax `type[size] name`, not `type name[size]`.

6. **Type System and Variable Declaration:**
   - Hopper has strict type definitions and does not support implicit type inference with keywords like `var`.
   - Local members are referred to directly without using `this.`, with private members having a lowercase first letter in their identifier.

7. **Single File Programs and Units:**
   - Each Hopper program and unit resides in a single file, similar to Turbo Pascal.

8. **Delegates:**
   - Delegates in Hopper are typed function pointers, similar to those in C#.

9. **Entry Point:**
   - The entry point for a Hopper program is named `Hopper`, not `main` or any other conventional name.

10. **Garbage Collection:**
    - Hopper uses a garbage collector that relies on the stack to determine live objects, with a strict copy-on-write policy for reference types.

11. **Boolean Expressions:**
    - Hopper requires boolean expressions for conditionals and does not allow using integers as substitutes for boolean expressions (unlike in C).

12. **Array Initialization Limitations:**
    - Hopper only allows direct initialization of arrays for types like `byte` or `char`. Arrays of `bool`, `uint`, `int`, `float`, or `long` cannot be directly initialized.

13. **No Objects:**
    - Hopper does not have objects, so the syntax `byte[] shape = new byte[PieceSize * PieceSize];` is incorrect. It should be `byte[PieceSize * PieceSize] shape;`.

14. **Time and Random Numbers:**
    - There is no `System.TickCount`. Use `Time.Millis` instead. Since it returns a signed long and only the LSB is needed, use `return ((Time.Millis).GetByte(0) % ShapeCount);`.

15. **Debug Logging:**
    - Use a centralized method for debug logging to easily enable or disable debug output. For example, `DisplayHelper.DebugLog(message)`.

16. **Input Handling on MCUs:**
    - Implement input handling using interrupts for buttons to ensure single event generation per button press. The `Input` unit can queue key events and handle them consistently across platforms.

17. **Piece Manipulation in Tetris:**
    - Centralize shape drawing, clearing, and placing logic in helper methods to ensure consistency and simplify updates.

## Programs and Units

### Program Structure

In Hopper, a program is defined in a single file using the `program` keyword. The entry point for a Hopper program is named `Hopper`.

```hopper
program Blink
{
    uses "/Source/Library/Boards/CytronMakerPiRP2040"
    
    // Initialize the built-in LED pin
    Setup()
    {
        MCU.PinMode(Board.BuiltInLED, MCU.PinModeOption.Output);
    }
    
    Hopper()
    {
        Blink.Setup();
        loop
        {
            // Turn the LED on
            MCU.DigitalWrite(Board.BuiltInLED, true);
            Time.Delay(1000); // Delay for 1000 milliseconds (1 second)
            
            // Turn the LED off
            MCU.DigitalWrite(Board.BuiltInLED, false);
            Time.Delay(1000); // Delay for 1000 milliseconds (1 second)
        }
    }
}
```

### Units

Units are similar to modules or classes in other languages and are defined using the `unit` keyword. Each unit typically corresponds to a single file.

```hopper
unit MCU
{
    uses "/Source/System/System"
    uses "/Source/System/Runtime"
    uses "/Source/System/IO"

    enum PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }

    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
}
```

## Basic Concepts

### Statements and Expressions

Hopper separates statements and expressions. Statements cannot return values, preventing common bugs such as unintentional assignments in conditions.

```hopper
uint a = 10;
if (a == 10)
{
    IO.WriteLn("a is 10");
}
```

### Control Flow

Hopper provides standard control flow constructs: `if`, `switch`, `for`, `foreach`, `while`, and `loop`.

#### Loop Constructs

```hopper
// Infinite loop
loop
{
    // Code to repeat indefinitely
    if (condition)
    {
        break; // Exit the loop
    }
}

// For loop
for (uint i = 0; i < 10; i++)
{
    IO.WriteLn(i.ToString());
}

// Foreach loop
<string> list = ["one", "two", "three"];
foreach (var item in list)
{
    IO.WriteLn(item);
}

// While loop
uint i = 0;
while (i < 10)
{
    IO.WriteLn(i.ToString());
    i++;
}
```

### Functions and Delegates

Functions are defined with a code block and can be assigned to delegates for callback and event handling.

#### Function Definition

```hopper
PrintHello()
{
    IO.WriteLn("Hello");
}
```

#### Delegate Usage

```hopper
delegate void SimpleDelegate();

SimpleDelegate myDelegate = PrintHello;
myDelegate();
```

## Hopper Language Features

### Types and Variables

Hopper has value types like `int`, `uint`, `char`, and reference types like `string`, `list`, and `dictionary`.

#### Variable Declaration

```hopper
uint a = 10;
string s = "Hello";
```

### Arrays and Collections

Hopper supports arrays and collection types like lists and dictionaries.

#### Arrays

```hopper
uint[10] numbers;
numbers[0] = 1;
```

#### Lists

```hopper
<string> list = ["apple", "banana", "cherry"];
list.Add("date");
```

#### Dictionaries

```hopper
<string, uint> ages;
ages["Alice"] = 30;
ages["Bob"] = 25;
```

### Error Handling

Hopper does not support exceptions. Errors result in runtime failures.

### Input/Output

Hopper provides IO functionalities primarily through the `IO` unit for serial communication.

#### Example

```hopper
IO.WriteLn("Enter your name:");
string name = IO.ReadLine();
IO.WriteLn("Hello, " + name);
```

## Hopper for Microcontrollers

### Pin and Port Handling

Hopper provides `MCU` unit for controlling microcontroller pins.

#### Example

```hopper
MCU.PinMode(13, MCU.PinModeOption.Output);
MCU.DigitalWrite(13, true);
```

### Timers and Delays

Use the `Time` unit for creating delays.

#### Example

```hopper
Time.Delay(1000); // Delay for 1000 milliseconds
```

### PWM and Analog I/O

Analog functions for PWM and reading analog values are provided by the `MCU` unit.

#### Example

```hopper
MCU.AnalogWrite(9, 128); // Set PWM value
uint value = MCU.AnalogRead(0); // Read analog value
```

### Communication Protocols

Hopper supports I2C, SPI, and UART for communication with peripherals.

#### Example (I2C)

```hopper
I2C.Initialize();
I2C.StartTransmission(0x40);
I2C.Write(0x01);
I2C.EndTransmission();
```

## Hopper Runtime and Libraries

### System Units

Hopper's standard library includes several essential units.

#### IO Unit

Provides methods for serial communication.

#### Time Unit

Provides methods for creating delays and handling timers.

#### MCU Unit

Provides methods for interacting with microcontroller pins and peripherals.

#### Pitches Unit

Defines frequencies for musical notes.

### Example of Using System Units

```hopper
program Melody
{
    uses "/Source/Library/Boards/CytronMakerPiRP2040"
    uses "Pitches"

    const string melody = "E4,E4,F4,G4,G4,F4,E4,D4,C4,C4,D4,E4,E4,D4,D4,E4,E4,F4,G4,G4,F4,E4,D4,C4,C4";
    const byte[] rhythm = {
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
    };

    const byte PIEZO_PIN = 22;

    PlayMelody(<string> notes, const byte[] durations)
    {
        uint tuneSize = notes.Count;
        <string,uint> toneFrequencies = Pitches.GetTones();

        for (uint thisNote = 0; thisNote < tuneSize; thisNote++)
        {
            string note = notes[thisNote];
            byte length = durations[thisNote];
            float fduration = (60.0 / 120.0) * (4.0 / float(length)) * 1000.0;
            uint duration = uint(fduration);

            if (toneFrequencies.Contains(note))
            {
                uint frequency = toneFrequencies[note];
                MCU.Tone(PIEZO_PIN, frequency);
                Time.Delay(duration);
                MCU.NoTone(PIEZO_PIN);
            }
            else
            {
                Time.Delay(duration);
            }

            uint pauseBetweenNotes = uint(fduration * 1.20);
            Time.Delay(pauseBetweenNotes);
        }
    }

    Hopper()
    {
        <string> notes = (melody).Split(',');
        PlayMelody(notes, rhythm);
    }
}
```

## Advanced Topics

### Event Handling

Hopper supports an event-driven programming model for handling hardware events like interrupts.

#### Example

```hopper
delegate void ButtonPressed();

ButtonPressed buttonHandler = OnButtonPressed;

void OnButtonPressed()
{
    IO.WriteLn("Button pressed!");
}

MCU.AttachToPin(2, buttonHandler, MCU.PinStatus.Falling);
```

### Concurrency

Hopper does not support multithreading. Instead, it uses an event-driven approach for concurrency.

### Memory Management

Hopper has a garbage collector that collects objects not reachable from the stack. The stack is the live list for the garbage collector.

## Handling Dictionaries and Lists

### Using Dictionaries

Hopper supports dictionaries, which are useful for overcoming some of the limitations of arrays, especially when dealing with reference types like strings.

```hopper
unit Pitches
{
    <string,uint> tones;

    <string,uint> GetTones()
    {
        if (tones.Count == 0)
        {
            tones["A4"] = 440;
            tones["F4"] = 349;
            tones["C5"] = 523;
            // Add other tones as needed
        }
        return tones;
    }
}
```

### Using Lists

Hopper supports lists, which can be used to store sequences of value types. Lists provide more flexibility than arrays.

```hopper
<byte> RGB(byte r, byte g, byte b)
{
    <byte> rgb;
    rgb.Append(r);
    rgb.Append(g);
    rgb.Append(b);
    return rgb;
}

unit Colors
{
    <string, <byte>> colors;

    InitializeColors()
    {
        colors["RED"] = RGB(255, 0, 0);
        colors["GREEN"] = RGB(0, 255, 0);
        colors["BLUE"] = RGB(0, 0, 255);
    }
}
```

### Example Usage

Here is an example that demonstrates how to use dictionaries and lists to handle colors for NeoPixels:

```hopper
program NeoPixelDemo
{
    uses "/Source/Library/Boards/CytronMakerNanoRP2040"
    
    const byte pixelPin = Board.BuiltInNeoPixel;
    const uint numPixels = 2;

    byte brightness = 20;

    <string, <byte>> colors;

    <byte> RGB(byte r, byte g, byte b)
    {
        <byte> rgb;
        rgb.Append(r);
        rgb.Append(g);
        rgb.Append(b);
        return rgb;
    }

    InitializeColors()
    {
        colors["RED"] = RGB(255, 0, 0);
        colors["YELLOW"] = RGB(255, 150, 0);
        colors["GREEN"] = RGB(0, 255, 0);
        colors["CYAN"] = RGB(0, 255, 255);
        colors["BLUE"] = RGB(0, 0, 255);
        colors["PURPLE"] = RGB(180, 0, 255);
        colors["WHITE"] = RGB(50, 50, 50);
    }

    <byte> Wheel(byte pos)
    {
        if (pos < 85)
        {
            return RGB(byte(255 - pos * 3), byte(pos * 3), 0);
        }
        else if (pos < 170)
        {
            pos -= 85;
            return RGB(0, byte(255 - pos * 3), byte(pos * 3));
        }
        else
        {
            pos -= 170;
            return RGB(byte(pos * 3), 0, byte(255 - pos * 3));
        }
    }

    ColorChase(<byte> color, float wait)
    {
        for (uint i = 0; i < numPixels; i++)
        {
            NeoPixel.SetColor(i, color[0], color[1], color[2]);
            NeoPixel.Show();
            Time.Delay(uint(wait * 1000));
        }
        Time.Delay(1000);
    }

    RainbowCycle(float wait)
    {
        for (uint j = 0; j < 255; j++)
        {
            for (uint i = 0; i < numPixels; i++)
            {
                byte rcIndex = byte((256 / numPixels) + j);
                <byte> color = Wheel(rcIndex & 255);
                NeoPixel.SetColor(i, color[0], color[1], color[2]);
            }
            NeoPixel.Show();
            Time.Delay(uint(wait * 1000));
        }
    }

    PlayDemo()
    {
        InitializeColors();
        
        NeoPixel.BuiltIn();
        NeoPixel.Brightness = brightness;

        loop
        {
            foreach (var kv in colors)
            {
                string colorName = kv.key;
                <byte> color = kv.value;
                NeoPixel.Fill(0, numPixels, color[0], color[1], color[2]);
                NeoPixel.Show();
                Time.Delay(1000);
            }

            // Color Chase
            ColorChase(colors["RED"], 0.5);
            ColorChase(colors["YELLOW"], 0.5);
            ColorChase(colors["GREEN"], 0.5);
            ColorChase(colors["CYAN"], 0.5);
            ColorChase(colors["BLUE"], 0.5);
            ColorChase(colors["PURPLE"], 0.5);

            // Rainbow Cycle
            RainbowCycle(0.05);
            Time.Delay(1000);
        }
    }

    Hopper()
    {
        PlayDemo();
    }
}
```

## Conclusion

This document, "Hopper for GPT," is designed to provide a comprehensive understanding of the Hopper programming language, its syntax, and best practices. It includes detailed explanations of loops, switch statements, delegates, dictionaries, lists, and examples of real-world usage. This guide aims to help future versions of ChatGPT quickly and efficiently become proficient in Hopper.