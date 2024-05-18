# Hopper Language Guide

## Introduction
Hopper is a modern programming language designed to run efficiently on small devices like microcontrollers and 8-bit microprocessors. It aims to provide a structured alternative to Python in the microcontroller domain, much like Turbo Pascal did for BASIC forty years ago. Hopper programs are single-threaded, run on a stack-based virtual machine, and have a garbage collector that collects objects not reachable from the stack.

## Basic Concepts

### Programs and Units
In Hopper, a program is the main executable file, and units are reusable code modules. Each Hopper source file contains either a program or a unit.

#### Example Program
```hopper
program HelloWorld
{
    Hopper()
    {
        IO.WriteLn("Hello, World!");
    }
}
```

#### Example Unit
```hopper
unit MyUtilities
{
    PrintMessage(string message)
    {
        IO.WriteLn(message);
    }
}
```

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
void PrintHello()
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

## Conclusion
Hopper is a powerful and efficient programming language for microcontrollers, providing structured programming constructs and a rich set of libraries for hardware interaction. By understanding the basics, system units, and advanced topics covered in this guide, you will be well-equipped to develop applications in Hopper.