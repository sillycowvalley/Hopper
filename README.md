# Hopper

## Getting Started

This currently works on Windows (Intel 32 and 64 bit).
- clone the repo
- run the only .exe in the /Bin folder (HopperNET.exe)
- some documentation is included in the PDF under /Docs

The Hopper Language playlist on YouTube:

https://www.youtube.com/playlist?list=PLriDPC-IsO2mIq_u9_mh_GN6wfX59X-wY

Here is the TLDR walk though:

https://docs.google.com/presentation/d/1ZfqbBSqEAHBDnZLWSe-3sygjJOas6WJYlS9MmkPlFR8/

And another quick hit on how the editor / IDE deals with "projects":

https://docs.google.com/presentation/d/1AP5HfBwkcK6xj-8Z5_-iPWaELDSroDuUKiATp3Y2iq4/

## Notes

This is an early release to experiment with the language. It runs on Windows, 6502 and RP2040 (Raspberry Pi Pico).

From a security point of view, Hopper is:
- unable to escape from the root folder of the repo (Hopper thinks that subtree is your entire disc)
- aside from files in the repro tree, Hopper can only access the console, keyboard, mouse and serial port.

## Current State

**Note:** This project is incomplete with plenty of work still to be done. Treat this as a *beta release*.

The primary target for Hopper is small devices. This includes microcontrollers and 8 bit CPUs. In addition to running on Windows, Hopper currently works on the 6502 and on RP2040-based microcontrollers.

Developing for the 6502 and MCUs is a first class IDE experience: source for both the editor (with syntax highlighting) and the full symbolic source-level debugger are included (written in Hopper, of course). The debugger works live on Windows accross the regular serial connection to your device.

## Toolset
All of these tools are written in Hopper and run and the Hopper Runtime on Windows.

### Shell
The shell is the main windows when running under the Windows Runtime. It is a command line interface written in Hopper that has also been ported to run on RP2040 MCUs (via serial). Type `help` or `man` for the list of available commands.

### Preprocess
First compilation phase which walks the entire project to collect definitions (the stuff outside the curly braces). `preprocess` outputs `<project.json>` which is consumed by the Hopper compiler (`compile`) or the 6502 assembler (`assemble`).

### Compile
Second compilation phase for Hopper programs compiles the code within the methods (the stuff between the curly braces). `compile` generates `<project.code>` which is an intermediate format of the Hopper VM instructions. It can be consumed by either the optimizer (`optimize`) or the code generator (`codegen`).

### Assemble
Second compilation phase for 6502 assembly programs assebles the code within the methods (the stuff between the curly braces). `assemble` generates `<project.code>` which is an intermediate format of 6502 instructions. It can be consumed by either the 6502 optimizer (`optasm`) or the 6502 code generator (`asmgen`).

### Optimize
Optional phase that makes obvious optimizations to the intermediate output from the Hopper compiler. Consumes `<project.code>`, optimizes it and then outputs a new leaner and meaner `<project.code>`.

### OptAsm
Optional phase that makes obvious optimizations to the intermediate output from the 6502 assembler. Consumes `<project.code>`, optimizes it and then outputs a new leaner and meaner `<project.code>`.

### CodeGen
Transforms the Hopper intermediate format from `<project.code>` into Hopper VM byte code executable `<project.hexe>`. Also emits this same output as an Intel HEX file (`<project.ihexe>`) if the target is an MCU or 6502. This Intel HEX is what is uploaded via the serial connection to your device.

### AsmGen
Transforms the 6502 intermediate format from `<project.code>` into 6502 executable machine code `<project.hex>`. This output is a Intel HEX file that can be burned to EEPROM or uploaded to the 6502 emulator (`e6502`).

### DASM
Disassembles Hopper binaries `<project.hexe>` into assembler listings `<project.hasm>`.

### 65DASM
Disassembles 6502 binaries `<project.hex>` into assembler listings `<project.lst>`.

### C Translator
`translate` is a tool that converts Hopper source into `C` code. This tool is currently used to migrate the Hopper Portable Runtime, via the Arduino IDE, to run as the Hopper Runtime on RP2040 MCUs. Typically you'd build the Hopper project, `runtime` in this case, first. That way you know it is free of compilation errors before you try to translate it to `C`.
Since there is no Hopper memory management and garbage collection support in `C`, projects that can be translated will need to avoid using reference types and stick to value types. The Hopper Portable runtime is such a project.

### Runtime
The portable runtime is written in Hopper and is used to generate the RP2040 MCU runtime. It also runs under the Windows Runtime for emulation and testing. It provides full support for Hopper `Debug` and `HopperMon` via synthesized `COM0` serial interface when running on Windows or via the actual serial interface when running on a MCU.

### RP2040 Runtime
Current state of Hopper on RP2040 MCUs (via the Arduino IDE) is:
- tested on dozens of RP2040 devices
- full support for the Hopper `Debug` and `HopperMon` via serial interface
- seperate configurable 64K code and 64K data segments (unlike 6502 which only has a monolithic 64K segment for code, data and stacks)
- GPIO API including digital, PWM, analog and pin interrupts
- LittleFS for Hopper file system (to store your current program too built-in FLASH)
- SD support via FAT file system mounted into the Hopper file system under `/SD/`
- I2C
- SPI
- Timer API including alarm events
- WiFi, web server and web client functionality
- numerous display drivers (written in Hopper) for OLED, TFT LCD and ePaper displays
  - text : scale, several fonts
  - simple graphics
- RTC drivers for 3 different chips (including alarm and timer events)

### 6502 Runtime
This is the second generation Hopper runtime for 6502.  Currently it (`r6502`) can be loaded into the emulator (`e6502`) under the Windows Runtime and you can upload and debug Hopper programs on it using HopperMon (`hm`) or the Hopper debugger (`debug`) via the synthesized `COM0` connection. 

### Editor
The editor, `edit`, is a colour syntax highlighting editor for both Hopper (`.hs`) and 6502 assembly (`.asm`) projects that runs under the Hopper runtime on Windows. It also serves as the IDE with good project navigation tools and integration with both the Hopper and 6502 project building toolchains. It can also upload successfully built projects (either Hopper or 6502 assembly) to your device and launch them directly in the debugger (`debug`).

### Debugger
Full source-level symbolic debugging experience (`debug`) which can be launched from the IDE (`edit`). Works with Hopper projects and 6502 assembly projects. Works by connecting to the Hopper Runtime via a serial connection. This also works in emulation mode by connecting to the Portable Runtime (`runtime`) via a synthesized `COM0` serial interface to a 2nd instance of the Hopper runtime running on Windows.

### HopperMon
`hm` runs under the Windows Runtime. It uses the same serial connection debugging protocol as the debugger (`debug`) but has more of a console-style interface. It also has more utility commands than the source debugger and can step one Hopper VM instruction at a time (rather than one Hopper source line at a time in the GUI debugger).

### 6502 Emulator
`e6502` runs under the Hopper runtime on Windows and can be connected to by HopperMon (`hm`) or the source debugger (`debug`) via the IDE using a synthesized `COM0` serial interface. This allows you to debug Hopper programs on the 6502 platform without a physical 6502 device. The emulator itself also has a HopperMon-style interface that can be used to debug and single step through the 6502 code.

## What's Next?

1. fix line-ending functionality throught (consistent across platforms): parsing, serial IO, etc.
2. Continue to refine the v2 version of the 6502 runtime
3. Port to Z80 again ..
4. Sound APIs (8-bit retro style possibly)

Contact me for more info: BiggerTigger at sillycowvalley dot com
