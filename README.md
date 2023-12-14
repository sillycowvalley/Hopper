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

The primary target for Hopper is small devices. Currently it is running well on 6502 and there
are drivers for the following functionality:

1. LCD (using 65C22 VIA)
2. Timer (using 65C22 VIA)
3. Serial (using either the Motorola 6850 or the WDC/Rockwell 6551)
4. mimimal GPIO (built-in LED using 65C22)
5. PS/2 keyboard (using 65C22 VIA)

Developing for the 6502 is a first class IDE experience: source for both the editor (with syntax highlighting)
and the full symbolic source-level debugger are included (written in Hopper, of course).

The compilation toolchain is broken into 5 applications, all written in Hopper:
- Preprocess: first compilation phase which walks the entire project to collect definitions
- Compile:    second compilation phase compiles the code within the methods
- Optimize:   optional phase that makes obvious optimizations to the intermediate output from the compiler
- Codegen:    compiles the intermediate output into Hopper VM byte code
- Dasm:       disassembles Hopper binaries (.hexe) into assembler listings (.hasm)

I've also recently written a portable runtime in Hopper which, along with a Hopper to C translation tool (written in Hopper of course), made a fresh version of the runtime in the Arduino Environment a fairly trivial project.

Current state on microcontrollers (via the Arduino IDE) is:
- tested on several ESP8266 and RP2040 devices
- seperate configurable code and data segments (unlike 6502 which only has a monolithic 64K segment for both)
- GPIO API including digital, PWM and analog
- LittleFS for file system (to store your current program too)
- I2C (via Arduino Wire.h)
- SPI (mostly for display support so far)
- several display drivers for OLED and TFT LCD
- Graphics unit with more text functionality than Screen and some drawing primitives
- full support for the Hopper debugger via serial interface (identical protocol to 6502 interface)

## What's Next?

1. generic SPI interface (beyond the current display drivers)
2. system | compiler | debugger:
- split constant literals into own 'text' segment (to give code segment full 64K)
- block comment support (/*..*/)
- fix line-ending functionality throught (consistent across platforms): parsing, serial IO, etc.
- compiler and debugger options per-project (not global)
3. SD card driver (FAT?)
4. Web / WiFi support (beyond current simple HTTP request support)
5. Port to Z80 again
6. Get Hopper up and running on some currently available 8 bit kits (like RC2014 and Neo6502 for example)
7. Sound APIs (8-bit retro style possibly)
8. UF2 binary for simpler getting starting on RP2040

## Fun Project Ideas

1. Better support for Forth in the VM:
- built-in word dictionary support in the VM
- a handful more VM opcodes
- support for remote word-level debugging in the debugger
2. A second Hopper client to support more than just Windows (probably Raspberry Pi next)
3. Some utility libraries to make it easier to support retro game development on microcontrollers
4. Revisit TinyHopper (minimal runtime with no reference types or GC) to support non-Hopper languages

Contact me for more info: BiggerTigger at sillycowvalley dot com
