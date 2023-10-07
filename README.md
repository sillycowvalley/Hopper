# Hopper

## Getting Started

This currently works on Windows (Intel 32 and 64 bit).
- clone the repo
- run the only .exe in the /Bin folder (HopperNET.exe)
- some documentation is included in the PDF under /Docs

Here is the TLDR walk though:

https://docs.google.com/presentation/d/1ZfqbBSqEAHBDnZLWSe-3sygjJOas6WJYlS9MmkPlFR8/

And another quick hit on how the editor / IDE deals with "projects":

https://docs.google.com/presentation/d/1AP5HfBwkcK6xj-8Z5_-iPWaELDSroDuUKiATp3Y2iq4/

## Notes

This is an early release to experiment with the language. It runs on Windows and on 6502.

From a security point of view, Hopper is:
- unable to escape from the root folder of the repo (Hopper thinks that subtree is your entire disc)
- aside from files in the repro tree, Hopper can only access the console, keyboard and mouse

## Current State

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

## What's Next?

1. Port to ESP-2866 and ESP-32 (again)
- implement interface for GPIO pins, I2C and SPI
- driver (in Hopper) for SD cards (probably using LittleFS) then port File and Directory
2. Web / WiFi
- HTTP client and server drivers (in Hopper)
3. Port to RP240
4. Port to Z80 again

Contact me for more info: michael at sillycowvalley dot com