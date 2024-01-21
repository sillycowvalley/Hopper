# Hopper

## Getting Started

First, you don't need to build this project if the current release
satisfies your needs.

Releases can be found here:

https://github.com/sillycowvalley/Hopper/releases

There are multiple UF2 files that you can flash onto RP2040 style devices. The `HopperPico.uf2`
works on the Raspberry Pi Pico but also supports most RP2040 devices.

WiFi support requires a specific UF2 though depending on which WiFi coprocessor is on the device. The
coprocessor on the Raspberry Pi Pico W is the CYW43439 and this is supported by the `HopperPicoW.uf2`.

Currently the only other coprocessor supported is the ESP8285 used by the Challenger series of boards
and this is supported by `HopperChallengerWiFi.uf2`

If your needs are still not satisfied, read **Build Instructions** below.

## Build Instructions

Again, the pre-created versions of `Runtime.h` and `Runtime.cpp` will probably satisfy your needs
and that means you can skip the next section and move on to **Building in Arduino**

## Generating the Runtime##

The *Portable Runtime* is actually written in Hopper and there it can run on Hopper on Windows in
emulation mode which is very handy for testing and debugging. That project can be found here: `/Hopper/Source/Runtime/`

To generate the C++ version, load the project (`Runtime.hs`) into the Hopper IDE and build it (we need the preprocessor
to succeed and it is good to compiler the Hopper version to make sure there are no errors). At the console, run:<br>
`/Hopper/Source/Runtime/translate runtime.hs`

This will generate new versions of `Runtime.cpp` and `Runtime.h` in this location:<br>
`/Hopper/Debug/Obj/`

The contents of these two files need to be manually inserted into the files of the same names under `/Hopper/Runtimes/Arduino/HopperPortable/'.
This step needs to be automated but, for now, be careful to just replace the generated code. For the header file, this means keeping the trailing `#endif` intact.

Now you are ready to build ..

## Building in Arduino ##

We use Arduino IDE 2.0 to build this project and we require a few 3rd party libraries to be installed.

From the *File | Preferences ..* menu go to the dialog which has a field for *Additional Board Managers* and add this URL:<br>
https://github.com/earlephilhower/arduino-pico/releases/download/global/package_rp2040_index.json

From the *Board Manager* panel, make sure *Raspberry Pi Pico / Pico W* by Earle F. Philhower, III is installed.

From the *Library Manager* panel, install the following libraries:
*Adafruit NeoPixel* by Adafruit
*WiFiNINA* by Arduino if you are building for the Arduino Nano
*WiFiEspAT* by Juraj Andrássy if you are using a board from the Challenger series (not sure if this is included with Earle's board package)

(a *LittleFS* library may be required if you're not targetting an RP2040 device)

In `HopperConfiguration.h` you'll select the board profile you are targeting. The choices that are currently well tested are:
- `RP2040PICOW`
- `RP2040PICO`
- `CHALLENGER_RP2040_WIFI`
- `ARDUINONANO_RP2040`

Make sure only one of these is uncommented.

**Important Settings**

1. On the *Tools* menu, choose your *Board*.
2. On the *Tools* menu, choose *Flash Size* to split Flash between firmware and the file system. Choose the smallest possible firmware size to get the largest possible file system.
3. On the *Tools* menu, choose *Optimize* and select the `Fast (-OFast)` option for best performance.

 You should be ready to build now.
