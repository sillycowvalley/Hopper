# Hopper

## Getting Started

This currently works on Windows (Intel 32 and 64 bit).
- clone the repo
- run the only .exe in the /Bin folder (HopperNET.exe)
- some documentation is included in the PDF under /Docs

## Notes

This is an early release to experiment with the language. It isn't terribly useful yet since you can only write console-style apps so far.

From a security point of view, Hopper is:
- unable to escape from the root folder of the repo (Hopper thinks that subtree is your entire disc)
- aside from files in the repro tree, Hopper can only access the console, keyboard and mouse

## What's Next?

An earlier version of Hopper has been ported to:
- microcontrollers (ESP-8266 - Wemos D1 Mini)
- Z80 (see the Z80 subfolder under /Source)

My plan is to port this latest iteration to ESP-8266 again and to make it really easy to develop and debug in that environment (first class IDE experience).

Initially I'll have drivers for:
1. Screen
- 20x4 LCD
- 68x48 pixel OLED
2. Microcontroller IO
- GPIO pins
- I2C
- SPI
3. Disc storage
- Flash (LittleFS)
- SD card
4. Keyboard
- PS/2
- matrix
5. Web / WiFi
- HTTP GET requests

I'm not sure what I'll do with the serial port yet. Depends if I use WiFi or serial for debugging and uploading programs.

Contact me for more info: michael at sillycowvalley dot com