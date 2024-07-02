# Bill of Materials for the Hopper 6502 Single Board Computer

## PCB

This is the PCB. I got mine made by JLCPCB:
[Gerber File](https://github.com/sillycowvalley/Hopper/blob/main/Source/Projects/6502SBC/EasyEDA/6502%20Machine/rev12/Gerber_6502-v2_PCB_6502-v4_2024-05-15.zip)


## ICs

| Quantity | Part Number         | Description                                |
|----------|---------------------|--------------------------------------------|
| 1        | WD65C02S / 6502     | Microprocessor                             |
| 1        | DS1813+5            | Supervisor / Reset IC                      |
| 1        | GAL20V8 / ATF22V10C | Programmable Logic                         |
| 1        | 65C22               | VIA (Versatile Interface Adapter)          |
| 1        | 24AA1026            | EEPROM (I2C)                               |
| 1        | MC6850              | ACIA (Asynchronous Communications Interface Adapter) |
| 1        | AT28C256            | EEPROM (Parallel)                          |
| 1        | 6C1008 / 62256      | SRAM (Static RAM)                          |
| 1        | DS1813+5            | 5V Supervisor (150ms power/reset up delay) |

## Can Oscillators (DIP-4 or DIP-14)

| Quantity | Description                    |
|----------|--------------------------------|
| 1        | Crystal, 1-8 MHz               |
| 1        | Crystal, 1.8432 / 3.6864 MHz   |

## Passive Components

| Quantity | Description          |
|----------|----------------------|
| 5        | Capacitor, 0.1uF     |
| 1        | Capacitor, 10uF      |
| 2        | Resistor, 3.3kΩ      |
| 3        | Resistor, 4.7kΩ      |
| 2        | Resistor, 470Ω       |


## Connectors and Headers

| Quantity | Description       |
|----------|-------------------|
| 1        | FTDI Header       |
| 1        | Velleman Header   |
| 1        | Waveshare Header  |

## Diodes

| Quantity | Part Number | Description       |
|----------|-------------|-------------------|
| 2        | 1N5817      | Schottky Diode    |


## Miscellaneous

| Quantity | Description    |
|----------|----------------|
| 1        | Reset Button   |
| 1        | NMI Button     |
| 1        | User Button    |


## Tools

This is my ideal FTDI solution. You can probably find multiple sources for it. Just be sure you are ordering a 5V version:  
[FTDI TTL-232R-5V](https://ftdichip.com/products/ttl-232r-5v/)

You need a wire stripper that goes down to at least 24AWG. Something like this:  
[Wire Stripper](https://www.jameco.com/z/HT-1043-R-Hanlong-Tools-Tool-Hand-7-in-1-HT-1043-22-30AWG_127871.html)

DIP IC Extractor:  
[DIP IC Extractor](https://www.jameco.com/z/08-609-HT103A--Jameco-BenchPro-DIP-IC-Extractor-Tool_16838.html)

DIP IC Pin benders to 3D print:  
[Pin Bender 1](https://www.thingiverse.com/thing:3124978)  
[Pin Bender 2](https://www.thingiverse.com/thing:3121797)


