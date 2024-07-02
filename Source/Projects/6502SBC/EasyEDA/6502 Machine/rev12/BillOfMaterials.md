# Bill of Materials for the Hopper 6502 Single Board Computer

## PCB

This is the PCB:
[Gerber File](https://github.com/sillycowvalley/Hopper/blob/main/Source/Projects/6502SBC/EasyEDA/6502%20Machine/rev12/Gerber_6502-v2_PCB_6502-v4_2024-05-15.zip)

I got mine made by [JLCPCB](https://jlcpcb.com/).


## ICs

| Quantity | Part Number         | Description                                |
|----------|---------------------|--------------------------------------------|
| 1        | [WD65C02S](https://www.jameco.com/z/W65C02S6TPG-14-Western-Design-Center-MPU-8-Bit-14MHz-65KB-Memory-40-Pin-PDIP_2143638.html) / 6502     | Microprocessor                             |
| 1        | [65C22](https://www.jameco.com/z/W65C22S6TPG-14-Western-Design-Center-Versatile-Interface-Adapter-via-8-Bit-I-O-Ports-14-MHz-40-Pin-PDIP-CMOS-5-Volt_2143591.html)               | VIA (Versatile Interface Adapter)          |
| 1        | [MC6850](https://www.jameco.com/z/6850-Major-Brands-IC-6850-Asynchronous-Communications-Interface-Adapter-24-pin-DIP_43633.html)              | ACIA (Asynchronous Communications Interface Adapter) |
| 1        | [GAL20V8](https://www.jameco.com/z/GAL20V8B-25LP-Lattice-Semiconductor-Corporation-CPLD-Complex-Programmable-Logic-Device-25ns-8MC-DIP-24_876766.html) / ATF22V10C | Address Logic ([default JED file](https://github.com/sillycowvalley/Hopper/blob/main/Source/Projects/6502SBC/CPLD/6502SBC.jed))                        |
| 1        | [24AA512](https://au.mouser.com/ProductDetail/Microchip-Technology/24AA512-I-P?qs=t4j2cOJKO62XNcEsx%2F77Xw%3D%3D&countryCode=US&currencyCode=USD)            | EEPROM (I2C)                               |
| 1        | [AT28C256](https://au.mouser.com/ProductDetail/Microchip-Technology/AT28C256-15DM-883-815?qs=lqAf%2FiVYw9gtSFr69lKk6g%3D%3D)            | EEPROM (Parallel) - speed matters, 150ns or better                         |
| 1        | 6C1008 / [62256](https://www.jameco.com/z/HM62256LP-70-Major-Brands-IC-62256LP-70-Low-Power-CMOS-SRAM-256K-Bit-32Kx8-70ns_82472.html)      | SRAM (Static RAM) - speed matters, 70ns or better                         |
| 1        | [DS1813+5](https://au.mouser.com/ProductDetail/Analog-Devices-Maxim-Integrated/DS1813-5+?qs=Jw2w9zrI6w%2Fv9tYN5eKaiw%3D%3D&countryCode=US&currencyCode=USD)            | 5V Supervisor (150ms power/reset up delay) |

## Can Oscillators (DIP-4 or DIP-14)

| Quantity | Description                    |
|----------|--------------------------------|
| 1        | Crystal, [1-8 MHz](https://www.jameco.com/z/OSC8-000-James-Electronics-8-MHz-Full-Can-Crystal-Oscillator_27991.html)               |
| 1        | Crystal, 1.8432 / [3.6864 MHz](https://www.jameco.com/z/H5C-2E3-3-6864-FOX-ELECTRONICS-Standard-Clock-Oscillator-3-6864MHz-Half-Can-DIP-8_2322439.html)   |

## Passive Components

| Quantity | Description          |
|----------|----------------------|
| 5        | [Capacitor, 0.1uF](https://www.jameco.com/z/DC-1-16-James-Electronics-0-1uF-16V-plusmn-20-Ceramic-Disc-Capacitor_2289855.html)    |
| 1        | [Capacitor, 10uF](https://www.jameco.com/z/ECA-1EM100-JVP-Jameco-ValuePro-10uF-25-Volt-Radial-Electrolytic-Capacitor-20-85c-5x11x5mm_1946367.html)      |
| 2        | [Resistor, 3.3kΩ](https://www.jameco.com/z/CF1-4W332JRC-Jameco-ValuePro-Resistor-Carbon-Film-3-3k-Ohm-1-4-Watt-5-_690988.html)      |
| 3        | [Resistor, 4.7kΩ](https://www.jameco.com/z/CF1-4W472JRC-Jameco-ValuePro-Resistor-Carbon-Film-4-7k-Ohm-1-4-Watt-5-_691024.html)      |
| 2        | [Resistor, 470Ω](https://www.jameco.com/z/CF1-4W471JRC-Jameco-ValuePro-Resistor-Carbon-Film-470-Ohm-1-4-Watt-5-_690785.html)       |

## Sockets

| Quantity | Part Number         | Description                                |
|----------|---------------------|--------------------------------------------|
| 1        | 28 PIN 0.3" ZIF Socket       |  Two variants: [low profile](https://www.jameco.com/z/28-526-10-Aries-Electronics-ZIF-Socket-28-Position-2-54mm-Solder-Straight-Thru-Hole_102745.html) or [conventional](https://www.jameco.com/z/28-6554-10-Aries-Electronics-Connector-Test-Socket-Receptacle-28-Position-2-54mm-Solder-Straight-Thru-Hole_104003.html) (easier to use)                                     |

## Connectors and Headers

| Quantity | Description       |
|----------|-------------------|
| 1        | [40 pin female headers](https://www.jameco.com/z/RS1-40-T-Adam-Technologies-40-Position-Single-Row-Vertical-Mount-Receptacle-3mm-Pin-Length_2168173.html) (cut as needed)      |
| 1        | [40 pin male right angle headers](https://www.jameco.com/z/7000-1X40RG-A-Jameco-ValuePro-Connector-Unshrouded-Breakaway-Header-40-Position-2-54mm-Right-Angle-Thru-Hole-Solder_2294663.html) |

## Diodes

| Quantity | Part Number | Description       |
|----------|-------------|-------------------|
| 2        | [1N5817](https://www.jameco.com/z/1N5817-Major-Brands-Diode-1N5817-Schottky-20-Volt-1A_177949.html)      | Schottky Diode    |


## Miscellaneous

| Quantity | Description    |
|----------|----------------|
| 1        | [Reset Button](https://www.jameco.com/z/SKH42012R-Alps-Electric-Tactile-Switch-SPST-NO-Top-Actuated-Through-Hole-12VDC-50mA_2323671.html) (red)   |
| 2        | [NMI and User Buttons](https://www.jameco.com/z/TL1105AF160Q-JVP-Jameco-ValuePro-Tactile-Pushbutton-Switch-SPST-NO-OFF-Momentary-ON-12-Volt-DC-50-mA_153252.html) (black)     |


## Tools

This same programmer can be used to program both the firmware EEPROM and the GAL (CPLD):
[EEPROM Programmer](https://www.jameco.com/z/TL866-3G-T48-Jameco-BenchPro-USB-High-Performance-Programmer-3rd-Generation-_2304999.html)

This is my ideal FTDI solution. You can probably find multiple sources for it. Just be sure you are ordering a 5V version:  
[FTDI TTL-232R-5V](https://ftdichip.com/products/ttl-232r-5v/)

Extractor Tool:  
[DIP IC Extractor](https://www.jameco.com/z/08-609-HT103A--Jameco-BenchPro-DIP-IC-Extractor-Tool_16838.html)

DIP IC Pin benders to 3D print:  
[0.3" DIP Pin Bender](https://www.thingiverse.com/thing:3124978)  
[0.6" Pin Bender](https://www.thingiverse.com/thing:3121797)

For building Ben Eater's computer you need a wire stripper that goes down to at least 24AWG. Not needed for the Hopper 6502 SBC. 
Something like this:  
[Wire Stripper](https://www.jameco.com/z/HT-1043-R-Hanlong-Tools-Tool-Hand-7-in-1-HT-1043-22-30AWG_127871.html)

## Software

[WinCUPL](https://www.microchip.com/en-us/products/fpgas-and-plds/spld-cplds/pld-design-resources) is used to program the CPLD (GAL20V8 or ATF22V10C). Optional if you don't want the [default addressing scheme](https://github.com/sillycowvalley/Hopper/blob/main/Source/Projects/6502SBC/CPLD/6502SBC.jed).
