# System Overview: Hopper 65C02S SBC

## Overview
The Hopper 65C02S Single Board Computer (SBC) is designed around the 65C02S microprocessor, providing a robust platform for educational Hopper development. This SBC incorporates essential components such as memory, I/O interfaces, and peripheral controllers to create a complete, standalone computing environment.

![6b77147d-3645-4c71-a7c9-a2f34844debd](https://github.com/sillycowvalley/Hopper/assets/89331909/2724f2ed-a58a-49ef-ba6e-063c8c85b00d)


## Major Components

### 65C02S Microprocessor
The 65C02S is the central processing unit of the SBC, offering enhanced features and reduced power consumption compared to its predecessor, the original 6502 microprocessor. It manages the execution of instructions, interfacing with memory and peripherals to perform various computational tasks.

### AS6C62256 SRAM
This 32KB static RAM provides fast, volatile memory storage for the SBC. With an access time of 55 ns, it ensures rapid read and write operations, crucial for the system's performance.

### AT28C256-15U EEPROM
The AT28C256 EEPROM offers 32KB of non-volatile memory storage. It is used to store the system's firmware, the Hopper Minimal Runtime. Despite its 150 ns access time, the system can reliably operate at 8 MHz.

### ATF22V10C Programmable Logic Device
The ATF22V10C PLD is utilized for custom logic implementation, providing flexibility in interfacing and control logic for the SBC. With a propagation delay of 7.5 ns, it significantly contributes to the system's speed and efficiency.

### 65C22 Versatile Interface Adapter (VIA)
The 65C22 VIA provides parallel I/O ports, timers, and interrupt capabilities. In the Hopper SBC, it provides:
- **16 General Use GPIO Pins**: Two 8-bit parallel I/O ports (Port A and Port B) for interfacing with external devices.
- **Built-in LED**: Connected to PA0 (Port A bit 0) for user indications.
- **I2C Interface**: Hopper implements SCL on PB0 and SDA on PB1, with built-in 4k7 pull-up resistors.
- **Timer**: One timer is used to support the `Time.Seconds` and `Time.Delay` APIs in Hopper.

### MC6850 ACIA
The MC6850 Asynchronous Communications Interface Adapter facilitates serial communication, enabling data exchange with other systems or devices through serial ports. In the Hopper SBC, it provides:
- **Program Uploading**: Facilitates the uploading of Hopper programs from a host computer.
- **Hopper Monitor**: Supports the Hopper Monitor interface for Hopper VM system-level debugging.
- **Hopper Debugger**: Provides the interface for the Hopper Debugger, enabling development and source-level debugging of Hopper applications.

### 24AA1026 EEPROM (I2C)
This serial EEPROM provides additional non-volatile memory with an I2C interface, an easy and reliable way to persist the last Hopper program uploaded. The Hopper Minimal Runtime will load and run this program on reboot unless the User button is held down.

### Oscillators
- **1, 2, 4 or 8 MHz Crystal (X1)**: Provides the main clock signal for the 65C02S microprocessor, setting the operating frequency of the SBC.
- **1.8432 / 3.6864 MHz Crystal (X2)**: Generates clock signals for the serial communication interface, ensuring accurate data transmission rates.

### Reset Circuit (DS1813)
The DS1813 reset IC ensures the system starts in a known state by providing a reliable power-on reset signal to the 65C02S microprocessor.

### Power Supply
The SBC takes its power from the USB (via the FTDI). There are GND and +5V header pins to accompany the GPIO header but keep in mind the limit of what we can draw from the USB (about 600mA).

### Connectors and Headers
- **FTDI Header**: Facilitates USB to serial communication, allowing for easy interfacing with a PC for programming and debugging.
- **Velleman and Waveshare Headers**: Provide additional pin configurations for directly plugging in several common FTDI breakout boards.
- **GPIO**: 16 pins to expose Port A and Port B of the 65C22 along with GND and +5V headers for peripherals.
- **I2C**: 4 pin header for GND, +5V, SCL and SDA (SSD1306 OLED module can plug in directly). SBC includes 2x 4k7 pullup resistors for I2C. SCL is on PB0 and SDA is on PB1.

### Buttons
- **Reset**: Holds the reset line low, and with the help of the DS1813, provides a stable reset (150ms delay).
- **NMI**: Debounced break button. Hopper Minimal Runtime uses this as another way to execute a break (<ctrl><C>).
- **User**: During boot, Hopper Minimal Runtime will respond to this button down as a signal *not* to boot from the I2C EEPROM. After boot, this button can be used for anything else the user chooses.

### LEDs
- **PWR**: Lights up when the board is powered.
- **LED**: Built-in user LED connected to PA0 (Port A bit 0).

## Experimentation with Clock Speeds

The Hopper 65C02S SBC is designed to allow experimentation with different microprocessor variants and clock speeds, providing flexibility and insight into the performance characteristics of the system under various conditions.

### MOS 6502
The board supports swapping in earlier versions of the 6502, such as the MOS 6502. Experiments with the Rockwell version of the MOS 6502 have successfully run at 2 MHz, despite the chip being rated for 1 MHz.

### Western Design Center 65C02S
Switching back to the Western Design Center's 65C02S, experimentation has revealed that:

- **ATF22V10C-7PX vs. GAL20V8-25LP**: Despite the ATF22V10C having a theoretical faster speed, it has not run reliably above 4 MHz. On the other hand, the GAL20V8-25LP has been running reliably in an 8 MHz system.
- **Overclocking**: The GAL20V8-25LP appears to overclock more effectively than the ATF22V10C-7PX, suggesting it is the more reliable choice for higher-speed operation.

### Serial Port Speed
When operating the system at different clock speeds:

- **4 MHz and Below**: The serial port needs to be throttled down to 28800 baud. This is accomplished by exchanging the can oscillator to 1.8432 MHz.
- **8 MHz Operation**: At 8 MHz, the serial port can run at 56600 baud, which works well for uploading large programs. This is achieved using a 3.6864 MHz can oscillator.

### Conclusion
Experimentation has shown that for a more reliable 8 MHz system, the CPLD (Complex Programmable Logic Device) is the weak point. The GAL20V8-25LP performs better under overclocking conditions compared to the ATF22V10C-7PX, making it the preferred choice for high-speed operations.
