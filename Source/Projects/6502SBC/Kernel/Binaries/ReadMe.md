For the MECB 6502, we need to add a little EEPROM adapter to the I/O card.

I didn't bother with pullup resistors since the Motorola 6821 PIA has built-in pullups and they seem to work fine.

![MECB I/O card I2C adapter](MECBI2C.png)

This same schematic works for a variety of similar serial I2C EEPROMs. I chose this addressing scheme (A0 low, A1 low and A2 high) since
it will make the 24AA1026 or the 24LC1025 appear as 2x 64K devices at 0x50 and 0x54 respectively.

![MECB I/O card I2C adapter](MECBI2C.jpg)