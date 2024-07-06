unit LCD
{
    // Hitachi HD44780 LCD driver
    // https://www.hackster.io/michael-cartwright/hd44780-lcd-driver-20x4-and-16x2-parallel-8-or-4-bit-bc853a
    
    #define LCD_8_BITS
    
    // GP0 is the built in LED
    // GP1 is the user button
    // GP8 and GP9 are I2C

    const byte rs      = GP2;      // device pin 4  0-Command, 1-Data
    const byte rw      = GP3;      // device pin 5  0-Write, 1-Read
    const byte e       = GP4;      // device pin 6
    
#ifdef LCD_8_BITS
    const byte d0 = GP5;
    const byte d1 = GP6;
    const byte d2 = GP7;
    const byte d3 = GP10;
#endif
    const byte d4 = GP11;
    const byte d5 = GP12;
    const byte d6 = GP13;
    const byte d7 = GP14;
    
    byte lcdColumns;
    byte lcdRows;
    byte currentCol;
    byte currentRow;
    byte[4] lcdRowStart; // see LCD.Instruction(byte data)
    
    dataAsOutput()
    {
#ifdef LCD_8_BITS
        PinMode(d0, PinModeOption.Output);
        PinMode(d1, PinModeOption.Output);
        PinMode(d2, PinModeOption.Output);
        PinMode(d3, PinModeOption.Output);
#endif
        PinMode(d4, PinModeOption.Output);
        PinMode(d5, PinModeOption.Output);
        PinMode(d6, PinModeOption.Output);
        PinMode(d7, PinModeOption.Output);
    }
    dataAsInput()
    {
#ifdef LCD_8_BITS
        PinMode(d0, PinModeOption.Input);
        PinMode(d1, PinModeOption.Input);
        PinMode(d2, PinModeOption.Input);
        PinMode(d3, PinModeOption.Input);
#endif
        PinMode(d4, PinModeOption.Input);
        PinMode(d5, PinModeOption.Input);
        PinMode(d6, PinModeOption.Input);
        PinMode(d7, PinModeOption.Input);
    }
    
#ifdef LCD_8_BITS 
    writeData8(byte data)
    {
        DigitalWrite(d0, (data & 0b00000001) != 0);
        DigitalWrite(d1, (data & 0b00000010) != 0);
        DigitalWrite(d2, (data & 0b00000100) != 0);
        DigitalWrite(d3, (data & 0b00001000) != 0);
        DigitalWrite(d4, (data & 0b00010000) != 0);
        DigitalWrite(d5, (data & 0b00100000) != 0);
        DigitalWrite(d6, (data & 0b01000000) != 0);
        DigitalWrite(d7, (data & 0b10000000) != 0);
    }
#else
    writeData4(byte data)
    {
        DigitalWrite(d4, (data & 0b00000001) != 0);
        DigitalWrite(d5, (data & 0b00000010) != 0);
        DigitalWrite(d6, (data & 0b00000100) != 0);
        DigitalWrite(d7, (data & 0b00001000) != 0);
    }
#endif

    writeData(byte data)
    {
#ifdef LCD_8_BITS  
        writeData8(data);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
#else
        writeData4(data >> 4);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        writeData4(data &0x0F);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
#endif  
    }
#ifdef LCD_8_BITS  
    byte readData8()
    {
        byte data = 0;
        data |= (DigitalRead(d0) ? 0b00000001 : 0b00000000);
        data |= (DigitalRead(d1) ? 0b00000010 : 0b00000000);
        data |= (DigitalRead(d2) ? 0b00000100 : 0b00000000);
        data |= (DigitalRead(d3) ? 0b00001000 : 0b00000000);
        data |= (DigitalRead(d4) ? 0b00010000 : 0b00000000);
        data |= (DigitalRead(d5) ? 0b00100000 : 0b00000000);
        data |= (DigitalRead(d6) ? 0b01000000 : 0b00000000);
        data |= (DigitalRead(d7) ? 0b10000000 : 0b00000000);
        return data;
    }
#else
    byte readData4()
    {
        byte data = 0;
        data |= (DigitalRead(d4) ? 0b00000001 : 0b00000000);
        data |= (DigitalRead(d5) ? 0b00000010 : 0b00000000);
        data |= (DigitalRead(d6) ? 0b00000100 : 0b00000000);
        data |= (DigitalRead(d7) ? 0b00001000 : 0b00000000);
        return data;
    }
#endif
    byte readData()
    {
        byte data = 0;
#ifdef LCD_8_BITS  
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        data = readData8();
        DigitalWrite(e, false);
#else
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        data = readData4() << 4;
        DigitalWrite(e, false);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        data = data | readData4();
        DigitalWrite(e, false);
#endif
        return data;
    }    
    
    wait()
    {
        dataAsInput();
        DigitalWrite(rs, false);
        DigitalWrite(rw, true);
        loop
        {
            byte data = readData();
            if ((data & 0x80) == 0) // busy?
            {
                break;
            }
        }
        dataAsOutput();
    }
    
    byte getCurrentAddress()
    {
        wait();
        dataAsInput();
        DigitalWrite(rs, false);
        DigitalWrite(rw, true);
        byte data = readData();
        dataAsOutput();
        return data;
    }
    instruction(byte data)
    {
        wait();
        DigitalWrite(rs, false);
        DigitalWrite(rw, false);
        writeData(data);
    }
    SetCursorPosition(byte col, byte row)
    {
        byte address = lcdRowStart[row] + col;
        if (currentRow < lcdRows) // don't wrap around if (col,row) out of range (less confusion)
        {
            byte instruction = 0b10000000 | address;
            instruction(instruction); // Set DDRAM address
        }
        currentCol = col;
        currentRow = row;
    }
    Character(char c)
    {
        SetCursorPosition(currentCol, currentRow);
        if (currentRow < lcdRows) // don't wrap around if (col,row) out of range (less confusion)
        {
            wait();
            DigitalWrite(rs, true);
            DigitalWrite(rw, false);
            writeData(byte(c));
        }
        currentCol++;
        if (currentCol == lcdColumns)
        {
            currentCol = 0;
            currentRow++;
        }
    }

    CharacterAt(char c, byte col, byte row)
    {
        currentCol = col;
        currentRow = row;
        Character(c);
    }
#ifdef LCD_8_BITS 
    initialize8Bit(byte functionSet)
    {
        // as per Figure 23 (page 45) of the Hitachi data sheet - yes, much like beating it with a rock!
        
        Delay(1); // > 40ms for Vcc to rise above 2.7V
        
        writeData8(functionSet);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        Delay(5);// 4500 us
        
        writeData8(functionSet);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        Delay(1);// 150 us
        
        writeData8(functionSet);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        
        instruction(functionSet);
    }
#else
    initialize4Bit(byte functionSet)
    {
        // as per Figure 24 (page 46) of the Hitachi data sheet - yes, much like beating it with a rock!
        
        Delay(1); // > 40ms for Vcc to rise above 2.7V
        
        writeData4(0b0010);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        Delay(5); // 4500 us
        
        writeData4(0b0010);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        Delay(1); // 150 us
        
        writeData4(0b0010);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        
        // This 4 bit initialization works well for cold reset (no power to the LCD) but not for resetting
        // an already initialized and powered up LCD (without power cycling).
        // More luck with warm reset with even number of 4 bit writes (in case LCD is already in 4 bit mode)
        writeData4(0b0010);
        // pulse Enable pin
        DigitalWrite(e, false);
        DigitalWrite(e, true);
        DigitalWrite(e, false);
        
        instruction(functionSet);
    }
#endif
    Initialize()
    {
        // 20x4
        /*
        lcdRowStart[0] = 0x00;
        lcdRowStart[1] = 0x40;
        lcdRowStart[2] = 0x14;
        lcdRowStart[3] = 0x54;
        lcdColumns = 20;
        lcdRows = 4;
        */
        
        // 16x2
        lcdRowStart[0] = 0x00;
        lcdRowStart[1] = 0x40;
        lcdColumns = 16;
        lcdRows = 2;
        
        currentCol = 0;
        currentRow = 0;
        
        PinMode(rs, PinModeOption.Output);
        PinMode(rw, PinModeOption.Output);
        PinMode(e, PinModeOption.Output);
        dataAsOutput();
        DigitalWrite(rs, false);
        DigitalWrite(rw, false);
        DigitalWrite(e, false);
        
#ifdef LCD_8_BITS 
        initialize8Bit(0b00111000); // Set 8-bit mode; 2-line display; 5x8 font
#else
        initialize4Bit(0b00101000); // Set 4-bit mode; 2-line display; 5x8 font
#endif
        instruction(0b00001110); // Display on; cursor on; blink off
        instruction(0b00000110); // Increment and shift cursor; don't shift display
        instruction(0b00000001); // Clear screen
        SetCursorPosition(0,0);
    }    
    
}
