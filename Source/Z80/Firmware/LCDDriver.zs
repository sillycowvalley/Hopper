unit LCDDriver
{
    uses "/Source/Z80/Firmware/Utilities" // for Delay
    
    const byte portA         = 0b00000000;
    const byte portB         = 0b00000001;
    const byte portC         = 0b00000010;
    const byte controlPort   = 0b00000011;
    const byte ModeZero      = 0b10000000;
    const byte portARead     = 0b00000000;
    const byte portBWrite    = 0b00000000;
    const byte portCWrite    = 0b00000000;
    const byte portCRead     = 0b00001001;
    
    const byte buttonPort       = portA;
    const byte lcdDataPort      = portC;
    const byte lcdControlPort   = portB;
    const byte lcdRS            = 0b00000001;
    const byte lcdRW            = 0b00000010;
    const byte lcdEN            = 0b00000100;
    const byte lcdRows          = 4;
    const byte lcdColumns       = 20;
    
    byte currentCol;
    byte currentRow;
    
    LCDInstruction(byte instruction)
    {
        port[controlPort] = (ModeZero | portCWrite | portBWrite | portARead);
        port[lcdDataPort] = instruction;
        // toggle EN
        port[lcdControlPort] = 0;
        port[lcdControlPort] = lcdEN;
        port[lcdControlPort] = 0;
    }
    LCDData(byte db)
    {
        port[controlPort] = (ModeZero | portCWrite  | portBWrite | portARead);
        port[lcdDataPort] = db;
        // toggle EN
        port[lcdControlPort] = (lcdRS);
        port[lcdControlPort] = (lcdRS | lcdEN);
        port[lcdControlPort] = (lcdRS);
    }
   
    LCDWait()
    {
        // getting ready to read the instruction port
        port[controlPort] = (ModeZero | portCRead  | portBWrite | portARead);
        loop
        {
            port[lcdControlPort] = lcdRW;
            port[lcdControlPort] = (lcdRW | lcdEN); // toggle EN
            byte db = port[lcdDataPort];
            if (0 != (db & 0b10000000))
            {
                continue; // busy
            }
            break;
        }
        port[lcdControlPort] = lcdRW;
    }
    
    LCDInitialize()
    {
        // as per Figure 24 (page 46) of the Hitachi data sheet - yes, much like beating it with a rock!

        Delay(50); // > 40ms for Vcc to rise above 2.7V
        LCDInstruction(0b00111000); // Set 8-bit mode; 2-line display; 5x8 font
        Delay(5);  // 5ms, not 4500us
        LCDInstruction(0b00111000); // Set 8-bit mode; 2-line display; 5x8 font
        Delay(1);  // 1ms, not 150us
        LCDInstruction(0b00111000); // Set 8-bit mode; 2-line display; 5x8 font
        
        LCDWait();
        LCDInstruction(0b00001110); // Display on; cursor on; blink off
        LCDWait();
        LCDInstruction(0b00000110); // Increment and shift cursor; don't shift display
        LCDClear();
    }
    LCDClear()
    {
        LCDWait();
        LCDInstruction(0b00000001); // Clear screen
        LCDWait();
        LCDSetCursorPosition(0,0);
    }
    LCDSetCursorPosition(byte col, byte row)
    {
        byte lcdRowStart = 0x00;
        if (row == 1)
        {
            lcdRowStart = 0x40;
        }
        else if (row == 2)
        {
            lcdRowStart = 0x14;
        }
        else if (row == 3)
        {
            lcdRowStart = 0x54;
        }
        
        byte address = lcdRowStart + col;
        if (currentRow < lcdRows) // don't wrap around if (col,row) out of range (less confusion)
        {
            byte instruction = (0b10000000 | address);
            LCDWait();
            LCDInstruction(instruction); // Set DDRAM address
        }
        currentCol = col;
        currentRow = row;
    }
    LCDCharacter(char c)
    {
        LCDSetCursorPosition(currentCol, currentRow);
        if (currentRow < lcdRows) // don't wrap around if (col,row) out of range (less confusion)
        {
            LCDWait();
            LCDData(byte(c));
        }
        currentCol++;
        if (currentCol == lcdColumns)
        {
            currentCol = 0;
            currentRow++;
        }
    }
    LCDCharacterAt(char c, byte col, byte row)
    {
        currentCol = col;
        currentRow = row;
        LCDCharacter(c);
    }

    ButtonWait(byte portMask)
    {
        Delay(5);  // 5ms to debounce
        port[controlPort] = (ModeZero | portCWrite | portBWrite | portARead);
        loop
        {
            byte db = port[buttonPort];
            if (0 != (db & portMask))
            {
                break; // button is down
            }
        }
        Delay(5);  // 5ms to debounce
        loop
        {
            byte db = port[buttonPort];
            if (0 == (db & portMask))
            {
                break; // button is back up
            }
        }
        Delay(50);  // 50ms to debounce
    }
}
