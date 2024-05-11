unit Sprites
{
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    // |ssss|yyyy|zz|c|xxxxx|
    // 
    // ssss:  sprite index
    // yyyy:  y location (0..16)
    // xxxxx: x location (0..32)
    // zz:    z order    (0 means 'invisible')
    // c:     collision  (1 means 'solid')
    
    const byte numberOfSprites = 16;
    const byte[] blank  =  { 0b0000, 0b0000, 0b0000, 0b0000 };
    const byte[] block  =  { 0b1111, 0b1111, 0b1111, 0b1111 };
    const byte[] circle =  { 0b0110, 0b1111, 0b1111, 0b0110 };
    const byte[] diamond = { 0b0110, 0b1001, 0b1001, 0b0110 };
    const byte[] box  =    { 0b1111, 0b1001, 0b1001, 0b1111 };
    
    const byte[] heart =   { 0b0101, 0b1111, 0b1111, 0b0110 };
    const byte[] arrow =   { 0b0010, 0b0110, 0b1010, 0b0010 };
    const byte[] smiley =  { 0b0000, 0b0101, 0b0000, 0b0110 };
    const byte[] star =    { 0b0101, 0b1111, 0b1111, 0b0101 };
    





    uint[numberOfSprites] sprites;
    
    Initialize()
    {
        for (byte i = 0; i < numberOfSprites; i++)
        {
            byte x     = Tools.Random() % 32;
            byte y     = Tools.Random() % 16;
            byte z     = 1;
            byte index = Tools.Random() % 5;
            sprites[i] = (index << 12) | (y << 8) | (z << 6) | x;
            
            // Debugging output
            IO.WriteLn("Sprite: " + i.ToString() + " Index: " + index.ToString() + " X: " +  x.ToString() + " Y: " + y.ToString() + " Z: " + z.ToString());
        }
    }
    byte[4] GetSprite(byte spriteIndex)
    {
        switch (spriteIndex)
        {
            case 1:  { return block; }
            case 2:  { return circle; }
            case 3:  { return diamond; }
            case 4:  { return box; }
        }
        return blank;
    }
    bool SpritePresent(byte cellX, byte cellY, ref byte foundIndex)
    {
        foundIndex = 0;
        for (byte i = 0; i < numberOfSprites; i++)
        {
            uint sprite = sprites[i];
            byte index = byte((sprite >> 12) & 0xF);    // Get sprite index
            byte x     = byte(sprite & 0b11111);        // Get x coordinate
            byte y     = byte((sprite >> 8) & 0b1111);  // Get y coordinate
            byte z     = byte((sprite >> 6) & 0b11);    // Get z order (visibility check)
            
            
            if ((z != 0) && (cellX == x) && (cellY == y))  // Check if sprite is visible
            {
                foundIndex = index;
                return true;
            }         
        }
        return false;
    }
    Render()
    {
        
        for (byte cellX = 0; cellX < 32; cellX++)
        {
            for (byte cellY = 0; cellY < 16; cellY += 2)
            {
                byte topSpriteIndex;   
                byte bottomSpriteIndex;
                bool topSlot    = SpritePresent(cellX, cellY,   ref topSpriteIndex);
                bool bottomSlot = SpritePresent(cellX, cellY+1, ref bottomSpriteIndex);
                if (topSlot || bottomSlot)
                {
                    byte[] topSpriteData  = GetSprite(topSpriteIndex);
                    byte[] bottomSpriteData = GetSprite(bottomSpriteIndex);
                    RenderBlock(cellX*4, cellY*4, topSpriteData, bottomSpriteData);
                }
            }
        }
    }
    
    RenderBlock(byte startX, byte startY, byte[] topSpriteData, byte[] bottomSpriteData)
    {
        byte page = startY / 8;              // Calculate the page from startY, where each page is 8 pixels high
        
        //byte offsetWithinPage = (startY % 8) * 16;  // Calculate initial byte offset within the page
        //offsetWithinPage += (startX / 8);           // Adjust offset by the horizontal starting position
        byte offsetWithinPage = startX;
        
        Wire.BeginTx(DisplayDriver.I2CController, I2CAddress);
        Wire.Write(DisplayDriver.I2CController, 0x00);                          // Command mode
        Wire.Write(DisplayDriver.I2CController, 0xB0 + page);                   // Set the page address
        Wire.Write(DisplayDriver.I2CController, 0x10 | ((offsetWithinPage & 0xF0) >> 4)); // Set column high nibble
        Wire.Write(DisplayDriver.I2CController, 0x00 | (offsetWithinPage & 0x0F));        // Set column low nibble
         _ = Wire.EndTx(I2CController); // End transmission
        
        Wire.BeginTx(I2CController, I2CAddress);
        Wire.Write(DisplayDriver.I2CController, 0x40);  // Data mode
        for (byte col = 0; col < 4; col++)  // Each sprite is 4 pixels wide
        {
            // Combine the two sprite data, aligning right sprite data adjacent to the left
            byte combinedRowData = (bottomSpriteData[col] << 4) | topSpriteData[col];
            Wire.Write(DisplayDriver.I2CController, combinedRowData);
        }
        _ = Wire.EndTx(DisplayDriver.I2CController);
    }
    
    writeCommands(byte[] commands)
    {
        Wire.BeginTx(I2CController, I2CAddress); // Start transmission, SSD1306's address needs to be defined
        for (uint i = 0; i < commands.Count; i++)
        {
            Wire.Write(I2CController, commands[i]); // Send each command byte
        }
        _ = Wire.EndTx(I2CController); // End transmission
    }
    
    writeData(byte data)
    {
        Wire.BeginTx(I2CController, I2CAddress); // Start transmission, SSD1306's address
        Wire.Write(I2CController, 0x40); // Data byte prefix for SSD1306
        Wire.Write(I2CController, data); // Send the data byte
        _ = Wire.EndTx(I2CController); // End transmission
    }
    
    
    /*
    RenderBlock(byte x, byte y, byte[] spriteData1, byte[] spriteData2)
    {
        byte page = byte(y / 8);
        byte colAddress; 
        byte[3] data;
            
        for (byte col = 0; col < 4; col++)
        {
            colAddress = x + col; // Compute the current column address
            data[0] = 0xB0 + page;                             // set page address
            data[1] = byte(((colAddress & 0xF0) >> 4) | 0x10); // set column high nibble
            data[2] = byte(colAddress & 0x0F);                 // set column low nibble
            
            // send command
           writeCommands(data);
            
            // write pixel
            writeData((spriteData1[col] << 4) | spriteData2[col]);
        }
    } 
    */
    
    
    DirectPixel(int vx, int vy, uint colour)
    {
        uint ux = uint(vx);
        uint uy = uint(vy);
        
        byte page = byte(uy / 8);
        byte bitMask = byte(1 << (uy % 8));
        byte[3] data;
    
        // command to set the page and column
        data[0] = 0xB0 + page;                     // set page address
        data[1] = byte(((ux & 0xF0) >> 4) | 0x10); // set column high nibble
        data[2] = byte(ux & 0x0F);                 // set column low nibble
    
        byte pixelData = (colour == Colour.White) ? bitMask : 0;
        
        // send command
        writeCommands(data);
    
        // write pixel
        writeData(pixelData);
    }  
       
    
    
        
    /*
    DirectPixel2x2(int vx, int vy, uint colour)
    {
        // Adjust vx and vy to handle 2x2 pixel blocks
        vx *= 2;
        vy *= 2;
    
        // Set the two vertical pixels within the same byte
        for (int dx = 0; dx < 2; dx++)  // Only iterate over x since y pixels are within the same byte
        {
            int ux = vx + dx;          // Calculate the x coordinate for each sub-pixel
            int uy = vy;               // y coordinate remains the same for two vertical pixels
            byte page = byte(uy / 8);
            byte bitMask1 = byte(1 << (uy % 8));           // Bit mask for the first pixel
            byte bitMask2 = byte(1 << ((uy + 1) % 8));     // Bit mask for the pixel right below it
    
            byte[3] commandData;
    
            // Command to set the page and column
            commandData[0] = 0xB0 + page;                       // Set page address
            commandData[1] = byte(((ux & 0xF0) >> 4) | 0x10);    // Set column high nibble
            commandData[2] = byte(ux & 0x0F);                    // Set column low nibble
    
            // Combine bit masks for two vertical pixels
            byte pixelData = (colour == Colour.White) ? (bitMask1 | bitMask2) : 0;
    
            // Send command
            writeCommands(commandData);
    
            // Write pixel data for two vertical pixels simultaneously
            writeData(pixelData);
        }
    }
    */
}
