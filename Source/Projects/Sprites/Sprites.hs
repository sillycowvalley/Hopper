unit Sprites
{
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    // |ssss|yyyy|zz|s|xxxxx|
    // 
    // ssss:  sprite index
    // yyyy:  y location (0..15)
    // xxxxx: x location (0..31)
    // zz:    z order    (0 means 'invisible')
    // s:     user bit   (1 means 'solid')

    // Sample sprites designs:    
    const byte[] blank  =  { 0b0000, 0b0000, 0b0000, 0b0000 };
    const byte[] block  =  { 0b1111, 0b1111, 0b1111, 0b1111 };
    const byte[] circle =  { 0b0110, 0b1111, 0b1111, 0b0110 };
    const byte[] diamond = { 0b0110, 0b1001, 0b1001, 0b0110 };
    const byte[] box  =    { 0b1111, 0b1001, 0b1001, 0b1111 };
    
    const byte numberOfSprites = 16;
    const byte maxSolids       = 6;
    
    // memory required to maintain and render scene:
    //     2 x numberOfSprites bytes
    uint[numberOfSprites] sprites;    
    
    Initialize()
    {   
        byte solids;
        for (byte i = 0; i < numberOfSprites; i++)
        {
            loop
            {
                byte x     = Tools.Random() % 32;
                byte y     = Tools.Random() % 16;
                byte z     = 1;
                byte s;
                byte index = (Tools.Random() % 4)+1;
                if ((index == 1) || (index == 2))
                {
                    if (solids == maxSolids) { continue; } // limit 
                    z = 2;
                    s = 1;
                    solids++;
                }
                sprites[i] = (index << 12) | (y << 8) | (z << 6) | (s << 5) | x;
                
                IO.WriteLn("Sprite: " + (sprites[i]).ToBinaryString() + " "
                                      + i.ToString() + " Index: " + index.ToString() 
                                      + " X: " +  x.ToString() + " Y: " + y.ToString() + " Z: " + z.ToString()
                                      );
                break;
            }
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
        // if there is a sprite at cellX, cellY:
        // - with z > 0 (visible)
        // - with the greatest z value if there is more than one visible sprite at this location
        bool found;
        foundIndex = 0; // blank
        byte bestZ;
        for (byte i = 0; i < numberOfSprites; i++)
        {
            uint sprite = sprites[i];
            byte x     = byte(sprite & 0b11111);
            byte y     = byte((sprite >> 8) & 0b1111);
            if ((cellX == x) && (cellY == y))
            {
                found = true;
                byte z     = byte((sprite >> 6) & 0b11);
                if (z > bestZ)
                {
                    bestZ = z;
                    foundIndex = byte((sprite >> 12) & 0xF);
                }
            }         
        }
        return found;
    }
    Move()
    {
        for (byte i = 0; i < numberOfSprites; i++)
        {
            uint sprite = sprites[i];
            byte s = byte(sprite & 0b100000) >> 5;
            if (s != 0) // only move the 'solids'
            {
                byte index = byte((sprite >> 12) & 0xF);
                byte z     = byte((sprite >> 6) & 0b11);
                if (z != 0)  // is sprite visible?
                {
                    byte cellX = byte(sprite & 0b11111);
                    byte cellY = byte((sprite >> 8) & 0b1111);
                    
                    // hide it
                    sprite &= 0b1111111100111111; // z == 0
                    sprites[i] = sprite;
                    RenderCell(cellX, cellY);
                    
                    // move it randomly
                    int ix = int(cellX) + (Random() % 3) - 1;
                    
                    // bounce off the left and right edges
                    if (ix == -1)  { ix = 1;  }
                    if (ix > 31)   { ix = 30; }
                    
                    int iy = int(cellY) + (Random() % 3) - 1;
                    
                    // bounce off the top and bottom edges
                    if (iy == -1) { iy = 1; }
                    if (iy > 15)   { iy = 14; }
                    
                    cellX = ix.GetByte(0);
                    cellY = iy.GetByte(0);
                    
                    // show it
                    sprites[i] = (index << 12) | (cellY << 8) | (z << 6) | (s << 5) | cellX;
                    RenderCell(cellX, cellY);
                }
            }
        }
    }
    Render()
    {
        for (byte i = 0; i < numberOfSprites; i++)
        {
            uint sprite = sprites[i];
            byte z     = byte((sprite >> 6) & 0b11);    
            if (z != 0)  // check if sprite is visible
            {
                byte cellX = byte(sprite & 0b11111);
                byte cellY = byte((sprite >> 8) & 0b1111);
                RenderCell(cellX, cellY);
            }
        }
    }
    RenderCell(byte cellX, byte cellY)
    {
        // cell: 4x4 square of pixels:
        cellY = cellY & 0xE; // top cell (could be the one above the one we need to render)
        byte topSpriteIndex;   
        byte bottomSpriteIndex;
        bool topSlot    = SpritePresent(cellX, cellY,   ref topSpriteIndex);
        bool bottomSlot = SpritePresent(cellX, cellY+1, ref bottomSpriteIndex);
        if (topSlot || bottomSlot)
        {
            byte[] topSpriteData    = GetSprite(topSpriteIndex);
            byte[] bottomSpriteData = GetSprite(bottomSpriteIndex);
            RenderSlot(cellX*4, cellY*4, topSpriteData, bottomSpriteData);
        }
    }
    RenderSlot(byte startX, byte startY, byte[] topSpriteData, byte[] bottomSpriteData)
    {
        // slot: 4x8 column of pixels
        byte page = startY / 8; // calculate the page from startY, where each page is 8 pixels high
        
        Wire.BeginTx(DisplayDriver.I2CController, I2CAddress);
        Wire.Write(DisplayDriver.I2CController, 0x00);                          // command mode
        Wire.Write(DisplayDriver.I2CController, 0xB0 + page);                   // set the page address
        Wire.Write(DisplayDriver.I2CController, 0x10 | ((startX & 0xF0) >> 4)); // set column high nibble
        Wire.Write(DisplayDriver.I2CController, 0x00 |  (startX & 0x0F));       // set column low nibble
         _ = Wire.EndTx(I2CController);
        
        Wire.BeginTx(I2CController, I2CAddress);
        Wire.Write(DisplayDriver.I2CController, 0x40);  // data mode
        for (byte col = 0; col < 4; col++)              // each slot is 4 pixels wide (and 8 pixels high)
        {
            Wire.Write(DisplayDriver.I2CController, (bottomSpriteData[col] << 4) | topSpriteData[col]);
        }
        _ = Wire.EndTx(DisplayDriver.I2CController);
    }
}
