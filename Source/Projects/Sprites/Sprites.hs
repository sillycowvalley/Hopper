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
    const byte[] blank  = { 0b0000, 0b0000, 0b0000, 0b0000 };
    const byte[] block  = { 0b1111, 0b1111, 0b1111, 0b1111 };
    const byte[] disc   = { 0b0110, 0b1111, 0b1111, 0b0110 };
    const byte[] circle = { 0b0110, 0b1001, 0b1001, 0b0110 };
    const byte[] square = { 0b1111, 0b1001, 0b1001, 0b1111 };
    const byte[] pill   = { 0b0000, 0b0110, 0b0110, 0b0000 };
    
    const byte numberOfSprites = 16;
    const byte maxSolids       = 6;
    
    // memory required to maintain and render scene:
    //     2 x numberOfSprites bytes
    uint[numberOfSprites] sprites;
    
    Initialize() // demo code
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
                /*
                IO.WriteLn("Sprite: " + (sprites[i]).ToBinaryString() + " "
                                      + i.ToString() + " Index: " + index.ToString() 
                                      + " X: " +  x.ToString() + " Y: " + y.ToString() + " Z: " + z.ToString()
                                      );
                */
                break;
            }
        }
    }
    
    New(byte iSprite, byte spriteIndex, byte cellX, byte cellY, byte z, bool moving)
    {
        sprites[iSprite] = (spriteIndex << 12) | (cellY << 8) | (z << 6) | ((moving ? 1 : 0) << 5) | cellX;
    }
    DeleteSprite(byte iSprite)
    {
        uint sprite = sprites[iSprite];
        byte cellX = byte(sprite & 0b11111);
        byte cellY = byte((sprite >> 8) & 0b1111);
        sprites[iSprite] = 0;
        RenderCell(cellX, cellY);
    }
    MoveTo(byte iSprite, byte cellX, byte cellY)
    {
        uint sprite = sprites[iSprite];
        byte spriteIndex = byte((sprite >> 12) & 0xF);
        byte s = byte(sprite & 0b100000) >> 5;
        byte z     = byte((sprite >> 6) & 0b11);
        if (z != 0) // was visible?
        {
            byte previousCellX = byte(sprite & 0b11111);
            byte previousCellY = byte((sprite >> 8) & 0b1111);
                
            // hide it
            sprite &= 0b1111111100111111; // z == 0
            sprites[iSprite] = sprite;
            RenderCell(previousCellX, previousCellY);
        }
        // show it
        sprites[iSprite] = (spriteIndex << 12) | (cellY << 8) | (z << 6) | (s << 5) | cellX;
        RenderCell(cellX, cellY);
    }
    bool IsSpriteAvailable(ref byte iSprite)
    {
        for (byte i = 1; i < numberOfSprites; i++)
        {
            uint sprite      = sprites[i];
            byte spriteIndex = byte((sprite >> 12) & 0xF);
            if (spriteIndex == 0) // available
            {
                iSprite = i;
                return true;
            }
        }
        return false;
    }
    
    Move()
    {
        for (byte iSprite =0; iSprite < numberOfSprites; iSprite++)
        {
            uint sprite = sprites[iSprite];
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
                    sprites[iSprite] = sprite;
                    RenderCell(cellX, cellY);
                    if (cellX > 0)
                    {
                        cellX = cellX - 1;
                        
                        // show it
                        sprites[iSprite] = (index << 12) | (cellY << 8) | (z << 6) | (s << 5) | cellX;
                        RenderCell(cellX, cellY);
                    }
                    else
                    {
                        // destroy sprite
                        sprites[iSprite] = 0;
                    }
                }
            }
        }
    }
    
    byte[4] GetSpritePattern(byte spriteIndex)
    {
        switch (spriteIndex)
        {
            case 1:  { return block;  }
            case 2:  { return disc;   }
            case 3:  { return circle; }
            case 4:  { return square; }
            case 5:  { return pill;   }
        }
        return blank;
    }
    bool Collision(byte collisionCellX, byte collisionCellY, ref byte spriteType)
    {
        spriteType = 0;
        for (byte i = 0; i < numberOfSprites; i++)
        {
            uint sprite = sprites[i];
            byte s = byte(sprite & 0b100000) >> 5;
            if (s != 0) // collision 'solids'
            {
                byte cellX = byte(sprite & 0b11111);
                byte cellY = byte((sprite >> 8) & 0b1111);
                if ((collisionCellX == cellX) && (collisionCellY == cellY))
                {
                    spriteType = byte((sprite >> 12) & 0xF);
                    sprites[i] = 0; // destroy the thing that collided so we don't get multiple collisions
                    RenderCell(collisionCellX, collisionCellY); // re-render the thing it collided with
                    return true;
                }
            }
        }
        return false;
    }
    byte GetVisibleSpriteIndex(byte cellX, byte cellY)
    {
        byte foundIndex; // 0 -> blank
        byte iSprite;
        if (GetVisibleSprite(cellX, cellY, ref iSprite))
        {
            uint sprite = sprites[iSprite];
            foundIndex = byte((sprite >> 12) & 0xF);
        }
        return foundIndex;
    }
    bool GetVisibleSprite(byte cellX, byte cellY, ref byte foundIndex)
    {
        // if there is a sprite at cellX, cellY:
        // - with z > 0 (visible)
        // - with the greatest z value if there is more than one visible sprite at this location
        // - or, failing that, zero to indicate the 'blank' sprite
        bool found;
        byte bestZ;
        for (byte iSprite = 0; iSprite < numberOfSprites; iSprite++)
        {
            uint sprite = sprites[iSprite];
            byte x     = byte(sprite & 0b11111);
            byte y     = byte((sprite >> 8) & 0b1111);
            if ((cellX == x) && (cellY == y))
            {
                byte z     = byte((sprite >> 6) & 0b11);
                if (z > bestZ)
                {
                    bestZ = z;
                    foundIndex = iSprite;
                    found = true;
                }
            }         
        }
        return found;
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
        
        cellY = cellY & 0xE;      // round down to the top cell in the slot
        byte topSpriteIndex     = GetVisibleSpriteIndex(cellX, cellY);
        byte bottomSpriteIndex  = GetVisibleSpriteIndex(cellX, cellY+1); 
        byte[] topSpriteData    = GetSpritePattern(topSpriteIndex);
        byte[] bottomSpriteData = GetSpritePattern(bottomSpriteIndex);
        RenderSlot(cellX*4, cellY*4, topSpriteData, bottomSpriteData);
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
