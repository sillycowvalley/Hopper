unit Sprites
{    
    uses "/Source/Runtime/6502/ZeroPage"
    uses "I2C.asm"
    
    
    // |ssss|yyyy|zz|s|xxxxx|
    // 
    // ssss:  sprite index
    // yyyy:  y location (0..15)
    // xxxxx: x location (0..31)
    // zz:    z order    (0 means 'invisible')
    // s:     user bit   (1 means 'solid')

    // Sample sprites designs:    
    //   Note: While it is possible to store each 4x4 sprite
    //         in just 2 bytes (rather than 4), this approach
    //         is favouring rendering speed over storage.
    //         Also, this is a wasteful use of the sprite index
    //         to avoid shifting at runtime for rendering.
    const byte[] sprites = { 0b0000, 0b0000, 0b0000, 0b0000,   // 0 : blank
                             0b1111, 0b1111, 0b1111, 0b1111,   // 4 : block
                             0b0110, 0b1001, 0b1001, 0b0110,   // 8 : circle
                             0b0000, 0b0110, 0b0110, 0b0000 }; // 12: pill
    const byte numberOfSprites = 16;
    
    // RAM storage locations:
    // Hopper:
    //   Global variables:
    const byte Sprites         = 0x90; // 32 bytes from 0x90..0xAF
    const byte YPos            = 0xB0; // y position of user: 1..15
    const byte Lives           = 0xB1; // number of lives remaining
    const byte GameLoops       = 0xB2; // game loop counter
    //   Local working variables:
    const byte CellX           = 0xB3; // cellX: 0..31
    const byte CellY           = 0xB4; // cellY: 0..15
    const byte BestZ           = 0xB5; // 0, 64, 128, 92 (z == 00, 01, 10, 11)
    const byte Result          = 0xB6;
    const byte Current         = 0xB7;
    
    // 65uinio:
    //
    
    // ######################## Sprites code ##########################
    
    Reset()
    {
        // clear all the sprites
        LDY # (numberOfSprites + 1)
        LDX # 0
        loop
        {
            LDA # 0
            STA Sprites.Sprites, X
            INX
            STA Sprites.Sprites, X
            INX
            DEY
            if (Z) { break; }
        }   
    }
    GetAvailable()
    {
        // returns next available sprite (currently 'blank')
        //     - A : 1..15
        //     - A : 0 is the player slot which means "none available"
        LDX # (numberOfSprites * 2 - 2) // start from the rightmost LSB
        loop
        {
            LDA Sprites.Sprites, X
            AND # 0b11110000
            if (Z)
            {
                // found a slot that is the "blank" index
                TXA
                LSR
                break; 
            }
            DEX
            DEX
            if (Z)
            {
                // none available
                LDA # 0
                break;
            }
        } // loop
    }
    
    MoveTo()
    {
        // sprite to move in A
        // destination in (CellX, CellY)
        
        // TODO:
        
        RenderCell();
    }
    Move()
    {
        // move all the 'movable' sprites left by one
        // TODO
    }
    
    
    GetVisibleSprite()
    {
        PHX
        
        LDA # 0
        STA Result // start with 'blank' sprite in case we get no hits
        STA BestZ  // nothing yet
        LDX # (numberOfSprites * 2) // 32
        loop
        {
            // LSB first : |ssss|yyyy|
            DEX
            DEX
            
            LDA Sprites.Sprites, X
            STA Current
            AND # 0b00001111
            CMP Sprites.CellY
            if (Z) 
            {
                // Y coordinate matches
                
                // now MSB:  |zz|s|xxxxx|
                INX    
                LDA Sprites.Sprites, X
                AND # 0b00011111
                CMP Sprites.CellX
                if (Z)
                {
                    // X coordinate matches
                    LDA Sprites.Sprites, X
                    AND # 0b11000000
                    if (NZ) // non-zero z
                    {
                        // TOD: > BestZ?
                        STA BestZ
                        LDA Current
                        LSR LSR LSR LSR // ssss
                        STA Result
                    }
                }
                DEX
            }
            CPX #0
            if (Z)
            {
                break;
            }
        }
        PLX
        
        LDA Result
    }
    
    Render()
    {
        LDX # (numberOfSprites * 2)
        loop
        {
            DEX
            // ssss|yyyy| zz|s|xxxxx
            LDA Sprites.Sprites, X // MSB
            AND # 0b11000000
            if (NZ) 
            {
                // sprite is visible since zz != 0
                LDA Sprites.Sprites, X // MSB
                AND # 0b00011111
                STA Sprites.CellX
                DEX
                LDA Sprites.Sprites, X // LSB
                AND # 0b00001111
                STA Sprites.CellY
                
                RenderCell();
            }
            else
            {
                DEX
            }
            CPX # 0
            if (Z) { break; }
        }
    }
    RenderCell() // (CellX, CellY)
    {
        // cell: 4x4 square of pixels:
        
        // round up to the top/odd cell in the slot
        LDA Sprites.CellY
        ORA # 0b00000001
        STA Sprites.CellY
        Sprites.GetVisibleSprite(); // (CellX,CellY) -> A (bottom sprite index)
        PHA
        
        DEC Sprites.CellY
        Sprites.GetVisibleSprite(); // (CellX,CellY) -> A (top sprite index)
        PHA
        
        RenderSlot();
    }
    RenderSlot() // cellX*4, cellY*4, topSpriteData, bottomSpriteData
    {
        LDA # 0x0A
        Serial.WriteChar();
        
        LDA # ' '
        Serial.WriteChar();
        LDA Sprites.CellX
        Serial.HexOut();
        LDA # ','
        Serial.WriteChar();
        LDA Sprites.CellY
        Serial.HexOut();
        
        LDA # '['
        Serial.WriteChar();
        PLA
        Serial.HexOut();
        LDA # ':'
        Serial.WriteChar();
        PLA
        Serial.HexOut();
        LDA # ']'
        Serial.WriteChar();
        // TODO
    }
}
    
    
