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
    
#ifdef CPU_65UINO
    const byte userLand = 0x10;
#else
    const byte userLand = 0x90;
#endif    
    // RAM storage locations:
    //   Global variables:
    const byte Sprites         = userLand;      // 32 bytes from 0x90..0xAF
    const byte YPos            = userLand + 32; // y position of user: 1..15
    const byte Lives           = userLand + 33; // number of lives remaining
    const byte GameLoops       = userLand + 34; // game loop counter
    //   Local working variables:
    const byte CellX           = userLand + 35; // cellX: 0..31
    const byte CellY           = userLand + 36; // cellY: 0..15
    const byte BestZ           = userLand + 37; // 0, 64, 128, 92 (z == 00, 01, 10, 11)
    const byte Result          = userLand + 38; // used in GetVisibleSprite (rendering)
    const byte Current         = userLand + 49; // used in GetVisibleSprite (rendering)
    const byte Index           = userLand + 50; // index of the sprite from GetVisibleSprite
    
    // ######################## Sprites code ##########################
    
    RandomY()
    {
        // returns A : 1..15
        loop
        {
            LDA ZP.TICK0
            AND # 0xF
            if (NZ) { break; }
        }
    }
    Reset()
    {
        // clear all the sprites
        LDY # numberOfSprites
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
        ASL // *= 2
        TAX
        
        LDA Sprites.Sprites, X // LSB: |ssss|yyyy|
        AND # 0b11110000
        PHA //  ssss____
        
        LDA CellX
        PHA
        LDA CellY
        PHA
        
        LDA Sprites.Sprites, X // LSB: |ssss|yyyy|
        AND # 0b00001111
        STA Sprites.Sprites, X
        STA CellY
        
        INX
        LDA Sprites.Sprites, X // MSB: |zz|s|xxxxx|
        AND # 0b00011111
        STA CellX
        DEX
        
        RenderCell(); // hide old position
        
        PLA
        STA CellY
        PLA
        STA CellX
        
        PLA
        ORA CellY
        STA Sprites.Sprites, X
        INX
        LDA Sprites.Sprites, X // MSB: |zz|s|xxxxx|
        AND # 0b11100000
        ORA CellX
        STA Sprites.Sprites, X
        
        RenderCell(); // show new position (CellX, CellY)
    }
    Move()
    {
        // move all the 'movable' sprites left by one
        LDX # (numberOfSprites * 2)
        loop
        {
            DEX
            LDA Sprites.Sprites, X // MSB:  |zz|s|xxxxx|
            STA Current
            DEX
            AND # 0b00100000 // must it move? (s != 0)
            if (NZ)
            {
                LDA Current
                AND # 0b11000000 // is it visible? (z != 0)
                if (NZ)
                {
                    LDA Current
                    AND # 0b00011111
                    STA CellX
                    if (Z)
                    {
                        // remove it
                        LDA Sprites.Sprites, X // MSB: |ssss|yyyy|
                        AND # 0b00001111
                        STA Sprites.Sprites, X 
                        STA CellY   
                        RenderCell();
                        LDA # 0
                        STA Sprites.Sprites, X
                        INX
                        STA Sprites.Sprites, X
                        DEX
                    }
                    else
                    {
                        // move it left
                        DEC CellX
                        LDA Sprites.Sprites, X // MSB: |ssss|yyyy|
                        AND # 0b00001111
                        STA CellY
                        TXA
                        PHA                   
                        
                        LSR A // sprite to move in A
                        // destination in (CellX, CellY)
                        MoveTo();
#ifdef CPU_65C02S
                        PLX
#else
                        PLA TAX
#endif
                    }
                }
            }
            CPX # 0
            if (Z) { break; }
        }
    }
    
    Collision()
    {
        LDA # 0
        STA Result // start with 'blank' sprite in case we get no hits
        LDX # (numberOfSprites * 2) // 32
        loop
        {
            CPX #0
            if (Z)
            {
                break;
            }
            
            // LSB first : |ssss|yyyy|
            DEX
            DEX
            
            LDA Sprites.Sprites, X
            AND # 0b11110000
            STA Current
            if (Z)  { continue; } // blank
            CMP # 0b10000000
            if (Z)  { continue; } // circle : self | life
            
            // block or pill            
            LDA Sprites.Sprites, X
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
                    if (NZ) // non-zero z -> visible
                    {
                        LDA Current
                        LSR LSR LSR LSR // ssss
                        STA Result
                        STX Index
                        LSR Index
                        break;
                    }
                }
                DEX
            }
        } // loop
        LDA Result
    }
    
    GetVisibleSprite()
    {
#ifdef CPU_65C02S
        PHX
#else
        TXA PHA
#endif
        
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
            AND # 0b11110000
            if (NZ) // not blank?
            {
                LDA Current
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
                            CMP BestZ
                            if (NZ) // != BestZ ?
                            {
                                if (C) // > BestZ ?
                                {
                                    STA BestZ
                                    LDA Current
                                    LSR LSR LSR LSR // ssss
                                    STA Result
                                    STX Index
                                    LSR Index
                                }
                            }
                        }
                    }
                    DEX
                }
            }
            CPX #0
            if (Z)
            {
                break;
            }
        }
#ifdef CPU_65C02S
        PLX
#else
        PLA TAX
#endif
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
#ifdef CPU_65C02S
        PHX
        PHY
#else
        TXA PHA
        TYA PHA
#endif
        // round up to the bottom/odd cell in the slot
        LDA Sprites.CellY
        ORA # 0b00000001
        STA Sprites.CellY
        Sprites.GetVisibleSprite(); // (CellX,CellY) -> Y (bottom sprite index)
        TAY
        
        DEC Sprites.CellY
        Sprites.GetVisibleSprite(); // (CellX,CellY) -> X (top sprite index)
        TAX
        
        RenderSlot();
#ifdef CPU_65C02S
        PLY
        PLX
#else
        PLA TAY
        PLA TAX
#endif
    }
    RenderSlot() 
    {
        // slot: 4x8 column of pixels
        // CellX, CellY
        // X - top sprite index
        // Y - bottom sprite index
        
        /*
        LDA # 0x0A
        Serial.WriteChar();
        LDA CellX
        Serial.HexOut();
        LDA # ','
        Serial.WriteChar();
        LDA CellY
        Serial.HexOut();
        LDA # '['
        Serial.WriteChar();
        TXA
        Serial.HexOut();
        LDA # ':'
        Serial.WriteChar();
        TYA
        Serial.HexOut();
        LDA # ']'
        Serial.WriteChar();
        */
        
        // CellY 0..15
        // CellX 0..31
        Display.GotoXY(); // munts A, CellX and CellY
        
        LDA sprites, Y
        ASL ASL ASL ASL
        ORA sprites, X
        Display.WriteCol();
        INX
        INY
        
        LDA sprites, Y
        ASL ASL ASL ASL
        ORA sprites, X
        Display.WriteCol();
        INX
        INY
        
        LDA sprites, Y
        ASL ASL ASL ASL
        ORA sprites, X
        Display.WriteCol();
        INX
        INY
        
        LDA sprites, Y
        ASL ASL ASL ASL
        ORA sprites, X
        Display.WriteCol();
        //INX
        //INY
    }
}
    
    
