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
    const byte Sprites         = 0x90; // 32 bytes from 0x90..0xAF
    const byte YPos            = 0xB0; // y position of user: 1..15
    const byte Lives           = 0xB1; // number of lives remaining
    
    // 65uinio:
    //
    
    // ######################## Sprites code ##########################
    
    Reset()
    {
        // clear all the sprites
        LDY # numberOfSprites
        INY
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
        LDX # 30 // start from the rightmost LSB
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
}
    
    
