program Game
{
    
    #define CPU_65UINO
    
    uses "RIOT"
    uses "Utilities"
    uses "I2C"
    
    uses "Display.asm"
    uses "Buttons.asm"
    uses "Sprites.asm"
    
    AddLife()
    {
        Sprites.GetAvailablex2(); // next available sprite x 2 -> X
        if (NZ)
        {
            // Initialize life sprite:
            //   LSB: |ssss|yyyy|
            //     ssss:  sprite index : 8
            //     yyyy:  y location   : 0 is top row
            LDA # ((8 << 4) | 0)
            STA Sprites.Sprites, X
            INX
            //   MSB: |zz|s|xxxxx|
            //     s:     user bit   : 0 means don't scroll it
            //     zz:    z order    : 2 is between blocks (3) and pills (1)
            //     xxxxx: x location : column = life
            INC Sprites.Lives
            LDA Sprites.Lives 
            ORA # (2 << 6)
            STA Sprites.Sprites, X
        }
    }
    Restart()
    {
        LDA # 0 
        STA Lives
        LDA # 7
        STA YPos
        
        Sprites.Reset();
        
        // Initialize user sprite:
        //   LSB: |ssss|yyyy|
        //     ssss:  sprite index : 8
        //     yyyy:  y location   : starts at 7
        LDX # 0
        LDA # ((8 << 4) | 7)
        STA Sprites.Sprites, X
        INX
        //   MSB: |zz|s|xxxxx|
        //     s:     user bit   : 0 means don't scroll it
        //     zz:    z order    : 2 is between blocks (3) and pills (1)
        //     xxxxx: x location : 0 is leftmost column
        LDA # ((2 << 6) | 0)
        STA Sprites.Sprites, X
        
        AddLife();
        AddLife();
        AddLife();
        AddLife();
        
        Sprites.Render();
    }
    CheckCollisions()
    {
        LDA # 0
        STA CellX
        LDA YPos
        STA CellY
        
        
        Sprites.Collision();
        CMP # 4 // block
        if (Z)
        {
            // remove block sprite
            LDA Index
            ASL
            TAX
            LDA # 0b00000000
            STA Sprites.Sprites, X
            INX
            STA Sprites.Sprites, X
            
            LDA # 0
            STA CellY
            LDA Lives
            STA CellX
            Sprites.GetVisibleSprite();
            if (NZ)
            {
                // remove life sprite
                LDA Index
                ASL
                TAX
                LDA Sprites.Sprites, X // MSB: |ssss|yyyy|
                AND # 0b00001111       // blank
                STA Sprites.Sprites, X 
                
                RenderCell();
                DEC Lives
                
                LDA # 0
                STA Sprites.Sprites, X 
                INX
                STA Sprites.Sprites, X 
            }
            
            LDA # 0
            STA CellX
            LDA YPos
            STA CellY
            RenderCell();
            return;
        }
        CMP # 12 // pill
        if (Z)
        {
            // remove it
            LDA Index
            ASL
            TAX
            LDA # 0b00000000
            STA Sprites.Sprites, X
            INX
            STA Sprites.Sprites, X
            
            AddLife();
            
            LDA Lives
            STA CellX
            LDA # 0
            STA CellY
            RenderCell();
        }

    }
    
    Hopper()
    {
        LDA # 0b00000000 // PA all input
        STA RIOT.DDRA
        
        LDA # 0b10000000 // PB7 is output (GREEN LED)
        STA RIOT.DDRB
        
        LEDOff();
        
        Display.Initialize(); // initialize display
        Buttons.Initialize();
        
        Display.Clear();               
        
        Restart(); // initialize game
 
        loop
        {
            CheckCollisions();
            LDA Lives
            if (Z) 
            { 
                break;  // game over
            }
                     
            ButtonOneDown();
            if (Z)
            {
                LDA # 1
                CMP YPos
                if (NZ)
                {
                    DEC YPos
                    LDA YPos
                    STA CellY
                    LDA # 0
                    STA CellX
                    LDA # 0
                    Sprites.MoveTo();
                }
            }
            ButtonTwoDown();
            if (Z)
            {
                LDA # 15
                CMP YPos
                if (NZ)
                {
                    INC YPos
                    LDA YPos
                    STA CellY
                    LDA # 0
                    STA CellX
                    LDA # 0
                    Sprites.MoveTo();
                    
                    
                }
            }
                       
            LDA GameLoops
            AND # 0b00011111
            if (Z) // gameLoop % 32 == 0
            {
                Sprites.GetAvailablex2(); // next available sprite x 2 -> X
                if (NZ)
                {
                    // spawn block sprite
                    
                    // Initialize block sprite:
                    //   LSB: |ssss|yyyy|
                    //     ssss:  sprite index : 4
                    //     yyyy:  y location   : 0 is top row
                    Sprites.RandomY(); // munts Y
                    STA CellY
                    ORA # (4 << 4)
                    STA Sprites.Sprites, X
                    INX
                    //   MSB |zz|s|xxxxx|
                    //     s:     user bit   : 1 means scroll it
                    //     zz:    z order    : 3 for blocks
                    //     xxxxx: x location : 31 for start column
                    LDA # ((3 << 6) | (1 << 5) | 31)
                    STA Sprites.Sprites, X
                    LDA # 31
                    STA CellX
                    
                    Sprites.RenderCell();
                }
                
            }
            
            CLC
            LDA GameLoops
            ADC # 16
            AND # 0b00111111
            if (Z) // (gameLoop+16) % 64 == 0
            {
                Sprites.GetAvailablex2(); // next available sprite x 2 -> X
                if (NZ)
                {
                    // spawn pill sprite
                    
                    // Initialize block sprite:
                    //   LSB: |ssss|yyyy|
                    //     ssss:  sprite index : 12
                    //     yyyy:  y location   : 0 is top row
                    Sprites.RandomY(); // munts Y
                    STA CellY
                    ORA # (12 << 4)
                    STA Sprites.Sprites, X
                    INX
                    //   MSB: |zz|s|xxxxx|
                    //     s:     user bit   : 1 means scroll it
                    //     zz:    z order    : 1 for pills
                    //     xxxxx: x location : 31 for start column
                    LDA # ((1 << 6) | (1 << 5) | 31)
                    STA Sprites.Sprites, X
                    LDA # 31
                    STA CellX
                    Sprites.RenderCell();
                }
                else
                {
                    LEDOn();
                    break; // game over -> won
                }
            }
            
            LDA GameLoops
            AND # 0b00000011
            if (Z) // gameLoop % 4 == 0
            {
                Sprites.Move();
            }
            INC GameLoops    
        }
    }
}
