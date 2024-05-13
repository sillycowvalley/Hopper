program Game
{
    // https://wilsonminesco.com/6502primer/GENRLI2C.ASM
    
    // https://github.com/hauerdie/6502_i2c/blob/master/i2c.s
    
    #define CPU_1MHZ
    
    //#define CPU_65UINO
    #define CPU_65C02S
    //#define CPU_6502
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/Devices/W65C22"
    uses "/Source/Runtime/6502/Time"
    
    uses "I2C.asm"
    uses "Display.asm"
    uses "Buttons.asm"
    uses "Sprites.asm"
    
    DumpSprites()
    {
        LDA # 0x0A
        Serial.WriteChar();
            
        LDX # 0
        LDY # 16
        loop
        {
            LDA # ' '
            Serial.WriteChar();
            
            LDA Sprites.Sprites, X
            Serial.HexOut();
            INX       
            LDA Sprites.Sprites, X
            Serial.HexOut();
            INX
               
            DEY
            if (Z) { break; }
        }
        LDA # ' '
        Serial.WriteChar();
        LDA Lives
        Serial.HexOut();
    }
    
    AddLife()
    {
        Sprites.GetAvailable();
        if (NZ)
        {
            ASL     // x2
            TAX     // next available sprite x 2
            
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
        
        LDA # 0x0A
        Serial.WriteChar();
        DumpSprites();
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
            //LDA # 0x0A
            //Serial.WriteChar();
            //DumpSprites();
            
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
            
            //DumpSprites();
        }
        CMP # 12 // pill
        if (Z)
        {
            //LDA # 0x0A
            //Serial.WriteChar();
            //DumpSprites();
            
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
            
            //DumpSprites();
        }
    }
    
    IRQ()
    {
        Serial.ISR();
        W65C22.ISR();
    }
    NMI()
    {
    }
    Hopper()
    {
        Serial.Initialize(); // since the 6850 is powered up, we'd better initialize it
        W65C22.Initialize(); // sets all pins to input, initializes timer
        
        LDA # 250
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        Time.Delay();
        
        LDA # 0x3C      // Address of the device (0x78 on the back of the module is 0x3C << 1)
        STA ZP.I2CADDR
        Display.Initialize(); // initialize display
        Buttons.Initialize();
        
        // use the Hopper runtime Time.Delay() (VIA timer)
        LDA # 250
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        Time.Delay();
        
        LDA # 0x00
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        Display.Clear();               
        
        Restart(); // initialize game
 
        // LEDs       
#ifdef CPU_65C02S
        SMB0 ZP.DDRA
        SMB1 ZP.DDRA
#else
        LDA ZP.DDRA
        ORA # 0b00000011
        STA ZP.DDRA
#endif        
        loop
        {
            CheckCollisions();
            LDA Lives
            if (Z) { break; } // game over
            
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
                Sprites.GetAvailable();
                if (NZ)
                {
                    // spawn block sprite
                    
                    ASL     // x2
                    TAX     // next available sprite x 2
                    
                    // Initialize block sprite:
                    //   LSB: |ssss|yyyy|
                    //     ssss:  sprite index : 4
                    //     yyyy:  y location   : 0 is top row
                    Sprites.RandomY();
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
                Sprites.GetAvailable();
                if (NZ)
                {
                    // spawn pill sprite
                    ASL     // x2
                    TAX     // next available sprite x 2
                    
                    // Initialize block sprite:
                    //   LSB: |ssss|yyyy|
                    //     ssss:  sprite index : 12
                    //     yyyy:  y location   : 0 is top row
                    Sprites.RandomY();
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
            }
            
            LDA GameLoops
            AND # 0b00000011
            if (Z) // gameLoop % 4 == 0
            {
                Move();
            }
            
            // LED:
            LDA ZP.PORTA
            EOR # 0b00000001
            STA ZP.PORTA
            
            // use the Hopper runtime Time.Delay() (VIA timer)
            /*
            LDA # 25
            STA ZP.TOPL
            LDA # 0
            STA ZP.TOPH
            Time.Delay();
            */
            INC GameLoops    
        }
    }
}
