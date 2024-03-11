program BreakOut
{
    // This port from uPython is based on source found on the
    // Raspberry Pi Pico facebook group:
    //    https://www.facebook.com/groups/pipico/files/files
    //
    // Improvements in this version:
    //    - scales depending on DisplayDriver dimensions
    //        - initial paddle width a percentage of display width
    //    - pre-configured buttons for several WaveShare devices
    //    - offset the bricks in alternate rows
    //    - only allow random row colours to be used once
    //    - shrinking paddle as levels go up
    //    - increasing speed as levels go up
    //    - portrait or landscape orientations supported
    //    - TFT (colour) and OLED (mono) supported
    //
    // This version has no sound effects since none of the devices
    // available to me have sound capability (yet).
    //
    
    uses "/Source/Library/Devices/WSPicoLCD144"
    //uses "/Source/Library/Devices/WSPicoLCD114"
    //uses "/Source/Library/Devices/WSPicoLCD096"
    //uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    //#define TEST_PLAY // for testing
    
    uses "Utilities"
    
    uses "Paddle"
    uses "Ball"
    uses "Bricks"
    
    const uint startLives = 5;
    
    uint lives = startLives;
    uint score = 0;
    uint level = 1;
    
    uint Score { get { return score; } set { score = value; } }
    uint Lives { get { return lives; } set { lives = value; } }
    
    bool LeftButton
    {
        get
        {
#if defined(WAVESHARE_PICO_LCD_114) || defined(WAVESHARE_PICO_LCD_096)
            return DisplayDriver.IsPortrait ? ButtonB : ButtonLeft;
#endif
#ifdef WAVESHARE_PICO_LCD_144            
            return Button0;
#endif
#ifdef OLED_FEATHERWING_128x64
            return ButtonA;
#endif
        }
    }
    bool RightButton
    {
        get
        {
#if defined(WAVESHARE_PICO_LCD_114) || defined(WAVESHARE_PICO_LCD_096)
            return DisplayDriver.IsPortrait ? ButtonA : ButtonRight;
#endif
#ifdef WAVESHARE_PICO_LCD_144
            return Button3;
#endif
#ifdef OLED_FEATHERWING_128x64
            return ButtonC;
#endif
        }
    }
    bool RestartButton
    {
        get
        {
#if defined(WAVESHARE_PICO_LCD_114) || defined(WAVESHARE_PICO_LCD_096)
            return DisplayDriver.IsPortrait ? ButtonButton : (ButtonA || ButtonB);
#endif
#ifdef WAVESHARE_PICO_LCD_144
            return Button1 || Button2;
#endif
#ifdef OLED_FEATHERWING_128x64
            return ButtonB;
#endif
        }    
    }
    
    
    updateScore()
    {
        Display.Suspend();
#ifdef DISPLAY_IS_MONO             
        uint foreColour = Colour.Black;
#else
        uint foreColour = Colour.White;
#endif
        Display.FilledRectangle(0,0, Display.PixelWidth, (CellHeight+1)*2, Colour.Blue);
        SetCursor(0,0);
        Screen.PrintLn("Score: " + score.ToString(), foreColour, Colour.Blue);
        
        string livesText = "Lives: " + lives.ToString();
        string levelText = "Level: " + level.ToString();
        if (Display.PixelWidth <= 64)
        {
            levelText = "";
        }
        else if (livesText.Length + levelText.Length + 1 > Screen.Columns)
        {
            levelText = "Lvl:" + level.ToString();    
            livesText = "Lives:" + lives.ToString();
        }
        if (livesText.Length + levelText.Length + 1 > Screen.Columns)
        {
            livesText = "Lvs:" + lives.ToString();  
        }
        SetCursor(0,1);
        Screen.Print  (livesText, foreColour, Colour.Blue);
        SetCursor(byte(Screen.Columns - levelText.Length),1);
        Screen.Print  (levelText, foreColour, Colour.Blue);
        Display.Resume();
    }
    gameOver(bool first)
    {
        updateScore();
        
        int flash = 1;
        long start = Millis;
        loop
        {
            long elapsed = Millis - start;
            if (elapsed > 700)
            {
                flash = flash * -1;
                start = Millis;
            }
            string gameOver = "Game Over";
            int x = Display.PixelWidth/2 - int(gameOver.Length+2)/2 * (CellWidth+1);
            int y = Display.PixelHeight/2;
            int w = int(gameOver.Length+2)*(CellWidth+1);
            int h = (CellHeight+1)*2;
            Display.Suspend();
            if (!first)
            {
                Display.FilledRectangle(x, y, w, h, (flash == -1) ? Colour.Red : Colour.Black);
                Screen.DrawText(x + CellWidth+1, y+(CellHeight+1)/2, gameOver, (flash == -1) ? Colour.Black : Colour.Red, (flash == -1) ? Colour.Red : Colour.Black, 1);
            }
            
            string startText = "Press Start";
            
            x = Display.PixelWidth/2 - int(startText.Length)/2 * (CellWidth+1);
            y = Display.PixelHeight/2 + (CellHeight+1) * 3;
            Screen.DrawText(x, y, startText, Colour.Yellow, Colour.Black, 1);
            Display.Resume();
            
            if (RestartButton)
            {
                lives = startLives;
                score = 0;
                break;
            }

            Display.Suspend();
            Paddle.Render(Colour.Black);
            Ball.Render(Colour.Black);
            Display.Resume();
            
            Time.Delay(25);
            Ball.Y = Paddle.Y - 4;
            Ball.X = Paddle.X - 3;
            Paddle.Move();
            _ = Ball.Move();
            Display.Suspend();
            Paddle.Render(Colour.White);
            Ball.Render(Colour.White);
            Display.Resume();
            Time.Delay(25);
            
#ifdef TEST_PLAY
            lives = startLives;
            score = 0;
            break;
#endif
        }
    }
    
    Hopper()
    {
#if defined(WAVESHARE_PICO_LCD_114) || defined(WAVESHARE_PICO_LCD_096) || defined(OLED_FEATHERWING_128x64)
        DisplayDriver.IsPortrait = true;
#endif
#if defined(WAVESHARE_PICO_LCD_096)
        if (DisplayDriver.IsPortrait)
        {
            DisplayDriver.FlipX = true;
            DisplayDriver.FlipY = true;
        }
#endif
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize DeviceDriver");
            return;
        }
        
        RandomSeed(uint(Millis % 0xFFFF));
        
        Ball.Initialize();
        Bricks.CreateWall(); // clears screen
        bool first = true;
        loop
        {
            // delay for each game update loop (gets faster as levels go up)
            uint msDelay = 50;
    
            Display.Suspend();
            Paddle.Render(Colour.Black);
            Display.Resume();
            Paddle.Initialize(); // reset paddle width for new game
            gameOver(first);
            Bricks.CreateWall();
            updateScore();
            
            first = false;
            while (lives > 0)
            {
                Display.Suspend(); // for OLED displays
                
                if (LeftButton)
                {
                    Paddle.Left();
                }
                else if (RightButton)
                {
                    Paddle.Right();
                }
                if (Paddle.Dx != 0)
                {
                    Paddle.Render(Colour.Black);
                }
                Ball.Render(Colour.Black);
                
                Paddle.Move();
                bool update = Ball.Move();
                if (Ball.IsCollisionWithPaddle())
                {
                    Ball.CalculateAngle();
                    update = false;
                }   
                
                if (Bricks.RemoveDeadBricks())
                {
                    update = true;
                }
                
                if (Bricks.Count == 0)
                {
                    // next level
                    Bricks.CreateWall(); // clears screen
                    Score += 100;
                    lives++;
                    level++;
                    Paddle.Shrink();
                    Ball.Y = Paddle.Y - 4;
                    Ball.X = Paddle.X;
                    Ball.Dy = -3;
                    update = true;
                    if (msDelay > 10)
                    {
                        msDelay -= 10;
                    }
                }
                if (update)
                {
                    updateScore();
                }
                
                Paddle.Render(Colour.White);
                Ball.Render(Colour.White);
                 
                Display.Resume(); // for OLED displays
#ifdef TEST_PLAY                
                Time.Delay(5);
#else
                Time.Delay(msDelay);
#endif
            }
        }
    }
}
