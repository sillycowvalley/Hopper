program Game
{
    uses "/Source/Library/Boards/PiPico" // MCU
    //uses "/Source/Minimal/System"      // 6502
    
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    uses "Tools"
    uses "Sprites"
    
    byte yPos;
    byte lives;
    AddLife(byte iSprite)
    {
        lives++;
        Sprites.New(iSprite, 3, lives, 0, 2, false);
    }
    Restart()
    {
        // player
        yPos = 7;
        Sprites.New(0, 3, 0, yPos, 2, false);
        
        AddLife(1);
        AddLife(2);
        AddLife(3);
        AddLife(4);
        
        
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        Sprites.Render();
    }
    CheckCollisions()
    {
        byte spriteType;
        if (Sprites.Collision(0, yPos, ref spriteType))
        {
            if (spriteType == 1) // block
            {
                byte iLostLife;
                if (Sprites.GetVisibleSprite(lives, 0, ref iLostLife))
                {
                    DeleteSprite(iLostLife);
                    lives--;
                }
            }
            if (spriteType == 5) // pill
            {
                byte iSprite;
                if (Sprites.IsSpriteAvailable(ref iSprite))
                {
                    AddLife(iSprite);
                    Sprites.RenderCell(lives, 0);
                }
            }
        }
    }
    
    Hopper()
    {
        Seed();
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        PinMode(GP2, PinModeOption.Input);
        PinMode(GP3, PinModeOption.Input);
        
        Restart();
        
        
        byte gameLoop;
        loop
        {
            CheckCollisions();
            if (lives == 0)
            {
                break;
            }
            if (!DigitalRead(GP2))
            {
                if (yPos > 1)
                {
                    yPos--;
                    MoveTo(0, 0, yPos);
                }
            }
            if (!DigitalRead(GP3))
            {
                if (yPos < 15)
                {
                    yPos++;
                    MoveTo(0, 0, yPos);
                }
            }
            if (gameLoop % 32 == 0)
            {
                // spawn block sprite
                byte iSprite;
                if (IsSpriteAvailable(ref iSprite))
                {
                    byte yStart = (Random() % 15) + 1;
                    Sprites.New(iSprite, 1, 31, yStart, 3, true);
                }
            }
            if ((gameLoop+16) % 64 == 0)
            {
                // spawn pill sprite
                byte iSprite;
                if (IsSpriteAvailable(ref iSprite))
                {
                    byte yStart = (Random() % 15) + 1;
                    Sprites.New(iSprite, 5, 31, yStart, 1, true);
                }
            }
            if (gameLoop % 4 == 0)
            {
                Move();
            }
            if (gameLoop == 255)
            {
                gameLoop = 0;
            }
            else
            {
                gameLoop++;
            }
        }
    }
}
