program Snake
{
    // https://www.geeksforgeeks.org/snake-game-in-c/
    
#define SERIALCONSOLE
#define RP2040PICOW
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Graphics"
    
    uses "/Source/Samples/MCU/Games/Pico096"
    
    bool gameOver;
    uint gRandom;
    
    uint fruitX;
    uint fruitY;
    uint x;
    uint y;
    uint width;
    uint height;
    uint score;
    
    flags Direction
    {
        None,
        Left  = 0x01,
        Right = 0x02,
        Up    = 0x04,
        Down  = 0x08
    }
    Direction direction;
    
    const byte cellSize = 6;
    const uint tickLength = 5;         // ms per tick
    const uint ticksPerLogicStep = 40; // ticks per move
    
    
    uint Random(uint range)
    {
        // PRNG from here: 
        // https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
        gRandom = gRandom ^ (gRandom << 7);
        gRandom = gRandom ^ (gRandom >> 9);
        gRandom = gRandom ^ (gRandom << 8);
        return uint(gRandom % range);
    }
    
    GameSetup()
    {
        Graphics.FlipDisplay(true);
        
        width  = Graphics.Width  / cellSize;
        height = Graphics.Height / cellSize;
        x = width  / 2;
        y = height / 2;
        
        score = 0;
        gameOver = false;
        
        // seed gRandom using time
        loop // just to avoid zero
        {
            <byte> bytes = (Time.Millis).ToBytes();
            gRandom = UInt.FromBytes(bytes[0], bytes[1]);
            if (gRandom != 0) { break; }
        }
        
        NewFruit();    
        
        switch (Random(4))
        {
            case 1:
            {
                direction = Direction.Left;
            }
            case 2:
            {
                direction = Direction.Right;
            }
            case 3:
            {
                direction = Direction.Up;
            }
            default:
            {
                direction = Direction.Down;
            }
        }
    }
    
    NewFruit()
    {
        loop
        {
            loop // just to avoid zero
            {
                fruitX = Random(width - 2) + 1;
                if (fruitX != 0) { break; }
            }
            loop // just to avoid zero
            {
                fruitY = Random(height - 2) + 1;
                if (fruitY != 0) { break; }
            }
            if (x == fruitX)
            {
                continue;
            }
            if (y == fruitY)
            {
                continue;
            }
            //WriteLn(fruitX.ToString() + "," + fruitY.ToString());
            break;
        }
    }
    DrawSolid(uint cx, uint cy, uint colour)
    {
        uint x0 = cx * cellSize;
        uint y0 = cy * cellSize;
        for (uint i = 0; i < cellSize; i++)
        {
            uint y = y0 + i;
            for (uint j = 0; j < cellSize; j++)
            {
                uint x = x0 + j;
                Graphics.SetPixel(x, y, colour);   
            }
        }
    }
    DrawFruit(uint cx, uint cy, uint colour)
    {
        uint x0 = cx * cellSize;
        uint y0 = cy * cellSize;
        for (uint i = 0; i < cellSize; i++)
        {
            uint y = y0 + i;
            if (y % 2 == 0) { continue; }
            for (uint j = 0; j < cellSize; j++)
            {
                uint x = x0 + j;
                if (x % 2 == 0) { continue; }
                Graphics.SetPixel(x, y, colour);   
            }
        }
    }
    DrawBox(uint cx, uint cy, uint colour)
    {
        uint x0 = cx * cellSize;
        uint y0 = cy * cellSize;
        for (uint i = 0; i < cellSize; i++)
        {
            uint y = y0 + i;
            for (uint j = 0; j < cellSize; j++)
            {
                uint x = x0 + j;
                if ((x == x0) || (y == y0) || (x == x0 + cellSize-1) || (y == y0 + cellSize - 1))
                {
                    Graphics.SetPixel(x, y, colour);   
                }
            }
        }
    }
    DrawBoard()
    {
        Screen.Suspend();
        Graphics.Clear(Color.Black);
        for (uint yi = 0; yi < height; yi++)
        {
            for (uint xi = 0; xi < width; xi++)
            {
                if ((xi == 0) || (xi == width-1) || (yi == 0) || (yi == height-1))
                {
                    DrawBox(xi, yi, Color.White);
                }
            }
        }
        DrawFruit(fruitX, fruitY, Color.White);
        Screen.Resume(false);
    }
    DrawString(uint x, uint y, string str)
    {
        Screen.Suspend();
        foreach (var ch in str)
        {
            Graphics.DrawChar(x, y, ch, Color.White, Color.Black, 1, false);
            x += 5;
        }
        Screen.Resume(false);
    }
    DrawGameOver()
    {
        DrawString(Graphics.Width  / 4, Graphics.Height / 4, "Game Over");
    }
    DrawScore()
    {
        string scoreString = "Score: " + score.ToString();
        uint x = Graphics.Width  / 4;
        uint y = Graphics.Height / 2 - 5;
        DrawString(x, y, scoreString);
    }
    Draw(uint x, uint y, bool show)
    {
        Screen.Suspend();
        DrawSolid(x, y, show ? Color.White : Color.Black);
        Screen.Resume(false);
    }
    
    Input()
    {
        if (ButtonLeft)
        {
            direction = Direction.Left;
        }
        if (ButtonRight)
        {
            direction = Direction.Right;
        }
        if (ButtonUp)
        {
            direction = Direction.Up;
        }
        if (ButtonDown)
        {
            direction = Direction.Down;
        }
    }
    bool Logic()
    {
        bool gobbled;
        switch (direction) 
        { 
            case Direction.Down:  { y++; }
            case Direction.Right: { x++; }
            case Direction.Up:    { y--; }
            case Direction.Left:  { x--; }
        }
        
        // hit the edge: game over
        if ((x == 0) || (x == width-1) || (y == 0) || (y == height-1))
        { 
            gameOver = true; 
        }
        
        // gobble the fruit:
        if ((x == fruitX) && (y == fruitY))
        { 
            NewFruit();
            score++;
            DrawScore();
            Delay(250);
            DrawBoard();
            Draw(x, y, true);
            gobbled = true;
        }
        return gobbled;
    }
    
    ButtonISR(byte pin, PinStatus status) 
    { 
    }
    
    {
        ISRDelegate buttonDelegate = ButtonISR;
        if (!Pico096.Initialize(buttonDelegate))
        {
            WriteLn("Setup failed");
            return;
        }
        bool exit = false;
        uint ticks;
        loop
        {
            GameSetup();
            DrawBoard();
            Draw(x, y, true);
            while (!gameOver)
            {
                Delay(tickLength);
                ticks++;
                Input();
                uint px = x;
                uint py = y;
                if (ticks == ticksPerLogicStep)
                {
                    if (!Logic())
                    {
                        Screen.Suspend();
                        Draw(px, py, false);
                        Draw(x,   y, true);
                        Screen.Resume(false);
                    }
                    ticks = 0;
                }
            }
            DrawScore();
            DrawGameOver();
            
            // wait for a button
            loop
            {
                if (ButtonA) { break; }
                if (ButtonB) { exit = true; break; }
            }
            if (exit) { break; }
        }
    }
}
