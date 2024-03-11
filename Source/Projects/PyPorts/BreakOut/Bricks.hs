unit Bricks
{
    record Brick
    {
        int X;
        int Y;
        int Width;
        int Height;
        uint colour;
    }
    
    <Brick> bricks;
    
    uint Count { get { return bricks.Count; } }
    
    Brick create(int x, int y, int w, int h, uint colour)
    {
        Brick brick;
        brick.X = x;
        brick.Y = y;
        brick.Width  = w;
        brick.Height = h;
        brick.colour = colour;
        return brick;
    }
    
    render(Brick brick, uint colour)
    {
        Display.Suspend();
        Display.FilledRectangle(brick.X - brick.Width/2, brick.Y - brick.Height/2, brick.Width, brick.Height, colour);
        Display.Resume();
    }
    Render()
    {
        Display.Suspend();
        Display.Clear();
        foreach (var brick in bricks)
        {
            render(brick, brick.colour);
        }
        Display.Resume();
    }
    
    CreateWall()
    {
        bricks.Clear();
        
        // Tricky to make block size generic:
        //    - too many blocks will slow down the game (collision detection)
        //    - for tall portrait displays, too many rows looks odd
        //    - full size blocks in the right column are ideal (not truncated)
        int brickWidth  = 22;
        int brickHeight = 7;
        switch (Display.PixelWidth)
        {
            case 64:
            {
                brickWidth = 14;
                brickHeight = 6;
            }
            case 80:
            {
                brickWidth = 18;
                brickHeight = 9;
            }
            case 128:
            {
                brickWidth = 14;
            }
            case 135:
            {
                brickWidth = 21;
                brickHeight = 11;
            }
            case 160:
            {
                brickWidth  = 18;
                brickHeight = 6;
            }
            case 240:
            {
                brickWidth = 28;
            }
            case 320:
            {
                brickWidth = 38;
            }
        }
        
        int titleSpace = (CellHeight+1)*2;
        
        int startY    = titleSpace + brickHeight/2 + 1;
        int endY      = titleSpace + (Display.PixelHeight - titleSpace)/2;
        int rowHeight = brickHeight+3;
        
        <uint> usedColours;
        int offset = 0;
        for (int y = startY; y <= endY; y += rowHeight)
        {
            uint colour;
            loop
            {
                byte random = byte(Utilities.Random(13));
                switch (random)
                {
                    case  0 : { colour = Colour.Red; }
                    case  1 : { colour = Colour.Orange; }
                    case  2 : { colour = Colour.Yellow; }
                    case  3 : { colour = Colour.Green; }
                    case  4 : { colour = Colour.Blue; }
                    case  5 : { colour = Colour.Indigo; }
                    case  6 : { colour = Colour.Violet; }
                    case  7 : { colour = Colour.Cyan; }
                    case  8 : { colour = Colour.Magenta; }
                    case  9 : { colour = Colour.Raspberry; }
                    case 10 : { colour = Colour.Ocean; }
                    case 11 : { colour = Colour.Spring; }
                    case 12 : { colour = Colour.Turquoise; }
                }
                if (!usedColours.Contains(colour)) { break; }
            }
            usedColours.Append(colour);
            int x = brickWidth/2 + offset + 1;
            loop
            {
                bricks.Append(create(x, y, brickWidth, brickHeight, colour));
                x += brickWidth+2;
                if (x - brickWidth/2 > Display.PixelWidth - 1)
                {
                    break;
                }
            }
            // uncomment for offset bricks in alternate rows
            // offset = (offset == 0) ? -int(brickWidth/2) : 0;
        }
        Render();
    }
    
    bool RemoveDeadBricks()
    {
        bool hits = false;
        <Brick> liveBricks;
        foreach (var brick in bricks)
        {
            if (Ball.IsCollisionWithBrick(brick))
            {
                Ball.Dy *= -1;
                Score += 10;
                render(brick, Colour.Black);
                hits = true;
            }
            else
            {
                liveBricks.Append(brick);
            }
        }
        bricks = liveBricks;
        return hits;
    }       
}
