unit Bricks
{
    const int brickWidth  = 22;
    const int brickHeight = 7;
    
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
    
    Brick Create(int x, int y, uint colour)
    {
        Brick brick;
        brick.X = x;
        brick.Y = y;
        brick.Width = brickWidth;
        brick.Height = brickHeight;
        brick.colour = colour;
        return brick;
    }
    
    render(Brick brick, uint colour)
    {
        Display.FilledRectangle(brick.X - brick.Width/2, brick.Y - brick.Height/2, brick.Width, brick.Height, colour);
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
        
        <uint> usedColours;
        int offset = 0;
        
        int titleSpace = (CellHeight+1)*2;
        
        int startY    = titleSpace + brickHeight/2 + 1;
        int endY      = titleSpace + (Display.PixelHeight - titleSpace)/2;
        int rowHeight = brickHeight+3;
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
            int x = brickWidth/2 + offset;
            loop
            {
                bricks.Append(Create(x, y, colour));
                x += brickWidth+2;
                if (x - brickWidth/2 > Display.PixelWidth - 1)
                {
                    break;
                }
            }
            offset = (offset == 0) ? -int(brickWidth/2) : 0;
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
