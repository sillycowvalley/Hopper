unit Paddle
{
    int x;
    int y;
    int dx;
    int width;
    const int height = 4;  
    
    int maxPaddleSpeed = 5;
       
    int X      { get { return x; } }
    int Y      { get { return y; } }
    int Width  { get { return width; } }
    int Height { get { return height; } }
    int Dx     { get { return dx; } }
    
    Initialize()
    {
        width = Display.PixelWidth / 4;
        x = Display.PixelWidth / 2;
        y = Display.PixelHeight-4;
        dx = 0;
        if (Display.PixelWidth > Display.PixelHeight)
        {
            maxPaddleSpeed = 9;
        }
    }
    Left()
    {
        if (dx > -maxPaddleSpeed)
        {
            dx -= 3;
        }
    }
    Right()
    {
        if (dx < maxPaddleSpeed)
        {
            dx += 3;
        }
    }
    Shrink()
    {
        if (width > Display.PixelWidth / 6)
        {
            width -= 2;
        }
    }
    Move()
    {
        x += dx;
        if (x < 0 + width/2)
        {
            x = 0 + width/2;
            dx = 0;
        }
        else if (x > Display.PixelWidth-1 - width/2)
        {
            x = Display.PixelWidth-1 - width/2;
            dx = 0;
        }
        // half-life to slow the paddle down
        if (dx > 0)
        {
            dx--;
        }
        else if (dx < 0)
        {
            dx++;
        }
    }
    Render(uint colour)
    {
        Display.FilledRectangle(x-width/2, y-height/2, width, height, colour);
    }
}
