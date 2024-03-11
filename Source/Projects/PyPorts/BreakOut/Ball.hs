unit Ball
{
    int x;
    int y;
    int dx;
    int dy;
    int speed;
    const int width = 4;
    const int height = 4;
    
    int Dx { set { dx = value; } get { return dx; } }
    int Dy { set { dy = value; } get { return dy; } }
    int X { set { x = value; } get { return x; } }
    int Y { set { y = value; } get { return y; } }
    
    Initialize()
    {
        x = Paddle.X;
        y = Paddle.Y-3;
        dx = 3;
        dy = -3;
        speed =1;
    }
    CalculateAngle()
    {
        int dx = x - (Paddle.X-12);
        float theta = (180.0/Paddle.Width)*dx;
        long vx = long(100*Float.Cos(Float.Radians(theta)));
        long vy = long(100*Float.Sin(Float.Radians(theta)));
        vx = vx/15;
        vy = vy/15;
        if (vy < 2) { vy = 2; }
        if (vx < 2) { vx = 2; }
        Ball.Dx = int(vx * -1);
        Ball.Dy = int(vy * -1);
    }
    bool Move()
    {
        x += dx;
        y += dy;
        
        if (x < 0 + width/2)
        {
            x = 0 + width/2;
            dx *= -1;
        }
        else if (x > Display.PixelWidth - width/2)
        {
            x = Display.PixelWidth - width/2;
            dx *= -1;
        }

        if (y < 15 + height/2)
        {
            y = 15 + height/2;
            dy *= -1;
        }
        else if (y > Display.PixelHeight - height/2)
        {
            y = Display.PixelHeight - height/2;
            y = Paddle.Y - 4;
            x = Paddle.X;
            dy = -3;
            dx = -3;
            Lives -= 1;
            return false;
        }
        return true;
    }
    Render(uint colour)
    {
        Display.FilledRectangle(x-width/2, y-height/2, width, height, colour);
    }
    
    bool IsCollisionWithPaddle()
    {
        return ((Int.Abs(x - Paddle.X) * 2) < (width + Paddle.Width)) && 
               ((Int.Abs(y - Paddle.Y) * 2) < (height + Paddle.Height));
    }
    bool IsCollisionWithBrick(Brick brick)
    {
        return ((Int.Abs(x - brick.X) * 2) < (width + brick.Width)) && 
               ((Int.Abs(y - brick.Y) * 2) < (height + brick.Height));
    }
}
