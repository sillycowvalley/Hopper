unit Canvas
{
    float width;
    float height;
    int pixelWidth; 
    int pixelHeight;
    int pixelXOrigin; 
    int pixelYOrigin;
    float pixelToCanvasX;
    float pixelToCanvasY;
    float canvasToPixelX;
    float canvasToPixelY;
    
    float Width      { get { return width;  } set { width  = value; setXScale(); } }
    float Height     { get { return height; } set { height = value; setYScale(); } }
    
    int PixelWidth   { get { return pixelWidth;   } set { pixelWidth   = value; setXScale(); } }
    int PixelHeight  { get { return pixelHeight;  } set { pixelHeight  = value; setYScale(); } }
    int PixelXOrigin { get { return pixelXOrigin; } set { pixelXOrigin = value; } }
    int PixelYOrigin { get { return pixelYOrigin; } set { pixelYOrigin = value; } }
    
    int ToX(float fx)
    {
        long x = long(fx * canvasToPixelX + pixelXOrigin);
        return int(x);
    }
    int ToY(float fy)
    {
        long y = long(fy * canvasToPixelY + pixelYOrigin);
        return int(y);
    }
    
    setXScale()
    {
        if ((width != 0) && (pixelWidth != 0))
        {
            pixelToCanvasX = 1.0 * width / pixelWidth;
            canvasToPixelX = 1.0 * pixelWidth / width;
            if (canvasToPixelY != 0)
            {
                if (canvasToPixelY > canvasToPixelX)
                {
                    canvasToPixelY = canvasToPixelX;
                }
                else
                {
                    canvasToPixelX = canvasToPixelY;
                }
            }
            if (pixelToCanvasY != 0)
            {
                if (pixelToCanvasY > pixelToCanvasX)
                {
                    pixelToCanvasY = canvasToPixelX;
                }
                else
                {
                    pixelToCanvasX = pixelToCanvasY;
                }
            }
        }
    }
    setYScale()
    {
        if ((height != 0) && (pixelHeight != 0))
        {
            pixelToCanvasY = 1.0 * height / pixelHeight;
            canvasToPixelY = 1.0 * pixelHeight / height;
            if (canvasToPixelX != 0)
            {
                if (canvasToPixelY > canvasToPixelX)
                {
                    canvasToPixelY = canvasToPixelX;
                }
                else
                {
                    canvasToPixelX = canvasToPixelY;
                }
            }
            if (pixelToCanvasX != 0)
            {
                if (pixelToCanvasY > pixelToCanvasX)
                {
                    pixelToCanvasY = canvasToPixelX;
                }
                else
                {
                    pixelToCanvasX = pixelToCanvasY;
                }
            }
        }
    }
}
