unit DisplayHelper
{
    
    uint Cyan    { get { return 0x0FF; } }  // Cyan
    uint Blue    { get { return 0x00F; } }  // Blue
    uint Orange  { get { return 0xFA0; } }  // Orange
    uint Yellow  { get { return 0xFF0; } }  // Yellow
    uint Green   { get { return 0x0F0; } }  // Green
    uint Purple  { get { return 0x808; } }  // Purple
    uint Red     { get { return 0xF00; } }  // Red
    
    uint GetColorForShape(byte shapeIndex)
    {
        uint color;
        switch (shapeIndex)
        {
            case 0:  { color = Cyan; }
            case 1:  { color = Blue; }
            case 2:  { color = Orange; }
            case 3:  { color = Yellow; }
            case 4:  { color = Green; }
            case 5:  { color = Purple; }
            case 6:  { color = Red; }
            default: { color = Colour.White; }
        }
        return color;
    }
    
    DrawCell(byte x, byte y, uint colour)
    {
        Screen.DrawChar(x, y, ' ', Colour.White, colour);
    }

    DrawText(byte startX, byte startY, string text, uint fgColor, uint bgColor)
    {
        for (byte i = 0; i < text.Length; i++)
        {
            Screen.DrawChar(startX + i, startY, text[i], fgColor, bgColor);
        }
    }
}

