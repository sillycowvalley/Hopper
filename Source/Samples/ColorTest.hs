program ColorTest
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    
    {
        string hexChars = "0123456789ABCDEF";
    
        < uint > backgrounds;
        backgrounds.Append(MarginFace);
        backgrounds.Append(PopupFace); // good
        backgrounds.Append(0xDCB); // button
        backgrounds.Append(uint(0x07D)); // blue
        
        Screen.Clear();
        uint r = 0; uint g = 0; uint b = 0;
        uint rinc = 3; uint binc = 3; uint ginc = 3;
        uint counter = 0;
        SetCursor(0, 0);
        foreach (var background in backgrounds)
        {
            loop
            {
                bool done = false;
                uint foreColor = (r * 256) + (g * 16) + b;
                string content;
                content = content + hexChars[r];
                content = content + hexChars[g];
                content = content + hexChars[b];
                if (content == "FFF")
                {
                    done = true;
                }
                content = content.Pad(' ', 4);
                Print(content, foreColor, background);
                byte cy = CursorY;
                if (CursorX + 4 > Columns)
                {
                    SetCursor(0, cy+1);
                }
                if (cy + 1 > Rows)
                {
                    break;
                }
                r = r + rinc; if (r >= 16)
                {
                    r = 0; g = g + ginc; if (g >= 16)
                    {
                        g = 0; b = b + binc;
                        if (b >= 16) { b = 0; }
                    }
                }
                counter++;
                if ((counter > 1000) || done) { break; }
            }
            PrintLn();
        }
        Key k = ReadKey();
    }

}
