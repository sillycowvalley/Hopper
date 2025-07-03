unit StatusBar
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    
    
    // Panel 
    //   x0     : uint
    //   y0     : uint
    //   width  : uint
    //   height : uint
    //   background : uint   - colour
    
    uint row;
    uint col;
    string textContent;
    string clickPath;
    < <uint> > clickAreas;
       
    <string, variant> New()
    {
        <string, variant> instance = Panel.New(Editor.Left, Editor.Top + Editor.Height-1, Editor.Width, 1);
        
        SetBackground(instance, Colour.StatusFace);
        return instance;
    }
    
    Dump(<string, variant> this)
    {
        < string, uint> values = this["values"];
        
        PrintLn("StatusBar:");
        Panel.Dump(this);
    }
        
    bool OnKey(<string, variant> this, Key key)
    {
        if ((key == Key.Click) && (clickPath.Length != 0))
        {
            uint x = ClickX;
            uint y = ClickY;
            if (ClickUp && ClickDouble)
            {
                bool winner = false;
                if (clickAreas.Count != 0)
                {
                    <uint> area = clickAreas[0];
                    uint xm = area[0];
                    uint ym = area[1];
                    uint wm = area[2];
                    uint hm = area[3];
                    if ((x >= xm) && (x <= xm + wm))
                    {
                        if ((y >= ym) && (y < ym + hm))
                        {
                            winner = true;
                        }
                    }
                    if (winner)
                    {
                        ClickStack.Push(Editor.CurrentPath, Editor.GetCurrentLineNumber(), Editor.GetCurrentColumnNumber());
                        ClickStack.Load(clickPath);
                        return true; // consumed key
                    }
                }
            }
        }
        return Panel.OnKey(this, key);
    }
    
    SetLocation(<string, variant> this, uint x, uint y)
    {
        col = x+1;
        row = y+1;
        StatusBar.Draw(this);
    }
    SetText(<string, variant> this, string text)
    {
        uint lengthPad = textContent.Length;
        textContent = text;
        textContent = textContent.Pad(' ', lengthPad);
        StatusBar.Draw(this);
    }
    
    Draw(<string, variant> this)
    {
        Suspend();
        
        clickAreas.Clear();
        
        string statusText;
        uint panelWidth = Panel.GetWidth(this);
        uint panelX0 = Panel.GetX0(this);
        uint backColour = Panel.GetBackground(this);
        uint x;
        uint y;
        
        if (textContent.Length != 0)
        {
            x = panelX0 + 3;
            y = Panel.GetY0(this);
            
            <uint> area;
            uint cw = 0;
            bool foundPath = false;
            clickPath = "";
            uint xLimit = panelX0 + panelWidth;
            foreach (var c in textContent)
            {
                if (x < xLimit)
                {
                    statusText += c;
                }
                x++;
                if ((c == '[') && (clickPath.Length == 0))
                {
                    foundPath = true;
                    area.Append(x+1);
                    area.Append(y);
                }
                else if (foundPath)
                {
                    if (c == ']')
                    {
                        foundPath = false;
                        area.Append(cw);
                        area.Append(uint(1));
                    }
                    else
                    {
                        clickPath = clickPath + c;
                        cw++;
                    }
                }
            }
            if (clickPath.Length != 0)
            {
                clickAreas.Append(area);
            }
        }
        
        statusText = "   " + statusText;
        statusText = statusText.Pad(' ', panelWidth);
        
        if ((row != 0) || (col != 0))
        {   
            string coordsText = "Ln: " + row.ToString() + "  Ch: " + col.ToString();
            x = panelWidth - coordsText.Length - 2;
            statusText = statusText.Substring(0, x);
            statusText += coordsText;
            statusText = statusText.Pad(' ', panelWidth);
        }
        
        
        x = panelX0;
        y = Panel.GetY0(this);
        foreach (var c in statusText)
        {
            DrawChar(x, y, c, Colour.StatusText, backColour);
            x++;
        }
        
        Resume(false);
    }
}
