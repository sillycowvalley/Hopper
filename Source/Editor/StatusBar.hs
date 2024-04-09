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
        Panel.Draw(this);
        
        clickAreas.Clear();
        uint panelWidth = Panel.GetWidth(this);
        uint panelX0 = Panel.GetX0(this);
        if ((row != 0) || (col != 0))
        {   
            uint backColour = Panel.GetBackground(this);
            string content = "Ln: " + row.ToString() + "  Ch: " + col.ToString();
            uint x = panelX0 + panelWidth - content.Length - 2;
            uint y = Panel.GetY0(this);
            foreach (var c in content)
            {
                DrawChar(x, y, c, Colour.StatusText, backColour);
                x++;
            }
        }
        if (textContent.Length != 0)
        {
            uint backColour = Panel.GetBackground(this);
            uint x = panelX0 + 3;
            uint y = Panel.GetY0(this);
            
            <uint> area;
            uint cw = 0;
            bool foundPath = false;
            clickPath = "";
            uint xLimit = panelX0 + panelWidth;
            foreach (var c in textContent)
            {
                if (x < xLimit)
                {
                    DrawChar(x, y, c, Colour.StatusText, backColour);
                }
                x++;
                if (c == '[')
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
        
        Resume(false);
    }
}
