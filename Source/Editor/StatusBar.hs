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
        <string, variant> instance = Panel.New(0, Screen.Rows-1, Screen.Columns, 1);
        
        SetBackground(instance, Color.Button);
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
        if ((key == Key.Click) && (clickPath.Length > 0))
        {
            uint x = ClickX;
            uint y = ClickY;
            if (ClickUp && ClickDouble)
            {
                bool winner = false;
                if (clickAreas.Length > 0)
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
                        <string> parts = clickPath.Split(':');
                        if (parts.Length == 2)
                        {
                            string sourcePath = parts[0];
                            string currentPath = Editor.GetCurrentPath();
                            uint ln;
                            bool gotoLine = false;
                            if (Token.TryParseUInt(parts[1], ref ln))
                            {
                                gotoLine = true;
                            }
                            
                            if (sourcePath.ToLower() != currentPath.ToLower())
                            {
                                if (Editor.CanUndo())
                                {
                                    Editor.OpenPath(sourcePath); // offer undo
                                }
                                else
                                {
                                    Editor.LoadFile(sourcePath);
                                }
                                currentPath = Editor.GetCurrentPath();
                                gotoLine = (sourcePath.ToLower() == currentPath.ToLower());
                            }
                            OutputDebug(ln.ToString());
                            if (gotoLine)
                            {
                                if (Editor.GotoLineNumber(ln))
                                {
                                }
                            }
                        }
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
        
        if ((row != 0) || (col != 0))
        {   
            uint backColour = Panel.GetBackground(this);
            string content = "Ln: " + row.ToString() + "  Ch: " + col.ToString();
            uint x = Panel.GetX0(this) + Panel.GetWidth(this) - content.Length - 2;
            uint y = Panel.GetY0(this);
            foreach (var c in content)
            {
                DrawChar(x, y, c, Color.ButtonText, backColour);
                x++;
            }
        }
        if (textContent.Length != 0)
        {
            uint backColour = Panel.GetBackground(this);
            uint x = Panel.GetX0(this) + 3;
            uint y = Panel.GetY0(this);
            
            <uint> area;
            uint cw = 0;
            bool foundPath = false;
            clickPath = "";
            foreach (var c in textContent)
            {
                DrawChar(x, y, c, Color.ButtonText, backColour);
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
            if (clickPath.Length > 0)
            {
                clickAreas.Append(area);
            }
        }
        
        Resume(false);
    }
}
