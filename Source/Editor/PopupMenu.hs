unit PopupMenu
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/System/System"
    uses "/Source/Editor/Panel"
    uses "/Source/Editor/Commands"
    uses "/Source/Editor/Editor"
    uses "/Source/System/Diagnostics"
        
    // Panel 
    //   x0     : uint
    //   y0     : uint
    //   width  : uint
    //   height : uint
    //   background : uint   - colour
    
    <string, string> currentShortcuts;
    
    <string, variant> New(byte x, byte y, <string> menuitems)
    {
        uint width = 0;
        foreach (var name in menuitems)
        {
            string menuText = Commands.GetMenuText(name);
            uint length = menuText.Length;
            if (menuText.Contains('&'))
            {
                length--;
            }
            string accelerator = Commands.GetAcceleratorName(name);
            if (accelerator.Length != 0)
            {
                length = length + accelerator.Length + 2;
            }
            if (length > width)
            {
                width = length;
            }
            
        }
        width = width + 2;
        
        <string, variant> instance = Panel.New(x, y, byte(width), byte(menuitems.Count));
        
        instance["menuitems"] = menuitems;

        < <uint> > clickAreas;
        instance["clickareas"] = clickAreas;        
        return instance;
    }
    
    Execute(<string, variant> this, <string, variant> menuBar, string selfName)
    {
        // draws the solid background
        Panel.SetBackground(this, Colour.ButtonFace);
        PopupMenu.Draw(this); 
        
        Key key = ReadKey();
        bool consumedKey = false;
     
        if (key == Key.Click)
        {
            uint x = ClickX;
            uint y = ClickY;
            if (!ClickDouble) // up or down is fine
            {
                < <uint> > areas = this["clickareas"];
                bool winner = false;
                uint nmenus = areas.Count;
                foreach (var area in areas)
                {
                    uint xm = area[0];
                    uint ym = area[1];
                    uint wm = area[2];
                    uint hm = area[3]; 
                    uint ki = area[4];           
                    
                    if ((x >= xm) && (x <= xm + wm))
                    {
                        if ((y >= ym) && (y < ym + hm))
                        {
                            winner = true;
                            key = Key(ki);
                            break;
                        }
                    }
                }
                if (!ClickUp && winner)
                {
                    Key toss = ReadKey(); // toss the up click
                }
            }
        }
        Editor.Draw(Panel.GetHeight(this));
        
        
        
        // was it one of the current shortcuts?
        uint i = uint(key);
        if ((i >= 32) && (i <= 127)) // printable ASCII
        {
            char c;
            c = char(i);
            c = c.ToUpper();
            foreach (var shortCut in currentShortcuts)
            {   
                string keyName = shortCut.key;
                char k = keyName.GetChar(0);
                if (k == c)
                {
                    Commands.Execute(shortCut.value);
                    consumedKey = true;
                    break;
                }
            }
        }
        
        if (!consumedKey)
        {
            // did the key match another menu?
            if (Key.Alt == (Key.Alt & key))
            {
                < string, Key> keys = menuBar["keys"];
                foreach (var kv in keys)
                {
                    if (kv.value == key)
                    {
                        if (kv.key != selfName) // but not self
                        {
                            MenuBar.DoPopupMenu(menuBar, kv.key); // chain into the next menu..
                            break;
                        }
                    }
                }
            }
        }
    }
    
    bool OnKey(<string, variant> this, Key key)
    {
        return Panel.OnKey(this, key);
    }
    
    Draw(<string, variant> this)
    {
        Suspend();
        Panel.Draw(this);
        
        currentShortcuts.Clear();
        
        <string> menuitems = this["menuitems"];
        uint x0 = GetX0(this);
        uint y = GetY0(this);
        uint w = GetWidth(this);
        x0++;

        < < uint > > listOfAreas;

        foreach (var name in menuitems)
        {
            uint x = x0;
            if (name.Length == 0)
            {
                string separator;
                separator = separator.Pad('-', w-2);
                foreach (var c in separator)
                {
                    DrawChar(x, y, c, Colour.DarkGray, Colour.ButtonFace);
                    x++;
                }
            }
            else
            {
                string menuText = Commands.GetMenuText(name);
                string label = menuText.Replace("&", "");
                
                string accelerator = Commands.GetAcceleratorName(name);
                label = label.Pad(' ', (w - 2 - accelerator.Length));
                label = label + accelerator;
                bool enabled = Commands.IsEnabled(name);
                bool checked = Commands.IsChecked(name);
                if (checked)
                {
                    label = label.Replace("[ ]", "[x]");
                }
                if (enabled)
                {
                    uint keyIndex;
                    if (menuText.IndexOf("&", ref keyIndex))
                    {
                        if (keyIndex > 0)
                        {
                            string sub = label.Substring(0, keyIndex);
                            foreach (var c in sub)
                            {
                                DrawChar(x, y, c, Colour.Black, Colour.ButtonFace);
                                x++;
                            }
                        }
                        string shortCut = label.Substring(keyIndex, 1);
                        DrawChar(x, y, shortCut[0], Colour.AltKey, Colour.ButtonFace);
                        x++;
                        if (keyIndex < label.Length - 1)
                        {
                            string sub = label.Substring(keyIndex+1);
                            foreach (var c in sub)
                            {
                                DrawChar(x, y, c, Colour.Black, Colour.ButtonFace);
                                x++;
                            }
                        }
                        string sc = shortCut.ToUpper();
                        currentShortcuts[sc] = name;
                        <uint> area;
                        area.Append(x0);
                        area.Append(y);
                        area.Append(w-2);
                        area.Append(uint(1));
                        uint ik = uint(sc[0]);
                        area.Append(ik);
                        listOfAreas.Append(area);     
                    }
                    else
                    {
                        foreach (var c in label)
                        {
                            DrawChar(x, y, c, Colour.Black, Colour.ButtonFace);
                            x++;
                        }
                    }
                }
                else
                {
                    foreach (var c in label)
                    {
                        DrawChar(x, y, c, Colour.DarkGray, Colour.ButtonFace);
                        x++;
                    }
                }
            }
            y++;
        }
        this["clickareas"] = listOfAreas;
        Resume(false);
    }
}
