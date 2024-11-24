unit MenuBar
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    
    uses "Panel"
    uses "PopupMenu"
    
    // Panel 
    //   x0     : uint
    //   y0     : uint
    //   width  : uint
    //   height : uint
    //   background : uint   - colour
    
    <string> menuOrder;
    string titleText;
    string titleText2;
    string menuProjectPath;
       
    <string, variant> New()
    {
        <string, variant> instance = Panel.New(Editor.Left, Editor.Top, Editor.Width, 1);

        Panel.SetBackground(instance, Editor.TitleColor);
        
        // globals
        menuOrder.Append("&File");
        menuOrder.Append("&Edit");
        if (IsDebugger)
        {
            menuOrder.Append("&Debug");
        }
        else
        {
            menuOrder.Append("&Build");
        }
        menuOrder.Append("&Options");
        
        <string, Key > keys;
        
        keys["&File"] = (Key.Alt | Key.ModF);
        keys["&Edit"] = (Key.Alt | Key.ModE);
        if (IsDebugger)
        {
            keys["&Debug"] = (Key.Alt | Key.ModD);
        }
        else
        {
            keys["&Build"]   = (Key.Alt | Key.ModB);
            keys["&Options"] = (Key.Alt | Key.ModO);
        }
        
        instance["keys"] = keys;
        
        <string, uint > xpos;
        
        xpos["&File"]  = 0;
        string menunames = " File ";
        xpos["&Edit"]  = menunames.Length;
        menunames = menunames + " Edit ";
        if (IsDebugger)
        {
            xpos["&Debug"] = menunames.Length;
        }
        else
        {
            xpos["&Build"] = menunames.Length;
            menunames = menunames + " Build ";
        }
        xpos["&Options"] = menunames.Length;
        
   
        instance["xpos"] = xpos;

        < < uint > > listOfAreas;
        instance["clickareas"] = listOfAreas;

        return instance;
    }
    
    SetTitleText(<string, variant> this, string cp, string pp)
    {
        titleText = cp;
        titleText2 = "";
        menuProjectPath = "";
        if (cp.ToLower() != pp.ToLower())
        {
            menuProjectPath = pp;
            titleText2 = "['" + Path.GetFileName(pp) + "']";
#ifdef DEBUGGER
            if (COMPort != 4242)
            {
                titleText2 += " on COM" + (COMPort).ToString();
            }
#endif            
        }
        Draw(this);
    }
   
    DoPopupMenu(<string, variant> this, string key)
    {
        < string, uint> xpos = this["xpos"];
        byte xp = byte(xpos[key]);
        
        < string > menuitems;
        
        switch (key)
        {
            case "&File":
            {
                if (IsDebugger)
                {
                    menuitems.Append("Open");
                    menuitems.Append("");
                    menuitems.Append("Exit");                             
                }
                else
                {
                    menuitems.Append("New");
                    menuitems.Append("Open");
                    menuitems.Append("");
                    menuitems.Append("Save");
                    menuitems.Append("SaveAs");
                    menuitems.Append("");
                    menuitems.Append("Exit");
                }
            }
            case "&Edit":
            {
                if (IsDebugger)
                {
                    menuitems.Append("Goto");
                    menuitems.Append("Find");
                    menuitems.Append("");
                    menuitems.Append("Copy");
                    menuitems.Append("");
                    menuitems.Append("SelectAll");
                }
                else
                {
                    menuitems.Append("Undo");
                    menuitems.Append("Redo");
                    menuitems.Append("");
                    menuitems.Append("Goto");
                    menuitems.Append("Find");
                    menuitems.Append("Replace");
                    menuitems.Append("");
                    menuitems.Append("Cut");
                    menuitems.Append("Copy");
                    menuitems.Append("Paste");
                    menuitems.Append("Delete");
                    menuitems.Append("");
                    menuitems.Append("SelectAll");
                }
            }
            case "&Debug":
            {
                menuitems.Append("Debug");
                menuitems.Append("Run");
                menuitems.Append("");
                menuitems.Append("Reload");
                menuitems.Append("");
                menuitems.Append("Break");
                menuitems.Append("");
                menuitems.Append("StepInto");
                menuitems.Append("StepOver");
                menuitems.Append("");
                menuitems.Append("Profile");
                menuitems.Append("Memory");
                menuitems.Append("");
                menuitems.Append("ToggleBreakpoint");
                menuitems.Append("DeleteAllBreakpoints");
            }
            case "&Build":
            {
                menuitems.Append("Build");
                menuitems.Append("");
                menuitems.Append("Run");
                menuitems.Append("Debug");
            }
            case "&Options":
            {
                if (IsDebugger)
                {
                    menuitems.Append("HexDisplay");
                    menuitems.Append("CaptureConsole");
                }
                else
                {
                    menuitems.Append("Checked");
                    menuitems.Append("Optimize");
                    menuitems.Append("Disassemble");
                    menuitems.Append("AutoSave");
                }
            }
        }
        
        <string, variant> popup = PopupMenu.New(Editor.Left + xp, Editor.Top + 1, menuitems);
        PopupMenu.Execute(popup, this, key);
    }
        
    bool OnKey(<string, variant> this, Key key)
    {
        if (Key.Alt == (key & Key.Alt))
        {
            < string, Key> keys = this["keys"];
            foreach (var kv in keys)
            {
                if (kv.value == key)
                {
                    DoPopupMenu(this, kv.key);
                    return true; // consumed the key
                }
            }
        }
        else if (key == Key.Click)
        {
            uint x = ClickX;
            uint y = ClickY;
            if (ClickUp && !ClickDouble)
            {
                < <uint> > clickAreas = this["clickareas"];
                uint nmenus = menuOrder.Count;
                bool winner = false;
                string clickkey;
                for (uint i=0; i < nmenus; i++)
                {
                    string name = menuOrder[i];
                    <uint> area = clickAreas[i];
                    uint xm = area[0];
                    uint ym = area[1];
                    uint wm = area[2];
                    uint hm = area[3];
                    if ((x >= xm) && (x <= xm + wm))
                    {
                        if ((y >= ym) && (y < ym + hm))
                        {
                            clickkey = name;
                            winner = true;
                            break;
                        }
                    }
                }
                if (winner)
                {
                    DoPopupMenu(this, clickkey);
                    return true; // consumed the key   
                }
            }
            else if (ClickUp && ClickDouble && (menuProjectPath.Length != 0))
            {
                uint x0 = Panel.GetX0(this);
                uint y0 = Panel.GetY0(this);
                uint w = Panel.GetWidth(this);
                uint tw = titleText2.Length;
                uint tx = x0 + w - tw;
                if ((x >= tx) && (x <= tx + tw))
                {
                    if ((y >= y0) && (y < y0 + 1))
                    {
                        if (Editor.CanUndo())
                        {
                            Editor.OpenPath(menuProjectPath); // offer undo
                        }
                        else
                        {
                            Editor.LoadFile(menuProjectPath); // just open it
                        }
                        return true; // consumed the key   
                    }
                }
            }
        }
        return Panel.OnKey(this, key);
    }
    
    Draw(<string, variant> this)
    {
        Suspend();
        Panel.Draw(this);
        
        uint x0 = Panel.GetX0(this);
        uint y = Panel.GetY0(this);
        uint w = Panel.GetWidth(this);
        
        uint x = x0;
        uint backcolour = Panel.GetBackground(this);
        string menutext;

        < < uint > > listOfAreas;

        foreach (var name in menuOrder)
        {
            <uint> area;
            DrawChar(x, y, ' ', backcolour, backcolour);
            x++;
            
            area.Append(x);
            area.Append(y);
            
            uint textcolour = Editor.MenuTextColor;
            uint cw = 0;
            foreach (var c in name)
            {
                if (c == '&')
                {
                    textcolour = Colour.PopupText;
                }
                else
                {
                    DrawChar(x, y, c, textcolour, backcolour);
                    x++;
                    textcolour = Editor.MenuTextColor;
                    cw++;        
                }
            }
            area.Append(cw);
            area.Append(uint(1));
            DrawChar(x, y, ' ', backcolour, backcolour);
            x++;
            listOfAreas.Append(area);
        }
        
        uint pathColor = Colour.TitlePath;
        if (Editor.CanUndo())
        {
            pathColor = Colour.ModifiedPath; // file has been modified
#ifndef DEBUGGER            
            //BuildCommand.WasModified = true;
#endif
        }
        string content = "'" + titleText + "'";
        uint len = content.Length;
        if (titleText2 != "")
        {
            len = len + titleText2.Length;
        }
        x = x0 + w - len - 1;
        foreach (var c in content)
        {
            DrawChar(x, y, c, pathColor, backcolour);
            x++;
        }
        if (titleText2 != "")
        {
            content = " " + titleText2;
            foreach (var c in content)
            {
                DrawChar(x, y, c, Colour.TitlePath, backcolour);
                x++;
            }   
        }
        this["clickareas"] = listOfAreas;
        Resume(false);
    }
}
