unit MenuBar
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/Editor/Panel"
    uses "/Source/Editor/PopupMenu"
    
    // Panel 
    //   x0     : uint
    //   y0     : uint
    //   width  : uint
    //   height : uint
    //   background : uint   - colour
    
    <string> menuOrder;
    string titleText;
    string titleText2;
    string projectPath;
       
    <string, variant> New()
    {
        <string, variant> instance = Panel.New(0, 0, Screen.Columns, 1);
        
        Panel.SetBackground(instance, Color.MenuBlue);
        
        // globals
        menuOrder.Append("&File");
        menuOrder.Append("&Edit");
        //menuOrder.Append("&Debug");
        menuOrder.Append("&Build");
        
        <string, Key > keys;
        
        keys["&File"] = (Key.Alt | Key.ModF);
        keys["&Edit"] = (Key.Alt | Key.ModE);
        //keys["&Debug"] = (Key.Alt | Key.ModD);
        keys["&Build"] = (Key.Alt | Key.ModB);
        
        instance["keys"] = keys;
        
        <string, uint > xpos;
        
        xpos["&File"]  = 0;
        string menunames = " File ";
        xpos["&Edit"]  = menunames.Length;
        menunames = menunames + " Edit ";
        //xpos["&Debug"] = menunames.Length;
        //menunames = menunames + " Debug ";
        xpos["&Build"] = menunames.Length;
        
        instance["xpos"] = xpos;

        < < uint > > listOfAreas;
        instance["clickareas"] = listOfAreas;

        return instance;
    }
    
    SetTitleText(<string, variant> this, string cp, string pp)
    {
        titleText = cp;
        titleText2 = "";
        projectPath = "";
        if (cp != pp)
        {
            projectPath = pp;
            titleText2 = Path.GetFileName(pp);
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
                menuitems.Append("New");
                menuitems.Append("Open");
                menuitems.Append("");
                menuitems.Append("Save");
                menuitems.Append("SaveAs");
                menuitems.Append("");
                menuitems.Append("Exit");
            }
            case "&Edit":
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
            case "&Debug":
            {
                menuitems.Append("StartDebugging");
                menuitems.Append("StartWithoutDebugging");
                menuitems.Append("StopDebugging");
                menuitems.Append("");
                menuitems.Append("StepInto");
                menuitems.Append("StepOver");
                menuitems.Append("StepOut");
                menuitems.Append("");
                menuitems.Append("ToggleBreakpoint");
                menuitems.Append("DeleteAllBreakpoints");
            }
            case "&Build":
            {
                menuitems.Append("Build");
                menuitems.Append("Run");
            }
        }
        
        <string, variant> popup = PopupMenu.New(xp, 1, menuitems);
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
                uint nmenus = menuOrder.Length;
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
            else if (ClickUp && ClickDouble && (projectPath.Length != 0))
            {
                uint x0 = Panel.GetX0(this);
                uint y0 = Panel.GetY0(this);
                uint w = Panel.GetWidth(this);
                uint tw = titleText2.Length+4;
                uint tx = x0 + w - tw;
                if ((x >= tx) && (x <= tx + tw))
                {
                    if ((y >= y0) && (y < y0 + 1))
                    {
                        if (Editor.CanUndo())
                        {
                            Editor.OpenPath(projectPath); // offer undo
                        }
                        else
                        {
                            Editor.LoadFile(projectPath); // just open it
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
        uint backcolor = Panel.GetBackground(this);
        string menutext;

        < < uint > > listOfAreas;

        foreach (var name in menuOrder)
        {
            <uint> area;
            DrawChar(x, y, ' ', backcolor, backcolor);
            x++;
            
            area.Append(x);
            area.Append(y);
            
            uint textcolor = Color.Black;
            uint cw = 0;
            foreach (var c in name)
            {
                if (c == '&')
                {
                    textcolor = Color.White;
                }
                else
                {
                    DrawChar(x, y, c, textcolor, backcolor);
                    x++;
                    textcolor = Color.Black;
                    cw++;        
                }
            }
            area.Append(cw);
            area.Append(uint(1));
            DrawChar(x, y, ' ', backcolor, backcolor);
            x++;
            listOfAreas.Append(area);
        }
        
        uint pathColor = 0x9F6;
        if (Editor.CanUndo())
        {
            pathColor = 0xF99; // file has been modified
        }
        string content = "'" + titleText + "'";
        uint len = content.Length;
        if (titleText2 != "")
        {
            len = len + titleText2.Length + 4;
        }
        x = x0 + w - len - 1;
        foreach (var c in content)
        {
            DrawChar(x, y, c, pathColor, backcolor);
            x++;
        }
        if (titleText2 != "")
        {
            content = " ['" + titleText2 + "']";
            foreach (var c in content)
            {
                DrawChar(x, y, c, 0x9F6, backcolor);
                x++;
            }   
        }
        this["clickareas"] = listOfAreas;
        Resume(false);
    }
}
