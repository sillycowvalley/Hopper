unit Panel
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    
    
    // Panel 
    //   x0     : uint
    //   y0     : uint
    //   width  : uint
    //   height : uint
    //   children : < Panel >
    
    <string, variant> New(byte x, byte y, byte w, byte h)
    {
        <string, variant> instance;
        
        < string, uint> values;
        values["x0"] = x;
        values["y0"] = y;
        values["width"] = w;
        values["height"] = h;
        values["background"] = 0xFFF;
        
        instance["values"] = values;
        
        return instance;
    }
    
    uint GetBackground(<string, variant> this ) 
    { 
        < string, uint> values = this["values"];
        uint background = values["background"];
        return background; 
    }
    SetBackground(<string, variant> this, uint colour) 
    { 
        < string, uint> values = this["values"];
        values["background"] = colour;
        this["values"] = values;
    }
    
    bool Contains(<string, variant> this, uint x, uint y)
    {
        bool contained = false;
        loop
        {
            uint x0 = GetX0(this)
            uint y0 = GetY0(this)
            if (x < x0)
            {
                break;
            }
            if (y < y0)
            {
                break;
            }
            if (x >= x0 + GetWidth(this))
            {
                break;
            }
            if (y >= y0 + GetHeight(this))
            {
                break;
            }
            contained = true;
            break;
        }
        return contained;
    }
    
    uint GetX0(<string, variant> this) 
    { 
        < string, uint> values = this["values"];
        return values["x0"]; 
    }
    uint GetY0(<string, variant> this) 
    { 
        < string, uint> values = this["values"];
        return values["y0"]; 
    }
    uint GetWidth(<string, variant> this) 
    { 
        < string, uint> values = this["values"];
        return values["width"]; 
    }
    uint GetHeight(<string, variant> this) 
    { 
        < string, uint> values = this["values"];
        return values["height"]; 
    }
    Draw(<string, variant> this)
    {
        Suspend();
        
        uint background = GetBackground(this);
        //if (background != 0xFFFF) // colour none
        //{
            uint x0 = GetX0(this);
            uint y0 = GetY0(this);
            uint height = GetHeight(this);
            uint width  = GetWidth(this);
            
            for (uint r=y0; r < y0+height; r++)
            {
               for (uint c = x0; c < x0+width; c++)
               {
                   DrawChar(c, r, ' ', 0x000, background);
               }
            }
        //}
        Resume(false);
    }
    
    bool OnKey(<string, variant> this, Key key)
    {
        return false; // nobody consumed the key
    }
}