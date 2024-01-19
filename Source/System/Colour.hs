unit Colour
{
    // Shades of Gray
    uint Black        { get { return 0x000; } }
    uint DarkGray     { get { return 0x666; } }
    uint Gray         { get { return 0x888; } }
    uint LightGray    { get { return 0xDDD; } }
    uint LightestGray { get { return 0xEEE; }}
    uint White        { get { return 0xFFF; } }
    uint Invert       { get { return 0xF000; } }
    
    // Boring generic colours
    uint DarkBlue    { get { return 0x003; }}
    uint DarkGreen   { get { return 0x030; }}
    uint DarkCyan    { get { return 0x033; }}
    uint DarkRed     { get { return 0x300; }}
    uint DarkMagenta { get { return 0x303; }}
    uint DarkYellow  { get { return 0x330; }}
    uint Blue        { get { return 0x00F; }}
    uint Green       { get { return 0x0F0; }}
    uint Cyan        { get { return 0x0FF; }}
    uint Red         { get { return 0xF00; }}
    uint Magenta     { get { return 0xF0F; }}
    uint Yellow      { get { return 0xFF0; }}
    
    uint Viridian    { get { return 0x497; }} // Darker green
    uint Avocado     { get { return 0x9B4; }} // Olive green
    uint JuneBud     { get { return 0xDE5; }} // Brighter green
    
    uint Denim       { get { return 0x269; }} // Darker blue
    uint DustyTeal   { get { return 0x3A8; }}
    uint HippieBlue  { get { return 0x4AB; }} // Pale blue
    
    // Console Colors (assuming a black background, Screen.Print(..) defaults)
    uint MatrixGreen         { get { return 0x7F7; } }
    uint MatrixBlue          { get { return 0x88F; } }
    uint MatrixCyan          { get { return 0x0AE; } }
    uint MatrixRed           { get { return 0xF77; } }
    
    // ProgressTick colours when in console mode
    uint ProgressTick        { get { return 0x258; } }
    uint ProgressBackground  { get { return 0x68A; } }
    uint ProgressText        { get { return 0x006; } }
    uint ProgressFace        { get { return 0xDCB; } }
    uint ProgressHighlight   { get { return 0x3A8; }}
    
    // UI colours (Editor and Debugger)
    uint MenuBlue            { get { return Denim; } }      // title bar background in editor
    uint MenuGreen           { get { return Viridian; } }   // title bar background in debugger
    uint MenuTextBlue        { get { return HippieBlue; } } // menu text colour in editor
    uint MenuTextGreen       { get { return 0x242; } }      // menu text colour in debugger
    
    
    uint TitlePath    { get { return 0x9F6; } }        // path colour in the title bar
    uint ModifiedPath { get { return 0xF99; } }        // path colour in the title bar if the file has been modified
    uint AltKey       { get { return 0x066; } }
    
    uint ButtonFace   { get { return 0xDCB; } }
    uint ButtonText   { get { return 0x006; } }
    uint PopupText    { get { return 0xFFF; } }        // popup title text (and bar menu shortcut key)
    uint PopupFace    { get { return 0xCCD; } }        // popup background
    uint LabelText    { get { return 0x000; } }        // popup field label text 
    uint EditText     { get { return 0x000; } }        // popup edit field text
    uint EditFace     { get { return 0xEEE; } }        // popup edit field background
    
    uint StatusFace   { get { return 0xDCB; } }
    uint StatusText   { get { return 0x006; } }
    uint MarginText   { get { return 0x006; } } // line numbers in left margin
    uint MarginFace   { get { return 0xAAA; } } // left margin for line numbers
    
    // Debugger:
    uint ActiveRed      { get { return 0xECC; } } // background for lines with breakpoints
    uint ActiveGray     { get { return 0xCCC; } } // background for current line
    uint ListGray       { get { return 0xDDD; } } // stack list background
    uint ActiveListGray { get { return 0xCCC; } } // current selected item in stack or variable list
    
    
    // Highlighter: used for syntax highlighting
    uint Comment     { get { return 0x360; }}
    uint Statement   { get { return 0x60C; }}
    uint Type        { get { return 0x33F; }}
    uint Delimiter   { get { return 0x636; }}
    uint Constant    { get { return 0x900; }}
    uint Private     { get { return 0x330; }}
    uint Public      { get { return 0x036; }}
    uint Directive   { get { return 0x666; }}
    
    
    
    
}
