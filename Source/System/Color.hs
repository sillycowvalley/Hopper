unit Color
{
    uint Black     { get { return 0x000; } }
    uint MatrixGreen { get { return 0x7F7; } }

    uint White     { get { return 0xFFF; } }
    uint MatrixBlue  { get { return 0x77F; } }
    
    uint MatrixRed  { get { return 0xF77; } }
    
    uint MenuBlue      { get { return 0x07D; } }
    uint PaleBlue  { get { return 0x0AE; } }
    uint SlateBlue { get { return 0x29B; } }
    //uint DarkBlue  { get { return 0x459; } }
    
    //uint Yellow    { get { return 0xFF0; } }
    
    //uint Green     { get { return 0xBE1; } }
    //uint DarkGreen { get { return 0x292; } }
    
    uint DarkGray { get { return 0x666; } }
    uint MarginGray { get { return 0xAAA; } }
    uint LightGray { get { return 0xDDD; } }
    uint LightestGray { get { return 0xEEE; }}
    uint Gray      { get { return 0x888; } }
    uint Button    { get { return 0xDCB; } }
    uint ButtonText { get { return 0x006; } }
    uint AltKey    { get { return 0x066; } }
    
        
    uint DarkBlue    { get { return 0x003; }}
    uint DarkGreen   { get { return 0x030; }}
    uint DarkCyan    { get { return 0x033; }}
    uint DarkRed     { get { return 0x300; }}
    uint DarkMagenta { get { return 0x303; }}
    uint DarkYellow  { get { return 0x330; }}
  //uint DarkGray    { get { return 0x111; }}
  //uint LightGray   { get { return 0x333; }}
    uint Blue        { get { return 0x00F; }}
    uint Green       { get { return 0x0F0; }}
    uint Cyan        { get { return 0x0FF; }}
    uint Red         { get { return 0xF00; }}
    uint Magenta     { get { return 0xF0F; }}
    uint Yellow      { get { return 0xFF0; }}
    
    uint Comment     { get { return 0x360; }}
    uint Statement   { get { return 0x60C; }}
    uint Type        { get { return 0x33F; }}
    uint Delimiter   { get { return 0x636; }}
    uint Constant    { get { return 0x900; }}
    uint Private     { get { return 0x330; }}
    uint Public      { get { return 0x036; }}
    uint Directive   { get { return 0x666; }}
}
