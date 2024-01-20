program Badger
{
    //#define DISPLAY_DIAGNOSTICS
    
    const string dataPath = "/Content/Tigger.path";
    
    uses "/Source/Library/Devices/Badger2040"
    uses "/Source/Library/Fonts/Hitachi5x7"
    uses "/Source/Library/Graphics/Canvas"
    uses "/Source/Library/Graphics/Vectors"
    
    bool exit;
    ButtonISR(byte pin, PinStatus status) 
    { 
        string pinName = PinToButton(pin);
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        if (pinName == "a")
        {
            exit = true;
        }
    }
    
    {
        // Setup code:
        ISRDelegate buttonDelegate = ButtonISR;
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize Badger 2040");
            return;
        }
        
        <char> commands;
        < <float> > coords;
        if (!Vectors.Load(dataPath, ref commands, ref coords))
        {
            IO.WriteLn("Failed to load '" + dataPath + "'");
            return;
        }
        
        Display.Suspend();
        Display.Clear(Colour.White);
        
        float minX;
        float minY;
        float maxX;
        float maxY;
        Vectors.Extents(coords, ref minX, ref minY, ref maxX, ref maxY);
        
        Canvas.Width        = (maxX - minX);
        Canvas.Height       = (maxY - minY);
        Canvas.PixelWidth   = Display.PixelWidth;
        Canvas.PixelHeight  = Display.PixelHeight;
        Canvas.PixelXOrigin = 0;
        Canvas.PixelYOrigin = 0;
        
        Vectors.Render(commands, coords);
        
        Display.Resume();
    }    
}
