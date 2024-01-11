program Pico096Sample
{
#define SERIAL_CONSOLE
#define RP2040
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Graphics"
    uses "/Source/Samples/MCU/Games/Pico096"
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        uint colour;
        switch (pin)
        {
            case 15: { colour = Color.Red; }      // A
            case 17: { colour = Color.Green; }    // B
            
            case 16: { colour = Color.Blue; }      // Left
            case 20: { colour = Color.DustyTeal; } // Right
            
            case 2:  { colour = Color.Avocado; }   // Up
            case 18: { colour = Color.Denim; }     // Down
            
            case  3: { colour = Color.Black; }     // Press
        }
        Redraw("Pin " + pin.ToString(), colour); 
    }
    
    Redraw(string message, uint colour)
    {
        WriteLn(message);
        Graphics.Clear(colour);
    }
    
    {
        ISRDelegate buttonDelegate = ButtonISR;
        if (!Pico096.Initialize(buttonDelegate))
        {
            return;
        }
        Redraw("Start", Color.Black);
        loop
        {
            // very tight loop
        }
    }
}
