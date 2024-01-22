program Pico144Sample
{
#define SERIAL_CONSOLE
#define RP2040
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Graphics"
    uses "/Source/Samples/MCU/Games/Pico144"
    
    ButtonISR(byte pin, PinStatus status) 
    { 
        uint colour;
        switch (pin)
        {
            case 15: { colour = Colour.Red; }
            case 17: { colour = Colour.Green; }
            case  2: { colour = Colour.Blue; }
            case  3: { colour = Colour.Black; }
        }
        Redraw("Pin " + pin.ToString(), colour); 
    }
    
    Redraw(string message, uint colour)
    {
        //WriteLn(message);
        Graphics.Clear(colour);
    }
    
    {
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!Pico144.Initialize(buttonDelegate))
        {
            return;
        }
        Redraw("Start", Colour.Black);
        loop
        {
            // very tight loop
        }
    }
}
