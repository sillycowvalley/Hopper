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
            case 15: { colour = Color.Red; }
            case 17: { colour = Color.Green; }
            case  2: { colour = Color.Blue; }
            case  3: { colour = Color.Black; }
        }
        Redraw("Pin " + pin.ToString(), colour); 
    }
    
    Redraw(string message, uint colour)
    {
        //WriteLn(message);
        Graphics.Clear(colour);
    }
    
    {
        ISRDelegate buttonDelegate = ButtonISR;
        if (!Pico144.Initialize(buttonDelegate))
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
