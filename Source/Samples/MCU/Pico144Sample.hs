program Pico144Sample
{
#define PORTABLE
#define SERIALCONSOLE
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
        WriteLn(message);
        Graphics.Clear(colour);
    }
    
    {
        if (!Pico144.Initialize())
        {
            return;
        }
        ISRDelegate buttonDelegate = ButtonISR;
        if (!AttachToPin(15, buttonDelegate, PinStatus.Rising))
        {
            WriteLn("Pin 15 not valid for interrupts.");
            return;
        }
        if (!AttachToPin(17, buttonDelegate, PinStatus.Rising))
        {
            WriteLn("Pin 17 not valid for interrupts.");
            return;
        }
        if (!AttachToPin(2, buttonDelegate, PinStatus.Rising))
        {
            WriteLn("Pin 2 not valid for interrupts.");
            return;
        }     
        if (!AttachToPin(3, buttonDelegate, PinStatus.Rising))
        {
            WriteLn("Pin 3 not valid for interrupts.");
            return;
        }  
        Redraw("Start", Color.Black);
        
        loop
        {
            //Graphics.FlipDisplay(true);
            Delay(50);
        }
    }
}
