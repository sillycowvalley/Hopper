program NeoPixelFeather
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/Library/Devices/AdafruitNeoPixelFeatherWing"

    byte ColorComponent(ref byte index)
    {
        long now = Millis;
        index++;
        if (index == 3) { index = 0; }
        return now.GetByte(index) ^ now.GetByte(index+1);
    }
       
    TestDrawRect(uint colour, uint colour2)
    {
        Screen.Clear();
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        Rectangle(0, 0, pw, ph, colour);
        Rectangle(1, 1, pw-2, ph-2, colour2);
    }
    TestDrawCross(uint colour)
    {
        int ph = Display.PixelHeight;
        int pw = Display.PixelWidth;
        Line(0, 0,    pw-1, ph-1, colour);
        Line(0, ph-1, pw-1, 0,    colour);
    }
    
    
    {
        
        if (!DeviceDriver.Begin())
        {
            WriteLn("Initialization Failed");
        }
        
        bool doDisplay = false; // Use Display driver:  draw lines, rectangles, text, etc. in RGB444 12-bit colour
        bool doPixels  = true; // Use NeoPixel driver: light pixels on string in RGB888 24-bit colour
        
        loop
        {
            if (doPixels)
            {
                byte bi = 0;
                byte gi = 1;
                byte ri = 2;
                
                for (uint laps = 0; laps < 10; laps ++)
                {
                    for (byte i = 0; i < NeoPixel.Length; i++)
                    {
                        NeoPixel.SetColor(i, ColorComponent(ref ri), ColorComponent(ref gi), ColorComponent(ref bi));
                        NeoPixel.Show();
                        Time.Delay(5);
                    }
                }
            }
            
            if (doDisplay)
            {
                Display.Clear(Colour.Black);
                Delay(250);
                Display.Clear(Colour.White);
                Delay(250);
                Display.Clear(Colour.Red);
                Delay(250);
                Display.Clear(Colour.Green);
                Delay(250);
                Display.Clear(Colour.Blue);
                Delay(250);
                TestDrawRect(Colour.Red, Colour.Blue);
                Delay(250);
                TestDrawCross(Colour.Yellow);
                Delay(250);
                TestDrawCross(Colour.Invert);
                Delay(250);
                TestDrawRect(Colour.Blue, Colour.Red);
                Delay(250);
                TestDrawCross(Colour.Green);
                Delay(250);
                TestDrawCross(Colour.Invert);
                Delay(250);
            }
            
        }
     
    }
    
}
