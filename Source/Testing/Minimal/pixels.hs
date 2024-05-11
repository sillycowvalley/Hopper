program Pixels
{
    //#define MCU
    
    uses "/Source/Minimal/System"
    
    uses "/Source/Minimal/Wire" // just for fast navigation for now
    uses "/Source/Library/Displays/OLEDSSD1306"
    
    byte lfsrState = 0;
    Seed()
    {
        lfsrState = byte(Time.Seconds); // Seed based on current second
        if (lfsrState == 0)
        {
            lfsrState = 0xA5; // Use a default non-zero value if seed is zero
        }
    }
    byte Random()
    {
        // Taps: 8, 6, 5, 4; characteristic polynomial: x^8 + x^6 + x^5 + x^4 + 1
        byte lsb = lfsrState & 1;         // Get LSB (least significant bit)
        lfsrState = lfsrState >> 1;       // Shift register
        if (lsb == 1)                     // If the output bit is 1, apply toggle mask
        {
            lfsrState = lfsrState^ 0xB8;  // Toggle pattern for feedback, corresponds to the taps
        }  
        if (lfsrState == 0)
        {  
            Seed();
        }
        return lfsrState;
    }
    Hopper()
    {
        Seed();
        
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }
        Display.Suspend();
        Display.Clear(Colour.Black);
        Display.Resume();
        
        loop
        {
            byte x = Random() % 128;
            byte y = Random() % 64;
            DirectPixel(x, y, Colour.White);   
            /*
            byte x = Random() % 64;
            byte y = Random() % 32;
            byte x2 = x << 1;
            byte y2 = y << 1;
            DirectPixel2x2(x2, y2, Colour.White);   
            */
        }
    }
}
