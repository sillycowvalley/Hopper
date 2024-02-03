program NeoPixelFeather
{
    #define ADAFRUIT_FEATHER_RP2040
    uses "/Source/Library/MCU"
    uses "/Source/Library/Devices/AdafruitNeoPixelFeatherWing"

    byte ColorComponent(ref byte index)
    {
        long now = Millis;
        index++;
        if (index == 3) { index = 0; }
        return now.GetByte(index) ^ now.GetByte(index+1);
    }
    
    {
        byte bi = 0;
        byte gi = 1;
        byte ri = 2;
        
        NeoPixel.Begin();
        
        loop
        {
            for (byte i = 0; i < NeoPixel.Length; i++)
            {
                NeoPixel.SetColor(i, ColorComponent(ref ri), ColorComponent(ref gi), ColorComponent(ref bi));
                NeoPixel.Show();
                Time.Delay(5);
            }
        }
    }
    
}
