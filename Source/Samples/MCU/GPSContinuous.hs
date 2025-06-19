program GPSContinuous
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    uses "/Source/Library/GPS"
    
    Hopper()
    {
        if (!GPS.Begin())
        {
            IO.WriteLn("Failed to initialize Mini GPS");
            return;
        }
        
        DisplayDriver.FlipX = true;
        if (!Display.Begin())
        {
            IO.WriteLn("Failed to initialize OLED display");
            return;
        }
        Display.Clear();
        
        string lastLatitude;
        string lastLongitude;
        string lastElevation;
        
        loop
        {
            string sentence;
            if (GPS.NextSentence(ref sentence))
            {
                if (GPS.Consume(sentence))
                {
                    IO.WriteLn(sentence);  
                    if ((GPS.Latitude != lastLatitude) || (GPS.Longitude != lastLongitude) || (GPS.Elevation != lastElevation))
                    {
                        lastLatitude  = GPS.Latitude;
                        lastLongitude = GPS.Longitude;
                        lastElevation = GPS.Elevation;
                        Display.Suspend();
                        Screen.Clear();
                        Screen.PrintLn("Time: " + GPS.UTC);
                        Screen.PrintLn("Lat:  " + lastLatitude);
                        Screen.PrintLn("Lon:  " + lastLongitude);
                        Screen.PrintLn("Elev: " + lastElevation);
                        Display.Resume();
                    }
                }
            }
        }
    }
}
 
 
