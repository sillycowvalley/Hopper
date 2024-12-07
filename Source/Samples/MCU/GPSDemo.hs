program GPSDemo
{
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    uses "/Source/Library/Boards/AdafruitFeatherRP2350Hstx"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    
    uses "/Source/Library/GPS"
    
    bool captureFix;
    ButtonISR(byte pin, PinStatus status) 
    { 
        IO.WriteLn("    Pressed: '" + PinToButton(pin) + "'");  
        captureFix = true;
    }  
    
    Hopper()
    {
        if (!GPS.Begin())
        {
            IO.WriteLn("Failed to initialize Mini GPS");
            return;
        }
        
        
        DisplayDriver.FlipX = true;
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            IO.WriteLn("Failed to initialize OLED display");
            return;
        }
        Display.Clear();
        
        Time.Delay(500);
        bool success = GPS.SendSentence("$PMTK314,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0*28"); // stop transmitting points
        
        string lastLatitude;
        string lastLongitude;
        string lastElevation;
        
        loop
        {
            if (captureFix)
            {
                captureFix = false;
                success = GPS.SendSentence("$PMTK101*32"); // refresh the fix
                success = GPS.SendSentence("$PMTK314,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0*29"); // request coordinates
            }
            string sentence;
            while (GPS.NextSentence(ref sentence))
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
 
 
