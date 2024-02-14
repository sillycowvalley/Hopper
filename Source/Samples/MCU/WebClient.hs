program WebClient
{
    //uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Boards/Challenger2040WiFi"
    //uses "/Source/Library/Boards/ArduinoNanoConnect"
    
    uses "/Source/System/Serialize"
    
    uses "/Source/Samples/MCU/Secrets2/Connect"
    
    {
#if !defined(BOARD_HAS_WIFI)
        #error "WiFi capable board required for this sample (see options at top of file)"
#endif            
        
        WriteLn(); 
        
        uint attempts = 0;
        loop
        {
            bool success = WiFi.Connect(SSID, Password);
            if (success)
            {
                WriteLn("Connected");
                break;
            }
            Write(".");
            attempts++;
            if (attempts >= 10)
            {
                WriteLn("Failed to Connect");
                return;
            }
        }
        
        // http://arduino.tips/asciilogo.txt
        
        WriteLn("IP: " + WiFi.IP);
        
        string timejson;
        <string, variant> time;
        if (WebClient.GetRequest("worldtimeapi.org/api/timezone/pacific/auckland", ref timejson))
        {
            uint iBrace;
            _ = timejson.IndexOf('{', ref iBrace);
            timejson = timejson.Substring(iBrace);
            if (!Serialize.TryFromJSON(timejson, ref time))
            {
                WriteLn("Deserialize failed");    
            }
        }
        
        WriteLn("Time:");
        foreach(var kv in time)
        {
            Write(kv.key + " ");
            WriteLn((kv.value).ToString());
        }

    }
}
