program WebClient
{
    //uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    //uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    //uses "/Source/Library/Boards/ArduinoNanoConnect"
    
    uses "/Source/System/Serialize"
    
    uses "/Source/Samples/MCU/Secrets2/Connect"
    
    Hopper()
    {
#if !defined(BOARD_HAS_WIFI)
        #error "WiFi capable board required for this sample (see options at top of board file)"
#endif            
        
        WriteLn(); 
        
        uint attempts = 0;
        loop
        {
            LED = true;
            
            bool success = WiFi.Connect(SSID, Password);
            
            LED = false;
            
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
        WriteLn("IP: " + WiFi.IP);
        
        
        // http://arduino.tips/asciilogo.txt
        LED = true;
        string arduinoText;
        if (WebClient.GetRequest("arduino.tips/asciilogo.txt", ref arduinoText))
        {
            LED = false;
            uint iContent;
            if (arduinoText.IndexOf("\r\n\r\n", ref iContent))
            {
                arduinoText = arduinoText.Substring(iContent+4);
            }
            WriteLn(arduinoText);
        }
        
        
        
        // http://"worldclockapi.com/api/json/utc/now
        LED = true;
        string timejson;
        <string, variant> time;
        if (WebClient.GetRequest("worldclockapi.com/api/json/utc/now", ref timejson))
        {
            LED = false;
            uint iBrace;
            _ = timejson.IndexOf('{', ref iBrace);
            timejson = timejson.Substring(iBrace);
            if (!Serialize.TryFromJSON(timejson, time))
            {
                WriteLn("Deserialize failed");    
            }
        }
        
        WriteLn("Time:");
        foreach(var kv in time)
        {
            Write(kv.key + " ");
            if (typeof(kv.value) == delegate)
            {
                WriteLn("null");
            }
            else
            {
                WriteLn((kv.value).ToString());
            }
        }
        WiFi.Disconnect();
    }
}
