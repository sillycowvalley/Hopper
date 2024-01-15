program WebClient
{
    #define SERIAL_CONSOLE // use Serial, not Screen for Write, WriteLn, etc.
    #define RP2040_PICOW
    uses "/Source/Library/MCU"
    
    uses "/Source/System/WiFi"
    uses "/Source/System/HttpClient"
    uses "/Source/System/Serialize"
    
    uses "/Source/Samples/MCU/Connect"
    
    
    {
        
        WriteLn(); 
        if (!WiFi.Connect(SSID, Password))
        {
            WriteLn("Failed to connect");
        }
        else
        {
            WriteLn("Connected");
        }
        
        string timejson;
        <string, variant> time;
        if (HttpClient.GetRequest("worldtimeapi.org/api/timezone/pacific/auckland", ref timejson))
        {
            uint iBrace;
            if (timejson.IndexOf('{', ref iBrace)) { }
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
