program WebServer
{
    #define SERIAL_CONSOLE // use Serial, not Screen for Write, WriteLn, etc.
    #define RP2040_PICOW
    uses "/Source/Library/MCU"
    uses "/Source/Samples/MCU/Secrets/Connect"
    
    uses "/Source/System/WiFi"
    uses "/Source/System/HttpServer"
    uses "/Source/System/Serialize"
    
    
    
    
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
        
    }
}
