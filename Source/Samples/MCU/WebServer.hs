program WebServer
{
    #define SERIAL_CONSOLE // use Serial, not Screen for Write, WriteLn, etc.
    
    //#define RP2040_PICOW
    //#define ARDUINO_NANO_RP2040
    #define CHALLENGER_RP2040_WIFI
    
    uses "/Source/Library/MCU"
    uses "/Source/Samples/MCU/Secrets2/Connect"
    
    uses "/Source/System/WiFi"
    uses "/Source/System/Serialize"
    
    NotFound(string uri, string method, <string,string> arguments)
    {
        WebServer.Send(404, "text/plain", "Not Found");
        WriteLn("404");
    }
    Root(string uri, string method, <string,string> arguments)
    {
        WebServer.Send("Success");
        WriteLn("200");
    }
    
    {
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
            if (attempts >= 5)
            {
                WriteLn("Failed to Connect");
                return;
            }
        }
        
        WriteLn("IP: " + WiFi.IP);
        
        HandlerDelegate rootHandler = Root;
        WebServer.On("/", rootHandler);
        HandlerDelegate notFoundHandler = NotFound;
        WebServer.OnNotFound(notFoundHandler);
        
        WebServer.Begin();
        
        loop
        {
            WebServer.Events();
        }
    }
}
