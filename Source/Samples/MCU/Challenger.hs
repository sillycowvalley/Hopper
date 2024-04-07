program ChallengerDemo
{
    #define SERIAL_CONSOLE // use Serial, not Screen for Write, WriteLn, etc.
    
    #define CHALLENGER_RP2040_WIFI
    
    uses "/Source/Library/MCU"
    uses "/Source/Samples/MCU/Secrets2/Connect"
    
    NotFound(string uri, string method, <string,string> arguments)
    {
        WebServer.Send(404, "text/plain", "My Not Found");
    }
    
    Root(string uri, string method, <string,string> arguments)
    {
        string content = "Success at " + uri;
        if (arguments.Count != 0)
        {
            foreach (var argument in arguments)
            {
                content += Char.EOL + "  " + argument.key + " = " + argument.value;
            }
        }
        WebServer.Send(content);
    }
    
    {
        if (!WiFi.Connect(SSID, Password))
        {
            WriteLn("Failed to Connect");
            return;
        }
        WriteLn("Connected: "  + WiFi.IP);
        
        WebServer.HandlerDelegate pageHandler = Root;
        WebServer.On("/", pageHandler);
        WebServer.On("/page", pageHandler);
        
        pageHandler = NotFound;
        WebServer.OnNotFound(pageHandler);
        
        WebServer.Begin();
        loop
        {
            WebServer.Events();
        }
    }
}
