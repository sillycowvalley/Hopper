program WebServer
{
    //uses "/Source/Library/Boards/PiPicoW"
    uses "/Source/Library/Boards/Challenger2040WiFi"
    //uses "/Source/Library/Boards/ArduinoNanoConnect"
    
    uses "/Source/Samples/MCU/Secrets2/Connect"
    
    uint counter;
    NotFound(string uri, string method, <string,string> arguments)
    {
        WebServer.Send(404, "text/plain", "Not Found");
        WriteLn("404 : NotFound");
    }
    
    Root(string uri, string method, <string,string> arguments)
    {
        WebServer.Send("Success");
        WriteLn("200: Root");
    }
    Reloader(string uri, string method, <string,string> arguments)
    {
        <string,string> headerContent;
        headerContent["Content-Type"] = "text/plain";
        headerContent["Refresh"]      = "5";     // Refresh the page automatically every 5 seconds.
        headerContent["Connection"]   = "close"; // The connection will be closed after completion of the response.
        
        WebServer.Send(200, headerContent, "Reloading .. " + counter.ToString());
        WriteLn("200: Reloader " +counter.ToString());
        counter++;
    }
    AnyPage(string uri, string method, <string,string> arguments)
    {
        string content = "AnyPage:";
        if (arguments.Count != 0)
        {
            foreach (var argument in arguments)
            {
                content += Char.EOL + "  " + argument.key + " = " + argument.value;
            }
        }
        WebServer.Send(content);
        WriteLn("200: AnyPage");
    }
    
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
            if (attempts >= 5)
            {
                WriteLn("Failed to Connect");
                return;
            }
        }
        WriteLn("IP: " + WiFi.IP);
        
        WebServer.HandlerDelegate 
        pageHandler = Root;
        WebServer.On("/", pageHandler);
        pageHandler = AnyPage;
        WebServer.On("/anypage.txt", pageHandler);
        pageHandler = Reloader;
        WebServer.On("/reload", pageHandler);
        pageHandler = NotFound;
        WebServer.OnNotFound(pageHandler);
        
        WebServer.Begin();
        loop
        {
            WebServer.Events();
        }
    }
}
