program WebAPServer
{
    //uses "/Source/Library/Boards/PiPicoW"
    //uses "/Source/Library/Boards/PiPico2W"
    //uses "/Source/Library/Boards/PimoroniPicoPlus2W"
    //uses "/Source/Library/Boards/Challenger2040WiFi"
    uses "/Source/Library/Boards/Challenger2350WiFi6Ble5"
    
    uint counter;
    NotFound(string uri, string method, <string,string> arguments)
    {
        WebServer.Send(404, "text/plain", "Not Found");
        IO.WriteLn("404 : NotFound");
    }
    
    Root(string uri, string method, <string,string> arguments)
    {
        string content = "Success";
        content += Char.EOL;
        content += "Millis: " + (Time.Millis).ToString();
        content += Char.EOL;
        WebServer.Send(content);
        IO.WriteLn("200: Root");
    }
    Reloader(string uri, string method, <string,string> arguments)
    {
        <string,string> headerContent;
        headerContent["Content-Type"] = "text/plain";
        headerContent["Refresh"]      = "5";     // Refresh the page automatically every 5 seconds.
        headerContent["Connection"]   = "close"; // The connection will be closed after completion of the response.
        
        WebServer.Send(200, headerContent, "Reloading .. " + counter.ToString());
        IO.WriteLn("200: Reloader " +counter.ToString());
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
        IO.WriteLn("200: AnyPage");
    }
    
    Hopper()
    {
        IO.WriteLn();
        for (uint i=0; i < 10; i++)
        {
            IO.Write('-');
            Time.Delay(250); // allow debugger to break into reboot
        }
        
#if !defined(BOARD_HAS_WIFI)
        #error "WiFi capable board required for this sample (see options at top of file)"
#endif                        
        IO.WriteLn(); 
        uint attempts = 0;
        loop
        {
            bool success = WiFi.BeginAP();
            if (success)
            {
                WriteLn("Access Point Started");
                break;
            }
            IO.Write(".");
            attempts++;
            if (attempts >= 5)
            {
                WriteLn("Failed to BeginAP");
                return;
            }
        }
        IO.WriteLn("IP: " + WiFi.IP);
        
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
