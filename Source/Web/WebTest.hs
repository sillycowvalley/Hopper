program WebTest
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/System/System"
    uses "/Source/Web/WebServer"
    
    FourOhFourHandler()
    {
        Send(404, "text/plain", "Not found!");
        PrintLn("404");
        <string,string> arguments = WebServer.Arguments;
        foreach (var arg in arguments)
        {
            PrintLn("  " + arg.key + "=" + arg.value);
        }
    }
    
    TestHandler()
    {
        Send(200, "text/plain", "Test:");
        PrintLn("200");
        <string,string> arguments = WebServer.Arguments;
        foreach (var arg in arguments)
        {
            PrintLn("  " + arg.key + "=" + arg.value);
        }
    }
        
    IDHandler()
    {
        Send(200, "text/plain", "Hopper!");
        PrintLn("ID");
    }
    
    {
        Screen.Clear();
        WebServer.New();
        
        RequestHandler handler = FourOhFourHandler;
        WebServer.On("*", handler);
        
        handler = IDHandler;
        WebServer.On("/id", handler);
        
        handler = TestHandler;
        WebServer.On("/test*", handler);
        
        loop
        {
            if (IsAvailable)
            {
                Key key = ReadKey();
                if (key == Key.Escape)
                {
                    break;
                }
            }
        }
        WebServer.ClearHandlers();
    }
}
