unit WebServer
{
    uses "/Source/System/System"
    
    delegate RequestHandler();
 
    string Method { get system; }  // "GET", "POST", ..
    string URL    { get system; }
    <string,string> Arguments { get system; }
    
    On(string url, RequestHandler requestHandler) system;
    On(string url, string httpMethod, RequestHandler requestHandler) system;
    ClearHandlers() system;
    
    New()
    {
        RequestHandler handler = DefaultHandler;
        On("*", handler);
    }
    
    Send(uint statusCode, string contentType, string content) system;
    
    DefaultHandler()
    {
        Send(404, "text/plain", "Not found!");
    }
    
}
