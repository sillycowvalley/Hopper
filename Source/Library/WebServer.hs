unit WebServer
{
    delegate HandlerDelegate(string uri, string method, <string,string> arguments);
    
    Begin() library; // default is port=80
    Begin(uint port) library;
    Close() library;
    Events() library;
    
    Send(uint httpCode, <string,string> headerContent, string content) library;
    Send(uint httpCode, string contentType, string content)
    {
        <string,string> headerContent;
        headerContent["Content-Type"] = contentType;
        Send(httpCode, headerContent, content);
    }
    Send(string content)
    {
        Send(200, "text/plain", content);
    }
    
    On(string uri, HandlerDelegate handler) library;
    OnNotFound(HandlerDelegate handler) library;
}
