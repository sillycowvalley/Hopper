unit HttpClient
{
    uses "/Source/System/System"
    
    // address: either IP or web server domains
    // true result means "connected"
    //bool Connect(string address, uint port) system; 
    //bool Connect(string address) system; 
    
    // send content to a valid connection
    //PrintLn(string content) system; // appends 0x0D, 0x0A
    //Print(string content) system;
    
    // returns true once the response is available
    //bool IsAvailable() system;
    
    // wait for IsAvailable, then read the response
    //string GetResponseContent() system;
    
    // close the connection
    //Stop() system;
    
    
    // or just call this .. ;-)
    bool GetRequest(string url, ref string response) system;
    
}
