unit WiFi
{
    enum WiFiStatus
    {
        Idle = 0,
        Connected,
        ConnectionFailed,
        ConnectionLost,
        Disconnected,
        AccesspointListening ,
        AccesspointConnected,
        AccessPointFailed,
        NoModule = 255,
    }
    string ToString(WiFiStatus wiFiStatus)
    {
        switch (wiFiStatus)
        {
            case WiFiStatus.Idle:                 { return "Idle"; }
            case WiFiStatus.Connected:            { return "Connected"; }
            case WiFiStatus.ConnectionFailed:     { return "Connection Failed"; }
            case WiFiStatus.ConnectionLost:       { return "Connection Lost"; }
            case WiFiStatus.Disconnected:         { return "Disconnected"; }
            case WiFiStatus.AccesspointListening: { return "Accesspoint Listening"; }
            case WiFiStatus.AccesspointConnected: { return "Accesspoint Connected"; }
            case WiFiStatus.AccessPointFailed:    { return "Accesspoint Failed"; }
            case WiFiStatus.NoModule:             { return "No Module"; }
        }
        return "Undefined";
    }
    
    bool Connect(string ssid, string password) system;
    Disconnect() system;
    string IP { get system; }
    WiFiStatus Status { get system; }
}
