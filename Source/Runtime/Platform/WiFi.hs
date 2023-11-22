unit HRWiFi
{
    bool Connect(uint ssid, uint password) { return External.WiFiConnect(ssid, password); }
}
