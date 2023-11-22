#include "HopperWifi.h"

#ifdef RP2040PICO
#define USEWIFI       // WiFi for Raspberry Pi Pico W
#endif

#if defined(RP2040XIAO) || defined(TINY2040)
#define USEWIFISTUBS    // no WiFi for Seeed XIA0 RP2040 or Pimoroni Tiny 2040
#endif

#ifdef RP2040NANO
#define USEWIFININA   // WiFi for the Arduino Connect RP2040
#endif

#ifdef LOLIND1MINI  // WiFi olin Wemos D1 Mini, ESP 8266
#define ESP8266WIFI
//#define USEWIFISTUBS
#endif 

#ifdef LOLIN_S2_PICO
#define ESP32WIFI
#endif

#ifdef SEEEDESP32C3
#define ESP32WIFI
#endif

#ifdef USEWIFININA
#include <SPI.h>
#include <WiFiNINA.h>
#endif

#ifdef USEWIFI
#include <WiFi.h>
#endif

#ifdef ESP8266WIFI
#include <ESP8266WiFi.h>
#define USEWIFI  // subset of ESP8266 WiFi
#endif

#ifdef USEWIFININA
#define USEWIFI  // subset of WiFiNina
#endif


String ssid;
String password;
bool wifiConnected = false;

#ifdef USEWIFISTUBS
bool WifiConnect()
{
    return false;
}
bool HttpClient_GetRequest(UInt hrurl, UInt& hrcontent)
{
    return false;
}
#endif


#ifdef USEWIFI

//#if defined(USEWIFININA) || defined(ESP8266WIFI)
WiFiClient wifiClient;
//#endif

bool WifiConnect()
{
    bool success = false;
    int count = 0;
    int status = WL_IDLE_STATUS;
#ifdef ESP8266WIFI
    WiFi.mode(WIFI_STA);
#endif
#if defined(ESP32WIFI) || defined (ESP8266WIFI)
    WiFi.begin(ssid.c_str(), password.c_str());
#endif    
    for (;;)
    {
#if defined(ESP32WIFI) || defined(ESP8266WIFI)
        status = WiFi.status();
#else
        status = WiFi.begin(ssid.c_str(), password.c_str());
#endif
        if (status == WL_CONNECTED)
        {
#ifdef DIAGNOSTICS          
            Serial.println();
            Serial.println("WiFi connected");
#endif
            success = true;
            break;
        }
        delay(500);
#ifdef DIAGNOSTICS                  
        Serial.print(".");
#endif        
        count++;
        if (count > 20)
        {
#ifdef DIAGNOSTICS          
            Serial.println();
            Serial.println("WiFi failed to connect");
#endif
            break;
        }
    }  
    return success;
}
bool HttpClient_GetRequest(UInt hrurl, UInt& hrcontent)
{
    UInt length = HRString_GetLength(hrurl);
    String host;
    String url;
    bool dotSeen = false;
    UInt i;
    for (i=0; i < length; i++)
    {
        Char ch = HRString_GetChar(hrurl, i);
        if (dotSeen && (ch == '/')) // first '/' after '.' seen
        {
            break;
        }
        host += (char)ch;
        if (ch == '.')
        {
           dotSeen = true;
        }
    }
    for (; i < length; i++)
    {
        url += (char)HRString_GetChar(hrurl, i);
    }

    if (wifiClient.connect(host.c_str(), 80))
    {
#ifdef DIAGNOSTICS      
        Serial.println("wifiClient: connected");
#endif
        wifiClient.print("GET " + url + " HTTP/1.1\r\nHost: " + host + "\r\nConnection: close\r\n\r\n");
        
        unsigned long timeout = millis();
        while (wifiClient.available() == 0) 
        {
          if (millis() - timeout > 10000)
          { 
#ifdef DIAGNOSTICS                  
            Serial.println(">>> " + host + " timeout !");
#endif
            wifiClient.stop(); 
            return false; 
          } 
        } 
        GC_Release(hrcontent);
        hrcontent = HRString_New();
        while (wifiClient.available()) 
        {
            char c = wifiClient.read();
            UInt length = HRString_GetLength(hrcontent);
            HRString_BuildChar_R(hrcontent, c);
        }
    }
    else
    {
#ifdef DIAGNOSTICS      
        Serial.println("wifiClient: failed to connect");
#endif
    }
    return true;
}
#endif

Bool External_HttpClientGetRequest_R(UInt hrurl, UInt& hrcontent)
{
    return HttpClient_GetRequest(hrurl, hrcontent);
}

Bool External_WiFiConnect(UInt hrssid, UInt hrpassword)
{
    if (!wifiConnected)
    {
        UInt length = HRString_GetLength(hrssid);
        ssid = "";
        for (UInt i=0; i < length; i++)
        {
            ssid += (char)HRString_GetChar(hrssid, i);
        }
        length = HRString_GetLength(hrpassword);
        password = "";
        for (UInt i=0; i < length; i++)
        {
            password += (char)HRString_GetChar(hrpassword, i);
        }
        wifiConnected =  WifiConnect();
    }
    return wifiConnected;
}
