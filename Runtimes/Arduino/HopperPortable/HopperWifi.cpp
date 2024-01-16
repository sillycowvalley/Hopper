#include "HopperWifi.h"

#if defined(RP2040PICOW) || defined(ARDUINONANO_RP2040)
#define USEWIFI       // WiFi for Raspberry Pi Pico W
#endif

#ifndef USEWIFI
#define USEWIFISTUBS    // no WiFi for Pi Pico, Seeed XIA0 RP2040 or Pimoroni Tiny 2040
#endif


#ifdef RP2040PICOW
#include <WiFi.h>
#include <WiFiClient.h> // added for RP2040W
#include <WebServer.h>  // added for RP2040W
#endif

#ifdef ARDUINONANO_RP2040
#include <SPI.h>
#include <WiFiNINA.h>
#endif


String ssid;
String password;
bool wifiConnected = false;

#ifdef USEWIFISTUBS
bool WifiConnect()
{
    return false;
}
bool WebClient_GetRequest(UInt hrurl, UInt& hrcontent)
{
    return false;
}
#endif


#if defined(USEWIFI)
bool currentRequestIsGet;
UInt notFoundDelegatePC = 0;
UInt responseCode = 0;

UInt handlerDelegatePCs = 0;
UInt currentRequestURI = 0;
UInt currentRequestArguments = 0;
UInt responseContent = 0;
UInt responseType = 0;


#endif
void WebServer_Restart()
{
#ifdef DIAGNOSTICS      
    //Serial.println("<WebServer_Restart");
#endif
    if ((0 != handlerDelegatePCs) || (0 != currentRequestArguments) || (0 != currentRequestURI) || (0 != responseContent))
    {
        WebServer_Release();
    }
#ifdef DIAGNOSTICS      
    //Serial.println("WebServer_Restart>");
#endif

}
void WebServer_Release()
{
#ifdef DIAGNOSTICS      
    Serial.println("<WebServer_Release");
#endif
    if (0 != handlerDelegatePCs)
    {
        GC_Release(handlerDelegatePCs);
        handlerDelegatePCs = 0;
    }
    if (0 != currentRequestURI)
    {
        GC_Release(currentRequestURI);
        currentRequestURI = 0;
    }
    if (0 != currentRequestArguments)
    {
        GC_Release(currentRequestArguments);
        currentRequestArguments = 0;
    }
    if (0 != responseContent)
    {
        GC_Release(responseContent);
        responseContent = 0;
    }
    if (0 != responseType)
    {
        GC_Release(responseType);
        responseType = 0;
    }
#ifdef DIAGNOSTICS      
    Serial.println("WebServer_Release>");
#endif
}

void HRString_FromString(UInt & hrstr, const String & str)
{
    if (hrstr != 0)
    {
        HRString_BuildClear_R(hrstr);
    }
    else
    {
        hrstr = HRString_New();
    }
    for (int i = 0; i < str.length(); i++)
    {
        HRString_BuildChar_R(hrstr, str.charAt(i));
    }
}

void HRString_ToString(UInt hrstr, String & str)
{
    str = "";
    if (0 != hrstr)
    {
        for (uint i = 0; i < HRString_GetLength(hrstr); i++)
        {
            str += (char)HRString_GetChar(hrstr, i);
        }
    }
}

#ifdef USEWIFI

WiFiClient wifiClient;

#ifdef RP2040PICOW
WebServer server(80);
#endif
#ifdef ARDUINONANO_RP2040
WiFiServer server(80);
#endif



bool WifiConnect()
{
    bool success = false;

    int status = WL_IDLE_STATUS;
#ifdef RP2040PICOW
    WiFi.mode(WIFI_STA);
    WiFi.begin(ssid.c_str(), password.c_str());
    for (int count = 0; count < 5; count++)
    {
        status = WiFi.status();
        if (status == WL_CONNECTED)
        {
#ifdef DIAGNOSTICS
            Serial.println();
            Serial.println("WiFi connected");
#endif
            success = true;
            break;
        }
        Serial.print("x");
        delay(1500);
    }  
#endif // RP2040PICOW
#ifdef ARDUINONANO_RP2040
    for (int count = 0; count < 5; count++)
    {
        status = WiFi.begin(ssid.c_str(), password.c_str());
        if (status == WL_CONNECTED)
        {
#ifdef DIAGNOSTICS
            Serial.println();
            Serial.println("WiFi connected");
#endif
            success = true;
            break;
        }
        Serial.print("x");
        delay(500);
    } 
#endif // ARDUINONANO_RP2040
    return success;
}
bool WebClient_GetRequest(UInt hrurl, UInt& hrcontent)
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

Bool External_WebClientGetRequest_R(UInt hrurl, UInt& hrcontent)
{
    return WebClient_GetRequest(hrurl, hrcontent);
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
    if (0 != handlerDelegatePCs)
    {
        HRDictionary_Clear(handlerDelegatePCs);
    }
    return wifiConnected;
}

UInt External_WiFiIP()
{
    UInt hrip = HRString_New();
#ifdef USEWIFI
    if (wifiConnected)
    {
        HRString_FromString(hrip, WiFi.localIP().toString());
    }
#endif
    return hrip;
}
void External_WebServerBegin(UInt port)
{
#if defined(USEWIFI)
    if (wifiConnected)
    {
#ifdef RP2040PICOW
        server.begin(port);
#endif
#ifdef ARDUINONANO_RP2040
        server.begin();
#endif
    }
#endif
}


#if defined(USEWIFI)

#ifdef ARDUINONANO_RP2040
UInt WebServerGetURI();
bool WebServerMethodIsGET();
UInt WebServerGetArguments();
#endif

void pushHandlerArguments()
{
  // string uri, string method, <string,string> arguments

#ifdef RP2040PICOW
  UInt hruri = 0;
  HRString_FromString(hruri, server.uri());
#endif
#ifdef ARDUINONANO_RP2040
   UInt hruri = WebServerGetURI();
#endif
  
  UInt hrmethod = HRString_New();
  bool isGet = true;
#ifdef RP2040PICOW
  isGet = (server.method() == HTTP_GET);
#endif
#ifdef ARDUINONANO_RP2040
  isGet = WebServerMethodIsGET();
#endif
  if (isGet) 
  {
      HRString_BuildChar_R(hrmethod, (Char)'G');
      HRString_BuildChar_R(hrmethod, (Char)'E');
      HRString_BuildChar_R(hrmethod, (Char)'T');
  }
  else
  {
      HRString_BuildChar_R(hrmethod, (Char)'P');
      HRString_BuildChar_R(hrmethod, (Char)'O');
      HRString_BuildChar_R(hrmethod, (Char)'S');
      HRString_BuildChar_R(hrmethod, (Char)'T');
  }
#ifdef RP2040PICOW
  UInt hrdict = HRDictionary_New(Type::eString, Type::eString);
  for (int a = 0; a < server.args(); a++) 
  {
      UInt hrname  = 0;
      UInt hrvalue = 0;
      HRString_FromString(hrname, server.argName(a));
      HRString_FromString(hrvalue, server.arg(a));
      HRDictionary_Set(hrdict, hrname, Type::eString, hrvalue, Type::eString);
      GC_Release(hrname);
      GC_Release(hrvalue);
  }
#endif
#ifdef ARDUINONANO_RP2040
  UInt hrdict = WebServerGetArguments();
#endif
  HopperVM_Push(hruri, Type::eString);
  HopperVM_Push(hrmethod, Type::eString);
  HopperVM_Push(hrdict, Type::eDictionary);
}

void handleNotFound()
{
#ifdef DIAGNOSTICS      
    Serial.println("<handleNotFound");
#endif

    // push 0x0000 onto the return address stack
    HopperVM_PushCS(0x0000);

    // push arguments onto stack:
    pushHandlerArguments();

    UInt pcBefore = HopperVM_pc;
    HopperVM_pc = notFoundDelegatePC;
    bool cleanExit = HopperVM_InlinedExecuteWarp(false);
    if (cleanExit)
    {
        HopperVM_pc = pcBefore;
    }
#ifdef DIAGNOSTICS      
    if (!cleanExit)
    {
        Serial.println("not clean exit");
    }
    Serial.println("handleNotFound>");
#endif
}
void handleRequest()
{
#ifdef DIAGNOSTICS      
    Serial.println("<handleRequest");
#endif

    // push 0x0000 onto the return address stack
    HopperVM_PushCS(0x0000);

    // push arguments onto stack:
    pushHandlerArguments();

    UInt uriStr;
#ifdef RP2040PICOW
    HRString_FromString(uriStr, server.uri());
#endif
#ifdef ARDUINONANO_RP2040
    if (0 != uriStr)
    {
        GC_Release(uriStr);
    }
    uriStr = WebServerGetURI();
#endif

    UInt delegatePC = 0;
    if ((0 != handlerDelegatePCs) && HRDictionary_Contains(handlerDelegatePCs, uriStr))
    {
        Type vtype;
        delegatePC = HRDictionary_Get_R(handlerDelegatePCs, uriStr, vtype);
    }
    if (0 != delegatePC)
    {
        UInt pcBefore = HopperVM_pc;
        HopperVM_pc = delegatePC;
        bool cleanExit = HopperVM_InlinedExecuteWarp(false);
        if (cleanExit)
        {
            HopperVM_pc = pcBefore;
        }
#ifdef DIAGNOSTICS     
        if (!cleanExit)
        {
            Serial.println("not clean exit");
        } 
#endif
    }
#ifdef DIAGNOSTICS         
    else
    {
        Serial.println("delegatePC == 0");
        String str = "";
        HRString_ToString(uriStr, str);
        Serial.println("uriStr == " + str);
    }
    Serial.println("handleRequest>");
#endif
    GC_Release(uriStr);
}

#endif


#ifdef ARDUINONANO_RP2040

void WebServerEvents()
{
    WiFiClient client = server.available();
    if (client)
    {
#ifdef DIAGNOSTICS
        //Serial.println("Request:");
#endif
        String currentLine = ""; 
        String request = "";
        while (client.connected())
        {
            if (client.available()) 
            {   
                char c = client.read(); 
                if (c == '\n')
                {
                    // newline is '\n'
                    if (currentLine.length() == 0) 
                    {
                        // 2x newline in a row means end of client HTTP request
                        
                        // first line of the header is the actual request:
                        // "GET /bob.htm?arg=value HTTP/1.1"
                        int i = request.indexOf(" HTTP/");
                        request = request.substring(0, i);
                        HRString_FromString(currentRequestURI, request);
                        if (currentRequestArguments == 0)
                        {
                            currentRequestArguments = HRString_New();
                        }
                        else
                        {
                            HRString_BuildClear_R(currentRequestArguments);
                        }
                        i = request.indexOf('?');
                        if (i != -1)
                        {
                            HRString_FromString(currentRequestArguments, request.substring(i+1));
                            HRString_FromString(currentRequestURI, request.substring(0, i));
                            if (HRString_GetLength(currentRequestArguments) > 0)
                            {
                                HRString_BuildChar_R(currentRequestArguments, '&');
                            }
                        }

                        if ((0 != handlerDelegatePCs) && HRDictionary_Contains(handlerDelegatePCs, currentRequestURI))
                        {
                            handleRequest();
                        }
                        else
                        {
                            handleNotFound();
                        }

                        String responseTypeStr = "";
                        HRString_ToString(responseType, responseTypeStr);
                        String responseStr = "";
                        HRString_ToString(responseContent, responseStr);

                        client.print("HTTP/1.1 "); client.print(responseCode); client.println(" OK");
                        client.println("Content-type:" + responseTypeStr);
                        client.println();

                        // the content of the HTTP response follows the header:
                        client.print(responseStr);
                        
                        // The HTTP response ends with another blank line:
                        client.println();

                        GC_Release(responseType);
                        responseType = 0;
                        GC_Release(responseContent);
                        responseContent = 0;

                        // close the connection after we return from the handler
                        client.stop();

                        break;
                    }
                    else 
                    {    
                        // if you got a newline, process, then clear currentLine:
                        if (currentLine.startsWith("GET "))
                        {
                            request = currentLine.substring(4);
                            currentRequestIsGet = true;
                        }
                        else if (currentLine.startsWith("POST "))
                        {
                            request = currentLine.substring(5);
                            currentRequestIsGet = false;
                        }
#ifdef DIAGNOSTICS
                        //Serial.println(currentLine);
#endif
                        currentLine = "";
                    }
                }
                else if (c != '\r') 
                {  // if you got anything else but a carriage return character,
                    currentLine += c;      // add it to the end of the currentLine
                }
            }          
        }
    }
}
void WebServerSend(UInt httpCode, UInt hrcontentType, UInt hrcontent)
{
    responseCode = httpCode;
    if (0 != responseContent)
    {
        HRString_BuildClear_R(responseContent);
    }
    else
    {
        responseContent = HRString_New();
    }
    if (0 != responseType)
    {
        HRString_BuildClear_R(responseType);
    }
    else
    {
        responseType = HRString_New();
    }
    HRString_BuildString_R(responseType, hrcontentType);
    HRString_BuildString_R(responseContent, hrcontent);
}
UInt WebServerGetURI()
{
#ifdef DIAGNOSTICS
    String str = "";
    HRString_ToString(currentRequestURI, str);
    Serial.println("WebServerGetURI:" + str);
#endif
    UInt result = HRString_New();
    HRString_BuildString_R(result, currentRequestURI);
    return result;
}
UInt WebServerGetArguments()
{
    UInt hrdict = HRDictionary_New(Type::eString, Type::eString);
    int iStart = 0;
    for (int iCurrent=0; iCurrent < HRString_GetLength(currentRequestArguments); iCurrent++)
    {
        Char ch = HRString_GetChar(currentRequestArguments, iCurrent);
        if (ch == '&')
        {
            int iEnd = iCurrent-1;
            if (iEnd - iStart > 0)
            {
                UInt hrarg = HRString_New();  
                UInt hrval = HRString_New(); 
                for (;;)
                {
                    ch = HRString_GetChar(currentRequestArguments, iStart);
                    if (ch == '=') { break; }
                    HRString_BuildChar_R(hrarg, ch);
                    iStart++;
                    if (iStart == iCurrent) { break; }
                }
                if (ch == '=')
                {
                    iStart++;
                    for (;;)
                    {
                        if (iStart == iCurrent) { break; }
                        ch = HRString_GetChar(currentRequestArguments, iStart);
                        HRString_BuildChar_R(hrval, ch);
                        iStart++;
                    }
                }
                HRDictionary_Set(hrdict, hrarg, Type::eString, hrval, Type::eString);
                GC_Release(hrarg);
                GC_Release(hrval);
            }
            iStart = iCurrent+1;
        }
    }
    return hrdict;
}

bool WebServerMethodIsGET()
{
    return currentRequestIsGet;
}
#endif

void External_WebServerClose()
{
#if defined(USEWIFI)
    if (wifiConnected)
    {
#ifdef RP2040PICOW      
        server.close();
#endif
    }
#endif    
}
void External_WebServerEvents()
{
#if defined(USEWIFI)
    if (wifiConnected)
    {
#ifdef RP2040PICOW      
        server.handleClient();
#endif
#ifdef ARDUINONANO_RP2040
        WebServerEvents();
#endif
    }
#endif
}

void External_WebServerSend(UInt httpCode, UInt hrcontentType, UInt hrcontent)
{
#ifdef DIAGNOSTICS      
    Serial.println("<WebServerSend");
#endif
#if defined(USEWIFI)
    if (wifiConnected)
    {
#ifdef DIAGNOSTICS      
        Serial.print("httpCode=");
        Serial.println(httpCode);
#endif
#ifdef RP2040PICOW      
        String contentType;
        String content;
        UInt length = HRString_GetLength(hrcontentType);
        for (UInt i = 0; i < length; i++)
        {
            contentType += (char)HRString_GetChar(hrcontentType, i);
        }
        length = HRString_GetLength(hrcontent);
        for (UInt i = 0; i < length; i++)
        {
            content += (char)HRString_GetChar(hrcontent, i);
        }
        server.send(httpCode, contentType, content);
#endif
#ifdef ARDUINONANO_RP2040
        WebServerSend(httpCode, hrcontentType, hrcontent);
#endif
    }
#endif
#ifdef DIAGNOSTICS      
    Serial.println("WebServerSend>");
#endif

}



void External_WebServerOn(UInt uri, UInt handlerMethodIndex)
{
#if defined(USEWIFI)
    if (wifiConnected)
    {
        UInt delegatePC = HopperVM_LookupMethod(handlerMethodIndex);
        if (0 == handlerDelegatePCs)
        {
            handlerDelegatePCs = HRDictionary_New(Type::eString, Type::eUInt);
        }
        HRDictionary_Set(handlerDelegatePCs, uri, Type::eString, delegatePC, Type::eUInt);
#if defined(RP2040PICOW) || defined(DIAGNOSTICS)
        String uriStr = "";
        HRString_ToString(uri, uriStr);
#endif
#ifdef RP2040PICOW
        server.on(uriStr, handleRequest);
#endif
#ifdef DIAGNOSTICS      
        Serial.print("on: '" + uriStr + "' ");
        Serial.println(delegatePC, HEX);
#endif
    }
#endif
}

void External_WebServerOnNotFound(UInt handlerMethodIndex)
{
#if defined(USEWIFI)
    if (wifiConnected)
    {
        notFoundDelegatePC = HopperVM_LookupMethod(handlerMethodIndex);
#ifdef RP2040PICOW
        server.onNotFound(handleNotFound);
#endif
#ifdef DIAGNOSTICS      
        Serial.print("onNotFound: ");
        Serial.println(notFoundDelegatePC, HEX);
#endif
    }
#endif
}

