#include "HopperWifi.h"

#ifdef USERSERVERSTUBS

void External_WebServerRelease()                { }
void External_WebServerOn(UInt uri, UInt handlerMethodIndex) {}
void External_WebServerOnNotFound(UInt handlerMethodIndex) { }
void External_WebServerBegin(UInt port) { }
void External_WebServerEvents()         { }
void External_WebServerClose()          { }
void External_WebServerSend(UInt httpCode, UInt hrheaderContent, UInt hrcontent) {}

#endif

#ifdef USESERVER
bool currentRequestIsGet;
UInt notFoundDelegatePC = 0;
UInt responseCode = 0;

UInt handlerDelegatePCs = 0;
UInt currentRequestURI = 0;
UInt currentRequestArguments = 0;
UInt responseContent = 0;
UInt headerContent = 0;

bool WebServerMethodIsGET();
UInt WebServerGetURI();
UInt WebServerGetArguments();

WiFiServer server(80);

void External_WebServerRelease()
{
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
    if (0 != headerContent)
    {
        GC_Release(headerContent);
        headerContent = 0;
    }
}

void pushHandlerArguments(bool isGet)
{
    // string uri, string method, <string,string> arguments
    UInt hruri = WebServerGetURI();
    UInt hrmethod = HRString_New();
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
    UInt hrdict = WebServerGetArguments();
    
    HopperVM_Push(hruri, Type::eString);
    HopperVM_Push(hrmethod, Type::eString);
    HopperVM_Push(hrdict, Type::eDictionary);
}


void handleNotFound()
{
    // push 0x0000 onto the return address stack
    HopperVM_PushCS(0x0000);

    // push arguments onto stack:
    pushHandlerArguments(WebServerMethodIsGET());

    responseCode = 404;
    if (0 != notFoundDelegatePC)
    {
        UInt pcBefore = HopperVM_pc;
        HopperVM_pc = notFoundDelegatePC;
        bool cleanExit = HopperVM_InlinedExecuteWarp(false);
        if (cleanExit)
        {
            HopperVM_pc = pcBefore;
        }
    }
    if (0 == responseContent)
    {
        HRString_FromString(responseContent, "404: Not Found");
    }
}

void handleRequest()
{
    // push 0x0000 onto the return address stack
    HopperVM_PushCS(0x0000);

    // push arguments onto stack:
    pushHandlerArguments(WebServerMethodIsGET());

    UInt uriStr = WebServerGetURI();
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
    }
    GC_Release(uriStr);
}

UInt WebServerGetURI()
{
    return HRString_Clone(currentRequestURI);
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
bool WebServerMethodIsGET() { return currentRequestIsGet; }

void External_WebServerEvents()
{
    if (!IsWiFiConnected()) { return; }

#ifdef RP2040PICOW
    WiFiClient client = server.accept(); // PLATFORM
#else    
    WiFiClient client = server.available(); // PLATFORM
#endif
    if (client)
    {
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

                        String responseStr = "";
                        if (0 != responseContent)
                        {
                            HRString_ToString(responseContent, responseStr);
                            GC_Release(responseContent);
                            responseContent = 0;
                        }

                        // PLATFORM:
                        client.print("HTTP/1.1 "); client.print(responseCode); client.println(" OK");
                        if (0 != headerContent)
                        {
                            UInt iterator = 0;
                            Type ktype = (Type)0;
                            UInt key = 0;
                            Type vtype = (Type)0;
                            UInt value = 0;
                            while (HRDictionary_next_R(headerContent, iterator, ktype, key, vtype, value))
                            {
                                String headerName = "";
                                String headerValue = "";
                                HRString_ToString(key, headerName);
                                HRString_ToString(value, headerValue);
                                client.println(headerName + ": " + headerValue);
                            }
                            GC_Release(headerContent);
                            headerContent = 0;
                        }
                        client.println();
                        // the content of the HTTP response follows the header:
                        client.print(responseStr);
                        // The HTTP response ends with another blank line:
                        client.println();

#if defined(USESWIFIESPAT) || defined(RP2040PICOW)
                        client.flush(); // PLATFORM
#endif
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

void External_WebServerOnNotFound(UInt handlerMethodIndex)
{
    if (IsWiFiConnected())
    {
        notFoundDelegatePC = HopperVM_LookupMethod(handlerMethodIndex);
    }
}

void External_WebServerOn(UInt uri, UInt handlerMethodIndex)
{
    if (!IsWiFiConnected()) { return; }

    UInt delegatePC = HopperVM_LookupMethod(handlerMethodIndex);
    if (0 == handlerDelegatePCs)
    {
        handlerDelegatePCs = HRDictionary_New(Type::eString, Type::eUInt);
    }
    HRDictionary_Set(handlerDelegatePCs, uri, Type::eString, delegatePC, Type::eUInt);
}


void External_WebServerSend(UInt httpCode, UInt hrheaderContent, UInt hrcontent)
{
    if (!IsWiFiConnected()) { return; }
    responseCode = httpCode;
    if (0 != responseContent)
    {
        HRString_BuildClear_R(responseContent);
    }
    else
    {
        responseContent = HRString_New();
    }
    HRString_BuildString_R(responseContent, hrcontent);
    if (0 != headerContent)
    {
        GC_Release(headerContent);
        headerContent = 0;
    }
    headerContent = HRDictionary_Clone(hrheaderContent);
}

void External_WebServerBegin(UInt port) { if (IsWiFiConnected()) { server.begin(); } }

#ifdef ARDUINONANO_RP2040
void External_WebServerClose() {}
#endif // ARDUINONANO_RP2040


#if defined(USESWIFIESPAT) || defined(RP2040PICOW)
void External_WebServerClose() { if (IsWiFiConnected()) { server.end(); } } // end() is the same as close() for RP2040PICOW
#endif

#endif // USEWIFI