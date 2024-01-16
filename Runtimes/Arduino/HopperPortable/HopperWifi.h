#ifndef HOPPERWIFI_H
#define HOPPERWIFI_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"
#include "Inlined.h"


Bool External_WebClientGetRequest_R(UInt hrurl, UInt& hrcontent);
Bool External_WiFiConnect(UInt hrssid, UInt hrpassword);
UInt External_WiFiIP();
void External_WebServerBegin(UInt port);
void External_WebServerClose();
void External_WebServerEvents();
void External_WebServerSend(UInt httpCode, UInt contentType, UInt content);
void External_WebServerOn(UInt uri, UInt handler);
void External_WebServerOnNotFound(UInt handler);




#endif // HOPPERWIFI_H