#ifndef HOPPERWIFI_H
#define HOPPERWIFI_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

Bool External_HttpClientGetRequest_R(UInt hrurl, UInt& hrcontent);
Bool External_WiFiConnect(UInt hrssid, UInt hrpassword);


#endif // HOPPERWIFI_H