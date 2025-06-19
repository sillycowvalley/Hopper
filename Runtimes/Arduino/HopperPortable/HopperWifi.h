#ifndef HOPPERWIFI_H
#define HOPPERWIFI_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

#if defined(USESWIFICYW43) || defined(ARDUINONANO_RP2040) || defined(USESWIFIESP)
#define USEWIFI       // WiFi for Raspberry Pi Pico W
#define USESERVER
#endif

#ifndef USEWIFI
#define USEWIFISTUBS    // no WiFi for Pi Pico, Seeed XIA0 RP2040 or Pimoroni Tiny 2040
#define USERSERVERSTUBS
#endif

#if defined(USESWIFICYW43)
#include <WiFi.h>
#include <WiFiClient.h>
#include <WebServer.h>
#endif

#ifdef ARDUINONANO_RP2040
#include <SPI.h>
#include <WiFiNINA.h>
#endif

#ifdef USESWIFIESP
#include <ChallengerWiFi.h>
#include <WiFiEspAT.h>
#endif


#ifdef USEWIFI
Bool IsWiFiConnected();
#endif

#endif // HOPPERWIFI_H