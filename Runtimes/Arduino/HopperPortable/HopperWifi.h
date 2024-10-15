#ifndef HOPPERWIFI_H
#define HOPPERWIFI_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

#if defined(RP2040PICOW) || defined(ARDUINONANO_RP2040) || defined(USESWIFIESPAT) || defined(RP2350PICO2W)
#define USEWIFI       // WiFi for Raspberry Pi Pico W
#define USESERVER
#endif

#ifndef USEWIFI
#define USEWIFISTUBS    // no WiFi for Pi Pico, Seeed XIA0 RP2040 or Pimoroni Tiny 2040
#define USERSERVERSTUBS
#endif

#if defined(RP2040PICOW) || defined(RP2350PICO2W)
#include <WiFi.h>
#include <WiFiClient.h> // added for RP2040W
#include <WebServer.h>  // added for RP2040W
#endif

#ifdef ARDUINONANO_RP2040
#include <SPI.h>
#include <WiFiNINA.h>
#endif

#ifdef USESWIFIESPAT
#include <ChallengerWiFi.h>
#include <WiFiEspAT.h>
#endif


#ifdef USEWIFI
Bool IsWiFiConnected();
#endif

#endif // HOPPERWIFI_H