#ifndef HOPPERCONFIGURATION_H
#define HOPPERCONFIGURATION_H

#include <Arduino.h>

const bool loadAuto = true; // set this to false if you are booting into a bad flashed Hopper program

//#define DIAGNOSTICS // turn on more Serial.print output
//#define CHECKED

// Raspberry Pi Pico W
// - LittleFS for built-in Flash
// - WiFi
//
#if defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define RP2040PICOW
#define USESDFS
#endif

// Raspberry Pi Pico
// - LittleFS for built-in Flash
// - no WiFi
//
#if defined(ARDUINO_RASPBERRY_PI_PICO)
#define RP2040PICO
#define USESDFS
#endif

// Raspberry Pi Pico 2
// - LittleFS for built-in Flash
// - no WiFi
//
#if defined(ARDUINO_RASPBERRY_PI_PICO_2)
#define RP2040PICO2
#define USESDFS
#endif

// Challenger RP2040 WiFi
// - LittleFS for built-in Flash
// - WiFiEspAT
//
#if defined(ARDUINO_CHALLENGER_2040_WIFI_RP2040)
#define CHALLENGER_RP2040_WIFI
#define USELITTLEFS
#endif

// Arduino Connect RP2040
// - LittleFS for built-in Flash
// - WiFiNina
#if defined(ARDUINO_NANO_RP2040_CONNECT)
#define ARDUINONANO_RP2040
#define USETIMERSTUBS
#endif

#if defined(ARDUINO_UNOR4_WIFI)
#define USETIMERSTUBS
#endif

#if defined(ARDUINO_TEENSY41)
#define TEENSY
#define USETIMERSTUBS
#define USELITTLEFS
//#define USESDFS // ?
#endif

// Lolin Wemos D1 Mini, ESP 8266
// - LittleFS for built-in Flash
// - no ESP 8266 WiFi
//#define LOLIND1MINI

// Work in progress:
//#define SEEEDESP32C3
//#define LOLIN_C3_MINI
//#define LOLIN_S2_PICO

//Lolin S2 & C3 boards:
// To put S2 and C3 boards into Device Firmware Upgrade (DFU) mode:
// - Hold on Button 0|9
// - Press Button Reset
// - Release Button 0|9 When you hear the prompt tone on usb reconnection
//
// The steps to then get the C3 Mini working in Hopper are currently nuts:
// - after uploading, press the reset button on the Mini
// - in Hopper, run TERM to drain the diagnostic info from serial
// - exit TERM then run HM!

// Just use 'ARDUINO_ARCH_RP2040' ?
#if defined(RP2040PICO) || defined(RP2040PICO2) || defined(RP2040PICOW) || defined(ARDUINONANO_RP2040) || defined(CHALLENGER_RP2040_WIFI)
#define RP2040
#define SPI_INCLUDED
#define USETIMER
#define USELITTLEFS
#endif

// Seems to be "ARDUINO_" + BOARD_NAME when using Pico board manager
// ARDUINO_NANO_RP2040_CONNECT 
//
// ARDUINO_ARCH_RP2040
// ARDUINO_RASPBERRY_PI_PICO_W
// ARDUINO_RASPBERRY_PI_PICO
// ARDUINO_CHALLENGER_2040_UWB_RP2040 
// ARDUINO_CHALLENGER_2040_WIFI_RP2040
// ARDUINO_CHALLENGER_2040_LTE_RP2040 
// ARDUINO_CHALLENGER_2040_SUBGHZ_RP2040
// ARDUINO_ADAFRUIT_QTPY_RP2040 
// ARDUINO_ADAFRUIT_METRO_RP2040
// ARDUINO_ADAFRUIT_KB2040_RP2040
// ARDUINO_ADAFRUIT_FEATHER_RP2040
// ARDUINO_ADAFRUIT_ITSYBITSY_RP2040
// etc.



#endif // HOPPERCONFIGURATION_H