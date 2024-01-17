#ifndef HOPPERCONFIGURATION_H
#define HOPPERCONFIGURATION_H

#include <Arduino.h>

const bool loadAuto = false; // set this to false if you are booting into a bad flashed Hopper program

//#define DIAGNOSTICS // turn on more Serial.print output
//#define CHECKED

// Raspberry Pi Pico W
// - LittleFS for built-in Flash
// - WiFi
//#define RP2040PICOW

// Raspberry Pi Pico
// - LittleFS for built-in Flash
// - no WiFi
#define RP2040PICO

// Challenger RP2040 WiFi
// - LittleFS for built-in Flash
// - WiFiNina
//#define CHALLENGER_RP2040_WIFI

// Arduino Connect RP2040
// - LittleFS for built-in Flash
// - WiFiNina
//#define ARDUINONANO_RP2040

// Seeed XIA0 RP2040
// - LittleFS for built-in Flash
// - no WiFi
//#define RP2040XIAO

// Pimoroni Tiny 2040
// - LittleFS for built-in Flash
// - no WiFi
//#define TINY2040

// Waveshare RP2040 One
// - LittleFS for built-in Flash
// - no WiFi
//#define WAVESHARERP2040ONE


// Lolin Wemos D1 Mini, ESP 8266
// - LittleFS for built-in Flash
// - no ESP 8266 WiFi
//#define LOLIND1MINI

// Work in progress:
//#define ARDUINONANOESP32
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


#if defined(RP2040PICO) || defined(RP2040PICOW) || defined(ARDUINONANO_RP2040) || defined(RP2040XIAO) || defined(TINY2040) || defined(WAVESHARERP2040ONE) || defined(CHALLENGER_RP2040_WIFI)
#define RP2040
#endif


#endif // HOPPERCONFIGURATION_H