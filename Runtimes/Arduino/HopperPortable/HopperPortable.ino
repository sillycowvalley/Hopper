#include "Platform.h"

void setup() 
{
  Serial.begin(57600); // always stick to 56K (it also works on 6502 and Z80 for the client side)
  delay(2000);

  Machine_Initialize();
  Platform_Initialize();


#ifndef RP2040PICO
  // Flashing LED_BUILTIN causes grief when a Pi Pico device uses pin 25 for something else (like the backlight for the LCD for Waveshare RP2040-LCD-0.96)

#if !defined(SEEEDESP32C3) && !defined(WAVESHARERP2040ONE)
  // flicker LED_BUILTIN to show that initialization completed
  pinMode(LED_BUILTIN, OUTPUT);
  for (int i = 0; i < 5; i++)
  {
    digitalWrite(LED_BUILTIN, HIGH);
    delay(50);
    digitalWrite(LED_BUILTIN, LOW);
    delay(50);
  }
#endif  
#ifdef LOLIND1MINI
  digitalWrite(LED_BUILTIN, HIGH); // high is "off" on the D1 Mini
#endif  

#endif

}

void loop() 
{
  if (!Machine_GetExited())
  {
    HopperEntryPoint();
    Platform_Release();
    Machine_SetExited(true);
  }
}

