unit GPIO
{
    

#ifdef WEMOS_D1_MINI

    const byte d0 = 16;
    const byte d1 = 5;
    const byte d2 = 4;
    const byte d3 = 0;
    const byte d4 = 2; // built in LED
    const byte d5 = 14;
    const byte d6 = 12;
    const byte d7 = 13;
    const byte d8 = 15;
    
    //const byte a0 = 3?; // test?
    
    const byte builtInLED = d4;    
#endif

#if defined(RP2040_PICO) || defined(SPARKFUN_THING_PLUS_RP2040) || defined(SEEED_XIAO_RP2040) || defined(WAVESHARE_RP2040_MATRIX) || defined(WAVESHARE_RP2040_MATRIX)
    // Raspberry Pi Pico
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    const byte a2         = 28; // A2
    const byte a3         = 29; // VSYS/3 on Pi Pico
    
    const byte builtInLED = 25; // different to the W
    
    const byte DefaultI2CController = 0;
    const byte DefaultI2CSDAPin     = 4;
    const byte DefaultI2CSCLPin     = 5;
    
#endif

#ifdef RP2040_PICOW
    // Raspberry Pi Pico
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    const byte a2         = 28; // A2
    const byte a3         = 29; // VSYS/3 on Pi Pico
    
    const byte builtInLED = 32;
    
    const byte DefaultI2CController = 0;
    const byte DefaultI2CSDAPin     = 4;
    const byte DefaultI2CSCLPin     = 5;
    
#endif

#if defined(ADAFRUIT_FEATHER_RP2040) || defined(ADAFRUIT_METRO_RP2040)
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    const byte a2         = 28; // A2
    const byte a3         = 29; // VSYS/3 on Pi Pico
    
    const byte builtInLED = 13; // like the Uno ..
#endif

#ifdef ADAFRUIT_QTPY
    // AdaFruit QT Py
    const byte a0         = 29; // A0
    const byte a1         = 28; // A1
    const byte a2         = 27; // A2
    const byte a3         = 26; // A3
#endif

#ifdef PIMORONI_TINY2040
    // Pimoroni Tiny 2040 
    const byte a0          = 26; // A0
    const byte a1          = 27; // A1
    const byte a2          = 28; // A2
    const byte a3          = 29; // A3
    
    const byte builtInLED  = 19;
    
    const byte builtInLEDR = 18;
    const byte builtInLEDG = 19;
    const byte builtInLEDB = 20;
    
    const byte DefaultI2CController = 1;
    const byte DefaultI2CSDAPin     = 26;
    const byte DefaultI2CSCLPin     = 27;
    
#endif

#ifdef ARDUINO_NANO_RP2040
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    
    const byte builtInLED = 6;
#endif
#ifdef WAVESHARE_RP2040_ONE
    const byte builtInLED = 16; // GP16 WS2812 RGB LED??
#endif
#ifdef SEEED_RP2040
    const byte builtInLED  = 17;
    const byte builtInLEDR = 17;
    const byte builtInLEDG = 16;
    const byte builtInLEDB = 25;
#endif

#ifdef ARDUINO_NANO_ESP32
    const byte builtInLED = 13;
#endif

#ifdef SEEED_RP2040
    bool LEDR
    { 
        set 
        { 
            MCU.PinMode(builtInLEDR, MCU.PinModeOption.Output);
            MCU.DigitalWrite(builtInLEDR, value); 
        } 
    }
    bool LEDG
    { 
        set 
        { 
            MCU.PinMode(builtInLEDG, MCU.PinModeOption.Output);
            MCU.DigitalWrite(builtInLEDG, value); 
        } 
    }
    bool LEDB
    { 
        set 
        { 
            MCU.PinMode(builtInLEDB, MCU.PinModeOption.Output);
            MCU.DigitalWrite(builtInLEDB, value); 
        } 
    }
#endif

    uint A0 { get { return MCU.AnalogRead(a0); } }
    uint A1 { get { return MCU.AnalogRead(a1); } }
#ifndef ARDUINO_NANO_RP2040    
    uint A2 { get { return MCU.AnalogRead(a2); } }
    uint A3 { get { return MCU.AnalogRead(a3); } }
#endif
    
    bool LED 
    { 
        set 
        { 
            MCU.PinMode(builtInLED, MCU.PinModeOption.Output);
#ifdef WEMOS_D1_MINI
            value = !value; // false = ON?!
#endif
#ifdef PIMORONI_TINY2040
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(builtInLED, value); 
        } 
    }
}
