unit GPIO
{
    

#ifdef WEMOSD1MINI

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

#ifdef RP2040PICO
    // Raspberry Pi Pico
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    const byte a2         = 28; // A2
    const byte a3         = 29; // VSYS/3 on Pi Pico
    
    const byte builtInLED = 25; // different to the W
    
#endif

#ifdef RP2040PICOW
    // Raspberry Pi Pico
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    const byte a2         = 28; // A2
    const byte a3         = 29; // VSYS/3 on Pi Pico
    
    const byte builtInLED = 32;
    
#endif

#ifdef ADAFRUITQTPY
    // AdaFruit QT Py
    const byte a0         = 29; // A0
    const byte a1         = 28; // A1
    const byte a2         = 27; // A2
    const byte a3         = 26; // A3
#endif

#ifdef TINY2040
    // Pimoroni Tiny 2040 
    const byte a0          = 26; // A0
    const byte a1          = 27; // A1
    const byte a2          = 28; // A2
    const byte a3          = 29; // A3
    
    const byte builtInLED  = 19;
    
    const byte builtInLEDR = 18;
    const byte builtInLEDG = 19;
    const byte builtInLEDB = 20;
    
#endif

#ifdef ARDUINONANORP2040
    const byte a0         = 26; // A0
    const byte a1         = 27; // A1
    
    const byte builtInLED = 6;
#endif
#ifdef WAVESHARERP2040ONE
    const byte builtInLED = 16; // GP16 WS2812 RGB LED??
#endif
#ifdef SEEEDRP2040
    const byte builtInLED  = 17;
    const byte builtInLEDR = 17;
    const byte builtInLEDG = 16;
    const byte builtInLEDB = 25;
#endif

#ifdef ARDUINONANOESP32
    const byte builtInLED = 13;
#endif

#ifdef SEEEDRP2040
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
#ifndef ARDUINONANORP2040    
    uint A2 { get { return MCU.AnalogRead(a2); } }
    uint A3 { get { return MCU.AnalogRead(a3); } }
#endif
    
    bool LED 
    { 
        set 
        { 
            MCU.PinMode(builtInLED, MCU.PinModeOption.Output);
#ifdef WEMOSD1MINI
            value = !value; // false = ON?!
#endif
#ifdef TINY2040
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(builtInLED, value); 
        } 
    }
}
