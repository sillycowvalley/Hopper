unit GPIO
{
    

#ifdef WEMOSD1MINI

    const byte builtInLED = 2;
    
    // #define WEMOS_D1_MINI_D0 16
    // #define WEMOS_D1_MINI_D1 5
    // #define WEMOS_D1_MINI_D2 4
    // #define WEMOS_D1_MINI_D3 0
    // #define WEMOS_D1_MINI_D4 2   // built in LED
    // #define WEMOS_D1_MINI_D5 14
    // #define WEMOS_D1_MINI_D6 12
    // #define WEMOS_D1_MINI_D7 13
    // #define WEMOS_D1_MINI_D8 15
    
#endif

#ifdef RP2040
    const byte builtInLED = 32;
#endif
#ifdef ARDUINONANORP2040
    const byte builtInLED = 6;
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
    bool LED 
    { 
        set 
        { 
            MCU.PinMode(builtInLED, MCU.PinModeOption.Output);
#ifdef WEMOSD1MINI
            MCU.DigitalWrite(builtInLED, value ? false : true); // false = ON?!
#else            
            MCU.DigitalWrite(builtInLED, value); 
#endif
        } 
    }
}
