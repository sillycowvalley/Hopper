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
    
#else

  #ifdef RP2040
    const byte builtInLED = 32; // GP25 is pin 2 on Pi Pico W
  #else
    A specific MCU board must be defined to use the built-in LED: RP2040, WEMOSD1MINI, etc.
  #endif
    
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
