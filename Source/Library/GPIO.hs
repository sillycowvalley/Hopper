unit GPIO
{
   
#if defined(BOARD_HAS_RGBLED)
    bool ledStateR;
    bool ledStateG;
    bool ledStateB;
    bool LEDR
    { 
        set 
        { 
            ledStateR = value;
            MCU.PinMode(Board.BuiltInLEDR, MCU.PinModeOption.Output);
#if defined(PIMORONI_TINY2040) || defined(PIMORONI_TINY2350)
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(Board.BuiltInLEDR, value); 
            
        } 
        get
        {
            return ledStateR;
        }
    }
    bool LEDG
    { 
        set 
        { 
            ledStateG = value;
            MCU.PinMode(Board.BuiltInLEDG, MCU.PinModeOption.Output);
#if defined(PIMORONI_TINY2040) || defined(PIMORONI_TINY2350)
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(Board.BuiltInLEDG, value); 
        } 
        get
        {
            return ledStateG;
        }
    }
    bool LEDB
    { 
        set 
        { 
            ledStateB = value;
            MCU.PinMode(Board.BuiltInLEDB, MCU.PinModeOption.Output);
#if defined(PIMORONI_TINY2040) || defined(PIMORONI_TINY2350)
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(Board.BuiltInLEDB, value); 
        } 
        get
        {
            return ledStateB;
        }
    }
#endif

#if defined(BOARD_HAS_A0)
    uint A0 { get { return MCU.AnalogRead(Board.BuiltInA0); } }
#endif
#if defined(BOARD_HAS_A1)
    uint A1 { get { return MCU.AnalogRead(Board.BuiltInA1); } }
#endif
#if defined(BOARD_HAS_A2)
    uint A2 { get { return MCU.AnalogRead(Board.BuiltInA2); } }
#endif
#if defined(BOARD_HAS_A3)
    uint A3 { get { return MCU.AnalogRead(Board.BuiltInA3); } }
#endif 
#if defined(BOARD_HAS_A4)
    uint A4 { get { return MCU.AnalogRead(Board.BuiltInA4); } }
#endif
#if defined(BOARD_HAS_A5)
    uint A5 { get { return MCU.AnalogRead(Board.BuiltInA5); } }
#endif   

#if defined(BOARD_HAS_LED)
    bool ledState;
    bool LED 
    { 
        set 
        { 
            ledState = value;
            MCU.PinMode(Board.BuiltInLED, MCU.PinModeOption.Output);
#if defined(PIMORONI_TINY2040) || defined(PIMORONI_TINY2350)
            value = !value; // false = ON?!
#endif
            MCU.DigitalWrite(Board.BuiltInLED, value); 
            
        } 
        get
        {
            return ledState;
        }
    }
#endif
}


