program Blink
{

#define RP2040
//#define SEEEDRP2040
//#define ARDUINONANORP2040
//#define ARDUINONANOESP32
//#define WEMOSD1MINI
    
    uses "/Source/System/MCU"
    
    {
        loop
        {
            LED = true;
            Delay(500);
            Write('+');
            LED = false;    
            Delay(500);
            Write('-');
        }
    }
}
