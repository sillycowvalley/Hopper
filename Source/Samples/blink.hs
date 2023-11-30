program Blink
{

//#define RP2040
#define TINY2040
//#define SEEEDRP2040
//#define ARDUINONANORP2040
//#define ARDUINONANOESP32
//#define WEMOSD1MINI
//#define WAVESHARERP2040ONE
    
    uses "/Source/System/MCU"
    
    {
        loop
        {
            LED = true;
            Write('+');
            Delay(2000);
            LED = false;    
            Write('-');
            Delay(2000);
        }
    }
}
