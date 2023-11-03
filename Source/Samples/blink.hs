program Blink
{

#define RP2040
//#define WEMOSD1MINI
    
    uses "/Source/System/MCU"
    
    {
        loop
        {
            LED = true;
            Delay(500);
            LED = false;    
            Delay(500);
        }
    }
}
