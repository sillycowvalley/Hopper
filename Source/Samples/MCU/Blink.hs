program Blink
{
//#define TINY_HOPPER
//#define RP2040_PICO
#define RP2040_PICOW
//#define PIMORONI_TINY2040
//#define SEEED_RP2040
//#define ARDUINO_NANO_RP2040
//#define ARDUINO_NANO_ESP32
//#define WEMOS_D1_MINI
//#define WAVESHARE_RP2040_ONE
    
    uses "/Source/Library/MCU"
    
    {
        loop
        {
            LED = true;
            Write('+');
            Delay(500);
            LED = false;    
            Write('-');
            Delay(500);
        }
    }
}
