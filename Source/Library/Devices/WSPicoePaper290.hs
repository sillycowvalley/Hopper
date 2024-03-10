unit DeviceDriver
{
    // TODO : not working yet (in progress)
    
#if !defined(MCU_BOARD_DEFINED)
    // plugs directly into the Pi Pico so no board defined, assume generic Pi Pico
    uses "/Source/Library/Boards/PiPico"
#endif
    
    // https://www.waveshare.com/wiki/Pico-ePaper-2.90
    #define WAVESHARE_PICO_ePAPER_290    
    #define HAS_RESET_PIN
    #define HAS_RED
    
    uses "/Source/Library/Displays/UC8151Driver"
    
    friend DisplayDriver;
    
    const int pw = 296;
    const int ph = 128;
    
    const byte spiController = 1;
    
    const byte dcPin   = Board.GP8;
    const byte csPin   = Board.GP9;
    const byte clkPin  = Board.GP10;
    const byte txPin   = Board.GP11;
    const byte rstPin  = Board.GP12;
    const byte busyPin = Board.GP13;
    
    bool Begin()
    {
        return Display.Begin();
    }
}
