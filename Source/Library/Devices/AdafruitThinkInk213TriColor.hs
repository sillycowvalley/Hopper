unit DeviceDriver
{
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    const int PW = 250;
    const int PH = 122;
    
    const byte DCPin = 10;
    const byte CSPin = 9;
    const byte ClkPin = 18;
    const byte TxPin  = 19;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    
    const bool BlackInverted     = true;
    const bool ColourInverted    = false;
    const byte BlackBufferIndex  = 0;
    const byte ColourBufferIndex = 1;
    
    const uint DefaultRefreshDelay   = 16000;
    
    
    flags EPDColour
    {
        Clear     = 0b00,
        BlackBit  = 0b01,
        ColourBit = 0b10,
        
        White = 0b00,
        Black = 0b01,
        Red   = 0b10,
        Gray  = 0b10,
        Light = 0b00,
        Dark  = 0b01,
    }
    
    bool Begin()
    {
        return Display.Begin();
    }
}