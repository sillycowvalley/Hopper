unit DeviceDriver
{
    uses "/Source/Library/Displays/EPDSSD1680.hs"
    
    const int PW = 250;
    const int PH = 122;
    
    const byte DCPin  = 10;
    const byte CSPin  = 9;
    const byte ClkPin = 18;
    const byte TxPin  = 19;
    const byte RxPin  = 20;
    const byte SPIController  = 0;
    
    const uint DefaultRefreshDelay   = 1; // seconds
    
    const bool EPDBlack0 = true;
    const bool EPDRed0   = true;
    const bool EPDWhite0 = false;
    
    bool Begin()
    {
        return Display.Begin();
    }
}
