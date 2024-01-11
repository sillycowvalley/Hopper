unit SPI
{
    enum DataOrder
    {
        LSBFirst = 0,
        MSBFirst = 1,
    }
    enum DataMode
    {
        Mode0 = 0,
        Mode1 = 1,
        Mode2 = 2,
        Mode3 = 3,
    }
    
    // SPI APIs : this first ones assume only one SPI controller
    byte CSPin  { set library; get library; } // CS
    byte ClkPin { set library; }              // SCK
    byte TxPin  { set library; }              // MOSI
    byte RxPin  { set library; }              // MISO
    
    Settings(long speedMaximum, DataOrder dataOrder, DataMode dataMode)  library;
    
    bool Begin() library; // assigns SPI, creates SPISettings0, calls begin() (returns false if pins not set)    
    BeginTransaction() library;
    EndTransaction() library;
    
    byte ReadByte() library;
    uint ReadWord() library;
    ReadBuffer(byte[] data, uint startIndex, uint length) library;
    WriteByte(byte data) library;
    WriteWord(uint data) library;
    WriteBuffer(byte[] data, uint startIndex, uint length) library;
    
    // These APIs support more than one SPI controller
    SetCSPin(byte spiController, byte csPin) library;
    SetClkPin(byte spiController, byte clkPin) library;
    SetTxPin(byte spiController, byte txPin) library;
    SetRxPin(byte spiController, byte rxPin) library;
    
    Settings(byte spiController, long speedMaximum, DataOrder dataOrder, DataMode dataMode)  library;
    
    bool Begin(byte spiController) library; // assigns SPI|SPI1, creates SPISettings0|SPISettings01, calls begin()  (returns false if pins not set)
    BeginTransaction(byte spiController) library;
    EndTransaction(byte spiController) library;
    
    byte ReadByte(byte spiController) library;
    uint ReadWord(byte spiController) library;
    ReadBuffer(byte spiController, byte[] data, uint startIndex, uint length) library;
    WriteByte(byte spiController, byte data) library;
    WriteWord(byte spiController, uint data) library;
    WriteBuffer(byte spiController, byte[] data, uint startIndex, uint length) library;
    
    // used internally by drivers
    byte GetCSPin(byte spiController) library;
    
}
