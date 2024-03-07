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
    
#if !defined(BOARD_HAS_NO_SPI0)
    const byte DefaultCSPin0     = Board.SPI0SCK;
    const byte DefaultClkPin0    = Board.SPI0SCK;
    const byte DefaultTxPin0     = Board.SPI0Tx;
    const byte DefaultRxPin0     = Board.SPI0Rx;
#endif
    
#if !defined(BOARD_HAS_NO_SPI1)
    const byte DefaultCSPin1     = Board.SPI1SS;
    const byte DefaultClkPin1    = Board.SPI1SCK;
    const byte DefaultTxPin1     = Board.SPI1Tx;
    const byte DefaultRxPin1     = Board.SPI1Rx;
#endif

    
    // SPI APIs : these first ones assume SPI0 controller
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
    WriteBytes(byte data, uint count) library;
    WriteWords(uint data, uint count) library;
    WriteBuffer(byte[] data, uint startIndex, uint length) library;
    WriteBuffer(uint[] data, uint startIndex, uint length) library;
    
    // These APIs support more than one SPI controller
    SetCSPin(byte spiController,  byte csPin) library;
    SetClkPin(byte spiController, byte clkPin) library;
    SetTxPin(byte spiController,  byte txPin) library;
    SetRxPin(byte spiController,  byte rxPin) library;
    
    Settings(byte spiController, long speedMaximum, DataOrder dataOrder, DataMode dataMode)  library;
    
    bool Begin(byte spiController) library; // assigns SPI|SPI1, creates SPISettings0|SPISettings01, calls begin()  (returns false if pins not set)
    BeginTransaction(byte spiController) library;
    EndTransaction(byte spiController) library;
    
    byte ReadByte(byte spiController) library;
    uint ReadWord(byte spiController) library;
    ReadBuffer(byte spiController, byte[] data, uint startIndex, uint length) library;
    WriteByte(byte spiController, byte data) library;
    WriteWord(byte spiController, uint data) library;
    WriteBytes(byte spiController, byte data, uint count) library;
    WriteWords(byte spiController, uint data, uint count) library;
    WriteBuffer(byte spiController, byte[] data, uint startIndex, uint length) library;
    WriteBuffer(byte spiController, uint[] data, uint startIndex, uint length) library;
    
    // used internally by drivers
    byte GetCSPin(byte spiController) library;
    
}
