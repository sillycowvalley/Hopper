unit HRSPI
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
    byte CSPin  { set { ErrorDump(140); Error = 0x0A; } get { ErrorDump(141); Error = 0x0A; return 0; } } // CS
    byte ClkPin { set { ErrorDump(142); Error = 0x0A; } }              // SCK
    byte TxPin  { set { ErrorDump(143); Error = 0x0A; } }              // MOSI
    byte RxPin  { set { ErrorDump(144); Error = 0x0A; } }              // MISO
    
    SetCSPin(byte spiController, byte csPin) { ErrorDump(155); Error = 0x0A; }
    SetClkPin(byte spiController, byte clkPin) { ErrorDump(156); Error = 0x0A; }
    SetTxPin(byte spiController, byte txPin) { ErrorDump(157); Error = 0x0A; }
    SetRxPin(byte spiController, byte rxPin) { ErrorDump(158); Error = 0x0A; }
    
    Settings(byte spiController, uint hrspeedMaximum, DataOrder dataOrder, DataMode dataMode)  { ErrorDump(159); Error = 0x0A; }
    
    bool Begin(byte spiController) { ErrorDump(160); Error = 0x0A; return false; }// assigns SPI|SPI1, creates SPISettings0|SPISettings01, calls begin()  (returns false if pins not set)
    BeginTransaction(byte spiController) { ErrorDump(161); Error = 0x0A; }
    EndTransaction(byte spiController) { ErrorDump(162); Error = 0x0A; }
    
    byte ReadByte(byte spiController) { ErrorDump(163); Error = 0x0A; return 0; }
    uint ReadWord(byte spiController) { ErrorDump(164); Error = 0x0A; return 0; }
    ReadBuffer(byte spiController, uint hrdata, uint startIndex, uint length) { ErrorDump(165); Error = 0x0A; }
    
    WriteByte(byte spiController, byte data) { ErrorDump(163); Error = 0x0A; }
    WriteWord(byte spiController, uint data) { ErrorDump(164); Error = 0x0A; }
    WriteBuffer(byte spiController, uint hrdata, uint startIndex, uint length) { ErrorDump(165); Error = 0x0A; }
    
    // used internally by drivers
    byte GetCSPin(byte spiController) { ErrorDump(169); Error = 0x0A; return 0; }
}
