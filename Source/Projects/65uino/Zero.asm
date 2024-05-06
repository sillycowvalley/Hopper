unit ZP
{
    const byte I2CADDR   = 0x00;        // Reserve 1 byte for I2CADDR
    const byte InB       = I2CADDR + 1; // Reserve 1 byte for InB - used for Serial and I2C
    const byte OutB      = InB + 1;     // Reserve 1 byte for InB - used for Serial and I2C
    
    const byte XTmp      = OutB + 1;    // Reserve 1 byte for xtmp
    const byte StringP   = XTmp + 1;    // Reserve 2 bytes for stringp (stringp + 1)
                                        // StringP +1 free for temp.
    const byte Mode      = StringP + 2; // Reserve 1 byte for mode
    const byte RxCnt     = Mode + 1;    // Reserve 1 byte for rxcnt
    const byte TxCnt     = RxCnt + 1;   // Reserve 1 byte for txcnt
    const byte RunPnt    = TxCnt + 1;   // Reserve 2 bytes for runpnt
    const byte Cursor    = RunPnt + 1;  // Reserve 1 byte for cursor ; SSD1306
    const byte Scroll    = Cursor  + 1; // Reserve 1 byte for scroll ; SSD1306
    const byte TFlags    = Scroll + 1;  // Reserve 1 byte for tflags ; SSD1306
    const byte SerialBuf = TFlags + 1;  // Reserve 1 byte for serialbuf - Used for text display and userland program storage
    
    const byte UserLand  = 0x0E;
}
