unit DisplayDriver
{
    friend Display;
    
    /*
        The constants and mapping table below are a modified version of
        the original from the Adafruit driver for Arduino devices.
        Adafruit invests time and resources providing this open source
        code,please support Adafruit and open-source hardware by purchasing
        products from Adafruit!
        
        Adapted from code written by Limor Fried/Ladyada for Adafruit Industries.
        
        https://www.adafruit.com/product/1910
    */
    
    const byte HT16K33_BLINK_CMD = 0x80;       // I2C register for BLINK setting
    const byte HT16K33_BLINK_DISPLAYON = 0x01; // I2C value for steady on
    const byte HT16K33_BLINK_OFF    = 0;       // I2C value for steady off
    const byte HT16K33_BLINK_2HZ    = 1;       // I2C value for 2 Hz blink
    const byte HT16K33_BLINK_1HZ    = 2;       // I2C value for 1 Hz blink
    const byte HT16K33_BLINK_HALFHZ = 3;       // I2C value for 0.5 Hz blink
    
    const byte HT16K33_CMD_BRIGHTNESS = 0xE0;  // I2C register for BRIGHTNESS setting
    
    const byte[] alphaFontTable = {
    
        0b00000000, 0b00000001, 0b00000000, 0b00000010, 0b00000000, 0b00000100,
        0b00000000, 0b00001000, 0b00000000, 0b00010000, 0b00000000, 0b00100000,
        0b00000000, 0b01000000, 0b00000000, 0b10000000, 0b00000001, 0b00000000,
        0b00000010, 0b00000000, 0b00000100, 0b00000000, 0b00001000, 0b00000000,
        0b00010000, 0b00000000, 0b00100000, 0b00000000, 0b01000000, 0b00000000,
        0b10000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000,
        0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000,
        0b00010010, 0b11001001, 0b00010101, 0b11000000, 0b00010010, 0b11111001,
        0b00000000, 0b11100011, 0b00000101, 0b00110000, 0b00010010, 0b11001000,
        0b00111010, 0b00000000, 0b00010111, 0b00000000,
        0b00000000, 0b00000000, //
        0b00000000, 0b00000110, // !
        0b00000010, 0b00100000, // "
        0b00010010, 0b11001110, // #
        0b00010010, 0b11101101, // $
        0b00001100, 0b00100100, // %
        0b00100011, 0b01011101, // &
        0b00000100, 0b00000000, // '
        0b00100100, 0b00000000, // (
        0b00001001, 0b00000000, // )
        0b00111111, 0b11000000, // *
        0b00010010, 0b11000000, // +
        0b00001000, 0b00000000, // ,
        0b00000000, 0b11000000, // -
        0b01000000, 0b00000000, // .
        0b00001100, 0b00000000, // /
        0b00001100, 0b00111111, // 0
        0b00000000, 0b00000110, // 1
        0b00000000, 0b11011011, // 2
        0b00000000, 0b10001111, // 3
        0b00000000, 0b11100110, // 4
        0b00100000, 0b01101001, // 5
        0b00000000, 0b11111101, // 6
        0b00000000, 0b00000111, // 7
        0b00000000, 0b11111111, // 8
        0b00000000, 0b11101111, // 9
        0b00010010, 0b00000000, // :
        0b00001010, 0b00000000, // ;
        0b00100100, 0b00000000, // <
        0b00000000, 0b11001000, // =
        0b00001001, 0b00000000, // >
        0b00010000, 0b10000011, // ?
        0b00000010, 0b10111011, // @
        0b00000000, 0b11110111, // A
        0b00010010, 0b10001111, // B
        0b00000000, 0b00111001, // C
        0b00010010, 0b00001111, // D
        0b00000000, 0b11111001, // E
        0b00000000, 0b01110001, // F
        0b00000000, 0b10111101, // G
        0b00000000, 0b11110110, // H
        0b00010010, 0b00001001, // I
        0b00000000, 0b00011110, // J
        0b00100100, 0b01110000, // K
        0b00000000, 0b00111000, // L
        0b00000101, 0b00110110, // M
        0b00100001, 0b00110110, // N
        0b00000000, 0b00111111, // O
        0b00000000, 0b11110011, // P
        0b00100000, 0b00111111, // Q
        0b00100000, 0b11110011, // R
        0b00000000, 0b11101101, // S
        0b00010010, 0b00000001, // T
        0b00000000, 0b00111110, // U
        0b00001100, 0b00110000, // V
        0b00101000, 0b00110110, // W
        0b00101101, 0b00000000, // X
        0b00010101, 0b00000000, // Y
        0b00001100, 0b00001001, // Z
        0b00000000, 0b00111001, // [
        0b00100001, 0b00000000, //
        0b00000000, 0b00001111, // ]
        0b00001100, 0b00000011, // ^
        0b00000000, 0b00001000, // _
        0b00000001, 0b00000000, // `
        0b00010000, 0b01011000, // a
        0b00100000, 0b01111000, // b
        0b00000000, 0b11011000, // c
        0b00001000, 0b10001110, // d
        0b00001000, 0b01011000, // e
        0b00000000, 0b01110001, // f
        0b00000100, 0b10001110, // g
        0b00010000, 0b01110000, // h
        0b00010000, 0b00000000, // i
        0b00000000, 0b00001110, // j
        0b00110110, 0b00000000, // k
        0b00000000, 0b00110000, // l
        0b00010000, 0b11010100, // m
        0b00010000, 0b01010000, // n
        0b00000000, 0b11011100, // o
        0b00000001, 0b01110000, // p
        0b00000100, 0b10000110, // q
        0b00000000, 0b01010000, // r
        0b00100000, 0b10001000, // s
        0b00000000, 0b01111000, // t
        0b00000000, 0b00011100, // u
        0b00100000, 0b00000100, // v
        0b00101000, 0b00010100, // w
        0b00101000, 0b11000000, // x
        0b00100000, 0b00001100, // y
        0b00001000, 0b01001000, // z
        0b00001001, 0b01001001, // {
        0b00010010, 0b00000000, // |
        0b00100100, 0b10001001, // }
        0b00000101, 0b00100000, // ~
        0b00111111, 0b11111111,
    };
    
    const uint ALPHANUM_SEG_DP = 0b0100000000000000; // Alphanumeric segment decimal point
    
    byte i2cController = Wire.DefaultI2CController;
    byte sdaPin        = Wire.DefaultI2CSDAPin;
    byte sclPin        = Wire.DefaultI2CSCLPin;
    byte I2CController { get { return i2cController; } set { i2cController = value; } }
    byte I2CAddress    { get { return i2cAddress; }    set { i2cAddress = value; } }
    byte I2CSDAPin     { get { return sdaPin; }        set { sdaPin = value; } }
    byte I2CSCLPin     { get { return sclPin; }        set { sclPin = value; } }
    
    enum BlinkRate
    {
        None,
        TwoHz,
        OneHz,
        HalfHz,
    }
    
    setBlinkRate(byte i2cAddress, BlinkRate blinkRate)
    {
        byte blink; // turn off if not sure
        switch (blinkRate)
        {
            case BlinkRate.TwoHz:  { blink = 1; }
            case BlinkRate.OneHz:  { blink = 2; }
            case BlinkRate.HalfHz: { blink = 3; }
        }
        Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
        Wire.Write(DisplayDriver.I2CController, HT16K33_BLINK_CMD | HT16K33_BLINK_DISPLAYON | (blink << 1));
        byte result = Wire.EndTx(DisplayDriver.I2CController);
    }
    setBrightness(byte i2cAddress, byte brightness)
    {
        if (brightness > 15)
        {
            brightness = 15; // limit to max brightness
        }
        Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
        Wire.Write(DisplayDriver.I2CController, HT16K33_CMD_BRIGHTNESS | brightness);
        byte result = Wire.EndTx(DisplayDriver.I2CController);
    }
    
    begin(byte i2cAddress)
    {
        Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
        Wire.Write(DisplayDriver.I2CController, 0x21); // turn on oscillator
        byte result = Wire.EndTx(DisplayDriver.I2CController);
        
        write(i2cAddress, ' ', ' ', ' ', ' ');
        
        setBlinkRate(i2cAddress, BlinkRate.None);

        setBrightness(i2cAddress, 15); // max brightness
    }
    
    write(byte i2cAddress, char a, char b, char c, char d)
    {
        Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
        Wire.Write(DisplayDriver.I2CController, 0);
        
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(a) * 2 + 1]);
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(a) * 2]);
        
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(b) * 2 + 1]);
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(b) * 2]);
        
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(c) * 2 + 1]);
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(c) * 2]);
        
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(d) * 2 + 1]);
        Wire.Write(DisplayDriver.I2CController, alphaFontTable[byte(d) * 2]);
        
        Wire.Write(DisplayDriver.I2CController, 0);
        Wire.Write(DisplayDriver.I2CController, 0);
        
        Wire.Write(DisplayDriver.I2CController, 0);
        Wire.Write(DisplayDriver.I2CController, 0);
        
        Wire.Write(DisplayDriver.I2CController, 0);
        Wire.Write(DisplayDriver.I2CController, 0);
        
        Wire.Write(DisplayDriver.I2CController, 0);
        Wire.Write(DisplayDriver.I2CController, 0);
        
        byte result = Wire.EndTx(DisplayDriver.I2CController);
    }
}
