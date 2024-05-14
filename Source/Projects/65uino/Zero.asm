unit ZP
{
    const byte I2CADDR   = 0x00;        // Reserve 1 byte for I2CADDR
    const byte OutB      = I2CADDR + 1; // Reserve 1 byte for OutB - used for I2C
}
