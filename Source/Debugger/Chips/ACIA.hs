unit ACIA
{
    uses "/Source/System/Serial"

    uint controlRegister;
    uint statusRegister;
    uint dataRegister;
    
    bool initialized;
    
    Initialize()
    {
        bool success;
        string aciaPath = Path.MakeOptions("acia.options");
        if (File.Exists(aciaPath))
        {
            <string, variant> dict;
            if (JSON.Read(aciaPath, ref dict))
            {
                <string, string> debugOptions = dict["6850"];
                uint count;
                if (debugOptions.Contains("CONTROL"))
                {
                    string value = debugOptions["CONTROL"];
                    if (UInt.TryParse(value, ref controlRegister))
                    {
                        count++;
                    }
                }
                if (debugOptions.Contains("STATUS"))
                {
                    string value = debugOptions["STATUS"];
                    if (UInt.TryParse(value, ref statusRegister))
                    {
                        count++;
                    }
                }
                if (debugOptions.Contains("DATA"))
                {
                    string value = debugOptions["DATA"];
                    if (UInt.TryParse(value, ref dataRegister))
                    {
                        count++;
                    }
                }
                success = (count == 3);
            }
        }
        if (success)
        {
            // 4242 is a magic number that means "0, but as server (not client)"
            // "COM0" is our fake interprocess COM port on Windows (named pipe)
            Serial.Connect(4242); 
            initialized = true;
        }
    }
    Close()
    {
        if (initialized)
        {
            Serial.Close();
            initialized = false;
        }
    }
    char readChar;
    bool readWaiting;
    
    bool ServiceSerial()
    {
        bool requestIRQ;
        if (initialized)
        {
            if (!readWaiting && Serial.IsAvailable)
            {
                readChar = Serial.ReadChar();
                readWaiting = true;
                requestIRQ = true;
                //PrintLn((byte(readChar)).ToHexString(2), Colour.Green, Colour.Black);
            }
        }
        return requestIRQ;
    }
    
    bool OfferWrite(uint address, byte value)
    {
        bool written;
        if (initialized)
        {
            if (address == controlRegister)
            {
                // 0b00000011        // reset the 6850
                // 0b10010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
                written = true;
            }
            if (address == dataRegister)
            {
                Serial.WriteChar(char(value));
                //Print(value.ToHexString(2), Colour.MatrixRed, Colour.Black);
                //if (value == 0x0D)
                //{
                //    PrintLn();
                //}
                written = true;
            }
        }
        return written;
    }
    bool OfferRead(uint address, ref byte value)
    {
        bool read;
        if (initialized)
        {
            if (address == statusRegister)
            {
                // 0b00000010    // Bit 1 - Transmit Data Register Empty (TDRE)
                //               // (bit set means TDRE is empty and ready)
                value = 0b00000010;
                if (readWaiting)
                {
                    // Bit 7 - is the irq bit
                    // Bit 0 - When high, the RDRF bit indicatesthat received data has been transferred 
                    //         into the receiver data register andis ready to be read by the microprocessor.
                    value |= 0b10000001;
                }
                read = true;
            }
            if (address == dataRegister)
            {
                value = 0;
                if (readWaiting)
                {
                    value = byte(readChar);
                    readWaiting = false;
                    //PrintLn(value.ToHexString(2), Colour.MatrixBlue, Colour.Black);
                }
                else
                {
                    //PrintLn("??", Colour.Yellow, Colour.Black);
                }
                read = true;
            }
        }
        return read;
    }
    
}
