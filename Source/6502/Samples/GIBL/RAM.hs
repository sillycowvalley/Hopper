unit RAM
{
    // #### globals at the top of the file so we can keep track of them:
    uint topPointer;
    // #### end of globals
    
    // Knobs and dials:
    const int  topLimit        = 0x0800;  // 2K: when TOP is used, how much memory to allocate with Memory.Allocate?
    const uint timerResolution =   1000;  // fractions of a second of the current harware timer (ms in this case)
    const byte timeOnZeroPage  =   0xA0;  // (160) where GIBL thinks the time is on zeroPage (2 bytes)
    const byte timeOnH6502     =   0xE0;  // where the time is on my 6502 machine's zeroPage (4 bytes)
    uses "/Source/6502/Firmware/Memory"
    
    uint TopSize     { get { if (topPointer != 0) { return topLimit; } return 0; } }
    uint TimeForSeed { get { return ReadWord(timeOnH6502); } }
    
    Free()
    {
        if (0 != topPointer)
        {
            Memory.Free(topPointer);
            uint sz = topLimit;
            //WriteLn("Free: 0x" + topPointer.ToHexString(4) + " " + sz.ToString());
            topPointer = 0;
        }
    }
    uint Top 
    { 
        get 
        { 
            if (topPointer == 0)
            {
                topPointer = Memory.Allocate(topLimit);
                uint sz = topLimit;
                //WriteLn("Allocate: 0x" + topPointer.ToHexString(4) + " " + sz.ToString());
            }
            return topPointer; 
        } 
    }
    
    uint ReadWord(uint address)
    {
        uint value;   
        if (address == timeOnZeroPage)
        {
            long elapsed = Millis / (timerResolution/10); // 1/10th of a second resolution
            value = uint(elapsed);
        }
        else
        {
            value = ReadByte(address) + ReadByte(address+1) << 8;
        }
        return value;
    }
    WriteWord(uint address, uint w)
    {
        if (address == timeOnZeroPage)
        {
            if (w != 0)
            {
                Error(18);
                return;
            }
            WriteByte(timeOnH6502,   0);
            WriteByte(timeOnH6502+1, 0);
            WriteByte(timeOnH6502+2, 0);
            WriteByte(timeOnH6502+3, 0);
        }
        else
        {
            WriteByte(address,   byte(w & 0xFF));
            WriteByte(address+1, byte(w >> 8));
        }
    }
    
}
