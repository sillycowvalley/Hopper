unit HRNeoPixel
{
    Begin(uint length, byte pin, uint pixelType) { ErrorDump(150); Error = 0x0A; }
    byte GetBrightness() { ErrorDump(151); Error = 0x0A; return 0; }
    SetBrightness(byte brightness) { ErrorDump(152); Error = 0x0A;  }
    SetColor(uint pixel, byte r, byte g, byte b, byte w) { ErrorDump(153); Error = 0x0A;  }
    Show() { ErrorDump(154); Error = 0x0A;  }
    uint GetLength() { ErrorDump(155); Error = 0x0A; return 0; }
}
