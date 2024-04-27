unit Byte
{
    string ToString(byte this)
    {
        int value;
        value = int(this);
        return value.ToString();
    }
    string ToHexString(byte this, byte digits)
    {
        int digit;
        char c;
        string result;
        for (int i = digits; i > 0; i--)
        {
            digit = this % 16;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 16;
        }
        return result;
    }
    string ToBinaryString(byte this)
    {
        int digit;
        char c;
        string result;
        for (int i = 8; i > 0; i--)
        {
            digit = this % 2;
            c = Byte.ToHex(byte(digit));
            String.BuildFront(ref result, c);
            this = this / 2;
        }
        return result;
    }
    char ToDigit(byte d)
    {
        d = d + 48; // +0
        return char(d);
    }
    char ToHex(byte this)
    {
        if (this > 0x09)
        {
            // +'A' - 10   = 55
            // + 48 below  = 7
            this += 7;
        }
        this += 48; // +'0'
        return char(this);
    } 
    
/*    

32 bytes in Hopper without packed instructions:

0x0D9A  0x49            ENTER
                        // if (this > 0x09)                                               byte.hs:5
0x0D9B  0x39 0xFF 0xFF  PUSHLOCAL 0xFFFF (BP-1)
0x0D9E  0x37 0x09 0x00  PUSHI 0x0009
0x0DA1  0x8A            GT
0x0DA2  0x31 0x0D 0x00  JZ 0x0DAF (+13)
                        // this += 6;                                                     byte.hs:10
0x0DA5  0x39 0xFF 0xFF  PUSHLOCAL 0xFFFF (BP-1)
0x0DA8  0x37 0x06 0x00  PUSHI 0x0006
0x0DAB  0x80            ADD
0x0DAC  0x38 0xFF 0xFF  POPLOCAL 0xFFFF (BP-1)
                        // this += 48; // +'0'                                            byte.hs:12
0x0DAF  0x39 0xFF 0xFF  PUSHLOCAL 0xFFFF (BP-1)
0x0DB2  0x37 0x30 0x00  PUSHI 0x0030
0x0DB5  0x80            ADD
0x0DB6  0x38 0xFF 0xFF  POPLOCAL 0xFFFF (BP-1)
                        // return char(this);                                             byte.hs:13
0x0DB9  0x39 0xFF 0xFF  PUSHLOCAL 0xFFFF (BP-1)
0x0DBC  0x36 0x01 0x00  RETRES 0x0001

// 21 bytes in optimized Hopper:

0x0901  0x49            ENTER
                        // if (this > 0x09)                                               byte.hs:5
0x0902  0x1C 0xFF       PUSHLOCALB 0xFF (BP-1)
0x0904  0x1A 0x09       PUSHIB 0x09
0x0906  0x8A            GT
0x0907  0x2E 0x08       JZB 0x090F (+8)
                        // this += 6;                                                     byte.hs:10
0x0909  0x1C 0xFF       PUSHLOCALB 0xFF (BP-1)
0x090B  0x6D 0x06       ADDB 0x06
0x090D  0x1B 0xFF       POPLOCALB 0xFF (BP-1)
                        // this += 48; // +'0'                                            byte.hs:12
0x090F  0x1C 0xFF       PUSHLOCALB 0xFF (BP-1)
0x0911  0x6D 0x30       ADDB 0x30
0x0913  0x1B 0xFF       POPLOCALB 0xFF (BP-1)
                        // return char(this);                                             byte.hs:13
0x0915  0x4A            RET0


23 bytes in .asm:

// /source/runtime/common/syscalls.asm:36
// ####  SysCall.byteToHex()  ####                                              0x0037

0xE525  0x20 0xFB 0xE3  JSR 0xE3FB              // PopA();                        syscalls.asm:38
0xE528  0xC9 0x0A       CMP #0x0A               // CMP # 0x0A                     syscalls.asm:39
0xE52A  0x90 0x02       BCC 0xE52E (+2)         // if (C)                         syscalls.asm:40
0xE52C  0x69 0x06       ADC #0x06               // ADC # 6                        syscalls.asm:45
0xE52E  0x69 0x30       ADC #0x30               // ADC # '0' // 48                syscalls.asm:48
0xE530  0x85 0xC2       STA 0xC2                // STA ZP.TOPL                    syscalls.asm:50
0xE532  0x64 0xC3       STZ 0xC3                // STZ ZP.TOPH                    syscalls.asm:51
0xE534  0xA9 0x01       LDA #0x01               // LDA # Types.Char               syscalls.asm:52
0xE536  0x85 0xCB       STA 0xCB                // STA ZP.TOPT                    syscalls.asm:53
0xE538  0x20 0x81 0xE3  JSR 0xE381              // Stacks.PushTop();              syscalls.asm:54
0xE53B  0x60            RTS                     // }                              syscalls.asm:55


*/
}
