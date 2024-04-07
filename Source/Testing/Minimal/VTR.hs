program ValueTypeRuntimeTests
{
    //#define CPU_Z80
    #define MCU
    
    //#define CDECL
    
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/Serial"
    uses "/Source/Minimal/Diagnostics"
    
    writeDigit(uint uthis)
    {
        byte digit = byte(uthis % 10);
        char c = digit.ToDigit();
        uthis = uthis / 10;
        if (uthis != 0)
        {
            writeDigit(uthis);
        }
        Serial.WriteChar(c);
    }
    WriteInt(int this)
    {
        if (this < 0)
        {
            Write('-');
            this = 0 - this;
        }
        uint uthis = uint(this);
        writeDigit(uthis);
    }
    WriteUInt(uint this)
    {
        writeDigit(this);
    }
    
    WriteHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        byte lsn = b & 0xF;
        Serial.WriteChar(msn.ToHex());
        Serial.WriteChar(lsn.ToHex());
    }
    WriteHex(uint u)
    {
        byte msb = byte(u >> 8);
        byte lsb = byte(u & 0xFF);
        WriteHex(msb);
        WriteHex(lsb);
    }
    
    Failure(uint instance)
    {    
        Serial.WriteChar(Char.EOL);Serial.WriteChar('D');Serial.WriteChar('A');Serial.WriteChar('N');Serial.WriteChar('G');Serial.WriteChar(':');
        WriteUInt(instance);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    
    uint uiGlobal0;
    uint uiGlobal;
    
    uint uiFunction()
    {
        uint a = 1;
        a = a + 36;
        return a;
    }
    
    ADD()
    {
        uiGlobal = 2;
        uint local = 2;
        local = local + uiFunction();
        if (local != 39)
        {
            Failure(1);
        }
        local = uiGlobal + 2 + local;
        if (local != 43)
        {
            Failure(2);
        }
        local++;
        if (local != 44)
        {
            Failure(3);
        }
        uiGlobal++;
        if (uiGlobal != 3)
        {
            Failure(4);
        }
        
    }
    SUB()
    {
        uiGlobal = 4;
        uint local = 2;
        local = uiGlobal - local;
        if (local != 2)
        {
            Failure(5);
        }
        local--;
        if (local != 1)
        {
            Failure(6);
        }
        uiGlobal--;
        if (uiGlobal != 3)
        {
            Failure(7);
        }
        local = 10;
        local = local - 2;
        local -= 2;
        if (local != 6)
        {
            Failure(8);
        }
        uiGlobal = local - 2;
        if (uiGlobal != 4)
        {
            Failure(9);
        }
    }
    EQ()
    {
        uint a = 42;
        uint b = 42;
        if (3 == 4)
        {
             Failure(10);       
        }
        if (4 != 4)
        {
             Failure(11);       
        }
        if (4 < 4)
        {
             Failure(12);       
        }
        if (4 > 4)
        {
             Failure(13);       
        }
        if (4 > 2)
        {
        }
        else
        {
            Failure(14);       
        }
        if (2 < 4)
        {
        }
        else
        {
            Failure(15);       
        }
        if (4 <= 4)
        {
             
        }
        else
        {
            Failure(16);       
        }
        if (4 >= 4)
        {
             
        }
        else
        {
            Failure(17);       
        }
        if (4 >= 2)
        {
        }
        else
        {
            Failure(18);       
        }
        if (2 <= 4)
        {
        }
        else
        {
            Failure(19);       
        }
        if (a == b)
        {
        }
        else
        {
            Failure(20);       
        }
        if (a != b)
        {
            Failure(21);       
        }
        if (a <= b)
        {
        }
        else
        {
            Failure(21);       
        }
    }
    uint GetBits(byte one, byte two, byte three)
    {
        uint bits = 1 << one;
        bits |= 1 << two;
        bits |= 1 << three;
        return bits;
    }
    BITS()
    {
        uint bits = GetBits(2,4,6);
        if (bits != 0b01010100)
        {
            Failure(22);
        }
        bits &= 0xFF;
        if (bits != 0b01010100)
        {
            Failure(23);
        }
        bits = bits ^ 0xFF;
        if (bits != 0b10101011)
        {
            Failure(24);
        }
        bits = 0x55AA;
        bits = bits >> 3;
        if (bits != 0xAB5)
        {
            Failure(25);
        }
        bits = 0x55AA;
        bits = ~bits;
        if (bits != 0xAA55)
        {
            Failure(26);
        }
        
    }
    uint ret0(uint argument)
    {
        return argument * 3;
    }
    MUL()
    {
        uint a = 13;
        uint b = 41;
        
        uint c = a * b;
        if (c != 533)
        {
            Failure(27);
        }
        c *= 1;
        if (c != 533)
        {
            Failure(28);
        }
        c *= 0;
        if (c != 0)
        {
            Failure(29);
        }
        b = b * ret0(a);
        if (b != 1599)
        {
            Failure(30);
        }
        
        
    }
    
    uint one = 3;
    uint One { get { return 3 * one; } }
    
    DIV()
    {
        uint c = 533;
        uint b = 41;
        
        uint a = c / b;
        if (a != 13)
        {
            Serial.WriteChar(' '); WriteUInt(c);
            Serial.WriteChar(' '); WriteUInt(b);
            Serial.WriteChar(' '); WriteUInt(a);
            Failure(31);
        }
        c /= One;
        if (c != 59)
        {
            Failure(32);
        }
    }
    
    MOD()
    {
        uint c = 512;
        uint b = 41;
        
        uint a = c % b;
        if (a != 20)
        {
            Failure(33);
        }
        a = c % 1;
        if (a != 0)
        {
            Failure(34);
        }
    }
        
    LTI()
    {
        uint wins = 0;
        uint losses = 0;
        
        int p5  =  5;
        int m10 = -10;
        int p10 = 10;
        int m5  = -5;
    
        if (m10 < m5)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (m5 < p10)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (p5 < m10)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (m5 < m10)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (p5 < p10)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (p10 < p5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (p5 < p5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (m5 < m5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (wins != 8)  
        { 
            Failure(36); 
        }

    }
    
    GEI()
    {
        uint wins = 0;
        uint losses = 0;
        
        
        int p5  =  5;
        int m10 = -10;
        int p10 = 10;
        int m5  = -5;
    
        if (m10 >= m5)
        {
            losses++;   
            Failure(1);
        }
        else
        {
            wins++;
        }
        if (m5 >= p10)
        {
            losses++;    
            Failure(2);
        }
        else
        {
            wins++;
        }
        if (p5 >= m10)
        {
            wins++;
        }
        else
        {
            losses++;
            Failure(3);
        }
        if (m5 >= m10)
        {
            wins++;
        }
        else
        {
            losses++;
            Failure(4);
        }
        if (p5 >= p10)
        {
            losses++;
            Failure(5);
        }
        else
        {
            wins++;
        }
        if (p10 >= p5)
        {
            wins++;
        }
        else
        {
            losses++;
            Failure(6);
        }
        if (p5 >= p5)
        {
            wins++;
        }
        else
        {
            losses++;
            Failure(7);
        }
        if (m5 >= m5)
        {
            wins++;
        }
        else
        {
            losses++;
            Failure(8);
        }
        if (wins != 8)
        {
            Failure(39);
        }
    }
    
    LEI()
    {
        uint wins = 0;
        uint losses = 0;
        
        
        int p5  =  5;
        int m10 = -10;
        int p10 = 10;
        int m5  = -5;
    
        if (m10 <= m5)
        {
            wins++;   
        }
        else
        {
            losses++;
        }
        if (m5 <= p10)
        {
            wins++;    
        }
        else
        {
            losses++;
        }
        if (p5 <= m10)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (m5 <= m10)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (p5 <= p10)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (p10 <= p5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (p5 <= p5)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (m5 <= m5)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        
        if (wins != 8)
        {
            Failure(37);
        }
    }
    
    GTI()
    {
        uint wins = 0;
        uint losses = 0;
        
        
        int p5  =  5;
        int m10 = -10;
        int p10 = 10;
        int m5  = -5;
    
        if (m10 > m5)
        {
            losses++;   
        }
        else
        {
            wins++;
        }
        if (m5 > p10)
        {
            losses++;    
        }
        else
        {
            wins++;
        }
        if (p5 > m10)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (m5 > m10)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (p5 > p10)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (p10 > p5)
        {
            wins++;
        }
        else
        {
            losses++;
        }
        if (p5 > p5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (m5 > m5)
        {
            losses++;
        }
        else
        {
            wins++;
        }
        if (wins != 8)
        {
            Failure(38);
        }
    }
    
    bool ref2(ref uint arg)
    {
        if (arg != 0x55)
        {
            Failure(43);
        }
        arg = 0xAA;
        if (arg != 0xAA)
        {
            Failure(44);
        }
        return true;
    }
    bool ref1(ref uint arg)
    {
        if (arg != 0x55)
        {
            Failure(42);
        }
        bool result = ref2(ref arg);
        if (arg != 0xAA)
        {
            Failure(45);
        }
        return result;
    }
    Ref()
    {
        uint local1;
        uint local2;
        bool result;
        uint local;
        local2 = 0x55;
        result = ref1(ref local2);
        if (local2 != 0xAA)
        {
            Failure(40);
        }
        
        local = 0x55;
        result = ref1(ref local);
        if (local != 0xAA)
        {
            Failure(46);
        }
        
        local1 = 0x55;
        result = ref1(ref local1);
        if (local1 != 0xAA)
        {
            Failure(46);
        }
        
        uiGlobal = 0x55;
        result = ref1(ref uiGlobal);
        if (uiGlobal != 0xAA)
        {
            Failure(41);
        }
    }
    RefRef()
    {
        uint another;
        Ref();
    }
    
    
    Hopper()
    {
        BITS(); // BITAND, BITOR, BITXOR, BITSHL, BITSHR, RETB
        ADD();  // ADD, ADDB, INCLOCALB, INCGLOBALB NE, JZB
        SUB();  // SUB, SUBB, DECLOCALB, DECGLOBALB
        EQ();   // EQ NE
        MUL();  // MUL RET0
        DIV();  // DIV RETFAST
        MOD();  // MOD
        LTI();  // LTI
        GEI();  // GEI
        LEI();  // LEI
        GTI();  // GTI
        
        //Ref();  // PUSHSTACKADDR, PUSHREL, POPREL
        //RefRef();
        
        Serial.WriteChar(Char.EOL); Serial.WriteChar('O'); Serial.WriteChar('K');Serial.WriteChar('!');
    }
}
