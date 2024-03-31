program TestNumbers
{
    #define CPU_Z80
    #define CDECL
    
    uses "/Source/Minimal/System"
    
    uses "/Source/Minimal/Diagnostics"
    uses "/Source/Minimal/Serial"
    
    WriteHex(byte b)
    {
        byte msn = ((b >> 4) & 0xF);
        Serial.WriteChar(msn.ToHex());
        byte lsn = b & 0xF;
        Serial.WriteChar(lsn.ToHex());
    }
    WriteHex(uint u)
    {
        byte msb = byte(u >> 8);
        WriteHex(msb);
        byte lsb = byte(u & 0xFF);
        WriteHex(lsb);
    }
    
    PrintFailed(uint instance)
    {    
#if !defined(MINIMAL_RUNTIME) && !defined(CPU_Z80)
        Serial.Connect();
#endif
        Serial.WriteChar('D');Serial.WriteChar('A');Serial.WriteChar('N');Serial.WriteChar('G');Serial.WriteChar(':');
        WriteHex(instance);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
   
    TestEquals()
    { 
        // contants
        if (1 == 0)
        {
            PrintFailed(1);
        }
        if (!(1 == 1))
        {
            PrintFailed(2);
        }
        
        if ('a' == 'b')
        {
            PrintFailed(3);
        }
        if (!('a' == 'a'))
        {
            PrintFailed(4);
        }
        
        byte ba = 1;
        byte bb = 0;
        byte bc = 1;
        if (!(ba == 1))
        {
            PrintFailed(7);
        }
        if (ba == 0)
        {
            PrintFailed(8);
        }
        if (!(1 == ba))
        {
            PrintFailed(9);
        }
        if (0 == ba)
        {
            PrintFailed(10);
        }
        if (!(ba == ba))
        {
            PrintFailed(11);
        }
        if (ba == bb)
        {
            PrintFailed(12);
        }
        if (!(ba == bc))
        {
            PrintFailed(13);
        }

        int ia = 1;
        int ib = 0;
        int ic = 1;
        if (!(ia == 1))
        {
            PrintFailed(14);
        }
        if (ia == 0)
        {
            PrintFailed(15);
        }
        if (!(1 == ia))
        {
            PrintFailed(16);
        }
        if (0 == ia)
        {
            PrintFailed(17);
        }
        if (!(ia == ia))
        {
            PrintFailed(18);
        }
        if (ia == ib)
        {
            PrintFailed(19);
        }
        if (!(ia == ic))
        {
            PrintFailed(20);
        }
        if (!(ia == ba))
        {
            PrintFailed(21);
        }
        if (ia == bb)
        {
            PrintFailed(22);
        }
        if (!(ba == ia))
        {
            PrintFailed(23);
        }
        if (ba == ib)
        {
            PrintFailed(24);
        }
      
        
        
    }
    
    const uint    globalUInt   = 10000;
    const uint    globalUInt2  = 20000;
    
    const int    globalInt  = 10000;
    const int    globalInt2  = 10001;

    
    TestConstants()
    {
        int    localInt   = 10000;
        if (globalInt != localInt)
        {
            PrintFailed(25);
        }
        localInt = localInt + 1;
        if (globalInt2 != localInt)
        {
            PrintFailed(26);
        }
    }
    
    TestLessThan()
    {
        int    localInt   = 10001;
        
        uint localUInt0 =   12;
        uint localUInt1 =   23;
        uint localUInt2 = 1234;
        uint localUInt3 = 4567;
        uint localUInt4 = 256;
        uint localUInt5 = 257;
        if (!(localUInt0 < localUInt1))
        {
            PrintFailed(27);    
        }
        if (!(localUInt1 < localUInt2))
        {
            PrintFailed(28);    
        }
        if (!(localUInt2 < localUInt3))
        {
            PrintFailed(29);    
        }
        if (localUInt1 < localUInt0)
        {
            PrintFailed(30);    
        }
        if (localUInt2 < localUInt1)
        {
            PrintFailed(31);    
        }
        if (localUInt3 < localUInt2)
        {
            PrintFailed(32);    
        }
        if (localUInt0 < localUInt0)
        {
            PrintFailed(33);    
        }
        if (localUInt1 < localUInt1)
        {
            PrintFailed(34);    
        }
        if (localUInt2 < localUInt2)
        {
            PrintFailed(35);    
        }
        if (localUInt3 < localUInt3)
        {
            PrintFailed(36);    
        }
        if (!(localUInt4 < localUInt5))
        {
            PrintFailed(37);    
        }
        if (localUInt5 < localUInt4)
        {
            PrintFailed(38);    
        }
        if (localUInt4 < localUInt4)
        {
            PrintFailed(39);    
        }
        if (!(globalInt < localInt))
        {
            PrintFailed(40);
        }
        if (localInt < globalInt)
        {
            PrintFailed(41);
        }
    }
    TestLessThanOrEqual()
    {
        int    localInt   = 10001;
        int localNegInt1  = -10000;
        int localNegInt2  = -10001;
        if (!(globalInt <= localInt))
        {
            PrintFailed(42);
        }
        if (!(globalInt <= globalInt))
        {
            PrintFailed(43);
        }
        if (!(localNegInt2 <= localNegInt1))
        {
            PrintFailed(44);
        }
        if (!(localNegInt1 <= globalInt))
        {
            PrintFailed(45);
        }
        if (!(localNegInt1 <= localInt))
        {
            PrintFailed(46);
        }
        
        if (!(localInt <= localInt))
        {
            PrintFailed(47);
        }
        if (!(localInt <= 20001))
        {
            PrintFailed(48);
        }
        if (localInt <= 5001)
        {
            PrintFailed(49);
        }
        if (!(localInt <= 10001))
        {
            PrintFailed(50);
        }
        
        if (localInt <= globalInt)
        {
            PrintFailed(51);
        }
        
    }
    
    TestGreaterThan()
    {
        int    localInt   = 10001;

        if ((globalInt > localInt))
        {
            PrintFailed(52);
        }
        if ((localInt > localInt))
        {
            PrintFailed(53);
        }
        if (!(localInt > globalInt))
        {
            PrintFailed(54);
        }
    }
    
    TestGreaterThanOrEqual()
    {
        int    localInt   = 10001;
        if (globalInt >= localInt)
        {
            PrintFailed(55);
        }
        if (!(globalInt >= globalInt))
        {
            PrintFailed(56);
        }
        if (!(localInt >= localInt))
        {
            PrintFailed(57);
        }
        if (!(localInt >= globalInt))
        {
            PrintFailed(58);
        }
    }
    flags PFlags
    {
        None = 0,
        One = 0x01,
        Two = 0x02,
        Four = 0x04
    }
    
    uint gProp;
    uint Prop { get { return gProp; } set { gProp = value; } }
    
    PFlags fProp;
    PFlags FProp { get { return fProp; } set { fProp = value; } }
    
    TestPropertyMath()
    {
        Prop = 42;
        Prop += 1;
        if (Prop != 43)
        {
            PrintFailed(59);        
        }
        Prop -= 1;
        if (Prop != 42)
        {
            PrintFailed(60);        
        }
        Prop = 3;
        Prop *= 6;
        if (Prop != 18)
        {
            PrintFailed(61);        
        }
        Prop /= 6;
        if (Prop != 3)
        {
            PrintFailed(62);        
        }
        Prop++;
        if (Prop != 4)
        {
            PrintFailed(63);
        }
        Prop--;
        if (Prop != 3)
        {
            PrintFailed(64);
        }
        
        fProp |= PFlags.Four;
        if (fProp != PFlags.Four)
        {
            PrintFailed(65);
        }
        fProp &= PFlags.Two;
        if (fProp != PFlags.None)
        {
            PrintFailed(66);
        }
        
        FProp |= PFlags.Four;
        if (FProp != PFlags.Four)
        {
            PrintFailed(67);
        }
        FProp &= PFlags.Two;
        if (FProp != PFlags.None)
        {
            PrintFailed(68);
        }
    }

    TestUIntMath()
    {
        // globalUInt   = 10000;
        // globalUInt2  = 20000;
        uint localUInt  = 10000;
        uint localUInt2 = 20000;
        
        // +
        if (localUInt + localUInt != 20000)
        {
            PrintFailed(69);        
        }
        if (localUInt2 + localUInt2 != 40000)
        {
            PrintFailed(70);        
        }
        if (globalUInt + localUInt != 20000)
        {
            PrintFailed(71);        
        }
        if (globalUInt2 + localUInt2 != 40000)
        {
            PrintFailed(72);        
        }
        if (localUInt2 + localUInt2 + globalUInt2 != 60000)
        {
            PrintFailed(73);
        }
        
        // -
        if (localUInt2 - localUInt != 10000)
        {
            PrintFailed(74);
        }
        if (localUInt2 - localUInt - localUInt != 0)
        {
            PrintFailed(75);
        }
        
        // *
        if (localUInt2 != 2 * localUInt)
        {
            PrintFailed(76);
        }
        if (localUInt * 5 != 50000)
        {
            PrintFailed(77);
        }
        
        // /
        if (localUInt2 / 2 != localUInt)
        {
            PrintFailed(78);
        }
        if (localUInt / 250 != 40)
        {
            PrintFailed(79);
        }
        
        // %
        if (localUInt % 3 != 1)
        {
            PrintFailed(80);
        }
        
        
        uint test = 0xAA55;
        
        uint testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(81);
        }
        
        testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(82);
        }
    } // TestUIntMath
    
    TestIntMath()
    {
        // globalInt   = 10000;
        // globalInt2  = 10001;
        int localInt  = 10000;
        int localInt2 = 10001;
        int localIntNeg = 0 - localInt;
        
        if (localInt.GetByte(0) != 0x10)
        {
            PrintFailed(83);        
        }
        if (localInt.GetByte(1) != 0x27)
        {
            PrintFailed(84);        
        }
        // +
        if (localInt + localInt != 20000)
        {
            PrintFailed(85);        
        }
        if (localInt2 + localInt2 != 20002)
        {
            PrintFailed(86);        
        }
        if (globalInt + localInt != 20000)
        {
            PrintFailed(87);        
        }
        if (globalInt2 + localInt2 != 20002)
        {
            PrintFailed(88);        
        }
        if (localInt2 + localInt2 + globalInt2 != 30003)
        {
            PrintFailed(89);
        }
        
        // -
        if (localInt2 - localInt != 1)
        {
            PrintFailed(90);
        }
        if (localInt - localInt2 - localInt2 != -10002)
        {
            PrintFailed(91);
        }
        if (-localInt2 != -10001)
        {
            PrintFailed(92);
        }
        
        // *
        if (20000 != 2 * localInt)
        {
            PrintFailed(93);
        }
        if (localInt * 3 != 30000)
        {
            PrintFailed(94);
        }
        if (-20000 != -2 * localInt)
        {
            PrintFailed(95);
        }
        
        if (20000 != -2 * localIntNeg)
        {
            PrintFailed(96);
        }
        
        // /
        if (localInt / 2 != 5000)
        {
            PrintFailed(97);
        }
        if (localInt / 250 != 40)
        {
            PrintFailed(98);
        }
        if (localInt / -2 != -5000)
        {
            PrintFailed(99);
        }
        if (localIntNeg / -2 != 5000)
        {
            PrintFailed(100);
        }
        if (2000 / 100 != 20)
        {
            PrintFailed(101);
        }
        if (2000 / 50 != 40)
        {
            PrintFailed(102);
        }
        if (2000 / 10 != 200)
        {
            PrintFailed(103);
        }
        if (-2000 / 100 != -20)
        {
            PrintFailed(104);
        }
        if (-2000 / 50 != -40)
        {
            PrintFailed(105);
        }
        if (int(2000) / -100 != -20)
        {
            PrintFailed(106);
        }
        if (int(2000) / -50 != -40)
        {
            PrintFailed(107);
        }
        if (int(2000) / -10 != -200)
        {
            PrintFailed(108);
        }
        if (-2000 / -100 != 20)
        {
            PrintFailed(109);
        }
        if (-2000 / -50 != 40)
        {
            PrintFailed(110);
        }
        if (-2000 / -10 != 200)
        {
            PrintFailed(111);
        }
        if (2000 / 1 != 2000)
        {
            PrintFailed(112);
        }
        if (2000 / 2 != 1000)
        {
            PrintFailed(113);
        }
        if (2000 / 4 != 500)
        {
            PrintFailed(114);
        }
        if (2000 / 8 != 250)
        {
            PrintFailed(115);
        }
        if (int(2000) / -1 != -2000)
        {
            PrintFailed(116);
        }
        if (int(2000) / -2 != -1000)
        {
            PrintFailed(117);
        }
        if (int(2000) / -4 != -500)
        {
            PrintFailed(118);
        }
        if (int(2000) / -8 != -250)
        {
            PrintFailed(119);
        }
        
        
        if (1*0 != 0)
        {
            PrintFailed(120);
        }
        if (0*1 != 0)
        {
            PrintFailed(121);
        }
        if (0*0 != 0)
        {
            PrintFailed(122);
        }
        if (1*2 != 2)
        {
            PrintFailed(123);
        }
        if (2*1 != 2)
        {
            PrintFailed(124);
        }
        if (7*11 != 77)
        {
            PrintFailed(125);
        }
        if (11*7 != 77)
        {
            PrintFailed(126);
        }
        if (303*2 != 606)
        {
            PrintFailed(127);
        }
        if (2*303 != 606)
        {
            PrintFailed(128);
        }
        if (303*3 != 909)
        {
            PrintFailed(129);
        }
        if (3*303 != 909)
        {
            PrintFailed(130);
        }
        if (303*4 != 1212)
        {
            PrintFailed(131);
        }
        if (4*303 != 1212)
        {
            PrintFailed(132);
        }
        if (303*8 != 2424)
        {
            PrintFailed(133);
        }
        if (8*303 != 2424)
        {
            PrintFailed(134);
        }
        
        int a = -49;
        int b = 229;
        if (a * b != -11221)
        {
            PrintFailed(135);
        }
        if (b * a != -11221)
        {
            PrintFailed(136);
        }
        
        // %
        if (localInt % 3 != 1)
        {
            PrintFailed(137);
        }
        
        int test = 0x1234;
        
        int testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(137);
        }
        
        testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed(138);
        }
        
    } // TestIntMath
	
	
    {
        
        TestConstants();
        TestEquals();
        TestLessThan();
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        TestUIntMath();
        TestIntMath();
        TestPropertyMath();
        
        Serial.WriteChar('O');//Serial.WriteChar('K');Serial.WriteChar('!');
    }
}

