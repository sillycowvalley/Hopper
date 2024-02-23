program TestNumbers
{
#define MCU
    //uses "/Source/6502/System"
    uses "/Source/System/System"
    
#ifndef HOPPER_6502
#define TESTFLOATS
#endif
    
    uses "/Source/System/IO"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"

#ifdef TEXTBUFFER
    uses "/Source/Editor/TextBuffer"
#endif
    PrintFailed(string message)
    {
        WriteLn("  " + message);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
   
    TestEquals()
    { 
        WriteLn("'==' constants");
        
        // contants
        if (1 == 0)
        {
            PrintFailed("1 == 0 failed");
        }
        if (!(1 == 1))
        {
            PrintFailed("!(1 == 1) failed");
        }
        
#ifdef TESTFLOATS
        if (1.0 == 0)
        {
            PrintFailed("1.0 == 0 failed 1");
        }
        if (1 == 0.0)
        {
            PrintFailed("1 == 0.0 failed");
        }
        if (!(1.0 == 1.0))
        {
            PrintFailed("!(1.0 == 1.0) failed");
        }
        if (!(1.0 == 1))
        {
            PrintFailed("!(1.0 == 1) failed 1");
        }
        if (!(1 == 1.0))
        {
            PrintFailed("!(1 == 1.0) failed 1");
        }
#endif
        if ('a' == 'b')
        {
            PrintFailed("'a' == 'b' failed");
        }
        if (!('a' == 'a'))
        {
            PrintFailed("!('a' == 'a') failed");
        }
        if ("a" == "b")
        {
            PrintFailed("\"a\" == \"b\" failed");
        }
        if (!("a" == "a"))
        {
            PrintFailed("!(\"a\" == \"a\") failed");
        }
        
        WriteLn("'==' byte");
        byte ba = 1;
        byte bb = 0;
        byte bc = 1;
        if (!(ba == 1))
        {
            PrintFailed("(!ba == 1) failed");
        }
        if (ba == 0)
        {
            PrintFailed("ba == 0 failed");
        }
        if (!(1 == ba))
        {
            PrintFailed("(!1 == ba) failed");
        }
        if (0 == ba)
        {
            PrintFailed("0 == ba failed");
        }
        if (!(ba == ba))
        {
            PrintFailed("(!ba == ba) failed");
        }
        if (ba == bb)
        {
            PrintFailed("ba == bb failed");
        }
        if (!(ba == bc))
        {
            PrintFailed("(!ba == bc) failed");
        }

        WriteLn("'==' int");
        int ia = 1;
        int ib = 0;
        int ic = 1;
        if (!(ia == 1))
        {
            PrintFailed("(!ia == 1) failed");
        }
        if (ia == 0)
        {
            PrintFailed("ia == 0 failed");
        }
        if (!(1 == ia))
        {
            PrintFailed("(!1 == ia) failed");
        }
        if (0 == ia)
        {
            PrintFailed("0 == ia failed");
        }
        if (!(ia == ia))
        {
            PrintFailed("(!ia == ia) failed");
        }
        if (ia == ib)
        {
            PrintFailed("ia == ib failed");
        }
        if (!(ia == ic))
        {
            PrintFailed("(!ia == ic) failed");
        }
        if (!(ia == ba))
        {
            PrintFailed("(!ia == bb) failed");
        }
        if (ia == bb)
        {
            PrintFailed("(!ia == bb) failed");
        }
        if (!(ba == ia))
        {
            PrintFailed("(!ba == ib) failed");
        }
        if (ba == ib)
        {
            PrintFailed("(!ba == ib) failed");
        }
      
        WriteLn("'==' long");
        long la = 1;
        long lb = 0;
        long lc = 1;
        if (!(la == 1))
        {
            PrintFailed("(!la == 1) failed");
        }
        if (la == 0)
        {
            PrintFailed("la == 0 failed");
        }
        if (!(1 == la))
        {
            PrintFailed("(!1 == la) failed");
        }
        if (0 == la)
        {
            PrintFailed("0 == la failed");
        }
        if (!(la == la))
        {
            PrintFailed("(!la == la) failed");
        }
        if (la == lb)
        {
            PrintFailed("la == lb failed");
        }
        if (!(la == lc))
        {
            PrintFailed("(!la == lc) failed");
        }
        if (!(la == ba))
        {
            PrintFailed("(!la == ba) failed");
        }
        if (la == bb)
        {
            PrintFailed("(!la == bb) failed");
        }
        if (!(ba == la))
        {
            PrintFailed("(!ba == la) failed");
        }
        if (ba == lb)
        {
            PrintFailed("(!ba == lb) failed");
        }
        if (!(la == ia))
        {
            PrintFailed("(!la == ia) failed");
        }
        if (la == ib)
        {
            PrintFailed("(!la == ib) failed");
        }
        if (!(ia == la))
        {
            PrintFailed("(!ia == la) failed");
        }
        if (ia == lb)
        {
            PrintFailed("(!ia == lb) failed");
        }

        
#ifdef TESTFLOATS        
        WriteLn("'==' float");
        float fa = 1;
        float fb = 0;
        float fc = 1;
        if (!(fa == 1))
        {
            PrintFailed("(!fa == 1) failed");
        }
        if (fa == 0)
        {
            PrintFailed("fa == 0 failed");
        }
        if (!(1 == fa))
        {
            PrintFailed("(!1 == fa) failed");
        }
        if (0 == fa)
        {
            PrintFailed("0 == fa failed");
        }
        if (!(fa == fa))
        {
            PrintFailed("(!fa == fa) failed");
        }
        if (fa == fb)
        {
            PrintFailed("fa == fb failed");
        }
        if (!(fa == fc))
        {
            PrintFailed("(!fa == fc) failed");
        }
        if (!(fa == ba))
        {
            PrintFailed("(!fa == ba) failed");
        }
        if (fa == bb)
        {
            PrintFailed("(!fa == bb) failed");
        }
        if (!(ba == fa))
        {
            PrintFailed("(!ba == fa) failed");
        }
        if (ba == fb)
        {
            PrintFailed("(!ba == fb) failed");
        }
        if (!(fa == ia))
        {
            PrintFailed("(!fa == ia) failed");
        }
        if (fa == ib)
        {
            PrintFailed("(!fa == ib) failed");
        }
        if (!(ia == fa))
        {
            PrintFailed("(!ia == fa) failed");
        }
        if (ia == fb)
        {
            PrintFailed("(!ia == fb) failed");
        }
        if (!(fa == la))
        {
            PrintFailed("(!fa == la) failed");
        }
        if (fa == lb)
        {
            PrintFailed("(!fa == lb) failed");
        }
        if (!(la == fa))
        {
            PrintFailed("(!la == fa) failed");
        }
        if (la == fb)
        {
            PrintFailed("(!la == fb) failed");
        }
#endif // TESTFLOATS        
    }


#ifdef TESTFLOATS  
    const float  globalFloat = 3.141;
    const float  globalFloat2 = 4.141;
    
    const float globalFloat3  = 10000.0;
    const float globalFloat4  = 10000.1;
    
    
#endif
    
    const uint    globalUInt   = 10000;
    const uint    globalUInt2  = 20000;
    
    const long   globalLong  = 100000;
    const long   globalLong2  = 100001;
    const string globalConstant = "aaaaa";
    const int    globalInt  = 10000;
    const string globalConstant2 = "zzzzz";
    const string globalConstant3 = "aaaaa1";
    const int    globalInt2  = 10001;

    
    TestConstants()
    {
        WriteLn("Global const");
        
        string localConstant = "aaaaa";
#ifdef TESTFLOATS          
        float  localFloat = 3.141;
#endif        
        long   localLong  = 100000;
        int    localInt   = 10000;
        if (globalConstant != localConstant)
        {
            PrintFailed("global const string failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat != localFloat)
        {
            PrintFailed("global const float 1 failed");
        }
#endif        
        if (globalLong != localLong)
        {
            PrintFailed("global const long failed");
        }
        if (globalInt != localInt)
        {
            PrintFailed("global const int failed");
        }
#ifdef TESTFLOATS        
        localFloat = localFloat + 1;
#endif
        localLong = localLong + 1;
        localConstant = localConstant + "1";
        localInt = localInt + 1;
        if (globalConstant3 != localConstant)
        {
            PrintFailed("global const string 2 failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat2 != localFloat)
        {
            PrintFailed("global const float 2 failed");
        }
#endif
        if (globalLong2 != localLong)
        {
            PrintFailed("global const long 2 failed");
        }
        if (globalInt2 != localInt)
        {
            PrintFailed("global const int 2 failed");
        }
    }
    
    TestLessThan()
    {
        WriteLn("'<'");
        string localConstant = "zzzzz";
        int    localInt   = 10001;
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        
        uint localUInt0 =   12;
        uint localUInt1 =   23;
        uint localUInt2 = 1234;
        uint localUInt3 = 4567;
        uint localUInt4 = 256;
        uint localUInt5 = 257;
        if (!(localUInt0 < localUInt1))
        {
            PrintFailed("uint < failed 1");    
        }
        if (!(localUInt1 < localUInt2))
        {
            PrintFailed("uint < failed 2");    
        }
        if (!(localUInt2 < localUInt3))
        {
            PrintFailed("uint < failed 3");    
        }
        if (localUInt1 < localUInt0)
        {
            PrintFailed("uint < failed 4");    
        }
        if (localUInt2 < localUInt1)
        {
            PrintFailed("uint < failed 5");    
        }
        if (localUInt3 < localUInt2)
        {
            PrintFailed("uint < failed 6");    
        }
        if (localUInt0 < localUInt0)
        {
            PrintFailed("uint < failed 7");    
        }
        if (localUInt1 < localUInt1)
        {
            PrintFailed("uint < failed 8");    
        }
        if (localUInt2 < localUInt2)
        {
            PrintFailed("uint < failed 9");    
        }
        if (localUInt3 < localUInt3)
        {
            PrintFailed("uint < failed 10");    
        }
        if (!(localUInt4 < localUInt5))
        {
            PrintFailed("uint < failed 11");    
        }
        if (localUInt5 < localUInt4)
        {
            PrintFailed("uint < failed 12");    
        }
        if (localUInt4 < localUInt4)
        {
            PrintFailed("uint < failed 13");    
        }
        if (!(globalConstant < localConstant))
        {
            PrintFailed("string < failed");
        }
#ifdef TESTFLOATS          
        if (!(globalFloat < localFloat))
        {
            PrintFailed("float < failed");
        }
#endif        
        if (!(globalLong < localLong))
        {
            PrintFailed("long < failed");
        }
        if (!(globalInt < localInt))
        {
            PrintFailed("int < failed");
        }
        if (localConstant < globalConstant)
        {
            PrintFailed("string < failed");
        }
#ifdef TESTFLOATS          
        if (localFloat < globalFloat)
        {
            PrintFailed("float < failed");
        }
#endif
        if (localLong < globalLong)
        {
            PrintFailed("long < failed");
        }
        if (localInt < globalInt)
        {
            PrintFailed("int < failed");
        }
        if (!(localInt < localLong))
        {
            PrintFailed("int < long failed");
        }
        if (localLong < localInt)
        {
            PrintFailed("long < int failed");
        }
#ifdef TESTFLOATS          
        if (localInt < localFloat)
        {
            PrintFailed("int < float failed");
        }
        if (localLong < localFloat)
        {
            PrintFailed("long < float failed");
        }
        if (!(localFloat < localInt))
        {
            PrintFailed("float < int failed");
        }
        if (localLong < localFloat)
        {
            PrintFailed("float < long failed");
        }
#endif        
    }
    TestLessThanOrEqual()
    {
        WriteLn("'<='");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif        
        long   localLong  = 100001;
        int    localInt   = 10001;
        int localNegInt1  = -10000;
        int localNegInt2  = -10001;
        if (!(globalConstant <= localConstant))
        {
            PrintFailed("string <= failed");
        }
        if (!(globalConstant <= globalConstant))
        {
            PrintFailed("string <= failed");
        }
        if (!(localConstant <= localConstant))
        {
            PrintFailed("string <= failed");
        }
#ifdef TESTFLOATS          
        if (!(globalFloat <= localFloat))
        {
            PrintFailed("float <= failed");
        }
        if (!(globalFloat <= globalFloat))
        {
            PrintFailed("float <= failed");
        }
        if (!(localFloat <= localFloat))
        {
            PrintFailed("float <= failed");
        }
#endif        
        if (!(globalLong <= localLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(globalLong <= globalLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(localLong <= localLong))
        {
            PrintFailed("long <= failed");
        }
        if (!(globalInt <= localInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(globalInt <= globalInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(localNegInt2 <= localNegInt1))
        {
            PrintFailed("int <= failed");
        }
        if (!(localNegInt1 <= globalInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(localNegInt1 <= localInt))
        {
            PrintFailed("int <= failed");
        }
        
        if (!(localInt <= localInt))
        {
            PrintFailed("int <= failed");
        }
        if (!(localInt <= 20001))
        {
            PrintFailed("int <= failed 1");
        }
        if (localInt <= 5001)
        {
            PrintFailed("int <= failed 2");
        }
        if (!(localInt <= 10001))
        {
            PrintFailed("int <= failed 3");
        }
        
        if (localConstant <= globalConstant)
        {
            PrintFailed("string <= failed");
        }
#ifdef TESTFLOATS          
        if (localFloat <= globalFloat)
        {
            PrintFailed("float <= failed");
        }
#endif
        if (localLong <= globalLong)
        {
            PrintFailed("long <= failed");
        }
        if (localInt <= globalInt)
        {
            PrintFailed("int <= failed");
        }
        
        if (!(localInt <= localLong))
        {
            PrintFailed("int <= long failed");
        }
        if (localLong <= localInt)
        {
            PrintFailed("long <= int failed");
        }
#ifdef TESTFLOATS  
        if (localInt <= localFloat)
        {
            PrintFailed("int <= float failed");
        }
        if (localLong <= localFloat)
        {
            PrintFailed("long <= float failed");
        }
        if (!(localFloat <= localInt))
        {
            PrintFailed("float <= int failed");
        }
        if (localLong <= localFloat)
        {
            PrintFailed("float <= long failed");
        }
#endif
    }
    
    TestGreaterThan()
    {
        WriteLn("'>'");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS          
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        int    localInt   = 10001;

        if (!(localConstant > globalConstant))
        {
            PrintFailed("string > failed");
        }
        if (globalConstant > localConstant)
        {
            PrintFailed("string > failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat > localFloat)
        {
            PrintFailed("float > failed");
        }
#endif
        if (globalLong > localLong)
        {
            PrintFailed("long > failed");
        }
        if ((globalInt > localInt))
        {
            PrintFailed("int > failed");
        }
        if ((localInt > localInt))
        {
            PrintFailed("int > failed 2");
        }
#ifdef TESTFLOATS  
        if (!(localFloat > globalFloat))
        {
            PrintFailed("float > failed");
        }
#endif
        if (!(localLong > globalLong))
        {
            PrintFailed("long > failed");
        }
        if (!(localInt > globalInt))
        {
            PrintFailed("int > failed");
        }
        if (localInt > localLong)
        {
            PrintFailed("int > long failed");
        }
        if (!(localLong > localInt))
        {
            PrintFailed("long > int failed");
        }
#ifdef TESTFLOATS  
        if (!(localInt > localFloat))
        {
            PrintFailed("int > float failed");
        }
        if (!(localLong > localFloat))
        {
            PrintFailed("long > float failed");
        }
        if (localFloat > localInt)
        {
            PrintFailed("float > int failed");
        }
        if (!(localLong > localFloat))
        {
            PrintFailed("float > long failed");
        }
#endif
    }
    
    TestGreaterThanOrEqual()
    {
        WriteLn("'>='");
        string localConstant = "zzzzz";
#ifdef TESTFLOATS  
        float  localFloat = 4.141;
#endif
        long   localLong  = 100001;
        int    localInt   = 10001;
        if (globalConstant >= localConstant)
        {
            PrintFailed("string >= failed");
        }
        if (!(globalConstant >= globalConstant))
        {
            PrintFailed("string >= failed");
        }
        if (!(localConstant >= localConstant))
        {
            PrintFailed("string >= failed");
        }
#ifdef TESTFLOATS          
        if (globalFloat >= localFloat)
        {
            PrintFailed("float >= failed");
        }
        if (!(globalFloat >= globalFloat))
        {
            PrintFailed("float >= failed");
        }
        if (!(localFloat >= localFloat))
        {
            PrintFailed("float >= failed");
        }
#endif
        if (globalLong >= localLong)
        {
            PrintFailed("long >= failed");
        }
        if (!(globalLong >= globalLong))
        {
            PrintFailed("long >= failed");
        }
        if (!(localLong >= localLong))
        {
            PrintFailed("long >= failed");
        }
        if (globalInt >= localInt)
        {
            PrintFailed("int >= failed");
        }
        if (!(globalInt >= globalInt))
        {
            PrintFailed("int >= failed");
        }
        if (!(localInt >= localInt))
        {
            PrintFailed("int >= failed");
        }
        if (!(localConstant >= globalConstant))
        {
            PrintFailed("string >= failed");
        }
#ifdef TESTFLOATS          
        if (!(localFloat >= globalFloat))
        {
            PrintFailed("float >= failed");
        }
#endif
        if (!(localLong >= globalLong))
        {
            PrintFailed("long >= failed");
        }
        if (!(localInt >= globalInt))
        {
            PrintFailed("int >= failed");
        }
        
        if (localInt >= localLong)
        {
            PrintFailed("int >= long failed");
        }
        if (!(localLong >= localInt))
        {
            PrintFailed("long >= int failed");
        }
#ifdef TESTFLOATS         
        if (!(localInt >= localFloat))
        {
            PrintFailed("int >= float failed");
        }
        if (!(localLong >= localFloat))
        {
            PrintFailed("long >= float failed");
        }
        if (localFloat >= localInt)
        {
            PrintFailed("float >= int failed");
        }
        if (!(localLong >= localFloat))
        {
            PrintFailed("float >= long failed");
        }
#endif
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
        WriteLn("'property' math");
        Prop = 42;
        Prop += 1;
        if (Prop != 43)
        {
            PrintFailed("'property' 1");        
        }
        Prop -= 1;
        if (Prop != 42)
        {
            PrintFailed("'property' 2");        
        }
        Prop = 3;
        Prop *= 6;
        if (Prop != 18)
        {
            PrintFailed("'property' 3");        
        }
        Prop /= 6;
        if (Prop != 3)
        {
            PrintFailed("'property' 4");        
        }
        Prop++;
        if (Prop != 4)
        {
            PrintFailed("'property' 5");
        }
        Prop--;
        if (Prop != 3)
        {
            PrintFailed("'property' 6");
        }
        
        fProp |= PFlags.Four;
        if (fProp != PFlags.Four)
        {
            PrintFailed("'property' 7");
        }
        fProp &= PFlags.Two;
        if (fProp != PFlags.None)
        {
            PrintFailed("'property' 8");
        }
        
        FProp |= PFlags.Four;
        if (FProp != PFlags.Four)
        {
            PrintFailed("'property' 9");
        }
        FProp &= PFlags.Two;
        if (FProp != PFlags.None)
        {
            PrintFailed("'property' 10");
        }
        
        string str = "Bob";
        str += " and Jane";
        if (str != "Bob and Jane")
        {
            PrintFailed("'property' 11");
        }
        str += '!';
        if (str != "Bob and Jane!")
        {
            PrintFailed("'property' 12");
        }
        
    }

    TestUIntMath()
    {
        WriteLn("'uint' math");
        
        // globalUInt   = 10000;
        // globalUInt2  = 20000;
        uint localUInt  = 10000;
        uint localUInt2 = 20000;
        
        // +
        if (localUInt + localUInt != 20000)
        {
            PrintFailed("'uint' 1");        
        }
        if (localUInt2 + localUInt2 != 40000)
        {
            PrintFailed("'uint' 2");        
        }
        if (globalUInt + localUInt != 20000)
        {
            PrintFailed("'uint' 3");        
        }
        if (globalUInt2 + localUInt2 != 40000)
        {
            PrintFailed("'uint' 4");        
        }
        if (localUInt2 + localUInt2 + globalUInt2 != 60000)
        {
            PrintFailed("'uint' 5");
        }
        
        // -
        if (localUInt2 - localUInt != 10000)
        {
            PrintFailed("'uint' 6");
        }
        if (localUInt2 - localUInt - localUInt != 0)
        {
            PrintFailed("'uint' 7");
        }
        
        // *
        if (localUInt2 != 2 * localUInt)
        {
            PrintFailed("'uint' 8");
        }
        if (localUInt * 5 != 50000)
        {
            PrintFailed("'uint' 12");
        }
        
        // /
        if (localUInt2 / 2 != localUInt)
        {
            PrintFailed("'uint' 9");
        }
        if (localUInt / 250 != 40)
        {
            PrintFailed("'uint' 10");
        }
        
        // %
        if (localUInt % 3 != 1)
        {
            PrintFailed("'uint' 11");
        }
        
        long total;
        for (uint fi = 0; fi < 400; fi = fi + 27)
        {
            for (uint si = 0; si < 150; si = si + 13)
            {
                uint prod = si * fi;
                total = total + prod;
                prod = fi * si;
                total = total + prod;
            }
        }
        if (total != 4864860)
        {
            PrintFailed("'uint' 12");
        }
        total = 0;
        for (uint fi = 0; fi < 10; fi = fi + 1)
        {
            for (uint si = 0; si < 25; si = si + 1)
            {
                uint prod = si * fi;
                total = total + prod;
                prod = fi * si;
                total = total + prod;
            }
        }
        if (total != 27000)
        {
            PrintFailed("'uint' 13");
        }
        
        uint test = 0xAA55;
        
        uint testAfter = UInt.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("'uint' 14");
        }
        
        <byte> bytes = test.ToBytes();
        testAfter = UInt.FromBytes(bytes[0], bytes[1]);
        if (test != testAfter)
        {
            PrintFailed("'uint' 15");
        }
    } // TestUIntMath
    
    TestIntMath()
    {
        WriteLn("'int' math");
        
        // globalInt   = 10000;
        // globalInt2  = 10001;
        int localInt  = 10000;
        int localInt2 = 10001;
        int localIntNeg = 0 - localInt;
        
        <byte> lb = localInt.ToBytes();
        if (lb.Count != 2)
        {
            PrintFailed("'int' ToBytes 1 failed");        
        }
        if (lb[0] != 0x10)
        {
            PrintFailed("'int' ToBytes 2  failed");        
        }
        if (lb[1] != 0x27)
        {
            PrintFailed("'int' ToBytes 3  failed");        
        }
        // +
        if (localInt + localInt != 20000)
        {
            PrintFailed("'int' 1");        
        }
        if (localInt2 + localInt2 != 20002)
        {
            PrintFailed("'int' 2");        
        }
        if (globalInt + localInt != 20000)
        {
            PrintFailed("'int' 3");        
        }
        if (globalInt2 + localInt2 != 20002)
        {
            PrintFailed("'int' 4");        
        }
        if (localInt2 + localInt2 + globalInt2 != 30003)
        {
            PrintFailed("'int' 5");
        }
        
        // -
        if (localInt2 - localInt != 1)
        {
            PrintFailed("'int' 6");
        }
        if (localInt - localInt2 - localInt2 != -10002)
        {
            PrintFailed("'int' 7");
        }
        if (-localInt2 != -10001)
        {
            PrintFailed("'int' 12");
        }
        
        // *
        if (20000 != 2 * localInt)
        {
            PrintFailed("'int' 8");
        }
        if (localInt * 3 != 30000)
        {
            PrintFailed("'int' 9");
        }
        if (-20000 != -2 * localInt)
        {
            PrintFailed("'int' 10");
        }
        
        if (20000 != -2 * localIntNeg)
        {
            PrintFailed("'int' 11");
        }
        
        // /
        if (localInt / 2 != 5000)
        {
            PrintFailed("'int' 12");
        }
        if (localInt / 250 != 40)
        {
            PrintFailed("'int' 13");
        }
        if (localInt / -2 != -5000)
        {
            PrintFailed("'int' 14");
        }
        if (localIntNeg / -2 != 5000)
        {
            PrintFailed("'int' 15");
        }
        if (2000 / 100 != 20)
        {
            PrintFailed("'int' 16");
        }
        if (2000 / 50 != 40)
        {
            PrintFailed("'int' 17");
        }
        if (2000 / 10 != 200)
        {
            PrintFailed("'int' 31");
        }
        if (-2000 / 100 != -20)
        {
            PrintFailed("'int' 18");
        }
        if (-2000 / 50 != -40)
        {
            PrintFailed("'int' 19");
        }
        if (int(2000) / -100 != -20)
        {
            PrintFailed("'int' 20");
        }
        if (int(2000) / -50 != -40)
        {
            PrintFailed("'int' 21");
        }
        if (int(2000) / -10 != -200)
        {
            PrintFailed("'int' 32");
        }
        if (-2000 / -100 != 20)
        {
            PrintFailed("'int' 22");
        }
        if (-2000 / -50 != 40)
        {
            PrintFailed("'int' 23");
        }
        if (-2000 / -10 != 200)
        {
            PrintFailed("'int' 33");
        }
        if (2000 / 1 != 2000)
        {
            PrintFailed("'int' 24");
        }
        if (2000 / 2 != 1000)
        {
            PrintFailed("'int' 25");
        }
        if (2000 / 4 != 500)
        {
            PrintFailed("'int' 26");
        }
        if (2000 / 8 != 250)
        {
            PrintFailed("'int' 27");
        }
        if (int(2000) / -1 != -2000)
        {
            PrintFailed("'int' 28");
        }
        if (int(2000) / -2 != -1000)
        {
            PrintFailed("'int' 29");
        }
        if (int(2000) / -4 != -500)
        {
            PrintFailed("'int' 30");
        }
        if (int(2000) / -8 != -250)
        {
            PrintFailed("'int' 31");
        }
        
        
        if (1*0 != 0)
        {
            PrintFailed("'int' 34");
        }
        if (0*1 != 0)
        {
            PrintFailed("'int' 35");
        }
        if (0*0 != 0)
        {
            PrintFailed("'int' 36");
        }
        if (1*2 != 2)
        {
            PrintFailed("'int' 38");
        }
        if (2*1 != 2)
        {
            PrintFailed("'int' 39");
        }
        if (7*11 != 77)
        {
            PrintFailed("'int' 40");
        }
        if (11*7 != 77)
        {
            PrintFailed("'int' 41");
        }
        if (303*2 != 606)
        {
            PrintFailed("'int' 42");
        }
        if (2*303 != 606)
        {
            PrintFailed("'int' 43");
        }
        if (303*3 != 909)
        {
            PrintFailed("'int' 44");
        }
        if (3*303 != 909)
        {
            PrintFailed("'int' 45");
        }
        if (303*4 != 1212)
        {
            PrintFailed("'int' 46");
        }
        if (4*303 != 1212)
        {
            PrintFailed("'int' 47");
        }
        if (303*8 != 2424)
        {
            PrintFailed("'int' 48");
        }
        if (8*303 != 2424)
        {
            PrintFailed("'int' 49");
        }
        
        int a = -49;
        int b = 229;
        if (a * b != -11221)
        {
            PrintFailed("'int' 50");
        }
        if (b * a != -11221)
        {
            PrintFailed("'int' 51");
        }
        long total;
        for (int fi = -100; fi < 50; fi = fi + 27)
        {
            for (int si = -30; si < 110; si = si + 13)
            {
                int prod = si * fi;
                total = total + prod;
                prod = fi * si;
                total = total + prod;
            }
        }
        if (total != -150150)
        {
            PrintFailed("'int' 52");
        }
        total = 0;
        for (int fi = -25; fi < 10; fi = fi + 1)
        {
            for (int si = -10; si < 25; si = si + 1)
            {
                int prod = si * fi;
                total = total + prod;
                prod = fi * si;
                total = total + prod;
            }
        }
        if (total != -137200)
        {
            PrintFailed("'int' 52");
        }
        
        // %
        if (localInt % 3 != 1)
        {
            PrintFailed("'int' 16");
        }
        
        int test = 0x1234;
        
        int testAfter = Int.FromBytes(test.GetByte(0), test.GetByte(1));
        if (test != testAfter)
        {
            PrintFailed("'int' 17");
        }
        
        <byte> bytes = test.ToBytes();
        testAfter = Int.FromBytes(bytes[0], bytes[1]);
        if (test != testAfter)
        {
            PrintFailed("'int' 18");
        }
        
    } // TestIntMath
	
	
    TestLongMath()
    {
        WriteLn("'long' math");
        
        // globalLong   = 100000;
        // globalLong2  = 100001;
        long localLong  = 100000;
        long localLong2 = 100001;
        long localLongNeg = 0 - localLong;
        
        // +
        if (localLong + localLong != 200000)
        {
            PrintFailed("'long' 1");        
        }
        if (localLong2 + localLong2 != 200002)
        {
            PrintFailed("'long' 2");        
        }
        if (globalLong + localLong != 200000)
        {
            PrintFailed("'long' 3");        
        }
        if (globalLong2 + localLong2 != 200002)
        {
            PrintFailed("'long' 4");        
        }
        if (localLong2 + localLong2 + globalLong2 != 300003)
        {
            PrintFailed("'long' 5");
        }
        
        // -
        if (localLong2 - localLong != 1)
        {
            PrintFailed("'long' 6");
        }
        if (localLong - localLong2 - localLong2 != -100002)
        {
            PrintFailed("'long' 7");
        }
        if (-localLong2 != -100001)
        {
            PrintFailed("'long' 8");
        }
        
        // *
        if (200000 != 2 * localLong)
        {
            PrintFailed("'long' 9");
        }
        if (localLong * 3 != 300000)
        {
            PrintFailed("'long' 10");
        }
        if (-200000 != -2 * localLong)
        {
            PrintFailed("'long' 11");
        }
        
        if (200000 != -2 * localLongNeg)
        {
            PrintFailed("'long' 12");
        }
        
        // /
        if (localLong / 2 != 50000)
        {
            PrintFailed("'long' 13");
        }
        
        if (localLong / 2500 != 40)
        {
            PrintFailed("'long' 14");
        }
        if (localLong / -2 != -50000)
        {
            PrintFailed("'long' 15");
        }
        if (localLongNeg / -2 != 50000)
        {
            PrintFailed("'long' 16");
        }
        
        // %
        if (localLong % 3 != 1)
        {
            PrintFailed("'long' 17");
        }
        //if (localLongNeg % 3 != 1)
        //{
        //    PrintFailed("'long' 18");
        //}
        
        long longRef = 10;
        long longResult = longRef * longRef;
        if (longResult != 100)
        {
            PrintFailed("'long' 18");
        }
        longResult = longRef + longRef;
        if (longResult != 20)
        {
            PrintFailed("'long' 19");
        }
        longRef = 10;
        longRef = longRef + 1;
        if (longRef != 11)
        {
            PrintFailed("'long' 20");
        }
        longRef = 10;
        longRef = longRef * longRef;
        if (longRef != 100)
        {
            PrintFailed("'long' 21");
        }
        longRef = 10;
        longRef = longRef * 5;
        if (longRef != 50)
        {
            PrintFailed("'long' 22");
        }
        longRef = 10;
        longRef = 5 * longRef;
        if (longRef != 50)
        {
            PrintFailed("'long' 23");
        }
        
        longRef = 10;
        longRef = 1 + longRef;
        if (longRef != 11)
        {
            PrintFailed("'long' 24");
        }
        longRef = 10;
        longRef = longRef + longRef;
        if (longRef != 20)
        {
            PrintFailed("'long' 25");
        }
        longRef = 10;
        longRef = longRef + 5;
        if (longRef != 15)
        {
            PrintFailed("'long' 26");
        }
        longRef = 10;
        longRef = 5 + longRef;
        if (longRef != 15)
        {
            PrintFailed("'long' 27");
        }
        
        long test = 0x55AA55AA;
        
        long testAfter = Long.FromBytes(test.GetByte(0), test.GetByte(1), test.GetByte(2), test.GetByte(3));
        if (test != testAfter)
        {
            PrintFailed("'long' 28");
        }
        
        <byte> bytes = test.ToBytes();
        testAfter = Long.FromBytes(bytes[0], bytes[1], bytes[2], bytes[3]);
        if (test != testAfter)
        {
            PrintFailed("'long' 29");
        }
        
        longRef = 42;
        longRef++;
        if (longRef != 43)
        {
            PrintFailed("'long' 30");
        }
        longRef--;
        if (longRef != 42)
        {
            PrintFailed("'long' 31");
        }
        longRef += 4;
        if (longRef != 46)
        {
            PrintFailed("'long' 32");
        }
        longRef -= 4;
        if (longRef != 42)
        {
            PrintFailed("'long' 33");
        }
        longRef = 4;
        longRef *= 6;
        if (longRef != 24)
        {
            PrintFailed("'long' 34");
        }
        longRef /= 4;
        if (longRef != 6)
        {
            PrintFailed("'long' 35");
        }
        
    } // TestLongMath
    
#ifdef TESTFLOATS
    TestFloatMath()
    {
        WriteLn("'float' math");
        
        float test = 3.141;
        
        float testAfter = Float.FromBytes(test.GetByte(0), test.GetByte(1), test.GetByte(2), test.GetByte(3));
        if (test != testAfter)
        {
            PrintFailed("'float' 28");
        }
        
        <byte> bytes = test.ToBytes();
        testAfter = Float.FromBytes(bytes[0], bytes[1], bytes[2], bytes[3]);
        if (test != testAfter)
        {
            PrintFailed("'float' 29");
        }
        
        // globalFloat3  = 10000.0;
        // globalFloat4  = 10000.1;
        float localFloat  = 10000.0;
        float localFloat2 = 10000.1;
        float localFloatNeg = 0 - localFloat;
        
        // +
        if (localFloat + localFloat != 20000.0)
        {
            PrintFailed("'float' 1");        
        }
        if (localFloat2 + localFloat2 != 20000.2)
        {
            PrintFailed("'float' 2");        
        }
        if (globalFloat3 + localFloat != 20000.0)
        {
            PrintFailed("'float' 3");        
        }
        if (globalFloat4 + localFloat2 != 20000.2)
        {
            PrintFailed("'float' 4");        
        }
        
        if (-localFloat2 != -10000.1)
        {
            PrintFailed("'float' 8");
        }
        
        // *
        if (20000.0 != 2 * localFloat)
        {
            PrintFailed("'float' 9");
        }
        if (localFloat * 3 != 30000.0)
        {
            PrintFailed("'float' 10");
        }
        if (-20000.0 != -2 * localFloat)
        {
            PrintFailed("'float' 11");
        }
        
        if (20000.0 != -2 * localFloatNeg)
        {
            PrintFailed("'float' 12");
        }
        
        // /
        if (localFloat / 2 != 5000.0)
        {
            PrintFailed("'float' 13");
        }
        
        if (localFloat / 2500 != 4.0)
        {
            PrintFailed("'float' 14");
        }
        if (localFloat / -2 != -5000.0)
        {
            PrintFailed("'float' 15");
        }
        if (localFloatNeg / -2 != 5000.0)
        {
            PrintFailed("'float' 16");
        }
        
        
        float floatRef = 10.0;
        float floatResult = floatRef * floatRef;
        if (floatResult != 100.0)
        {
            PrintFailed("'float' 18");
        }
        floatResult = floatRef + floatRef;
        if (floatResult != 20.0)
        {
            PrintFailed("'float' 19");
        }
        floatRef = 10.0;
        floatRef = floatRef + 1;
        if (floatRef != 11.0)
        {
            PrintFailed("'float' 20");
        }
        floatRef = 10.0;
        floatRef = floatRef * floatRef;
        if (floatRef != 100.0)
        {
            PrintFailed("'float' 21");
        }
        floatRef = 10.0;
        floatRef = floatRef * 5;
        if (floatRef != 50.0)
        {
            PrintFailed("'float' 22");
        }
        floatRef = 10.0;
        floatRef = 5 * floatRef;
        if (floatRef != 50.0)
        {
            PrintFailed("'float' 23");
        }
        
        floatRef = 10;
        floatRef = 1 + floatRef;
        if (floatRef != 11.0)
        {
            PrintFailed("'float' 24");
        }
        floatRef = 10;
        floatRef = floatRef + floatRef;
        if (floatRef != 20.0)
        {
            PrintFailed("'float' 25");
        }
        floatRef = 10;
        floatRef = floatRef + 5;
        if (floatRef != 15.0)
        {
            PrintFailed("'float' 26");
        }
        floatRef = 10;
        floatRef = 5 + floatRef;
        if (floatRef != 15.0)
        {
            PrintFailed("'float' 27");
        }
        
        float ya = 1.6;
        float xa = 0.04 * ya;
        
        if (xa.ToString() != "0.064")
        {
            WriteLn(xa.ToString());
            PrintFailed("'float' 30");
        }
        
        
        float x = 0.0;
        float y = 0.0;
        float xn =  0.85 * x + 0.04 * y;
        float yn = -0.04 * x + 0.85 * y + 1.6;
        
        if ((xn.ToString() != "0") || (yn.ToString() != "1.6"))
        {
            WriteLn(xn.ToString() + "," + yn.ToString());
            PrintFailed("'float' 31");
        }
        x = xn;
        y = yn;
        
        xn =   0.85 * x  + 0.04 * y;
        yn =  -0.04 * x  + 0.85 * y + 1.6;
        
        if ((xn.ToString() != "0.064") || (yn.ToString() != "2.96"))
        {
            WriteLn(xn.ToString() + "," + yn.ToString());
            PrintFailed("'float' 32");
        }
        
        
        
    } // TestFloatMath(..)
#endif    
    
    {
        //EchoToLCD = true;
        //Screen.Clear();

#ifdef TESTFLOATS
        TestFloatMath();
#endif
        
        TestEquals();
        TestGreaterThan();
        TestGreaterThanOrEqual();
        TestLessThanOrEqual();
        TestLessThan();
        
        TestConstants();
        
        TestLongMath();
        TestUIntMath();
        TestIntMath();
        
        TestPropertyMath();
        
        WriteLn();
        WriteLn("TestNumbers Ok");
#ifndef MCU
        Key key = ReadKey();
#endif
    }
}

