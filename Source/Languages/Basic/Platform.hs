unit Platform
{
    uses "HopperCode"  // code related to Hopper VM byte code
    
    const uint timerResolution =   1000;  // fractions of a second of the current harware timer (ms in this case)
    const byte timeOnZeroPage  =   0xA0;  // (160) where GIBL thinks the time is on zeroPage (2 bytes)
    
    // GOSUB -> RETURN stack
#ifdef DYNAMICSTACK    
    const uint           callStackLimit     = 64;  // arbitrary but we do need some limit (~6 byte overhead per list item)
    <uint>               gCallStack;               // prefer a list rather than yet another array of wasted space
#else
    uint                 gosubCSP;
    const uint           callStackLimit     = 1024; // same size as above but much deeper and faster (always wastes 2K though
    uint[callStackLimit] gCallStack;             
#endif    

    int vA; int vB; int vC; int vD; int vE; int vF; int vG; int vH;  
    int vI; int vJ; int vK; int vL; int vM; int vN; int vO; int vP;  
    int vQ; int vR; int vS; int vT; int vU; int vV; int vW; int vX;  
    int vY; int vZ; 
    
    Conditions gCondition; // runtime state
    long gMillisBase;
    uint gRnd;
    
    delegate   bool IsBreakDelegate();
    delegate        PlatformPrintDelegate(string str);
    delegate        PlatformPrintRefDelegate(uint address);
    delegate string IntToStringDelegate(int i);
    delegate string UIntToHexStringDelegate(uint i, byte w);
    delegate        PlatformErrorDelegate(uint number);
    delegate        uint HopperLineToAddressDelegate(uint ln);
    delegate uint   PlatformReadWordDelegate(uint addresss);
    delegate        PlatformWriteWordDelegate(uint addresss, uint word);
    delegate int    PlatformGetDelegate();
    delegate int    PlatformRndDelegate();
    delegate        PlatformSeedDelegate(int seed);
    delegate bool   PlatformInputDelegate(byte iVariable, bool isString);
    delegate bool   PlatformPushReturnDelegate(uint address);
    delegate uint   PlatformPopReturnDelegate();
    
    // runtime function pointers accessed from the inlined VM opcodes:
    IntToStringDelegate             IntToStringPtr         { get { return Int.ToString; } }
    UIntToHexStringDelegate         UIntToStringHexPtr     { get { return UInt.ToHexString; } } 
    IsBreakDelegate                 IsBreakPtr             { get { return Platform.isBreak; } }
    PlatformErrorDelegate           ErrorPtr               { get { return Platform.error; } }
    HopperLineToAddressDelegate     HopperLineToAddressPtr { get { return HopperCode.lineToAddress; } }
    PlatformReadWordDelegate        ReadWordPtr            { get { return Platform.readWord;  } }
    PlatformWriteWordDelegate       WriteWordPtr           { get { return Platform.writeWord; } }
    PlatformGetDelegate             GetChPtr               { get { return Platform.getCh; } }
    PlatformRndDelegate             RndPtr                 { get { return Platform.random; } }
    PlatformSeedDelegate            SeedPtr                { get { return Platform.seedRnd; } }
    PlatformInputDelegate           InputPtr               { get { return Platform.input; } }
    PlatformPushReturnDelegate      PushReturnPtr          { get { return Platform.pushReturn; } }
    PlatformPopReturnDelegate       PopReturnPtr           { get { return Platform.popReturn; } }
    PlatformPrintDelegate           PrintPtr               { get { return Platform.print; } }
    PlatformPrintRefDelegate        PrintRefPtr            { get { return Platform.printRef; } }
    
    flags Conditions
    {
        None  = 0x00,
        Jump  = 0x01,
        Break = 0x02,
        Error = 0x04,
    }
    
    Conditions Condition  { get { return gCondition; }  set { gCondition = value; } }
    
    uint       Variables     { get { return &vA;                                  } }
    
    bool pushReturn(uint address)
    {
        //PrintLn("pushReturn=" + address.ToHexString(4) + " (" + (address + Runtime.UserCode).ToHexString(4) + ")");
        address = address + Runtime.UserCode;
#ifdef DYNAMICSTACK
        if (gCallStack.Count == callStackLimit)
        {
            error(7); // out of memory for call stack
            return false;
        }
        gCallStack.Append(address);
#else
        if (gosubCSP == callStackLimit)
        {
            error(7); // out of memory for call stack
            return false;
        }
        gCallStack[gosubCSP] = address;
        gosubCSP++;
#endif
        return true;
    }
    uint popReturn()
    {
#ifdef DYNAMICSTACK
        if (gCallStack.Count == 0)
        {
            error(8); // RETURN without GOSUB
            return 0;
        }
        uint address = gCallStack[gCallStack.Count-1];
        gCallStack.Remove(gCallStack.Count-1);
#else
        if (gosubCSP == 0)
        {
            error(8); // RETURN without GOSUB
            return 0;
        }
        gosubCSP--;
        uint address = gCallStack[gosubCSP];
#endif
        //PrintLn("popReturn=" + address.ToHexString(4));
        return address;
    }
    
    error(uint number) // only one overload here
    { 
        uint ln = HopperCode.GetCurrentLineNumber();
        Errors.Error(number, char(0), ln);
    }
    
    Clear() // called by "CLEAR" and before each "RUN"
    {
        // clears the variables
        for (byte i=0; i<26; i++)
        {
            setVariable(i, 0);
        }
        loop // just to avoid zero
        {
            <byte> bytes = (Time.Millis).ToBytes();
            gRnd = UInt.FromBytes(bytes[0], bytes[1]);
            if (gRnd != 0) { break; }
        }
#ifdef DYNAMICSTACK 
        gCallStack.Clear();
#else
        gosubCSP = 0;
#endif        
    }
    
    uint readWord(uint address)
    {
        uint value;   
        if (address == timeOnZeroPage)
        {
            long elapsed = Millis - gMillisBase;
            elapsed = elapsed / (timerResolution/10); // 1/10th of a second resolution
            value = uint(elapsed);
        }
        else
        {
            value = Memory.ReadWord(address);
        }
        return value;
    }
    writeWord(uint address, uint w)
    {
        if (address == timeOnZeroPage)
        {
            if (w != 0)
            {
                Error(18);
                return;
            }
            // Poke 0 to 160 sets the base time to zero
            gMillisBase = Millis;
        }
        else
        {
            Memory.WriteWord(address, w);
        }
    }
    
    print(string str)
    {
        Write(str);
    }
    printRef(uint address)
    {
        loop
        {
            byte b = ReadByte(address);
            if (b == 0)
            {
                break;
            }
            Write(char(b));
            address++;
        } 
    }
    int random()
    {
        // PRNG from here: 
        // https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
        gRnd = gRnd ^ (gRnd << 7);
        gRnd = gRnd ^ (gRnd >> 9);
        gRnd = gRnd ^ (gRnd << 8);
        return int(gRnd & 0x7FFF);
    }
    seedRnd(int seed)
    {
        if (seed == 0) // bad for the PRND
        {
            seed = 1;
        }
        if (seed < 0)
        {
            seed = -seed;
        }
        gRnd = CastIntToUInt(seed);
    }
    int getCh()
    {
        char ch;
        loop
        {
            if (IO.IsAvailable)
            {
                if (IO.IsBreak()) 
                {
                    Condition = Conditions.Break;
                    ch = char(0);
                    break;
                }
                ch = IO.Read();
                break;
            }
        } // loop
        return int(ch);
    }
    bool input(byte variableIndex, bool isString)
    {
        int value;
        string content;
        bool ok;
        loop
        {
            if (IO.IsAvailable)
            {
                if (IO.IsBreak()) 
                {
                    Condition = Conditions.Break;
                    ok = false;
                    break;
                }
                char ch = IO.Read();
                byte b = byte(ch);
                uint clength = content.Length;
                if (ch.IsDigit())
                {
                    content = content + ch;
                    ok = isString || Int.TryParse(content, ref value);
                    Write(ch);
                }
                else if ((clength == 0) && ((ch == '-') || (ch == '+')))
                {
                    content = content + ch;
                }
                else if ((ch == Char.EOL) && ok)
                {
                    break;
                }
                else if ((ch == char(0x08)) && (clength != 0))
                {
                    content = content.Substring(0, clength-1);
                    clength--;
                    // backspace
                    Write(char(0x08));
                    Write(' ');
                    Write(char(0x08));
                    ok = isString || Int.TryParse(content, ref value);
                }
                else if (ch == char(0x1B))
                {
                    while (clength != 0)
                    {
                        content = content.Substring(0, clength-1);
                        clength--;
                        // backspace
                        Write(char(0x08));
                        Write(' ');
                        Write(char(0x08));
                    }
                    ok = false;
                }
                else if (isString && (byte(ch) >= 32) && (byte(ch) < 127))
                {
                    content = content + ch; // any printable ASCII character
                    ok = true;
                    Write(ch);
                }
            }
        } // loop
        WriteLn();
        if (ok)
        {   
            if (isString)
            {
                setString(variableIndex, content);
            }
            else
            {
                setVariable(variableIndex, value);
            }
        }
        return ok; // ok
    }
    bool isBreak()
    {
        // avoid yet another method call to IsBreak()
        //    Serial.IsAvailable compiles to a SYSCALL0
        if (Serial.IsAvailable) 
        {
            if (IO.IsBreak()) 
            { 
                Write('!');
                Condition = Conditions.Break;
                return true;
            }
        }
        return false;
    }
    
    setVariable(byte variableIndex, int value)
    {
        // 26 lines of stupid looking code in exchange for speed (much faster than an array)
        switch (variableIndex)
        {
            case  0: { vA = value; }
            case  1: { vB = value; }
            case  2: { vC = value; }
            case  3: { vD = value; }
            case  4: { vE = value; }
            case  5: { vF = value; }
            case  6: { vG = value; }
            case  7: { vH = value; }
            case  8: { vI = value; }
            case  9: { vJ = value; }
            case 10: { vK = value; }
            case 11: { vL = value; }
            case 12: { vM = value; }
            case 13: { vN = value; }
            case 14: { vO = value; }
            case 15: { vP = value; }
            case 16: { vQ = value; }
            case 17: { vR = value; }
            case 18: { vS = value; }
            case 19: { vT = value; }
            case 20: { vU = value; }
            case 21: { vV = value; }
            case 22: { vW = value; }
            case 23: { vX = value; }
            case 24: { vY = value; }
            case 25: { vZ = value; }
        }
    }
    int getVariable(byte variableIndex)
    {
        // 26 lines of stupid looking code in exchange for speed (much faster than an array)
        switch (variableIndex)
        {
            case  0: { return vA; }
            case  1: { return vB; }
            case  2: { return vC; }
            case  3: { return vD; }
            case  4: { return vE; }
            case  5: { return vF; }
            case  6: { return vG; }
            case  7: { return vH; }
            case  8: { return vI; }
            case  9: { return vJ; }
            case 10: { return vK; }
            case 11: { return vL; }
            case 12: { return vM; }
            case 13: { return vN; }
            case 14: { return vO; }
            case 15: { return vP; }
            case 16: { return vQ; }
            case 17: { return vR; }
            case 18: { return vS; }
            case 19: { return vT; }
            case 20: { return vU; }
            case 21: { return vV; }
            case 22: { return vW; }
            case 23: { return vX; }
            case 24: { return vY; }
            case 25: { return vZ; }
        }
        return 0;
    }
    setString(byte variableIndex, string content)
    {
        int  iAddress = getVariable(variableIndex);
        uint stringAddress = iAddress.GetByte(0) + (iAddress.GetByte(1) << 8);
        uint i = 0;
        loop
        {
            if (i == content.Length)
            {
                Memory.WriteByte(stringAddress, 0); // null terminator
                break;
            }
            char ch = content[i];
            Memory.WriteByte(stringAddress, byte(ch));
            i++;
            stringAddress++;
        }
    }
}
