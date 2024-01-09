unit Library
{
    uses "/Source/Runtime/Platform/LibCalls"
    uses "/Source/Runtime/Platform/Wire"
    
    delegate ISRDelegate();
    
    bool isrExists;
    bool ISRExists { get { return isrExists; } }
    
    bool ExecuteLibCall(byte iLibCall, uint iOverload)
    {
        bool doNext = true;
        switch (LibCall(iLibCall))
        {
            case LibCall.WireBegin:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        bool result = HRWire.Begin(0);
                        Push(result ? 1 : 0, Type.Bool);
                    }
                    case 1:
                    {
                        Type ctype;
                        uint controller = Pop(ref ctype);
#ifdef CHECKED             
                        AssertByte(ctype, controller);
#endif   
                        bool result = HRWire.Begin(byte(controller));
                        Push(result ? 1 : 0, Type.Bool);
                    }
                }
            }
            case LibCall.WireBeginTx:
            {
                Type atype;
                uint address = Pop(ref atype);
#ifdef CHECKED             
                AssertByte(atype, address);
#endif   
                switch (iOverload)
                {
                    case 0:
                    {
                        HRWire.BeginTx(0, byte(address));
                    }
                    case 1:
                    {
                        Type ctype;
                        uint controller = Pop(ref ctype);
#ifdef CHECKED             
                        AssertByte(ctype, controller);
#endif   
                        HRWire.BeginTx(byte(controller), byte(address));
                    }
                }
            }
            case LibCall.WireWrite:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        Type atype;
                        uint b = Pop(ref atype);
#ifdef CHECKED             
                        AssertByte(atype, b);
#endif   
                        HRWire.Write(0, byte(b));
                    }
                    case 1:
                    {
                        Type atype;
                        uint b = Pop(ref atype);
                        Type ctype;
                        uint controller = Pop(ref ctype);
#ifdef CHECKED             
                        AssertByte(atype, b);
                        AssertByte(ctype, controller);
#endif   
                        HRWire.Write(byte(controller), byte(b));
                    }
                    case 2:
                    {
                        Type ltype;
                        uint length = Pop(ref ltype);
                        Type stype;
                        uint startIndex = Pop(ref stype);
                        Type atype;
                        uint hrarray = Pop(ref atype);
                        Type ctype;
                        uint controller = Pop(ref ctype);
#ifdef CHECKED             
                        AssertByte(ctype, controller);
                        AssertUInt(stype, startIndex);
                        AssertUInt(ltype, length);
                        AssertUInt(atype, hrarray);
                        if (atype != Type.Array)
                        {
                            ErrorDump(11);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif   
                        HRWire.Write(byte(controller), hrarray, startIndex, length);
                    }
                }
            }
            case LibCall.WireConfigure:
            {
                Type cltype;
                uint sclPin = Pop(ref cltype);
                Type datype;
                uint sdaPin = Pop(ref datype);
                Type ctype;
                uint controller = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ctype,  controller);
                AssertByte(cltype, sclPin);
                AssertByte(datype, sdaPin);
#endif
                HRWire.Configure(byte(controller), byte(sdaPin), byte(sclPin));
            }
            case LibCall.WireEndTx:
            {
                switch (iOverload)
                {
                    case 0:
                    {
                        byte result = HRWire.EndTx(0);
                        Push(result, Type.Byte);
                    }
                    case 1:
                    {
                        Type ctype;
                        uint controller = Pop(ref ctype);
#ifdef CHECKED             
                        AssertByte(ctype, controller);
#endif   
                        byte result = HRWire.EndTx(byte(controller));
                        Push(result, Type.Byte);
                    }
                }
            }
            
            case LibCall.MCUPinMode:
            {
                byte mode  = byte(Pop());
                byte pin   = byte(Pop());
                External.PinMode(pin, mode);
            }
            case LibCall.MCUDigitalWrite:
            {
                byte value = byte(Pop());
                byte pin   = byte(Pop());
                External.DigitalWrite(pin, value);
            }
            case LibCall.MCUDigitalRead:
            {
                byte pin   = byte(Pop());
                byte value = External.DigitalRead(pin);
                Push(value, Type.Byte);
            }
            case LibCall.MCUAnalogRead:
            {
                byte pin   = byte(Pop());
                uint value = External.AnalogRead(pin);
                Push(value, Type.UInt);
            }
            case LibCall.MCUAnalogWrite:
            {
                uint value = Pop();
                byte pin   = byte(Pop());
                External.AnalogWrite(pin, value);
            }
            case LibCall.MCUAnalogWriteResolution:
            {
                byte value = byte(Pop());
                External.AnalogWriteResolution(value);
            }
            case LibCall.MCUAttachToPin:
            {
                byte state = byte(Pop());
                ISRDelegate isrDelegate = ISRDelegate(Pop());
                byte pin = byte(Pop());
                bool result = External.AttachToPin(pin, isrDelegate, state);
                Push(result ? 1 : 0, Type.Bool);
                isrExists = true;
            }
          
            
            default:
            {
                Runtime.Out4Hex(PC);
                Serial.WriteChar(':');
                Serial.WriteChar('L');
                Runtime.Out2Hex(iLibCall);
                Serial.WriteChar(' ');
                WriteHex(PC); Write(':'); Write('L'); WriteHex(iLibCall); Write(' '); ErrorDump(132);
                Error = 0x0A; // not implemented
            }
        }
        return doNext && (Error == 0);
    }
}
