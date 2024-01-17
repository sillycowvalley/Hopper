unit Library
{
    uses "/Source/Runtime/Platform/LibCalls"
    uses "/Source/Runtime/Platform/Wire"
    uses "/Source/Runtime/Platform/SPI"
    uses "/Source/Runtime/Platform/NeoPixel"
    
    delegate ISRDelegate();
    delegate HandlerDelegate(string uri, string method, <string,string> arguments);
    
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
                        if (atype != Type.Array)
                        {
                            ErrorDump(11);
                            Error = 0x0B; // system failure (internal error)
                        }
#endif   
                        HRWire.Write(byte(controller), hrarray, startIndex, length);
                        GC.Release(hrarray);
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
            case LibCall.MCUReboot:
            {
                External.MCUReboot();
                doNext = false;
            }
            
            case LibCall.SPIBegin:
            {
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif   
                }
                bool result = HRSPI.Begin(byte(spiController));
                Push(result ? 1 : 0, Type.Bool);
            }
            case LibCall.SPIBeginTransaction:
            {
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif   
                }
                HRSPI.BeginTransaction(byte(spiController));
            }
            case LibCall.SPIEndTransaction:
            {
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif   
                }
                HRSPI.EndTransaction(byte(spiController));
            }
            case LibCall.SPISetCSPin:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
                Type ctype;
                uint spiController = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
                AssertByte(ctype, spiController);
#endif   
                HRSPI.SetCSPin(byte(spiController), byte(pin));
            }
            case LibCall.SPIGetCSPin:
            {
                Type ctype;
                uint spiController = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
                AssertByte(ctype, spiController);
#endif   
                byte pin = HRSPI.GetCSPin(byte(spiController));
                Push(pin, Type.Byte);
            }
            case LibCall.SPISetClkPin:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
                Type ctype;
                uint spiController = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
                AssertByte(ctype, spiController);
#endif
                HRSPI.SetClkPin(byte(spiController), byte(pin));
            }
            case LibCall.SPISetTxPin:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
                Type ctype;
                uint spiController = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
                AssertByte(ctype, spiController);
#endif
                HRSPI.SetTxPin(byte(spiController), byte(pin));
            }
            case LibCall.SPISetRxPin:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
                Type ctype;
                uint spiController = Pop(ref ctype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
                AssertByte(ctype, spiController);
#endif
                HRSPI.SetRxPin(byte(spiController), byte(pin));
            }
            
            case LibCall.SPICSPinGet:
            {
                byte value = HRSPI.GetCSPin(0);
                Push(value, Type.Byte);
            }
            case LibCall.SPICSPinSet:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
#endif
                HRSPI.SetCSPin(0, byte(pin));
            }
            case LibCall.SPIClkPinSet:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
#endif
                HRSPI.SetClkPin(0, byte(pin));
            }
            case LibCall.SPITxPinSet:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
#endif
                HRSPI.SetTxPin(0, byte(pin));
            }
            case LibCall.SPIRxPinSet:
            {
                Type ptype;
                uint pin = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
#endif
                HRSPI.SetRxPin(0, byte(pin));
            }
            case LibCall.SPIReadByte:
            {
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                byte data = HRSPI.ReadByte(byte(spiController));
                Push(data, Type.Byte);
            }
            case LibCall.SPIWriteByte:
            {
                Type dtype;
                uint data = Pop(ref dtype);
#ifdef CHECKED             
                AssertByte(dtype, data);
#endif
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                HRSPI.WriteByte(byte(spiController), byte(data));
            }
            case LibCall.SPIReadWord:
            {
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                uint data = HRSPI.ReadWord(byte(spiController));
                Push(data, Type.UInt);
            }
            case LibCall.SPIWriteWord:
            {
                Type dtype;
                uint data = Pop(ref dtype);
#ifdef CHECKED             
                AssertUInt(dtype, data);
#endif
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                HRSPI.WriteWord(byte(spiController), data);
            }
            case LibCall.SPIReadBuffer:
            {
                Type ltype;
                uint length = Pop(ref ltype);
                Type stype;
                uint startIndex = Pop(ref stype);
                Type dtype;
                uint hrdata = Pop(ref dtype);
#ifdef CHECKED             
                AssertUInt(ltype, length);
                AssertUInt(stype, startIndex);
                if (dtype != Type.Array)
                {
                    ErrorDump(165);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                HRSPI.ReadBuffer(byte(spiController), hrdata, startIndex, length);
                GC.Release(hrdata);
            }
            case LibCall.SPIWriteBuffer:
            {
                Type ltype;
                uint length = Pop(ref ltype);
                Type stype;
                uint startIndex = Pop(ref stype);
                Type dtype;
                uint hrdata = Pop(ref dtype);
#ifdef CHECKED             
                AssertUInt(ltype, length);
                AssertUInt(stype, startIndex);
                if (dtype != Type.Array)
                {
                    ErrorDump(165);
                    Error = 0x0B; // system failure (internal error)
                }
#endif
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif
                }
                HRSPI.WriteBuffer(byte(spiController), hrdata, startIndex, length);
                GC.Release(hrdata);
            }
            case LibCall.SPISettings:
            {
                // long speedMaximum, DataOrder dataOrder, DataMode dataMode
                Type mtype;
                DataMode dataMode = DataMode(Pop(ref mtype));
                Type otype;
                DataOrder dataOrder = DataOrder(Pop(ref otype));
                Type stype;
                uint hrspeed = Pop(ref stype);
#ifdef CHECKED             
                AssertByte(mtype, uint(dataMode));
                AssertByte(otype, uint(dataOrder));
                if (atypestype != Type.Long)
                {
                    ErrorDump(170);
                    Error = 0x0B; // system failure (internal error)
                }
#endif                   
                uint spiController = 0;
                if (iOverload == 1)
                {
                    Type ctype;
                    spiController = Pop(ref ctype);
#ifdef CHECKED             
                    AssertByte(ctype, spiController);
#endif   
                }
                HRSPI.Settings(byte(spiController), hrspeed, dataOrder, dataMode);
                GC.Release(hrspeed);
            }
            case LibCall.NeoPixelBegin:
            {
                Type ptype;
                uint length = 1;
                uint pixelType = ((1 << 6) | (1 << 4) | (0 << 2) | (2)); // GRB
                if (iOverload == 2)
                {
                    // Begin(uint length, byte pin, PixelType pixelType)
                    pixelType = Pop(ref ptype);
#ifdef CHECKED             
                    AssertUInt(ptype, pixelType);
#endif
                }
                uint pin = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, pin);
#endif
                if (iOverload != 0)
                {
                    // Begin(uint length, byte pin)
                    // Begin(uint length, byte pin, PixelType pixelType)
                    length = Pop(ref ptype);
#ifdef CHECKED             
                    AssertUInt(ptype, length);
#endif
                }
                HRNeoPixel.Begin(length, byte(pin), pixelType);
            }
            case LibCall.NeoPixelBrightnessSet:
            {
                Type ptype;
                uint brightness = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, brightness);
#endif
                HRNeoPixel.SetBrightness(byte(brightness));        
            }
            case LibCall.NeoPixelBrightnessGet:
            {
                byte brightness = HRNeoPixel.GetBrightness(); 
                Push(brightness, Type.Byte);
            }
            case LibCall.NeoPixelSetColor:
            {
                uint w = 0;
                Type ptype;
      
                if (iOverload == 1)
                {
                    // SetColor(uint pixel, byte r, byte g, byte b, byte w)
                    w = Pop(ref ptype);
#ifdef CHECKED             
                    AssertByte(ptype, w);
#endif                    
                }
                // SetColor(uint pixel, byte r, byte g, byte b)
                uint b = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, b);
#endif           
                uint g = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, g);
#endif          
                uint r = Pop(ref ptype);
#ifdef CHECKED             
                AssertByte(ptype, r);
#endif      
                uint pixel = Pop(ref ptype);
#ifdef CHECKED             
                AssertUInt(ptype, pixel);
#endif      
                HRNeoPixel.SetColor(pixel, byte(r), byte(g), byte(b), byte(w));                  
            }
            case LibCall.NeoPixelShow:
            {
                HRNeoPixel.Show();
            }
            case LibCall.NeoPixelLengthGet:
            {
                uint length = HRNeoPixel.GetLength(); 
                Push(length, Type.UInt);
            }
            
            case LibCall.WebClientGetRequest:
            {
                Type htype;    
                uint address = HopperVM.Pop(ref htype);
                uint content = HopperVM.Get(address, ref htype);
                Type utype;    
                uint url = Pop(ref utype);
                
#ifdef CHECKED
                if ((htype != Type.String) || (utype != Type.String))
                {
                    ErrorDump(16);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                bool success = External.WebClientGetRequest(url, ref content);
                HopperVM.Put(address, content, Type.String);
                GC.Release(url);
                Push(success ? 1 : 0, Type.Bool);
                doNext = false;
            }
            case LibCall.WebServerBegin:
            {
                uint port = 80;
                if (iOverload == 2)
                {
                    Type ptype; 
                    port = Pop(ref ptype);
#ifdef CHECKED             
                    AssertUInt(ptype, port);
#endif
                }
                External.WebServerBegin(port);
                doNext = false;
            }
            
            case LibCall.WebServerOn:
            {
                HandlerDelegate handlerDelegate = HandlerDelegate(Pop());
                Type utype;    
                uint url = Pop(ref utype);
#ifdef CHECKED
                if (utype != Type.String)
                {
                    ErrorDump(16);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                External.WebServerOn(url, handlerDelegate);
                GC.Release(url);
            }
            case LibCall.WebServerOnNotFound:
            {
                HandlerDelegate handlerDelegate = HandlerDelegate(Pop());
                External.WebServerOnNotFound(handlerDelegate);
            }
            case LibCall.WebServerEvents:
            {
                External.WebServerEvents();
                doNext = false;
            }
            case LibCall.WebServerClose:
            {
                External.WebServerClose();
                doNext = false;
            }
            case LibCall.WebServerSend:
            {
                Type ctype;    
                uint content     = Pop(ref ctype);
                Type ttype;    
                uint headerContent = Pop(ref ttype);
                Type htype;    
                uint httpCode = Pop(ref htype);
#ifdef CHECKED
                AssertUInt(htype, httpCode);
                if ((ctype != Type.String) || (ttype != Type.Dictionary))
                {
                    ErrorDump(16);
                    Error = 0x0B; // system failure (internal error)
                }
#endif        
                External.WebServerSend(httpCode, headerContent, content);
                GC.Release(headerContent);
                GC.Release(content);
                doNext = false;
            }
            
            default:
            {
                IO.WriteHex(PC); IO.Write(':'); IO.Write('L'); IO.WriteHex(iLibCall); IO.Write('-'); IO.WriteHex(iOverload);  
                IO.Write(' '); ErrorDump(132);
                Error = 0x0A; // not implemented
            }
        }
        return doNext && (Error == 0);
    }
}
