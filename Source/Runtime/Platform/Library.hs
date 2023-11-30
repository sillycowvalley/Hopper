unit Library
{
    uses "/Source/Runtime/Platform/LibCalls"
    uses "/Source/Runtime/Platform/Graphics"
    uses "/Source/Runtime/Platform/Wire"
    
    
    bool ExecuteLibCall(byte iLibCall)
    {
        bool doNext = true;
        switch (LibCall(iLibCall))
        {
            case LibCall.WireBegin:
            {
                HRWire.Begin();
            }
            case LibCall.WireBeginTx:
            {
                Type atype;
                uint b = Pop(ref atype);
#ifdef CHECKED             
                AssertByte(atype, b);
#endif   
                HRWire.BeginTx(byte(b));
            }
            case LibCall.WireWrite:
            {
                Type atype;
                uint b = Pop(ref atype);
#ifdef CHECKED             
                AssertByte(atype, b);
#endif   
                HRWire.Write(byte(b));
            }
            case LibCall.WireEndTx:
            {
                HRWire.EndTx();
            }
            
            case LibCall.MemoryIncWord:
            {
                Type utype;
                uint address = Pop(ref utype);
#ifdef CHECKED             
                AssertByte(utype, address);
#endif                   
                uint w = Memory.ReadWord(address);
                w++;
                Memory.WriteWord(address, w);
            }
            
            case LibCall.GraphicsConfigureDisplay:
            {
                Type utype;
                uint h = Pop(ref utype);
                uint w = Pop(ref utype);
                Display display = Display(Pop(ref utype));
                HRGraphics.ConfigureDisplay(display, w, h);
            }
            case LibCall.GraphicsConfigureSPI:
            {
                Type utype;
                uint dc = Pop(ref utype);
                uint cs = Pop(ref utype);
                HRGraphics.ConfigureSPI(byte(cs), byte(dc));
            }
            case LibCall.GraphicsConfigureReset:
            {
                Type utype;
                uint rst = Pop(ref utype);
                HRGraphics.ConfigureReset(byte(rst));
            }
            case LibCall.GraphicsConfigureI2C:
            {
                Type utype;
                uint addr = Pop(ref utype);
                HRGraphics.ConfigureI2C(byte(addr));
            }
            case LibCall.GraphicsConfigureMatrix:
            {
                Type utype;
                uint intensity = Pop(ref utype);
                uint dp  = Pop(ref utype);
                uint cp  = Pop(ref utype);
                HRGraphics.ConfigureMatrix(byte(cp), byte(dp), byte(intensity));
            }
            case LibCall.GraphicsBegin:
            {
                uint result = uint(HRGraphics.Begin());
                Push(result, Type.UInt);
            }
            case LibCall.GraphicsEnd:
            {
                HRGraphics.End();
            }
            case LibCall.GraphicsClear:
            {
                Type ctype;
                uint color = Pop(ref ctype);
    #ifdef CHECKED
                AssertUInt(ctype, color);
    #endif
                HRGraphics.Clear(color);
            }
            case LibCall.GraphicsWidthGet:
            {
                Push(HRGraphics.Width, Type.UInt);
            }
            case LibCall.GraphicsHeightGet:
            {
                Push(HRGraphics.Height, Type.UInt);
            }
            case LibCall.GraphicsSetPixel:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint y = Pop(ref utype);
                uint x = Pop(ref utype);
                HRGraphics.SetPixel(x, y, color);
            }
            case LibCall.GraphicsLine:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint y2 = Pop(ref utype);
                uint x2 = Pop(ref utype);
                uint y1 = Pop(ref utype);
                uint x1 = Pop(ref utype);
                HRGraphics.Line(x1, y1, x2, y2, color);
            }
            case LibCall.GraphicsHorizontalLine:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint y2 = Pop(ref utype);
                uint x2 = Pop(ref utype);
                uint y1 = Pop(ref utype);
                uint x1 = Pop(ref utype);
                HRGraphics.HorizontalLine(x1, y1, x2, y2, color);
            }
            case LibCall.GraphicsVerticalLine:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint y2 = Pop(ref utype);
                uint x2 = Pop(ref utype);
                uint y1 = Pop(ref utype);
                uint x1 = Pop(ref utype);
                HRGraphics.VerticalLine(x1, y1, x2, y2, color);
            }
            case LibCall.GraphicsRectangle:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint h = Pop(ref utype);
                uint w = Pop(ref utype);
                uint y = Pop(ref utype);
                uint x = Pop(ref utype);
                HRGraphics.Rectangle(x, y, w, h, color);
            }
            case LibCall.GraphicsFilledRectangle:
            {
                Type utype;
                uint color = Pop(ref utype);
                uint h = Pop(ref utype);
                uint w = Pop(ref utype);
                uint y = Pop(ref utype);
                uint x = Pop(ref utype);
                HRGraphics.FilledRectangle(x, y, w, h, color);
            }
            case LibCall.GraphicsInvertDisplay:
            {
                Type ctype;
                uint flag  = Pop(ref ctype);
#ifdef CHECKED
                AssertBool(ctype, flag);
#endif
                HRGraphics.InvertDisplay(flag != 0);
            }
            case LibCall.GraphicsFlipDisplay:
            {
                Type ctype;
                uint flag  = Pop(ref ctype);
#ifdef CHECKED
                AssertBool(ctype, flag);
#endif
                HRGraphics.FlipDisplay(flag != 0);
            }
            case LibCall.GraphicsShow:
            {
                Type ctype;
                uint flag  = Pop(ref ctype);
#ifdef CHECKED
                AssertBool(ctype, flag);
#endif
                HRGraphics.Show(flag != 0);
            }
            case LibCall.GraphicsDrawChar:
            {
                Type aatype;
                uint aa = Pop(ref aatype);
                Type stype;
                uint scale = Pop(ref stype);
                Type atype;
                uint bc = Pop(ref atype);
                Type btype;
                uint fc = Pop(ref btype);
                Type ctype;
                uint ch = Pop(ref ctype);
                Type dtype;
                uint y = Pop(ref atype);
                Type etype;
                uint x = Pop(ref btype);
#ifdef CHECKED
                AssertUInt(aatype, aa);
                AssertUInt(stype, scale);
                AssertUInt(atype, bc);
                AssertUInt(btype, fc);
                AssertChar(ctype, ch);
                AssertUInt(dtype, y);
                AssertUInt(etype, x);
#endif
                HRGraphics.DrawChar(x, y, char(ch), fc, bc, byte(scale), aa != 0); 
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
