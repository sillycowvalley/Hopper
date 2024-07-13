unit SNVGMPlayer
{
    /*
    SN 76489 Library
    Authors : Dave Latham        - https://github.com/linuxplayground
            : Michael Cartwright - https://github.com/sillycowvalley
            
    Version: 1.0
     
    Copyright 2024 David Latham
    
    Permission is hereby granted, free of charge, to any person obtaining a copy of
    this software and associated documentation files (the Software), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    the Software, and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
    FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    
    July 2024 - blame Michael for the timing calibration futzing
    
    */    
    
#ifdef MCU
    const byte sn_WE    = GP10;
    const byte sn_READY = GP11;
    const byte d0 = GP2;
    const byte d1 = GP3;
    const byte d2 = GP4;
    const byte d3 = GP5;
    
    const byte d4 = GP6;
    const byte d5 = GP7;
    const byte d6 = GP8;
    const byte d7 = GP9;
#else
    const byte sn_WE    = 2;    // 32 clock cycle active low pulse to latch data.
    const byte sn_READY = 3;    // or poll for a rising edge on sn_READY.  See datasheet.
#endif
    
    //https://vgmrips.net/wiki/VGM_Specification#Commands
    const byte sn_SEND     = 0x50;
    const byte sn_END      = 0x66;
    const byte sn_WAIT     = 0x61;
    const byte sn_SIXTIETH = 0x62;
    const byte sn_N1       = 0x70;
    
    const uint timingIterations = 1000;
#ifdef MCU
    uint callSampleCost;
#else
    uint sixtiethIterations; 
    uint waitShift;
#endif
    
    uint idx;
    uint idy; // just for timing
    byte[] data;
    
    delegate bool VgmOpDelegate();
    VgmOpDelegate[256] vgmOp;
    bool notImplemented()
    {
        IO.WriteLn();
        byte current = data[idx-1]; // idx has already been incremented
        IO.WriteLn("VGM instruction 0x" + current.ToHexString(2) + " not implemented at 0x" + (idx-1).ToHexString(4));
        Die(0x0A);
        return false;
    }
    
    bool send()
    {
#ifdef MCU
        byte b = data[idx];
        DigitalWrite(d0, (b & 0b00000001) != 0);
        DigitalWrite(d1, (b & 0b00000010) != 0);
        DigitalWrite(d2, (b & 0b00000100) != 0);
        DigitalWrite(d3, (b & 0b00001000) != 0);
        DigitalWrite(d4, (b & 0b00010000) != 0);
        DigitalWrite(d5, (b & 0b00100000) != 0);
        DigitalWrite(d6, (b & 0b01000000) != 0);
        DigitalWrite(d7, (b & 0b10000000) != 0);
#else        
        Memory.WriteByte(PORTB, data[idx]);
#endif        
        DigitalWrite(sn_WE, false);
        loop {
            if (!DigitalRead(sn_READY)) { break; }
        }
        DigitalWrite(sn_WE, true);
        idx ++;
        return false;
    }
    bool end()
    {
        return true; // done
    }
    bool sixteenth()
    {
        // wait 735 samples (60th of a second = 1000 / 60 ms = 16.667 ms = 16667us),
#ifdef MCU
        Time.DelaySamples(735-callSampleCost);
#else
        uint lu = sixtiethIterations;
        loop
        {
            if (lu == 0) { break; }
            lu--;
        }
#endif
        return false;
    }
    bool wait()
    {
#ifdef MCU
        // a sample is 1 second / 44100 = 22.67us
        uint samples = (data[idx+1]<<8) | data[idx];
        if (samples > 0)
        {
            Time.DelaySamples(samples - callSampleCost);
        }
#else
        uint lu; // first local
        
        // n samples : // 0.. 1.49 seconds 
        // 1.49 seconds = 1490000 us / 65536 = ~23us per iteration
        lu = (data[idx+1]<<8) | data[idx];
        lu = lu >> waitShift;
        loop
        {
            if (lu == 0) { break; }
            lu--;
        }
#endif        
        idx += 2;
        
        return false;
    }
    bool n1() 
    {
        // 1..4 x 22us : 2.5 x 22us = 55us
#ifdef MCU
       uint samples = (data[idx-1] & 0x0F)+1;
       if (samples > callSampleCost)
       {
           Time.DelaySamples(samples - callSampleCost);
       }
#endif   
        return false;
    }
    bool n1_01()
    {
        // 5..8 x 22us : 6.5 x 22us = 143us
        return false;
    }
    bool n1_10()
    {
        // 9..12 x 22us : 10.5 x 22 = 231us
        return false;
    }
    bool n1_11()
    {
        // 13..16 x 22us : 14.5 x 22 = 319us
#ifdef HOPPER_6502_SBC
        // (319us - 248us) / ~20us = 4
        uint lt;
        lt--;
        lt--;
        lt--;
        lt--;
#endif        
        return false;
    }
        
    Initialize()
    {
        uint luint; // first local
        
        VgmOpDelegate callOp;
        callOp =  notImplemented;
        for (uint i=0; i < 256; i++)
        {
            vgmOp[i] = callOp;
        }
        callOp = send;       vgmOp[sn_SEND]     = callOp;
        callOp = end;        vgmOp[sn_END]      = callOp;
        callOp = wait;       vgmOp[sn_WAIT]     = callOp;
        callOp = sixteenth;  vgmOp[sn_SIXTIETH] = callOp;
        
        callOp = n1;         vgmOp[sn_N1 | 0b0000] = callOp; vgmOp[sn_N1 | 0b0001] = callOp; vgmOp[sn_N1 | 0b0010] = callOp; vgmOp[sn_N1 | 0b0011] = callOp;
#ifndef MCU        
        callOp = n1_01;      
#endif
        vgmOp[sn_N1 | 0b0100] = callOp; vgmOp[sn_N1 | 0b0101] = callOp; vgmOp[sn_N1 | 0b0110] = callOp; vgmOp[sn_N1 | 0b0111] = callOp;
#ifndef MCU                
        callOp = n1_10;      
#endif
        vgmOp[sn_N1 | 0b1000] = callOp; vgmOp[sn_N1 | 0b1001] = callOp; vgmOp[sn_N1 | 0b1010] = callOp; vgmOp[sn_N1 | 0b1011] = callOp;
#ifndef MCU        
        callOp = n1_11;      
#endif
        vgmOp[sn_N1 | 0b1100] = callOp; vgmOp[sn_N1 | 0b1101] = callOp; vgmOp[sn_N1 | 0b1110] = callOp; vgmOp[sn_N1 | 0b1111] = callOp;
        
#ifdef MCU        
        Time.SampleMicros = 22;
        callSampleCost = 0;
#endif
        byte[2] vgm;  
        vgm[0] = sn_N1;
        data = vgm;
        idx = 1;
        
        
        luint = timingIterations;
        long startTiming = Millis;
        loop
        {
            if (luint == 0) { break; }
            luint--; // compiles to: DECLOCALB <byte operand>
        }
        long elapsedDecrement = Millis - startTiming;
        
        luint = timingIterations;
        startTiming = Millis;
        
        loop
        {
            if (luint == 0) { break; }
            luint--;
            
            // how long does it take just to make an empty delegate call?
            callOp = vgmOp[data[idx-1]]; // idy is always 1 for this test
            idy++;
            _ = n1();
        }
        long elapsedN1 = Millis - startTiming - elapsedDecrement;
        
        // 248us on 6502 at 8MHz on Hopper 6502 SBC
        float usPerN1Iteration =  1000.0 * elapsedN1 / timingIterations;
#ifdef MCU        
        IO.WriteLn(usPerN1Iteration.ToString() + " us for n1()");
        callSampleCost = uint(usPerN1Iteration) - 22;
        IO.WriteLn(callSampleCost.ToString() + " us sample cost for calls");
#else
        // SIXTIETH:
        // 18us on 6502 at 8MHz on Hopper 6502 SBC
        float usPerDecIteration =  1000.0 * elapsedDecrement / timingIterations;
        sixtiethIterations = uint((16667.0 - usPerN1Iteration) / usPerDecIteration); 
        
        // WAIT:
        float fWaitDiv = usPerDecIteration / 23.0;
        if (fWaitDiv < 1.5)
        {
            waitShift = 0;
        }
        else if (fWaitDiv < 2.999) // /1.5 .. /3
        {
            waitShift = 1; // /2
        }
        else if (fWaitDiv < 5.999) // /3 .. /6
        {
            waitShift = 2; // /4
        }       
        else if (fWaitDiv < 9.999) // /6 .. /10
        {
            // 6.652099 for Dave -> 3
            waitShift = 3; // /8
        }
        else if (fWaitDiv < 23.999) // /10 .. /24
        {
            waitShift = 4; // /16
        }
        else
        {
            waitShift = 5; // /32
        }

        IO.WriteLn(usPerN1Iteration.ToString() + " us for n1()");
        IO.WriteLn(usPerDecIteration.ToString() + " us for each lu-- iteration");
        IO.WriteLn(sixtiethIterations.ToString() + " iterations for SIXTIETH");
        IO.WriteLn(fWaitDiv.ToString() + " fWaitDiv");
        IO.WriteLn(waitShift.ToString() + " waitShift");
#endif
        
#ifdef MCU
        PinMode(d0, PinModeOption.Output);
        PinMode(d1, PinModeOption.Output);
        PinMode(d2, PinModeOption.Output);
        PinMode(d3, PinModeOption.Output);
        PinMode(d4, PinModeOption.Output);
        PinMode(d5, PinModeOption.Output);
        PinMode(d6, PinModeOption.Output);
        PinMode(d7, PinModeOption.Output);
#else                
        Memory.WriteByte(DDRB, 0b11111111);
#endif
        PinMode(sn_WE, PinModeOption.Output);
        DigitalWrite(sn_WE, true);    // Make sure the SN76489 is not selected at start up.
        Silence();
    }
    Play(byte[] vgm)
    {
        data = vgm;
        
        byte last = data[data.Count-1];
        if (last != sn_END)
        {
            IO.WriteLn("Last VGM instruction not END");  
            Die(0x0B);
        }
        
        idx = 0;
        VgmOpDelegate callOp;
        loop {
            callOp = vgmOp[data[idx]];
            idx++;
            if (callOp()) { break; }
        }
    }
    Silence()
    {
        byte[] vgm = {sn_SEND, (0 << 5) | 0x9F, 
                      sn_SEND, (1 << 5) | 0x9F, 
                      sn_SEND, (2 << 5) | 0x9F, 
                      sn_SEND, (3 << 5) | 0x9F, 
                      sn_END};
        Play(vgm);
    }
}
