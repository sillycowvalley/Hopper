unit SNVGMPlayer
{
    /*
    SN 76489 Library
    Author : Dave Latham - https://github.com/linuxplayground
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
    
    const byte sn_WE    = 2;    // 32 clock cycle active low pulse to latch data.
    const byte sn_READY = 3;    // or poll for a rising edge on sn_READY.  See datasheet.
    
    //https://vgmrips.net/wiki/VGM_Specification#Commands
    const byte sn_SEND     = 0x50;
    const byte sn_END      = 0x66;
    const byte sn_WAIT     = 0x61;
    const byte sn_SIXTIETH = 0x62;
    const byte sn_N1       = 0x70;
    
    const uint timingIterations = 1000;
    
    uint sixtiethIterations; 
    uint waitShift;
    
    uint idx;
    byte[] data;
    
    delegate bool VgmOpDelegate();
    VgmOpDelegate[256] vgmOp;
    
    bool send()
    {
        Memory.WriteByte(PORTB, data[idx]);
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
        uint lu = sixtiethIterations;
        loop
        {
            if (lu == 0) { break; }
            lu--;
        }
        return false;
    }
    bool wait()
    {
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
        
        idx += 2;
        return false;
    }
    bool n1() 
    {
        // 1..4 x 22us : 2.5 x 22us = 55us
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
        uint lt;
        // 13..16 x 22us : 14.5 x 22 = 319us
#ifdef HOPPER_6502_SBC
        // (319us - 248us) / ~20us = 4
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
        callOp = send;       vgmOp[sn_SEND]     = callOp;
        callOp = end;        vgmOp[sn_END]      = callOp;
        callOp = wait;       vgmOp[sn_WAIT]     = callOp;
        callOp = sixteenth;  vgmOp[sn_SIXTIETH] = callOp;
        callOp = n1;         vgmOp[sn_N1 | 0b0000] = callOp; vgmOp[sn_N1 | 0b0001] = callOp; vgmOp[sn_N1 | 0b0010] = callOp; vgmOp[sn_N1 | 0b0011] = callOp;
        callOp = n1_01;      vgmOp[sn_N1 | 0b0100] = callOp; vgmOp[sn_N1 | 0b0101] = callOp; vgmOp[sn_N1 | 0b0110] = callOp; vgmOp[sn_N1 | 0b0111] = callOp;
        callOp = n1_10;      vgmOp[sn_N1 | 0b1000] = callOp; vgmOp[sn_N1 | 0b1001] = callOp; vgmOp[sn_N1 | 0b1010] = callOp; vgmOp[sn_N1 | 0b1011] = callOp;
        callOp = n1_11;      vgmOp[sn_N1 | 0b1100] = callOp; vgmOp[sn_N1 | 0b1101] = callOp; vgmOp[sn_N1 | 0b1110] = callOp; vgmOp[sn_N1 | 0b1111] = callOp;
        
        
        byte[1] data;  
        uint idy;  
        
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
            callOp = vgmOp[data[idy]]; // idy is always 0 for this test
            idx++;
            _ = n1();
        }
        long elapsedN1 = Millis - startTiming - elapsedDecrement;
        
        // 248us on 6502 at 8MHz on Hopper 6502 SBC
        float usPerN1Iteration =  1000.0 * elapsedN1 / timingIterations;
        
        // SIXTIETH:
        // 18us on 6502 at 8MHz on Hopper 6502 SBC
        float usPerDecIteration =  1000.0 * elapsedDecrement / timingIterations;
        sixtiethIterations = uint((16667.0 - usPerN1Iteration) / usPerDecIteration); 
        
        // WAIT:
        float fWaitDiv = usPerDecIteration / 23.0;
        waitShift = 0;
        if (fWaitDiv > 16)
        {
            waitShift = 4;
        }
        else if (fWaitDiv > 8)
        {
            waitShift = 3;
        }
        else if (fWaitDiv > 4)
        {
            waitShift = 2;
        }
        else if (fWaitDiv > 2)
        {
            waitShift = 1;
        }
        /*
        IO.WriteLn(usPerN1Iteration.ToString() + " us for n1()");
        IO.WriteLn(usPerDecIteration.ToString() + " us for each lu-- iteration");
        IO.WriteLn(sixtiethIterations.ToString() + " iterations for SIXTIETH");
        IO.WriteLn(fWaitDiv.ToString() + " fWaitDiv");
        IO.WriteLn(waitShift.ToString() + " waitShift");
        */
        Memory.WriteByte(DDRB, 0b11111111);
        PinMode(sn_WE, PinModeOption.Output);
        Silence();
    }
    Play(byte[] vgm)
    {
        data = vgm;
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
