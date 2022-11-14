unit Utilities
{
    //;******************************************************************
    //;        SUB-ROUTINE..: PAUSE                                      ;
    //;        Function....: Pause in 100uS. times value in BC          ;
    //;        Input.......: BC reg                                     ;
    //;        Output......: no                                         ;
    //;        call........: NONE                                       ;
    //;        Info........: KEA.      dato: 23/5-93                    ;
    //;******************************************************************
    //PAUSE:       PUSH   AF
    //             INC    B
    //             INC    C              ; ADJUST THE LOOP
    //PAUSELOOP1:  LD     A,13H          ; ADJUST THE TIME 13h IS FOR 4 MHZ
    //PAUSELOOP2:  DEC    A              ; DEC COUNTER. 4 T-states = 1 uS.
    //             JP     NZ,PAUSELOOP2  ; JUMP TO PAUSELOOP2 IF A <> 0.
    //             DEC    C              ; DEC COUNTER
    //             JP     NZ,PAUSELOOP1  ; JUMP TO PAUSELOOP1 IF C <> 0.
    //
    //             DJNZ   PAUSELOOP1     ; JUMP TO PAUSELOOP1 IF B <> 0.
    //PAUSESLUT:   POP    AF
    //             RET
             
    Delay(int ms)
    {
        // 4MHz:
        int j = 0;
        loop
        {
            int i = 0;
            loop
            {
                i++;
                if (i == 14)
                {
                    break;
                }
            }
            j++;
            if (j == ms)
            {
                break;
            }
        }
    }
    
    PrintAt(byte col, byte row, char a, char b, char c, char d, char e, char f, char g, char h, char i, char j)
    {
        LCDCharacterAt(a, col, row);
        if (byte(b) != 0)
        {
            LCDCharacter(b);
        }
        if (byte(c) != 0)
        {
            LCDCharacter(c);
        }
        if (byte(d) != 0)
        {
            LCDCharacter(d);
        }
        if (byte(e) != 0)
        {
            LCDCharacter(e);
        }
        if (byte(f) != 0)
        {
            LCDCharacter(f);
        }
        if (byte(g) != 0)
        {
            LCDCharacter(g);
        }
        if (byte(h) != 0)
        {
            LCDCharacter(h);
        }
        if (byte(i) != 0)
        {
            LCDCharacter(i);
        }
        if (byte(j) != 0)
        {
            LCDCharacter(j);
        }
    }
    
    char ToHexChar(uint hex)
    {
        uint result = 48; // '0'
        if (hex < 10)
        {
            result = result + hex;
        }
        else
        {
            result = 65; // 'A'
            result = result + hex - 10;
        }
        return char(result);
    }
    
    PrintHexAt(byte col, byte row, uint value)
    {
        uint c0 = (value & 0x000F);
        value = value >> 4;
        uint c1 = (value & 0x000F);
        value = value >> 4;
        uint c2 = (value & 0x000F);
        value = value >> 4;
        uint c3 = (value & 0x000F);
        
        PrintAt(col, row, ToHexChar(c3), ToHexChar(c2), ToHexChar(c1), ToHexChar(c0), char(0), char(0), char(0), char(0), char(0), char(0));
    }
}