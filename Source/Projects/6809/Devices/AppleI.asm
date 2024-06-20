unit SerialDevice
{
    // Motorola 6821 PIA as per Apple I
    
    // Motorola 6821
    
    // A used for Rx
    const uint PRA  = 0xD010; // (when CRA-2 == 1)
    const uint DDRA = 0xD010; // (when CRA-2 == 0)
    const uint CRA  = 0xD011;
    
    // B user for Tx
    const uint PRB  = 0xD012; // (when CRB-2 == 1)
    const uint DDRB = 0xD012; // (when CRB-2 == 0)
    const uint CRB  = 0xD013;
    
    friend Serial;
    
    initialize()
    {
        // soft reset by zeroing all 6 registers (like a hard reset would do
        
        LDA #0b00000000 // select DDRA and clear interrupt flags
        STA CRA
        
        LDA #0b00000000 // all pins are inputs
        STA DDRA
        
        LDA #0b00000010 // select PRA and clear interrupt flags
        STA CRA
        
        LDA #0b00000000 // select DDRB and clear interrupt flags
        STA CRB
        
        LDA #0b11111111 // all pins are outputs
        STA DDRB
        
        LDA #0b00000010 // select PRB and clear interrupt flags
        STA CRB
    }

    pollRead()
    {
        PSHS A, X, Y, U

        LDA CRA             // bit 7 set?
        AND #0x80
        if (NZ)
        {
            LDA PRA          // read serial byte
            CMP #0x03        // is it break? (<ctrl><C>)
            if (Z)
            {
                INC Serial.BreakFlag
            }
            else
            {
                LDX Serial.InWritePointer    // push it into serial input buffer
                STA Serial.InBuffer, X
                INC Serial.InWritePointer
            }
        }

        PULS A, X, Y, U
    }

    writeChar()
    {
        loop
        {
            LDA CRB             // bit 7 cleared yet?
            AND #0x80
            if (Z) { break; }  // No, wait for display.
        }
        STA PRB         // Output character. Sets DA.
    }
}
