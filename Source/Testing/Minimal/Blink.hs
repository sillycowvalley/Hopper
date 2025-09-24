program Blink
{
    //uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    uses "/Source/Library/Boards/MECB6502"
    
    //uses "/Source/Library/Boards/PiPico"
    
    /*
    TestPortBDirect()
    {
        // Manual test - no functions, no shadow, just raw register access
        
        // Set PB0 as output
        Memory.WriteByte(CRB, 0x00);    // Select DDRB
        Memory.WriteByte(PORTB, 0x01);  // PB0 = output, others input
        
        // Switch to port register
        Memory.WriteByte(CRB, 0x04);    // Select PRB
        
        // Blink PB0 with direct writes
        loop
        {
            Memory.WriteByte(PORTB, 0x01);  // PB0 high
            Delay(500);
            Memory.WriteByte(PORTB, 0x00);  // PB0 low
            Delay(500);
        }
    }
    */
    Hopper()
    {
        //TestPortBDirect();
        
        //SampleMicros = 1000;
        loop
        {
            LED = !LED;
            WriteLn((Time.Seconds).ToString());
            Delay(500);
        }
    }
}
