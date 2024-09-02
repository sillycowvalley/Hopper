program Count
{
    uses "/Source/Library/Boards/Hopper6502"
    
    Hopper()
    {
        Memory.WriteByte(DDRA, 0b11111111);
        Memory.WriteByte(DDRB, 0b11111111);
        
        uint counter;
        
        loop
        {
            Memory.WriteByte(PORTA, byte(counter & 0xFF));
            Memory.WriteByte(PORTB, byte(counter >> 8));
            
            Delay(100);
            counter++;
        }
    }
}
