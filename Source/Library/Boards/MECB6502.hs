unit Board
{
    #define MCU_BOARD_DEFINED
    #define MECB_6502
    #define M6821_PIA
    
    const byte GP0 = 0;
    const byte GP1 = 1;
    const byte GP2 = 2;
    const byte GP3 = 3;
    const byte GP4 = 4;
    const byte GP5 = 5;
    const byte GP6 = 6;
    const byte GP7 = 7;
    
    const byte GP8 = 8;
    const byte GP9 = 9;
    const byte GP10 = 10;
    const byte GP11 = 11;
    const byte GP12 = 12;
    const byte GP13 = 13;
    const byte GP14 = 14;
    const byte GP15 = 15;
    
    const byte BuiltInLED = Board.GP0;
    
    const byte I2CSCL0 = GP6; //GP8;
    const byte I2CSDA0 = GP7; //GP9;
    
    const uint PORTA                = 0xF010;
    const uint PORTB                = 0xF011;
    
    const uint CRA                  = 0xF012; // Control Register A
    const uint CRB                  = 0xF013; // Control Register B
    
    const uint DDRA                 = 0xF010;
    const uint DDRB                 = 0xF011;
    
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/MCU"
    
    string BoardName { get { return "MECB 6502"; } }
}
