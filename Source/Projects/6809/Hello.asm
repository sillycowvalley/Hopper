program HelloWorld6809
{
    #define CPU_6809
    #define CPU_8MHZ
    #define APPLE_I
    
    uses "/Source/Projects/6809/ZeroPage"
    uses "/Source/Projects/6809/Serial"

    IRQ()
    {
        Serial.ISR();
    }

    NMI()
    {
        INC ZP.SerialBreakFlag // hardware <ctrl><C>
    }

    Hopper()
    {
        // Initialize the serial port
        Serial.Initialize();

        // Print "Hello, World!" message
        LDA # 'H'
        Serial.WriteChar();
        LDA # 'e'
        Serial.WriteChar();
        LDA # 'l'
        Serial.WriteChar();
        LDA # 'l'
        Serial.WriteChar();
        LDA # 'o'
        Serial.WriteChar();
        LDA # ','
        Serial.WriteChar();
        LDA # ' '
        Serial.WriteChar();
        LDA # 'W'
        Serial.WriteChar();
        LDA # 'o'
        Serial.WriteChar();
        LDA # 'r'
        Serial.WriteChar();
        LDA # 'l'
        Serial.WriteChar();
        LDA # 'd'
        Serial.WriteChar();
        LDA # '!'
        Serial.WriteChar();
        LDA # 0x0D  // Carriage return
        Serial.WriteChar();
        LDA # 0x0A  // Line feed
        Serial.WriteChar();

        // Infinite loop to keep the program running
        loop
        {
            // Poll for new serial input (if any) and handle break flag
            LDA ZP.SerialBreakFlag
            if (NZ) 
            { 
                STZ ZP.SerialBreakFlag
            }
        }
    }
}
