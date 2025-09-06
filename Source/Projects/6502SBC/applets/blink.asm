program Blink
{
    #define HOPPER_BIOS_APPLET
    #define CPU_65C02S
    
    uses "../Kernel/Definitions/BIOSInterface"
    uses "../Kernel/Definitions/ZeroPage"
    
    const string blinkString = "Blink Applet\n";
    
    pinMode()
    {
        LDA #0 // 0 - builtin LED
        STA ZP.ACCL    
        LDA #1 // 1 = Output
        STA ZP.ACCH
        LDX # SysCall.PinMode
        JMP [ZP.BIOSDISPATCH]
    }
    
    pinWrite()
    {
        STA ZP.ACCH
        LDA #0 // 0 - builtin LED
        STA ZP.ACCL
        LDX # SysCall.PinWrite
        JMP [ZP.BIOSDISPATCH]
    }
    delay()
    {
        // 500ms delay
        LDA # (500 % 256)
        STA ZP.TOP0
        LDA # (500 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        LDX # SysCall.TimeDelay
        JMP [ZP.BIOSDISPATCH]
    }
    printString()
    {
        LDX # SysCall.PrintString
        JMP [ZP.BIOSDISPATCH]
    }
    printChar()
    {
        LDX # SysCall.PrintChar
        JMP [ZP.BIOSDISPATCH]
    }
    
    serialIsAvailable()
    {
        LDX # SysCall.SerialIsAvailable
        JMP [ZP.BIOSDISPATCH]
    }
    Hopper()
    {
        LDA #(blinkString % 256)
        STA ZP.STRL
        LDA #(blinkString / 256)
        STA ZP.STRH
        printString();
        
        pinMode();
        loop
        {
            LDA #1
            pinWrite();
            delay();
            LDA #0
            pinWrite();
            delay();
            
            LDA #'.'
            STA ZP.ACCL
            printChar();
            
            serialIsAvailable();
            if (C) { break; }
        }
    }
}
