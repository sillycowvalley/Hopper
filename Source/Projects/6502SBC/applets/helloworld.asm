program HelloWorld
{
    #define HOPPER_BIOS_APPLET
    #define CPU_65C02S
    
    uses "../Kernel/Definitions/BIOSInterface"
    uses "../Kernel/Definitions/ZeroPage"
    
    const string helloWord = "Hello World!\n";
    
    printString()
    {
        LDX # SysCall.PrintString
        JMP [ZP.BIOSDISPATCH]
    }
    
    Hopper()
    {
        LDA #(helloWord % 256)
        STA ZP.STRL
        LDA #(helloWord / 256)
        STA ZP.STRH
        printString();
    }
    
}
