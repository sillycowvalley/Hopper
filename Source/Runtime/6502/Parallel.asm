unit Parallel
{
    uses "/Source/Runtime/6502/ZeroPage"
#ifdef W65C22_VIA
    uses "/Source/Runtime/6502/Devices/W65C22"
#endif
#ifdef M6821_PIA
    uses "/Source/Runtime/6502/Devices/PIA6821"
#endif

    Initialize()
    {
#ifdef W65C22_VIA
        W65C22.initialize();
#endif
#ifdef M6821_PIA
        PIA6821.initialize();
#endif
    }
    ISR()
    {
#ifdef W65C22_VIA
        W65C22.isr();
#endif
#ifdef M6821_PIA
        PIA6821.isr();
#endif
    }
}
