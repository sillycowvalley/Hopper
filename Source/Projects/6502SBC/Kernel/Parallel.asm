unit Parallel
{
#ifdef W65C22_VIA
    uses "Devices/VIA65C22"
#endif
#ifdef M6821_PIA
    uses "Devices/PIA6821"
#endif


    Initialize()
    {
#ifdef W65C22_VIA
        VIA65C22.initialize();
#endif
#ifdef M6821_PIA
        PIA6821.initialize();
#endif
    }
    ISR()
    {
#ifdef W65C22_VIA
        VIA65C22.isr();
#endif
#ifdef M6821_PIA
        PIA6821.isr();
#endif
    }
}
