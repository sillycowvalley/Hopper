unit Parallel
{
#ifdef W65C22_VIA
    uses "Devices/VIA65C22"
#endif

    Initialize()
    {
#ifdef W65C22_VIA
        VIA65C22.initialize();
#endif
    }
    ISR()
    {
#ifdef W65C22_VIA
        VIA65C22.isr();
#endif
    }
}
