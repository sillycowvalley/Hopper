unit Runtime
{
#if defined(HOPPER_6502_SBC) || defined(MCU)
    // used from RTC.SetFromDebugger(..)
    bool InDebugger { get system; }
    string DateTime { get system; }
#endif
}
