unit Time
{
    long Millis  { get system; }

#if defined(MCU) || defined(HOPPER_6502)
    Delay(uint ms) system;
#else
    Delay(uint ms)
    {
        long endTime = Millis + ms;
        loop
        {
            if (Millis >= endTime)
            {
                break;
            }
        }
    }
#endif
    DelaySeconds(uint s)
    {
        s *= 2;
        for (uint i=0; i < s; i++)
        {
            Delay(500); // allows debugger to break into long delays
        }
    }
    string Time { get system; set system; }
    string Date { get system; set system; }
}
