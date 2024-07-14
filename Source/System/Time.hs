unit Time
{
    long Millis  { get system; }
    uint Seconds { get system; }

#if defined(MCU)
    Delay(uint ms) system;
    uint SampleMicros { set system; get system; }
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
#if !defined(MCU)    
    string Time      { get system; }
    string Date      { get system; }
#endif

}
