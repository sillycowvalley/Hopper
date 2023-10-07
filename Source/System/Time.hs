unit Time
{
#ifndef TINYHOPPER    
    long Micros { get system; }
    long Millis  { get system; }
#endif
#ifdef H6502
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
}
