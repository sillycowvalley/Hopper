unit Time
{
    long Millis { get system; }
#ifndef TINYHOPPER    
    long Micros { get system; }
#endif

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
}
