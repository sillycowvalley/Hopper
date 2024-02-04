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
}
