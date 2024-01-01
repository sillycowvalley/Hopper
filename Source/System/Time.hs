unit Time
{
    long Millis  { get system; }

#ifdef MCU
    Delay(uint ms) system;
#else
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
#endif
}
