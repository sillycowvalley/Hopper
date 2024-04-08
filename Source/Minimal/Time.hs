unit Time
{
    Delay(uint ms) system;
    DelaySeconds(uint s)
    {
        s *= 2;
        for (uint i=0; i < s; i++)
        {
            Delay(500); // allows debugger to break into long delays
        }
    }
    uint Seconds { get system; }
}
