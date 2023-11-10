using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    class HopperTime
    {
        static DateTime startTime = DateTime.Now;
        static public UInt16 Seconds
        {
            get
            {
                UInt16 seconds = (UInt16)(Millis / 1000);
                return seconds;
            }
        }
        static public Int32 Millis
        {
            get
            {
                DateTime now = DateTime.Now;
                TimeSpan elapsed = now - startTime;
                return (Int32)elapsed.TotalMilliseconds;
            }
        }
        /*
        static public Int32 Micros
        {
            get
            {
                DateTime now = DateTime.Now;
                TimeSpan elapsed = now - startTime;
                double ms = elapsed.Ticks / TimeSpan.TicksPerMillisecond;
                double us = ms * 1000;
                throw new NotImplementedException(); // does this work correctly?
                return (Int32)us;
            }
        }
        */
    }
}
