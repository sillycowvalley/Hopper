using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Restart
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Process[] processes = Process.GetProcessesByName("hoppernet");
            foreach (var process in processes)
            {
                process.Kill();
            }
            System.Threading.Thread.Sleep(1000);
            File.Delete(@"D:\Repos\Hopper\Temp\IPCPipeName.bin");

            Process hopper = new Process();
            hopper.StartInfo.FileName = @"D:\Repos\Hopper\Bin\HopperNET.exe";
            hopper.Start();
            hopper.Start();
        }
    }
}
