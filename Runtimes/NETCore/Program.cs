using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using HopperRuntime.Core;

namespace HopperRuntime
{
    public class Program
    {
        public static DateTime LaunchTime { get; private set; }
        public static bool TraceEnabled { get; private set; } = false;

        public static void Main(string[] args)
        {
            LaunchTime = DateTime.Now;

            if (args.Length == 0)
            {
                Console.WriteLine("Usage: Hopper <program.hexe>");
                return;
            }

            // Check for trace flag and find program file
            string? programFile = null;
            foreach (string arg in args)
            {
                if (arg == "-t")
                {
                    TraceEnabled = true;
                }
                else
                {
                    programFile = arg;
                }
            }

            if (programFile == null)
            {
                Console.WriteLine("Usage: Hopper <program.hexe>");
                return;
            }

            var vm = new HopperVM();
            var bytecode = File.ReadAllBytes(programFile);
            vm.LoadProgram(bytecode);

            
            vm.Run();
        }
    }
}