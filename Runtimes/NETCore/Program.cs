using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using HopperRuntime.Core;

namespace HopperRuntime
{
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Usage: Hopper <program.hexe>");
                return;
            }

            var vm = new HopperVM();

            var bytecode = File.ReadAllBytes(args[0]);

            vm.LoadProgram(bytecode);
            vm.Run();
        }
    }
}