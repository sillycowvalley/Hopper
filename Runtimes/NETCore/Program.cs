using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using HopperRuntime.Core;

namespace HopperRuntime
{
    public class Program
    {
        public static DateTime LaunchTime { get; private set; }
        public static List<String> Arguments { get; private set; } = new List<String>();
        public static bool TraceEnabled { get; private set; } = false;
        public static string HopperRootFolder { get; private set; } = String.Empty;

        public static void Main(string[] args)
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US");

            string exePath = Environment.ProcessPath ?? String.Empty;
            FileVersionInfo fileVersionInfo = FileVersionInfo.GetVersionInfo(exePath);
            string versionName = fileVersionInfo.FileVersion ?? String.Empty;
            Console.Title = exePath + " [" + versionName + "]";

            string rootFolder = exePath ?? String.Empty;
            while (!rootFolder.EndsWith("Hopper"))
            {
                rootFolder = Path.GetDirectoryName(rootFolder) ?? String.Empty;
                if (String.IsNullOrEmpty(rootFolder)) { break; }
            }
            if (!String.IsNullOrEmpty(rootFolder))
            {
                Directory.SetCurrentDirectory(rootFolder);
                HopperRootFolder = rootFolder;
            }

            LaunchTime = DateTime.Now;

            PowerShellAnsiEnabler.EnableAnsiSupport();

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
                else if (programFile == null)
                {
                    programFile = arg;
                }
                else
                {
                    Arguments.Add(arg);
                }
            }

            if (String.IsNullOrEmpty(programFile))
            {
                Console.WriteLine("Usage: Hopper <program.hexe>");
                return;
            }
            string extension = Path.GetExtension(programFile).ToLower();
            if (String.IsNullOrEmpty(extension))
            {
                programFile += ".hexe";
            }
            string launchFolder = Path.GetDirectoryName(programFile) ?? String.Empty;
            if (String.IsNullOrEmpty(launchFolder))
            {
                string exeFolder = Path.GetDirectoryName(exePath) ?? String.Empty;
                if (!String.IsNullOrEmpty(exeFolder) && File.Exists(Path.Combine(exeFolder, programFile)))
                {
                    programFile = Path.Combine(exeFolder, programFile);
                }
                else
                {
                    programFile = Path.Combine("Bin", programFile);
                }
            }
            if (!File.Exists(programFile))
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