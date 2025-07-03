using System.Diagnostics;
using System.Globalization;
using HopperNET;

namespace HopperRuntime
{
    public class Program
    {
        public static List<String> Arguments { get; private set; } = new List<String>();
        public static bool TraceEnabled { get; private set; } = false;

        static Screen screen = new Screen();
        static Keyboard keyboard = new Keyboard();


        static void Console_CancelKeyPress(object sender, ConsoleCancelEventArgs e)
        {
            // Cancel the termination (prevent immediate exit)
            e.Cancel = true;
            keyboard.PushToKeyboardBuffer(Key.ControlC);
        }

        public static int Main(string[] args)
        {
            Console.CancelKeyPress += Console_CancelKeyPress;

            CultureInfo.CurrentCulture = new CultureInfo("en-US");

            string exePath = Environment.ProcessPath;

            FileVersionInfo fileVersionInfo = FileVersionInfo.GetVersionInfo(exePath);
            string versionName = fileVersionInfo.FileVersion;
            if (!String.IsNullOrEmpty(versionName))
            {
                Console.Title = exePath + " [" + versionName + "]";
            }

            HopperPath.InitializeFolders();
            PowerShellAnsiEnabler.EnableAnsiSupport();

            // Check for trace flag and find program file
            string programFile = null;
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
                programFile = "Shell"; // default to launching the Hopper shell
            }

            Runtime runtime = new Runtime(screen, keyboard);

            HopperSystem hopperSystem = new HopperSystem();

            int exitCode = hopperSystem.Load(programFile, Arguments, screen, runtime, false);
            if (exitCode == 0)
            {
                ushort setError = 0;
                exitCode = hopperSystem.Execute(runtime, ref setError, false);
            }
            return exitCode;

        }
    }
}