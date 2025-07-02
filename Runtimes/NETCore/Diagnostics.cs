using System.Diagnostics;
using System.Text;

namespace HopperNET
{
    public class Diagnostics
    {
//#if DEBUG
        static public void ASSERT(bool assertion, string message)
        {
            if (!assertion)
            {
                Debug.WriteLine(message);
            }
        }
        static public void ASSERTDIE(bool assertion, string message, Runtime runtime)
        {
            if (!assertion)
            {
                Debug.WriteLine(message);
                Die(0x0B, runtime);
            }
        }
//#endif

        // lastError codes:
        //   0x00 - ok
        //   0x01 - list index out of range
        //   0x02 - array index out of range
        //   0x03 - no entry for key in dictionary
        //   0x04 - division by zero attempted
        //   0x05 - string index out of range
        //   0x06 - call stack overflow
        //   0x07 - argument stack overflow
        //   0x08 - failed dynamic cast
        //   0x09 - invalid variant type
        //   0x0A - feature not implemented
        //   0x0B - system failure (internal error)
        //   0x0C - memory allocation failure
        //   0x0D - numeric type out of range / overflow
        //   0x0E - error returned from a failing child exe

        internal static bool ErrorShown { get; set; }
        //internal static void Die(int lastError)
        //{
        //    throw new NotImplementedException();
        //}
        internal static void Die(int lastError, Runtime runtime)
        {
            // TODO LARS: we should figure out why we ever arrive here with runtime == null ...
            if (runtime == null) 
            {
                //int why = 0;
                //return; 
            }  

            StringBuilder sb = new StringBuilder();

            uint pc = runtime.InstructionPC;
            string line = "Error: 0x" + lastError.ToString("X2") + " at 0x" + pc.ToString("X4");
            sb.AppendLine(line);
            Debug.WriteLine(line);

            runtime.Screen.PrintLn("  Fatal Error 0x" + lastError.ToString("X2") + " at PC=0x" + pc.ToString("X4") + ":", 0xF77, 0x000);
            string errorMessage = "Unknown error in Die(..)";
            switch (lastError)
            {
                case 0x01: errorMessage = "List index out of range."; break;
                case 0x02: errorMessage = "Array index out of range."; break;
                case 0x03: errorMessage = "Key does not exist in dictionary."; break;
                case 0x04: errorMessage = "Division by zero."; break;
                case 0x05: errorMessage = "String index out of range."; break;
                case 0x06: errorMessage = "Call stack overflow."; break;
                case 0x07: errorMessage = "Value stack overflow."; break;
                case 0x08: errorMessage = "Runtime cast failed."; break;
                case 0x09: errorMessage = "Invalid variant type."; break;
                case 0x0A: errorMessage = "Feature not yet implemented."; break;
                case 0x0B: errorMessage = "Unexpected internal system error."; break;
                case 0x0C: errorMessage = "Memory allocation failure."; break;
                case 0x0D: errorMessage = "Numeric type out of range/overflow."; break;
                case 0x0E: errorMessage = "User error (like compilation failure)."; break;
                case 0x0F: errorMessage = "Invalid or uninitialized delegate."; break;
                case 0x10: errorMessage = "File not found."; break;
            }
            sb.AppendLine(errorMessage);
            Debug.WriteLine(errorMessage);
            runtime.Screen.PrintLn("      " + errorMessage, 0xF77, 0x000);

            List<UInt16> cs = runtime.CallStack;
            int dcsp = cs.Count;
            if (dcsp != 0)
            {
                line = "    Call Stack:";
                sb.AppendLine(line);
                Debug.WriteLine(line);
                runtime.Screen.PrintLn(line, 0xF77, 0x000);
                line = "      0x" + pc.ToString("X4");
                sb.AppendLine(line);
                Debug.WriteLine(line);
                runtime.Screen.PrintLn(line, 0xF77, 0x000);

                int stackLines = 0;
                for (; ; )
                {
                    dcsp = dcsp - 2;
                    if (dcsp <= 0)
                    {
                        break;
                    }
                    UInt16 returnAddress = cs[dcsp];
                    if (returnAddress == 0)
                    {
                        break; // fake return for child process
                    }
                    line = "      0x" + returnAddress.ToString("X4");
                    sb.AppendLine(line);
                    Debug.WriteLine(line);
                    runtime.Screen.PrintLn(line, 0xF77, 0x000);
                    stackLines++;
                    if (stackLines >= 10)
                    {
                        runtime.Screen.PrintLn("      ...", 0xF77, 0x000);
                        break;
                    }
                }
            }
            ErrorShown = true;
            runtime.SetError((ushort)lastError);
            runtime.Screen.Print("  Press any key.", 0xF77, 0x000);
            runtime.Keyboard.ReadKey();
            runtime.Screen.PrintLn();

            string dumpPath = HopperPath.ToWindowsPath("/Debug/crash.log");
            System.IO.File.WriteAllText(dumpPath, sb.ToString());
        }
        static public void OutputDebug(String content)
        {
            Trace.Write(content);
        }
    }
}
