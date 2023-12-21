using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Media;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    public class Context
    {
        public Context(string programPath, List<string> arguments)
        {
            ProgramPath = programPath;
            Arguments = arguments;
            RemainingClipboardText = "";

            // emulation APIs for small devices
            memoryArray = new byte[64 * 1024];
            memoryCodeArray = new byte[64 * 1024];
#if PROFILE
            CallStats = new SortedDictionary<Instruction, long>();
            SysCallStats = new SortedDictionary<SysCall, long>();
            FnCallStats = new SortedDictionary<ushort, long>();
            FnCallTimeStats = new SortedDictionary<ushort, double>();
            FnCallTimeStack = new List<DateTime>();
            FnCallTimeReturnPCs = new List<ushort>();
            FnCallTimeStackFn = new List<ushort>();
#endif
        }
        public string RemainingClipboardText { get; set; }

        public string ProgramPath { get; }
        public List<string> Arguments { get; }
        public ushort SetError { get; internal set; }
        public ushort ConstantsStart { get; internal set; }
        public ushort BinaryVersion { get; internal set; }
        public ushort EntryPoint { get; internal set; }
        public ushort CodeOffset { get; internal set; }
        public ushort[] MethodTable { get; internal set; }
        public byte[] Code { get; internal set; }

        // emulation APIs for small devices
        public byte[] memoryArray     { get; internal set; }
        public byte[] memoryCodeArray { get; internal set; }

#if PROFILE
        public SortedDictionary<Instruction, long> CallStats { get; set; }
        public SortedDictionary<SysCall, long> SysCallStats { get; set; }
        public SortedDictionary<UInt16, long> FnCallStats { get; set; }
        public SortedDictionary<ushort, double> FnCallTimeStats { get; set; }
        public List<DateTime>                   FnCallTimeStack     { get; set; }
        public List<ushort>                     FnCallTimeStackFn { get; set; }
        public List<ushort>                     FnCallTimeReturnPCs { get; set; }
#endif

        public ushort gpBefore;
        public ushort pcBefore;

        // used for Die
        public ushort bpBefore;
        public ushort spBefore;
        public ushort cspBefore;

        internal void LoadProgram(Screen screen)
        {
            long fileSize = HopperFile.GetSize(ProgramPath);
#if DEBUG
            Diagnostics.ASSERT(fileSize != 0, "program .hexe should not be empty");
#endif
            byte[] code = new byte[fileSize];
            HopperFile binaryFile = HopperFile.Open(ProgramPath);
            uint iCode = 0;
            for (; ; )
            {
                byte b = binaryFile.Read();
                if (!binaryFile.IsValid())
                {
                    break;
                }
                code[iCode] = b;
                iCode++;
            }
#if DEBUG
            Diagnostics.ASSERT(iCode == fileSize, "error loading binary file");
#endif
            Code = code;

            BinaryVersion  = (ushort)(code[0] + (code[1] << 8));
            ConstantsStart = (ushort)(code[2] + (code[3] << 8));
            EntryPoint     = (ushort)(code[4] + (code[5] << 8));

            if ((fileSize - EntryPoint) > 0xFFFF)
            {
                screen.PrintLn(ProgramPath + " is " + fileSize.ToString() + " bytes, more than 64K of code!!", 0xF77, 0);
            }
            CodeOffset = 0;
            if ((BinaryVersion & 0x0001) != 0)
            {
                CodeOffset = EntryPoint;
            }

            ushort iMethod = 6;
            ushort maxCallIndex = 0;
            for (; ; )
            {
                if (iMethod == ConstantsStart)
                {
                    break;
                }
                ushort callIndex = (ushort)(code[iMethod] + (code[iMethod + 1] << 8));
                if (callIndex > maxCallIndex)
                {
                    maxCallIndex = callIndex;
                }
                iMethod += 4;
            }
            MethodTable = new ushort[maxCallIndex + 1];
            iMethod = 6;
            for (; ; )
            {
                if (iMethod == ConstantsStart)
                {
                    break;
                }
                ushort callIndex = (ushort)(code[iMethod] + (code[iMethod + 1] << 8));
                ushort callAddress = (ushort)(code[iMethod + 2] + (code[iMethod + 3] << 8));
                MethodTable[callIndex] = callAddress;
                iMethod += 4;
            }
        }
    }
    public class HopperSystem
    {
        static string currentDirectory = "/";
        List<string> arguments;
        Stack<Context> contextStack = new Stack<Context>();
        ushort hexeVersion;

        public void ClearContextStack()
        {
            contextStack.Clear();
        }
        public int Load(string programPath, List<string> arguments, Screen screen)
        {
            this.arguments = arguments;

            if (programPath.IndexOf('.') == -1)
            {
                programPath = programPath + ".hexe";
            }
            if (!HopperFile.Exists(programPath))
            {
                programPath = "/Bin/" + programPath;
            }
            if (!HopperFile.Exists(programPath))
            {
                Diagnostics.Die(0x0F, null);
            }

            Context context = new Context(programPath, arguments);
            contextStack.Push(context);

            Context currentContext = contextStack.Peek();

            // load the program
            currentContext.LoadProgram(screen);

            hexeVersion = (ushort)(currentContext.Code[0] + (currentContext.Code[1] << 8));

            return currentContext.Code.Length;
        }

        public int Execute(Runtime runtime, ref ushort setError, bool clean)
        {
            setError = 0;

            int exitCode = 0;
            bool errorShown = false;
            Context currentContext = contextStack.Peek();
            try
            {
                // run
                
                exitCode = runtime.Run(currentContext);
                errorShown = Diagnostics.ErrorShown;
                Diagnostics.ErrorShown = false;
#if PROFILE
                SaveCallStats(currentContext);
#endif
            }
            catch (Exception /*ex*/)
            {
                Diagnostics.Die(0x0B, runtime); // internal error
                errorShown = true;
            }

            // on exit, display error info if exitCode != 0
            if ((exitCode != 0) && !errorShown)
            {
                Diagnostics.Die(exitCode, runtime);
            }

            // pop previous context (and unload program)
            setError = currentContext.SetError;
            if ((setError == 0) && (exitCode != 0))
            {
                setError = (ushort)exitCode;
            }
            if (clean)
            {
                contextStack.Pop();
            }
            if (null != currentContext)
            {
                // restore sp and cps
                // reload previous code
            }
            return exitCode;
        }

#if PROFILE
        private void SaveCallStats(Context currentContext)
        {
            string binPath = HopperPath.ToWindowsPath(currentContext.ProgramPath);
            string statPath = System.IO.Path.ChangeExtension(binPath.ToLower().Replace("bin", "debug"), ".txt");
            statPath = statPath.Replace("debugold", "debug");
            StringBuilder sb = new StringBuilder();
            foreach (var kv in currentContext.CallStats)
            {
                sb.AppendLine(kv.Key + "\t" + kv.Value);
            }
            sb.AppendLine();
            foreach (var kv in currentContext.SysCallStats)
            {
                sb.AppendLine(kv.Key + "\t" + kv.Value);
            }
            string codePath = statPath.Replace(@"\debug\", @"\debug\obj\");
            codePath = System.IO.Path.ChangeExtension(codePath, ".code");
            string[] codeLines = File.ReadAllLines(codePath);
            Dictionary<ushort, string> methodNames = new Dictionary<ushort, string>();
            uint i = 0;
            for (; ; )
            {
                if (i >= codeLines.Length)
                {
                    break;
                }
                string currentLine = codeLines[i].Trim();
                currentLine = currentLine.Replace(" ", "");
                if (currentLine.StartsWith(",\"") && currentLine.EndsWith("\":"))
                {
                    currentLine = currentLine.Substring(2, currentLine.Length - 4);
                    ushort methodIndex;
                    if (ushort.TryParse(currentLine, out methodIndex))
                    {
                        for (; ; )
                        {
                            i++;
                            if (i >= codeLines.Length)
                            {
                                break;
                            }
                            currentLine = codeLines[i].Trim();
                            currentLine = currentLine.Replace(" ", "");
                            if (currentLine.StartsWith(",\"name\":"))
                            {
                                currentLine = currentLine.Substring(9, currentLine.Length - 10);
                                methodNames[methodIndex] = currentLine;
                                break;
                            }
                        }

                    }
                }
                i++;
            }
            sb.AppendLine();
            foreach (var kv in currentContext.FnCallStats)
            {
                ushort methodIndex = kv.Key;
                methodIndex = (ushort)(methodIndex & 0x3FFF);
                string methodName = methodNames[methodIndex];
                double totalTime = 0;
                double avgTime = 0;
                if (currentContext.FnCallTimeStats.ContainsKey(methodIndex))
                {
                    totalTime = currentContext.FnCallTimeStats[methodIndex];
                    avgTime = 1.0 * totalTime / kv.Value;
                }
                sb.AppendLine(methodName + "\t" + kv.Value + "\t" + totalTime.ToString("0.####") + "\t" + avgTime.ToString("0.####"));
            }
            System.IO.File.WriteAllText(statPath, sb.ToString());
        }
#endif
        public Context CurrentContext { get { return contextStack.Peek(); } }

        public static void Beep()
        {
            SystemSounds.Beep.Play();
        }

        public List<String> Arguments 
        { 
            get 
            {
                return new List<string>(arguments);
            } 
        }
        public ushort HexeVersion
        {
            get
            {
                return hexeVersion;
            }
        }
        static public string CurrentDirectory { get { return currentDirectory; } set { currentDirectory = value; } }
    }
}
