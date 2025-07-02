using System.Diagnostics;

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
        public ushort ProgramOffset { get; internal set; }
        public byte[] memoryArray     { get; internal set; }
        public byte[] memoryCodeArray { get; internal set; }

        public ushort gpBefore;
        public ushort pcBefore;

        public StackSlot r0Before;
        public bool isCDeclBefore;

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

            
            long codeSegmentSize = fileSize - EntryPoint;
            if (codeSegmentSize > 0xFFFF)
            {
                screen.PrintLn(ProgramPath + " is " + codeSegmentSize.ToString() + " bytes, more than 64K of code!! (" + (codeSegmentSize  - 0x10000) + " bytes over)", 0xF77, 0);
            }
            CodeOffset = 0;
            if ((BinaryVersion & 0x0001) != 0)
            {
                CodeOffset = EntryPoint;
            }
            else
            {
                throw new InvalidOperationException(); // only extended code segment now?
            }
            if ((BinaryVersion & 0x0002) == 0)
            {
                throw new InvalidOperationException(); // only flat stack now
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
            uint methodTableSize = 0;
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
                methodTableSize += 4;
            }
            int constantsSize = EntryPoint - ConstantsStart;
            Trace.Write("\n" + ProgramPath + " has " + codeSegmentSize.ToString() + " bytes of code, method table is " +methodTableSize.ToString() + " bytes, constant segment is " + constantsSize.ToString() + " bytes");

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
        public int Load(string programPath, List<string> arguments, Screen screen, Runtime runtime, bool dieOnError)
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
                if (dieOnError)
                {
                    Diagnostics.Die(0x10, runtime);
                }
                else
                {
                    return -1; // file not found
                }
            }

            Context context = new Context(programPath, arguments);
            contextStack.Push(context);

            Context currentContext = contextStack.Peek();

            // load the program
            currentContext.LoadProgram(screen);

            hexeVersion = (ushort)(currentContext.Code[0] + (currentContext.Code[1] << 8));

            return 0;
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


        public Context CurrentContext { get { return contextStack.Peek(); } }

        public List<String> Arguments 
        { 
            get 
            {
                return new List<string>(arguments);
            } 
        }
        static public string CurrentDirectory { get { return currentDirectory; } set { currentDirectory = value; } }
    }
}
