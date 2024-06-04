using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Timers;
using System.Windows.Forms;
using System.Xml.Linq;
using static System.Net.Mime.MediaTypeNames;

namespace HopperNET
{
    public enum StepTypes
    {
        None,
        Run,
        Into,
        Over
    }

    public enum Instruction
    {
        NOP    = 0x00,
        DUP0   = 0x01,       // push [top]

        PUSHR0 = 0x02,       // R0 -> [top]
        POPR0  = 0x03,       // [top] -> R0

        BITSHL8  = 0x04,
        BITSHR8  = 0x05,
        BITANDFF = 0x06,

        LIBCALL  = 0x08,
        LIBCALL0 = 0x09,
        LIBCALL1 = 0x0A,

        SYSCALL2 = 0x0B, // syscall <byte operand>, overload 2

        BITSHLB  = 0x0C,
        BITSHRB  = 0x0D,
        BITANDB  = 0x0E,
        BITORB   = 0x0F,

        PUSHIB = 0x1A,       // operand is byte
        POPLOCALB,    // operand is the location to pop to: BP + offset
        PUSHLOCALB,   // operand is the location to push from: BP + offset
        POPRELB,      // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHRELB,     // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBALB,   // operand is the absolute address to pop to
        PUSHGLOBALB,  // operand is the absolute address to push from
        PUSHSTACKADDRB, // operand is the offset from BP of the variable - convert to absolute stack address and push that

        INCLOCALB,
        DECLOCALB,

        SYSCALL0,     // syscall <byte operand>, overload 0
        SYSCALL1,     // syscall <byte operand>, overload 1
        SYSCALL,      // syscall <byte operand>,  overload [next]

        DUP,          // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
        DECSP,

        DIE,       // 0x?? fail setting lastError to <byte operand>

        RETB,      // RET and pop <byte operand> bytes
        RETRESB,   // RET and pop <byte operand> bytes, but preserve [top] as the return value

        CALLB,     // <byte index operand>
        TESTBPB,   // verify that BP is what we expect it to be

        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZB,       // <signed byte offset>
        JNZB,      // <signed byte offset>
        JB,        // <signed byte offset>


        // instructions before here have a single byte operand

        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZ,        // <signed int offset>
        JNZ,       // <signed int offset>
        J,         // <signed int offset>

        CALL,     // <integer index operand>

        RET,      // RET and pop <uint operand> bytes
        RETRES,   // RET and pop <uint operand> bytes, but preserve [top] as the return value

        PUSHI,          // operand is uint
        POPLOCAL,       // operand is the location to pop to: BP + offset
        PUSHLOCAL,      // operand is the location to push from: BP + offset
        POPREL,         // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHREL,        // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBAL,      // operand is the absolute address to pop to
        PUSHGLOBAL,     // operand is the absolute address to push from
        PUSHSTACKADDR,  // operand is the offset from BP of the variable - convert to absolute stack address and push that

        INCLOCALBB,
        PUSHILE,

        // instructions before here have a word operand (or two bytes)




        BOOLNOT,      // ![top] -> [top]
        BITNOT,       // ~[top] -> [top]

        SWAP,         // swap [top] and [next] (consider object manager stack slots?)

        PUSHI0,
        PUSHI1,
        PUSHIM1,
        PUSHGP,        // GP, the global "floor" for child processes
        COPYNEXTPOP,   // what follows is a pop of a reference into a variable - should it be made into a copy?

        ENTER,
        RET0,

        CALLREL,   // call delegate based on <index> in [top]

        POPLOCALB00,
        POPLOCALB01,
        PUSHLOCALB00,
        PUSHLOCALB01,

        unusedNOP2,

        CAST, // operand is value type (byte) - change top of stack to this type

        PUSHGLOBALBB,  // operand is the absolute address to push from, x2
        INCGLOBALB,
        DECGLOBALB,

        PUSHILT,

        PUSHLOCALBB,

        POPCOPYLOCALB,
        POPCOPYRELB,
        POPCOPYGLOBALB,
        POPCOPYLOCAL,
        POPCOPYREL,
        POPCOPYGLOBAL,

        POPCOPYLOCALB00,
        POPCOPYLOCALB01,

        ENTERB,

        PUSHD,

        RETFAST,

        PUSHDB,
        EXIT,
        //BITXOR0,

        PUSHILEI = 0x65,
        INCGLOBALBB,

        JREL,
        JIXB,
        JIX,

        CALLI,

        PUSHIBLE,
        PUSHIBEQ,

        ADDB,
        SUBB,

        // pop 2 -> operation -> push 1: (bit 0 set means 'signed')
        ADD   = 0x80,
        ADDI  = 0x81,
        SUB   = 0x82,
        SUBI  = 0x83,
        DIV   = 0x84,
        DIVI  = 0x85,
        MUL   = 0x86,
        MULI  = 0x87,
        MOD   = 0x88,
        MODI  = 0x89,
        
        GT    = 0x8A,
        GTI   = 0x8B,
        LT    = 0x8C,
        LTI   = 0x8D,
        GE    = 0x8E,
        GEI   = 0x8F,
        LE    = 0x90,
        LEI   = 0x91,

        // pop 2 -> operation -> push 1: (bit 0 set means 'signed' so these are always unsigned)

        EQ      = 0x92,
        NE      = 0x94,
        BOOLOR  = 0x96,
        BOOLAND = 0x98,
        BITAND  = 0x9A,
        BITOR   = 0x9C,
        BITXOR  = 0x9E,
        BITSHR  = 0xA0,
        BITSHL  = 0xA2,

        INCLOCALIBB = 0xA3,
        INCLOCALIB  = 0xA4,
        INCGLOBALIB = 0xA5,
        DECLOCALIB  = 0xA6,
        DECGLOBALIB = 0xA7,

        SYSCALLB0   = 0xA8, // PUSHIB   SYSCALL0
        SYSCALL00   = 0xA9, // SYSCALL0 SYSCALL0
        PUSHIBB     = 0xAA, // PUSHIB   PUSHIB
        SYSCALLB1   = 0xAB, // PUSHIB   SYSCALL1
        SYSCALL01   = 0xAC, // SYSCALL0 SYSCALL1
        SYSCALL10   = 0xAD, // SYSCALL1 SYSCALL0

        UNDEFINED,
    };

    public enum SysCall
    {
        StringNewFromConstant = 0x00,
        StringBuild = 0x01,
        StringNew = 0x02,
        StringBuildFront = 0x03,
        ArrayNewFromConstant = 0x04,
        TimeSecondsGet = 0x05,
        StringLengthGet = 0x06,
        TimeDelay = 0x07,
        DiagnosticsDie = 0x08,
        SerialConnect = 0x09,
        StringGetChar = 0x0A,
        ArrayNew = 0x0B,
        ArrayCountGet = 0x0C,
        ArrayGetItem = 0x0D,
        ArraySetItem = 0x0E,

        SerialReadChar = 0x0F,
        SerialWriteChar  = 0x10,
        SerialIsAvailableGet = 0x11,
        MemoryReadByte = 0x12,
        MemoryWriteByte = 0x13,
        MemoryAvailable = 0x14,
        MemoryMaximum = 0x15,
        MemoryAllocate = 0x16,
        MemoryFree = 0x17,
        ByteToHex = 0x18,

        IntGetByte   = 0x19,
        IntFromBytes = 0x1A,
        ArraySlice   = 0x1B,
        ArrayItemTypeGet = 0x1C,

        LongNew = 0x1D,
        LongNewFromConstant = 0x1E,
        LongFromBytes = 0x1F,
        LongGetByte = 0x20,
        FloatNew = 0x21,
        FloatNewFromConstant = 0x22,
        FloatFromBytes = 0x23,
        FloatGetByte = 0x24,
        TimeMillisGet = 0x25,
        //VariantType = 0x26,
        VariantBox = 0x27,
        VariantUnBox = 0x28,
        ScreenPrint = 0x29,
        ScreenPrintLn = 0x2A,
        ScreenClear = 0x2B,
        ScreenSetCursor = 0x2C,
        ScreenColumnsGet = 0x2D,
        ScreenRowsGet = 0x2E,
        ScreenCursorXGet = 0x2F,
        ScreenCursorYGet = 0x30,
        ScreenSuspend = 0x31,
        ScreenResume = 0x32,
        ScreenDrawChar = 0x33,
        IntToFloat = 0x34,
        IntToLong = 0x35,
        UIntToLong = 0x36,
        UIntToInt = 0x37,
        LongToString = 0x38,
        //LongToBytes = 0x39,
        LongToFloat = 0x3A,
        LongToInt = 0x3B,
        LongToUInt = 0x3C,
        //LongNew2 = 0x3D,
        //LongNewFromConstant2 = 0x3E,
        LongAdd = 0x3F,
        LongSub = 0x40,
        LongDiv = 0x41,
        LongMul = 0x42,
        LongMod = 0x43,
        LongEQ = 0x44,
        LongLT = 0x45,
        LongLE = 0x46,
        LongGT = 0x47,
        LongGE = 0x48,
        LongNegate = 0x49,
        FloatToString = 0x4A,
        //FloatToBytes = 0x4B,
        //FloatNew2 = 0x4C,
        //FloatNewFromConstant2 = 0x4D,
        FloatAdd = 0x4E,
        FloatSub = 0x4F,
        FloatDiv = 0x50,
        FloatMul = 0x51,
        FloatEQ = 0x52,
        FloatLT = 0x53,
        FloatLE = 0x54,
        FloatGT = 0x55,
        FloatGE = 0x56,
        //TimeMillisGet2 = 0x57,
        ScreenShowCursorSet = 0x58,
        SystemArgumentsGet = 0x59,
        SystemCurrentDirectoryGet = 0x5A,
        SystemCurrentDirectorySet = 0x5B,
        SystemBeep = 0x5C,

        PairNew = 0x5D,
        StringAppend = 0x5E,

        FileExists = 0x5F,
        FileNew = 0x60,
        FileOpen = 0x61,
        FileCreate = 0x62,
        FileReadLine = 0x63,
        FileRead = 0x64,
        FileIsValid = 0x65,
        FileAppend = 0x66,
        FileFlush = 0x67,
        FileDelete = 0x68,
        FileGetSize = 0x69,
        DirectoryExists = 0x6A,
        DirectoryNew = 0x6B,
        DirectoryIsValid = 0x6C,
        DirectoryOpen = 0x6D,
        DirectoryGetDirectoryCount = 0x6E,
        DirectoryGetFileCount = 0x6F,
        DirectoryGetFile = 0x70,
        DirectoryGetDirectory = 0x71,
        KeyboardReadKey = 0x72,
        KeyboardIsAvailableGet = 0x73,
        KeyboardToKey = 0x74,
        KeyboardClickXGet = 0x75,
        KeyboardClickYGet = 0x76,
        KeyboardClickUpGet = 0x77,
        KeyboardClickDoubleGet = 0x78,
        KeyboardScrollDeltaGet = 0x79,
        DiagnosticsOutputDebug = 0x7A,
        DiagnosticsAssert = 0x7B,
        
        DiagnosticsSetError = 0x7D,
        TypesTypeOf = 0x7E,
        TypesValueTypeOf = 0x7F,
        TypesKeyTypeOf = 0x80,
        TypesBoxTypeOf = 0x81,
        TypesVerifyValueTypes = 0x82,

        WiFiConnect = 0x84,
        WiFiIPGet = 0x85,
        WiFiStatusGet = 0x86,
        WiFiDisconnect = 0x87,

        DirectoryCreate = 0x89,
        DirectoryDelete = 0x8A,

        RuntimePCGet = 0x8B,
        RuntimeSPGet = 0x8C,
        RuntimeBPGet = 0x8D,
        RuntimeCSPGet = 0x8E,

        RuntimeGetStackWord = 0x8F,
        RuntimeGetStackType = 0x90,
        RuntimeGetCallStackWord = 0x91,

        RuntimeExecute = 0x92,
        RuntimeInline = 0x93,

        RuntimeUserCodeGet = 0x94,

        TimeTime_Get = 0x95,
        TimeDate_Get = 0x96,

        RuntimeInDebuggerGet = 0x97,
        RuntimeDateTimeGet = 0x98,

        MemoryReadProgramByte = 0x99,
        MemoryWriteProgramByte = 0x9A,
        MemoryReadProgramWord = 0x9B,
        MemoryWriteProgramWord = 0x9C,

        FileGetDate = 0x9D,
        DirectoryGetDate = 0x9E,

        MemoryProgramOffsetSet = 0x9F,

        SerialWriteString = 0xA0,

        PairKey = 0xA1,

        DictionaryNew = 0xA2,
        SerialClose = 0xA3,
        SerialIsValid = 0xA4,

        StringInsertChar = 0xA5,
        PairValue = 0xA6,
        CharToString = 0xA7,


        // 6502
        HardwareLEDSet = 0xA8,
        DictionaryCountGet = 0xA9,
        DictionarySet = 0xAA,
        DictionaryContains = 0xAB,
        DictionaryGet = 0xAC,
        DictionaryNext = 0xAD,
        DictionaryClear = 0xAE,


        TraceSet = 0xAF,
        TraceGet = 0xB0,
        DictionaryHashKey = 0xB1,

        ClipboardHasTextGet = 0xB2,
        ClipboardGetText = 0xB3,
        ClipboardSetText = 0xB4,

        // 6502
        MemoryReadBit = 0xB6,
        MemoryWriteBit = 0xB7,
        CharToUpper = 0xB8,
        CharIsUpper = 0xB9,
        CharIsDigit = 0xBA,
        CharIsLetterOrDigit = 0xBB,
        CharIsLower = 0xBC,
        ByteToDigit = 0xBD,
        CharIsHexDigit = 0xBF,
        CharToLower = 0xC0,
        StringStartsWith = 0xC1,
        StringContains = 0xC2,
        StringIndexOf = 0xC3,
        SystemWarp_Set = 0xC4,
        SystemWarp_Get = 0xC5,

        //LongInc = 0xC7,
        //LongAddRef = 0xC8,
        //LongMulRef = 0xC9,

        // ArrayGetItemUInt = 0xCA,
        // ArraySetItemUInt = 0xCB,

        FileGetTimeStamp = 0xCC,
        //IntToBytes = 0xCD,

        FileGetTime = 0xCE,
        DirectoryGetTime = 0xCF,

        StringTrim = 0xD0,
        StringTrimLeft = 0xD1,
        StringTrimRight = 0xD2,
        StringPushImmediate = 0xD3,
        StringToUpper = 0xD4,
        StringToLower = 0xD5,

        ClipboardGetChar = 0xD6,

        MemoryReadWord = 0xD7,
        MemoryWriteWord = 0xD8,

        MemoryReadCodeByte = 0xDC,
        MemoryWriteCodeByte = 0xDD,
        MemoryReadCodeWord = 0xDE,
        MemoryWriteCodeWord = 0xDF,

        //LongGetByte2 = 0xE0,
        //FloatGetByte2 = 0xE2,
        //LongFromBytes2 = 0xE3,
        //FloatFromBytes2 = 0xE5,
        UIntToFloat = 0xE6,

        SerialPortsGet = 0xE7,

        StringCompare   = 0xE8,
        StringEndsWith  = 0xE9,
        StringSubstring = 0xEA,
        StringReplace = 0xEB,


        FloatToUInt = 0xEC,
        FloatToLong = 0xED,

        LongAddB = 0xEE,
        LongSubB = 0xEF,

        FloatSin   = 0xF0,
        FloatCos   = 0xF1,
        FloatATan2 = 0xF2,
        FloatSqrt  = 0xF3,

        ListNew = 0xF4,
        ListCountGet = 0xF5,
        ListAppend = 0xF6,
        ListInsert = 0xF7,
        ListGetItem = 0xF8,
        ListGetItemAsVariant = 0xF9,
        ListSetItem = 0xFA,
        ListClear = 0xFB,
        ListRemove = 0xFC,
        ListContains = 0xFD,
        

    };

    enum LibCall
    {
        TimerStart = 0x00,
        TimerStop = 0x01,
        TimerAlarm = 0x02,
        TimerCancel = 0x03,
    };

    public enum HopperType // if you change this, look at the end of ToByte(..) in Types.hs
    {
        tUndefined,

        tChar,   // char (for now)
        tInt,    // 16 bit signed

        tByte,   // unsigned char
        tUInt,   // internal type for unsigned 16 bit int (tFlags and tEnum)

        tReference,  // internal type for "ref" addresses (tUInt)

        tBool,
        tEnum,   // 16 bit ordinal named values : cannot be used in bit expressions
        tFlags,  // 16 bit named masks (similar to enums) : can be used in bit expressions

        tUnused1, // tIntP
        tUnused2, // tIntN

        tDelegate,
        tType, // for typeof(...) values like 'int' or 'dictionary'

        tFloat,    // 0x0D
        tLong,

        tString,   // 0x0F 
        
        tPair,     // key -> value pair
        tUnused3,  // tListItem
        tArray,
        tDictionary,
        
        tVariant,
                
        tFile,
        tDirectory,

        tUnused4, // tKey
        tUnused5, // tValue

        tList,

    }

    public class StackSlot
    {
        public StackSlot()
        {
#if DEBUG
            value = 0;
            reference = null;
            type = HopperType.tUndefined;
#endif
        }
        public HopperType type;
        public uint value;
        public Variant reference;
        public override string ToString()
        {
            if (type == HopperType.tUndefined)
            {
                return "";
            }
            return Runtime.Type_ToString(type);
        }
    }

    public class Runtime
    {


        static public bool Type_IsValueType(HopperType type)
        {
            /*
            bool isValue = false;
            switch (type)
            {
                case HopperType.tChar:
                case HopperType.tInt:
                case HopperType.tByte:
                case HopperType.tUInt:
                case HopperType.tBool:
                case HopperType.tEnum:
                case HopperType.tFlags:
                case HopperType.tDelegate:
                case HopperType.tType:
                case HopperType.tLong:
                case HopperType.tFloat:
                case HopperType.tReference:
                    isValue = true;
                    break;
            }
            return isValue;
            */
            return (type <= HopperType.tLong);
        }
        static public bool Type_IsCompoundType(HopperType type)
        {
            switch (type)
            {
                case HopperType.tList:
                case HopperType.tDictionary:
                case HopperType.tArray:
                    return true;
                default:
                    return false;
            }
        }

        static public bool Type_IsKeyType(HopperType type)
        {
            switch (type)
            {
                case HopperType.tByte:
                case HopperType.tChar:
                case HopperType.tEnum:
                case HopperType.tUInt:
                case HopperType.tString:
                    return true;
                default:
                    return false;
            }
        }

        static public string Type_ToString(HopperType type)
        {
            switch (type)
            {
                case HopperType.tByte:
                    return "byte";
                case HopperType.tBool:
                    return "bool";
                case HopperType.tChar:
                    return "char";
                case HopperType.tEnum:
                    return "enum";
                case HopperType.tFlags:
                    return "flags";
                case HopperType.tUInt:
                    return "uint";
                case HopperType.tInt:
                    return "int";
                case HopperType.tFloat:
                    return "float";
                case HopperType.tLong:
                    return "long";
                case HopperType.tString:
                    return "string";
                case HopperType.tList:
                    return "list";
                case HopperType.tDictionary:
                    return "dictionary";
                case HopperType.tArray:
                    return "array";
                case HopperType.tPair:
                    return "pair";
                case HopperType.tType:
                    return "type";
                case HopperType.tVariant:
                    return "variant";
                case HopperType.tFile:
                    return "file";
                case HopperType.tDirectory:
                    return "directory";
                case HopperType.tDelegate:
                    return "delegate";
                case HopperType.tReference:
                    return "reference";
                default:
#if DEBUG
                    Diagnostics.ASSERT(false, "not implemented");
#endif
                    return "undefined";
            }
        }

        private Screen screen;
        private IHopper hopper;
        private Keyboard keyboard;
        private ushort[] methodTable;
        private byte[] code;
        
        private ushort pcStore;
        private ushort bpStore;
        private ushort spStore;
        private ushort cspStore;
        private byte[] codeStore;
        
        ushort currentISR = 0;
        ushort preISRcsp  = 0;
        bool inISR = false;

        ushort pc = 0;
        uint instructionPC = 0;
        ushort sp = 0; // ok
        ushort bp = 0;
        ushort gp = 0;
        ushort csp = 0;

        StackSlot r0 = new StackSlot();
        bool isCDecl = false;

        StackSlot[] stack;
        ushort[] callstack;
        ushort lastError;

        const uint STACKSIZE = 2048;
        const uint CALLSTACKSIZE = 256;
        
        public uint PC { get { return pc; } }
        public uint InstructionPC 
        { 
            get 
            {
                uint pc = instructionPC;
                if (codeStore != null)
                {
                    uint codeSize = (uint)codeStore.Length;
                    if (codeSize < pc)
                    {
                        pc = pc - codeSize; // inline user code PC
                    }
                }
                return pc; 
            } 
        }
        public uint SP { get { return sp; } } // ok
        public uint BP { get { return bp; } }
        public uint CSP { get { return csp; } }
        public Screen Screen { get { return screen; } }
        public List<UInt16> CallStack
        {
            get
            {
                List<UInt16> cs = new List<ushort>();
                for (int i = 0; i < csp; i++)
                {
                    cs.Add(callstack[i]);
                }
                return cs;
            }
        }

        HopperSystem hopperSystem = null;

        internal Keyboard Keyboard { get { return keyboard; } }

        public Runtime(Screen screen, Keyboard keyboard)
        {
            this.screen = screen;
            this.keyboard = keyboard;
            this.hopper = screen.Console.Hopper;
            hopperSystem = new HopperSystem();

            stack = new StackSlot[STACKSIZE];
            for (uint i = 0; i < STACKSIZE; i++)
            {
                stack[i] = new StackSlot();
            }
            callstack = new ushort[CALLSTACKSIZE];
        }

        public void Load(string programPath, List<string> arguments)
        {
            hopperSystem.Load(programPath, arguments, screen);
        }
        public int Execute(ref ushort setError, bool clean)
        {
            return hopperSystem.Execute(this, ref setError, clean);
        }

        void PushCS(ushort address)
        {
            if (csp == CALLSTACKSIZE)
            {
                Diagnostics.Die(0x06, this);
                return;
            }
            callstack[csp] = address;
            csp++;
        }
        ushort PopCS()
        {
            if (csp == 0)
            {
                Diagnostics.Die(0x06, this);
                return 0;
            }
            csp--;
            ushort value = callstack[csp];
#if DEBUG
            callstack[csp] = 0;
#endif
            return value;
        }
        void PushBool(bool value)
        {
            Push((uint)(value ? 1 : 0), HopperType.tBool);
        }
        void PushInt(short value)
        {
            Int32 v = value;
            ushort sp2 = sp;
            stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(v), 0);
            stack[sp2].type = HopperType.tInt;
#if DEBUG
            stack[sp2].reference = null;
#endif
            sp++;
#if DEBUG
            Diagnostics.ASSERT(sp < STACKSIZE, "stack overflow");
#endif
        }
        void PushLong(Int32 value)
        {
            ushort sp2 = sp;
            stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(value), 0);
            stack[sp2].type = HopperType.tLong;
#if DEBUG
            stack[sp2].reference = null;
#endif
            sp++;
#if DEBUG
            Diagnostics.ASSERT(sp < STACKSIZE, "stack overflow");
#endif
        }
        void PushFloat(float value)
        {
            ushort sp2 = sp;
            stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(value), 0);
            stack[sp2].type = HopperType.tFloat;
#if DEBUG
            stack[sp2].reference = null;
#endif
            sp++;
#if DEBUG
            Diagnostics.ASSERT(sp < STACKSIZE, "stack overflow");
#endif
        }
        void Push(uint value, HopperType type)
        {
            //LDB Debug.WriteLine("Push {0} {1}", value, (int)type);
            if (sp == STACKSIZE)
            {
                Diagnostics.Die(0x07, this);
                return;
            }
            ushort sp2 = sp;
            stack[sp2].value = value;
            stack[sp2].type = type;

            if (stack[sp2].type == HopperType.tUInt)
            {
                if (stack[sp2].value > 0xFFFF)
                {
                    //int why = 0;
                }
            }

#if DEBUG
            stack[sp2].reference = null;
#endif
            sp++;
#if DEBUG
            Diagnostics.ASSERT(sp < STACKSIZE, "stack overflow");
#endif
        }
        void PutStack(ushort address, uint value, HopperType type)
        {
            ushort address2 = (address);
            stack[address2].value = value;
            stack[address2].type = type;
#if DEBUG
            stack[address2].reference = null;
#endif
        }
        void PutStackVariant(ushort address, Variant variant)
        {
            HopperType type = variant.Type;
            if (Type_IsValueType(type))
            {
                type = HopperType.tVariant;
            }
#if DEBUG
            else
            {
                variant.Validate();
            }
#endif
            ushort address2 = (address);
#if DEBUG
            stack[address2].value = 0;
#endif
            stack[address2].type = type;
            stack[address2].reference = variant;
        }
        void Push(Variant variant)
        {
            HopperType type = variant.Type;
            if (Type_IsValueType(type))
            {
                type = HopperType.tVariant;
            }
#if DEBUG
            else
            {
                variant.Validate();
            }
#endif
            ushort sp2 = sp;
#if DEBUG
            stack[sp2].value = 0;
#endif
            stack[sp2].type = type;
            stack[sp2].reference = variant;
            sp++;
#if DEBUG
            Diagnostics.ASSERT(sp < STACKSIZE, "stack overflow");
#endif
        }
        void ClearStack(ushort operand)
        {
#if DEBUG
            while (operand > 0)
            {
                operand -= 1;
                sp--;
                stack[sp].value = 0;
                stack[sp].reference = null;
                stack[sp].type = HopperType.tUndefined;
            }
#else
            sp -= operand;
#endif
        }
        short PopInt()
        {
#if DEBUG
            Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
            sp--;
#if DEBUG
            Diagnostics.ASSERTDIE(stack[sp].reference == null, "value type", this);
#endif
            ushort sp2 = sp;
            short result = 0;
            HopperType type = stack[sp2].type;
            uint value = stack[sp2].value;
            switch (type)
            {
                case HopperType.tChar:
                case HopperType.tByte:
                    result = (byte)value;
                    break;
                case HopperType.tInt:
                    byte[] bytes = BitConverter.GetBytes(value);
                    result = BitConverter.ToInt16(bytes, 0);
                    break;
                case HopperType.tUInt:
                    if (value > 32767)
                    {
                        Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                        break;
                    }
                    result = (short)value;
                    break;
                case HopperType.tLong:
                    {
                        Int32 l = BitConverter.ToInt32(BitConverter.GetBytes(value), 0);
                        if ((l > 32767) || (l < -32768))
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        result = (short)l;
                    }
                    break;
                default:
#if DEBUG
                    Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tInt, "tInt expected", this);
#endif
                    break;
            }
#if DEBUG
            stack[sp2].value = 0;
            stack[sp2].reference = null;
            stack[sp2].type = HopperType.tUndefined;
#endif

            return result;
            
        }
        Int32 PopLong()
        {
#if DEBUG
            Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
            sp--;
            ushort sp2 = sp;
#if DEBUG
            Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);            // TODO
            Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tLong, "tLong expected", this); // TODO
#endif
            uint value = stack[sp2].value;
#if DEBUG
            stack[sp2].value = 0;
            stack[sp2].reference = null;
            stack[sp2].type = HopperType.tUndefined;
#endif
            return BitConverter.ToInt32(BitConverter.GetBytes(value), 0); ;
        }
        float PopFloat()
        {
#if DEBUG
            Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
            sp--;
            ushort sp2 = sp;
#if DEBUG
            Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);
            Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tFloat, "tFloat expected", this);
#endif
            uint value = stack[sp2].value;
#if DEBUG
            stack[sp2].value = 0;
            stack[sp2].reference = null;
            stack[sp2].type = HopperType.tUndefined;
#endif
            return BitConverter.ToSingle(BitConverter.GetBytes(value), 0); ;
        }

        uint Pop()
        {
#if DEBUG
            Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
            sp--;
            ushort sp2 = sp;
#if DEBUG
            Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);
#endif
            uint value = stack[sp2].value;
#if DEBUG
            stack[sp2].value = 0;
            stack[sp2].reference = null;
            stack[sp2].type = HopperType.tUndefined;
#endif
            return value;
        }
        Variant PopVariant(HopperType expectedType)
        {
#if DEBUG
            Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
            sp--;
            ushort sp2 = sp;
#if DEBUG
            Diagnostics.ASSERTDIE(stack[sp2].reference != null, "reference type", this);
#endif
            Variant variant = stack[sp2].reference;
#if DEBUG
            if (expectedType != HopperType.tUndefined)
            {
                Diagnostics.ASSERTDIE(stack[sp2].type == expectedType, "reference type", this);
            }

            stack[sp2].value = 0;
            stack[sp2].reference = null;
            stack[sp2].type = HopperType.tUndefined;
#endif
            return variant;
        }

        ushort GetCallStack(ushort address)
        {
            ushort value = callstack[address];
            return value;
        }
        HopperType GetStackType(ushort address)
        {
            return stack[(address)].type;
        }
        uint GetStack(ushort address)
        {
            ushort address2 = (address);
#if DEBUG
            Diagnostics.ASSERTDIE(stack[address2].reference == null, "value type", this);
#endif
            if (stack[address2].type == HopperType.tUInt)
            {
                if (stack[address2].value > 0xFFFF)
                {
                    //int why = 0;
                }
            }
            return stack[address2].value;
        }
        Variant GetStackVariant(ushort address)
        {
            ushort address2 = (address);
            HopperType type = stack[address2].type;
            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
            {
                // these two are actually value type in the .NET version of the Runtime
                return new HopperValue(stack[address2].value, type);
            }
            else
            {
#if DEBUG
                Diagnostics.ASSERTDIE(stack[address2].reference != null, "reference type", this);
#endif
                return stack[address2].reference;
            }
        }
        
        public void ISR(ushort handlerIndex)
        {
            // don't return from this method until the ISR returns
            while (currentISR != 0)
            {
                Thread.Sleep(1);
            }
            currentISR = handlerIndex;
            while (currentISR != 0)
            {
                Thread.Sleep(1);
            }
        }
        bool singlestepping;

        bool waiting = false;
        public bool Halted { get; set; }
        public bool Waiting { get { return waiting; } }
        public bool Stepping { get { return singlestepping; } set { singlestepping = value; } }


        private void Step(StepTypes stepType)
        {
            lock (this)
            {
                StepType = stepType;
                switch (stepType)
                {
                    case StepTypes.Into:
                        Steps++;
                        break;
                    case StepTypes.Over:
                        StepCSP = csp;
                        Steps++;
                        break;
                    case StepTypes.Run:
                        Steps = 0;
                        break;
                }
            }
        }
        StepTypes StepType { get; set; }
        uint Steps { get; set; }
        uint StepCSP { get; set; }

        List<uint> breakpoints = new List<uint>();
        List<uint> statements = new List<uint>();

        private void SetBreakpoint(uint address)
        {
            if (!breakpoints.Contains(address))
            {
                breakpoints.Add(address);
            }
        }

        private void SetStatement(uint address)
        {
            if (!statements.Contains(address))
            {
                statements.Add(address);
            }
        }

        private void ClearStatements()
        {
            statements.Clear();
        }

        private void ClearBreakpoints()
        {
            breakpoints.Clear();
        }

        void WaitForStep()
        {
            for (; ; )
            {
                if (!statements.Contains(pc))
                {
                    break; // only stop on statements
                }
                if (Halted)
                {
                    //Debug.WriteLine("Halted 0x" + pc.ToString("X4"));
                    break;
                }
                lock (this)
                {
                    if (StepType == StepTypes.Run)
                    {
                        if (breakpoints.Contains(pc))
                        {
                            StepType = StepTypes.Into; // break!
                        }
                        else
                        {
                            //Debug.WriteLine("Run: 0x" + pc.ToString("X4"));
                            break;
                        }
                    }
                    if (StepType == StepTypes.Over)
                    {
                        if (breakpoints.Contains(pc))
                        {
                            StepType = StepTypes.Into; // break!
                        }
                        else
                        {
                            if (csp > StepCSP)
                            {
                                //Debug.WriteLine("csp != StepCSP: 0x" + pc.ToString("X4"));
                                break; // keep going until we return to this position in the call stack
                            }
                        }
                    }

                    if (Steps > 0)
                    {
                        Steps--;
                        //Debug.WriteLine("Step 0x" + pc.ToString("X4"));
                        break;
                    }
                }
                waiting = true;
            } // for (;;)
            waiting = false;
            
        }
        public int Run(Context currentContext)
        {
            lastError = 0;

            // do a fake CALL to load csp to detect final RET
            PushCS(0);
            pc = (ushort)(currentContext.EntryPoint - currentContext.CodeOffset); // ?
            gp = sp; // floor for globals (matters for child processes)

            code = currentContext.Code;
            methodTable = currentContext.MethodTable;
            
            Halted = false;
            waiting = false;

            if (singlestepping)
            {
                Steps = 0;
                StepType = StepTypes.Into;
                //Debug.WriteLine("EntryPoint: 0x" + pc.ToString("X4") + ", Steps=" + Steps.ToString());
            }

            ushort operand;
            bool copyNextPop = false;
            for (; ; )
            {
                if (singlestepping)
                {
                    //Debug.WriteLine("PC: 0x" + pc.ToString("X4") + ", Steps=" + Steps.ToString());
                    WaitForStep();
                    if (Halted)
                    {
                        break;
                    }
                }
                //Debug.WriteLine("PC: 0x" + pc.ToString("X4"));
                Instruction opCode = (Instruction)code[pc + currentContext.CodeOffset];
                instructionPC = pc;
                if (pc == 0x2C5A)
                {
                    //int why = 0;
                }
                if (!inISR && (currentISR != 0))
                {
                    if (copyNextPop)
                    {
                        // don't break this
                    }
                    else
                    {
                        preISRcsp = csp;
                        inISR = true;
                        PushCS(pc);
                        pc = methodTable[currentISR];
                        continue;
                    }
                }
                pc++;
                

                switch (opCode)
                {
                    case Instruction.ADD:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            Push(next + top, HopperType.tUInt);
#else
                            stack[sp2].value = next + top;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;
                    case Instruction.SUB:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        Push(next - top, HopperType.tUInt);
#else
                            stack[sp2].value = next - top;
                            stack[sp2].type = HopperType.tUInt;
#endif

                        }
                        break;
                    case Instruction.MUL:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        Push(next * top, HopperType.tUInt);
#else
                            stack[sp2].value = next * top;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;
                    case Instruction.DIV:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
                            if (top == 0)
                            {
                                Diagnostics.Die(0x04, this);
                                break;
                            }
#if UNDOINLINED
                        Push(next / top, HopperType.tUInt);
#else
                            stack[sp2].value = next / top;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;
                    case Instruction.MOD:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
                            if (top == 0)
                            {
                                Diagnostics.Die(0x04, this);
                                break;
                            }
#if UNDOINLINED
                        Push(next % top, HopperType.tUInt);
#else
                            stack[sp2].value = next % top;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;

                    case Instruction.EQ:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(next == top);
#else
                            stack[sp2].value = (uint)((next == top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.NE:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(next != top);
#else
                            stack[sp2].value = (uint)((next != top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.LE:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(next <= top);
#else
                            stack[sp2].value = (uint)((next <= top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.LT:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(next < top);
#else
                            stack[sp2].value = (uint)((next < top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.GE:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(next >= top);
#else
                            stack[sp2].value = (uint)((next >= top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.GT:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        PushBool(next > top);
#else
                            stack[sp2].value = (uint)((next > top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;

                    case Instruction.GTI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushBool(nexti > topi);
                        }
                        break;
                    case Instruction.GEI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushBool(nexti >= topi);
                        }
                        break;

                    case Instruction.LTI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushBool(nexti < topi);
                        }
                        break;
                    case Instruction.LEI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushBool(nexti <= topi);
                        }
                        break;

                    case Instruction.BITOR:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        Push(next | top, HopperType.tUInt);
#else
                            stack[sp2].value = (next | top);
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;
                    case Instruction.BITAND:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        Push(next & top, HopperType.tUInt);
#else
                            stack[sp2].value = (next & top);
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;

                    case Instruction.BITXOR:
                        Push(Pop() ^ Pop(), HopperType.tUInt);
                        break;


                    case Instruction.BITSHR:
                        {
#if UNDOINLINED
                uint top = Pop();
                uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
                            int lnext = (int)next;
                            lnext = lnext >> (int)top;
#if UNDOINLINED
                            Push((ushort)lnext, HopperType.tUInt);
#else
                            stack[sp2].value = (ushort)lnext;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;
                    case Instruction.BITSHL:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
                            int lnext = (int)next;
                            lnext = lnext << (int)top;
#if UNDOINLINED
                            Push((ushort)lnext, HopperType.tUInt);
#else
                            stack[sp2].value = (ushort)lnext;
                            stack[sp2].type = HopperType.tUInt;
#endif
                        }
                        break;

                    case Instruction.BITSHL8:
                        {
                            uint top = Pop();
                            int lnext = (int)top;
                            lnext = lnext << 8;
                            Push((ushort)lnext, HopperType.tUInt);
                        }
                        break;
                    case Instruction.BITSHR8:
                        {
                            uint top = Pop();
                            int lnext = (int)top;
                            lnext = lnext >> 8;
                            Push((ushort)lnext, HopperType.tByte);
                        }
                        break;
                    case Instruction.BITSHLB:
                        {
                            int next = (int)(code[pc + currentContext.CodeOffset]);
                            pc++;
                            uint top = Pop();

                            int lnext = (int)top;
                            lnext = lnext << next;
                            Push((ushort)lnext, HopperType.tUInt);
                        }
                        break;
                    case Instruction.BITSHRB:
                        {
                            int next = (int)(code[pc + currentContext.CodeOffset]);
                            pc++;
                            uint top = Pop();

                            int lnext = (int)top;
                            lnext = lnext >> next;
                            Push((ushort)lnext, HopperType.tUInt);
                        }
                        break;
                    case Instruction.BITANDB:
                        {
                            uint next = code[pc + currentContext.CodeOffset];
                            pc++;
                            uint top = Pop();
                            Push((ushort)(top & next), HopperType.tByte);
                        }
                        break;
                    case Instruction.BITORB:
                        {
                            uint next = code[pc + currentContext.CodeOffset];
                            pc++;
                            uint top = Pop();
                            Push((ushort)(top | next), HopperType.tUInt);
                        }
                        break;

                    case Instruction.BITANDFF:
                        {
                            Push((ushort)(Pop() & 0xFF), HopperType.tByte);
                        }
                        break;

                    case Instruction.BOOLOR:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                        PushBool(((0 != next) || (0 != top)));
#else
                            stack[sp2].value = (uint)(((0 != next) || (0 != top)) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;
                    case Instruction.BOOLAND:
                        {
#if UNDOINLINED
                            uint top = Pop();
                            uint next = Pop();
#else
                            sp--;
                            ushort sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;
#endif
#if UNDOINLINED
                            PushBool(((0 != next) && (0 != top)));
#else
                            stack[sp2].value = (uint)(((0 != next) && (0 != top)) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;
#endif
                        }
                        break;

                    case Instruction.ADDI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushInt((short)(nexti + topi));
                        }
                        break;
                    case Instruction.SUBI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushInt((short)(nexti - topi));
                        }
                        break;
                    case Instruction.MULI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushInt((short)(nexti * topi));
                        }
                        break;
                    case Instruction.DIVI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            if (topi == 0)
                            {
                                Diagnostics.Die(0x04, this);
                                break;
                            }
                            PushInt((short)(nexti / topi));
                        }
                        break;
                    case Instruction.MODI:
                        {
                            short topi = PopInt();
                            short nexti = PopInt();
                            if (topi == 0)
                            {
                                Diagnostics.Die(0x04, this);
                                break;
                            }
                            PushInt((short)(nexti % topi));
                        }
                        break;
                    

                    case Instruction.LIBCALL0:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            LibraryCall(currentContext, (LibCall)operand);
                        }
                        break;
                    case Instruction.LIBCALL1:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            LibraryCall(currentContext, (LibCall)operand);
                        }
                        break;
                    case Instruction.LIBCALL:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 2);
                        }
                        break;


                    case Instruction.SYSCALL0:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);
                        }
                        break;

                    case Instruction.SYSCALLB0:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            Push(operand, HopperType.tByte);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tByte;
                                sp++;
                            }
#endif

                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);
                        }
                        break;

                    case Instruction.SYSCALLB1:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            Push(operand, HopperType.tByte);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tByte;
                                sp++;
                            }
#endif

                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 1);
                        }
                        break;

                    case Instruction.SYSCALL00:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);

                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);
                        }
                        break;

                    case Instruction.SYSCALL01:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);

                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 1);
                        }
                        break;
                    case Instruction.SYSCALL10:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 1);

                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 0);
                        }
                        break;

                    case Instruction.SYSCALL1:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 1);
                        }
                        break;
                    case Instruction.SYSCALL2:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 2);
                        }
                        break;
                    case Instruction.SYSCALL:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            SystemCall(currentContext, (SysCall)operand, 3); // "3" means Pop()
                        }
                        break;

                    case Instruction.PUSHIB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                        Push(operand, HopperType.tByte);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tByte;
                                sp++;
                            }
#endif
                        }
                        break;

                    case Instruction.PUSHIBB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            Push(operand, HopperType.tByte);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tByte;
                                sp++;
                            }
#endif
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            Push(operand, HopperType.tByte);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tByte;
                                sp++;
                            }
#endif
                        }
                        break;

                    case Instruction.ADDB:
                        {
                            uint top = code[pc + currentContext.CodeOffset];
                            pc++;
                            uint next = Pop();
                            Push(next + top, HopperType.tUInt);
                        }
                        break;
                    case Instruction.SUBB:
                        {
                            uint top = code[pc + currentContext.CodeOffset];
                            pc++;
                            uint next = Pop();
                            Push(next - top, HopperType.tUInt);
                        }

                        break;

                    case Instruction.RETB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            bp = PopCS();

                            if (csp != 0) // Main() entry has no return address
                            {
                                // POP address -> PC                
                                pc = PopCS();
#if PROFILE
                                KeepFnReturn(currentContext, pc);
#endif
                            }

                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            ClearStack(operand);

                            if (currentISR != 0)
                            {
                                if (preISRcsp == csp)
                                {
                                    currentISR = 0; // ISR returned
                                    inISR = false;
                                }
                            }

                            if (pc == 0)
                            {
                                // RET from child process: pc = 0
                                Halted = true;
                            }
                            if (csp == 0)
                            {
                                Halted = true;
                            }
                        }
                        break;

                    case Instruction.RETRESB:
                        {
                            // function return value
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            uint top = 0;
                            Variant vtop = null;
                            HopperType type = GetStackType((ushort)(sp - 1));
                            bool isValueType = Type_IsValueType(type);
                            if (isValueType)
                            {
                                top = Pop(); 
                            }
                            else
                            {
                                vtop = PopVariant(HopperType.tUndefined);
                            }
                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            ClearStack(operand);
                            // restore function return value before GC
                            if (isValueType)
                            {
                                Push(top, type);
                            }
                            else
                            {
                                Push(vtop);
                            }
#else
                            int retLocation = sp - 1;
                            HopperType type = stack[retLocation].type;
                                
                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            sp -= operand;
                            int sp2 = sp - 1;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[retLocation].value;
                                stack[sp2].type = type;
                            }
                            else
                            {
                                type = stack[retLocation].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[retLocation].reference;
                                stack[sp2].type = type;
                            }
#endif

                            bp = PopCS();
                            if (csp != 0) // Main() entry has no return address
                            {
                                // POP address -> PC                
                                pc = PopCS();
#if PROFILE
                                KeepFnReturn(currentContext, pc);
#endif
                            }

                            if (currentISR != 0)
                            {
                                if (preISRcsp == csp)
                                {
                                    currentISR = 0; // ISR returned
                                    inISR = false;
                                }
                            }

                            if (csp == 0)
                            {
                                Halted = true;
                            }
                        }
                        break;

                    case Instruction.RETRES:
                        {
                            // function return value

                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;


#if UNDOINLINED
                            uint top = 0;
                            Variant vtop = null;
                            HopperType type = GetStackType((ushort)(sp - 1));
                            bool isValueType = Type_IsValueType(type);
                            if (isValueType)
                            {
                                top = Pop(); 
                            }
                            else
                            {
                                vtop = PopVariant(HopperType.tUndefined);
                            }
                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            ClearStack(operand);
                            // restore function return value before GC
                            if (isValueType)
                            {
                                Push(top, type);
                            }
                            else
                            {
                                Push(vtop);
                            }
#else
                            int retLocation = (sp) - 1;
                            HopperType type = stack[retLocation].type;

                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            sp -= operand;
                            int sp2 = (sp) - 1;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[retLocation].value;
                                stack[sp2].type = type;
                            }
                            else
                            {
                                type = stack[retLocation].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[retLocation].reference;
                                stack[sp2].type = type;
                            }
#endif

                            bp = PopCS();
                            if (csp != 0) // Main() entry has no return address
                            {
                                // POP address -> PC                
                                pc = PopCS();
#if PROFILE
                                KeepFnReturn(currentContext, pc);
#endif
                            }

                            if (currentISR != 0)
                            {
                                if (preISRcsp == csp)
                                {
                                    currentISR = 0; // ISR returned
                                    inISR = false;
                                }
                            }

                            if (csp == 0)
                            {
                                Halted = true;
                            }
                        }
                        break;


                    case Instruction.JB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            pc = (ushort)(pc + offset - 2);
                        }
                        break;

                    case Instruction.JNZB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            if (Pop() != 0)
#else
                            sp--;
                            ushort sp2 = sp;
                            uint top = stack[sp2].value;
                            if (top != 0)
#endif
                            {
                                short offset = (short)operand;
                                if (offset > 127)
                                {
                                    offset = (short)(offset - 256); // 255 -> -1
                                }
                                pc = (ushort)(pc + offset - 2);
                            }
                        }
                        break;

                    case Instruction.JZB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            if (Pop() == 0)
                            {
                                short offset = (short)operand;
                                if (offset > 127)
                                {
                                    offset = (short)(offset - 256); // 255 -> -1
                                }
                                pc = (ushort)(pc + offset - 2);
                            }
                        }
                        break;

                    case Instruction.POPGLOBALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            ushort address = (ushort)(operand + gp);
                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(address, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(address);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(address);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(address, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(address, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(address, PopVariant(type));
                            }
                        }
                        break;

                    case Instruction.POPLOCAL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (operand > 0x7FFF)
                            {
                                offset = (short)(operand - 0x10000); // 0xFFFF -> -1
                            }
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;


                    case Instruction.POPLOCALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;

                    case Instruction.POPREL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (operand > 0x7FFF)
                            {
                                offset = (short)(operand - 0x10000); // 0xFFFF -> -1
                            }

                            ushort referenceAddress = (ushort)(bp + offset);
                            ushort localAddress = (ushort)GetStack(referenceAddress);
                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }

                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;

                    case Instruction.POPRELB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort referenceAddress = (ushort)(bp + offset);
                            ushort localAddress = (ushort)GetStack(referenceAddress);
                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }

                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;

                    case Instruction.PUSHGLOBALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

#if UNDOINLINED
                            HopperType type = GetStackType((ushort)(operand + gp));
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack((ushort)(operand + gp)), type);
                            }
                            else
                            {
                                Push(GetStackVariant((ushort)(operand + gp)));
                            }
#else
                            ushort address = (ushort)(operand + gp);
                            HopperType type = stack[address].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[address].value;
                            }
                            else
                            {
                                type = stack[address].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[address].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;

                    case Instruction.PUSHLOCAL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (operand > 0x7FFF)
                            {
                                offset = (short)(operand - 0x10000); // 0xFFFF -> -1
                            }

#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
#else
                            ushort localAddress2 = (ushort)((bp + offset));
                            HopperType type = stack[localAddress2].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[localAddress2].value;
                            }
                            else
                            {
                                type = stack[localAddress2].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[localAddress2].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;

                    case Instruction.PUSHLOCALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }

#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
#else
                            ushort localAddress2 = (ushort)((bp + offset));
                            HopperType type = stack[localAddress2].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[localAddress2].value;
                            }
                            else
                            {
                                type = stack[localAddress2].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[localAddress2].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;

                    case Instruction.PUSHREL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (operand > 0x7FFF)
                            {
                                offset = (short)(operand - 0x10000); // 0xFFFF -> -1
                            }
                            ushort referenceAddress = (ushort)(bp + offset);
                            ushort localAddress = (ushort)GetStack(referenceAddress);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
                        }
                        break;

                    case Instruction.PUSHRELB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort referenceAddress = (ushort)(bp + offset);
                            ushort localAddress = (ushort)GetStack(referenceAddress);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
                        }
                        break;

                    case Instruction.PUSHSTACKADDRB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort localAddress = (ushort)(bp + offset);
                            Push(localAddress, HopperType.tReference);
                        }
                        break;

                    case Instruction.PUSHSTACKADDR:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (operand > 0x7FFF)
                            {
                                offset = (short)(operand - 0x10000); // 0xFFFF -> -1
                            }
                            ushort localAddress = (ushort)(bp + offset);
                            Push(localAddress, HopperType.tReference);
                        }
                        break;



                    case Instruction.INCLOCALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            // INCLOCALB is an optimization of "i = i + 1":
                            // If it were done using ADDI or ADD, then the result pushed on the stack
                            // would be tInt or tUInt, even if i was a tByte.
                            // POPLOCALB would then supply the type for the resulting expression.
                            //
                            // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
                            // (we need to avoid munting the type if it is currently a -ve tInt)
#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (type == HopperType.tByte)
                            {
                                type = HopperType.tUInt;
                            }
                            uint value = GetStack(localAddress);
                            PutStack(localAddress, value + 1, type);
#else
                            stack[(bp + offset)].value++;
                            if (stack[(bp + offset)].type == HopperType.tByte)
                            {
                                stack[(bp + offset)].type = HopperType.tUInt;
                            }
#endif
                        }
                        break;

                    case Instruction.DECLOCALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            PutStack(localAddress, GetStack(localAddress) - 1, type);
#else
                            stack[(bp + offset)].value--;
#endif
                        }
                        break;

                    case Instruction.CALLB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            PushCS(pc);
#if PROFILE
                            KeepFnCallLog(currentContext, operand, pc);
#endif
                            pc = methodTable[operand];
                        }
                        break;

                    case Instruction.DUP0:
                        {
                            ushort localAddress = (ushort)(sp - 1); // DUP 0 implies duplicating [top]
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
                        }
                        break;
                    case Instruction.DUP:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            ushort localAddress = (ushort)((sp - 1) - operand); // DUP 0 implies duplicating [top]
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
                        }
                        break;

                    case Instruction.DECSP:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            ClearStack(operand);
                        }
                        break;

                    case Instruction.TESTBPB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            ushort bpExpected = (ushort)(sp - operand);
#if DEBUG
                            Diagnostics.ASSERTDIE(bpExpected == bp, "stack mismatch on return", this);
#endif
                        }
                        break;

                    case Instruction.DIE:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            lastError = operand;
                            Diagnostics.Die(operand, this);
                        }
                        break;
                       
                    case Instruction.INCLOCALBB:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset0 = (short)(operand & 0x00FF); // first offset
                            short offset1 = (short)(operand >> 8);     // second offset
                            if (offset0 > 127)
                            {
                                offset0 = (short)(offset0 - 256); // 255 -> -1
                            }
                            if (offset1 > 127)
                            {
                                offset1 = (short)(offset1 - 256); // 255 -> -1
                            }
                            stack[(bp + offset0)].value += stack[(bp + offset1)].value;
                        }
                        break;

                    case Instruction.INCLOCALIBB:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset0 = (short)(operand & 0x00FF); // first offset
                            short offset1 = (short)(operand >> 8);     // second offset
                            if (offset0 > 127)
                            {
                                offset0 = (short)(offset0 - 256); // 255 -> -1
                            }
                            if (offset1 > 127)
                            {
                                offset1 = (short)(offset1 - 256); // 255 -> -1
                            }
                            byte[] bytes = BitConverter.GetBytes(stack[(bp + offset0)].value);
                            short a = BitConverter.ToInt16(bytes, 0);
                            bytes = BitConverter.GetBytes(stack[(bp + offset1)].value);
                            short b = BitConverter.ToInt16(bytes, 0);
                            a = (short)(a + b);
                            stack[(bp + offset0)].value = BitConverter.ToUInt16(BitConverter.GetBytes(a), 0);
                        }
                        break;


                    case Instruction.INCLOCALIB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            byte[] bytes = BitConverter.GetBytes(stack[(bp + offset)].value);
                            short a = BitConverter.ToInt16(bytes, 0);
                            a++;
                            stack[(bp + offset)].value = BitConverter.ToUInt16(BitConverter.GetBytes(a), 0);
                            stack[(bp + offset)].type = HopperType.tInt;
                        }
                        break;
                    case Instruction.DECLOCALIB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            byte[] bytes = BitConverter.GetBytes(stack[(bp + offset)].value);
                            short a = BitConverter.ToInt16(bytes, 0);
                            a--;
                            stack[(bp + offset)].value = BitConverter.ToUInt16(BitConverter.GetBytes(a), 0);
                            stack[(bp + offset)].type = HopperType.tInt;
                        }
                        break;
                    case Instruction.INCGLOBALIB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            ushort address = (ushort)(operand + gp);
                            byte[] bytes = BitConverter.GetBytes(stack[address].value);
                            short a = BitConverter.ToInt16(bytes, 0);
                            a++;
                            stack[address].value = BitConverter.ToUInt16(BitConverter.GetBytes(a), 0);
                            stack[address].type = HopperType.tInt;
                        }
                        break;
                    case Instruction.DECGLOBALIB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            ushort address = (ushort)(operand + gp);
                            byte[] bytes = BitConverter.GetBytes(stack[address].value);
                            short a = BitConverter.ToInt16(bytes, 0);
                            a--;
                            stack[address].value = BitConverter.ToUInt16(BitConverter.GetBytes(a), 0);
                            stack[address].type = HopperType.tInt;
                        }
                        break;

                    case Instruction.PUSHR0:
                        {
                            stack[sp].value = r0.value;
                            stack[sp].reference = r0.reference;
                            stack[sp].type = r0.type;
                            sp++;
                            isCDecl = true; // trick: make a call to this API to tell the runtime we are usind CDecl (should be in the binary version?)
                        }
                        break;
                    case Instruction.POPR0:
                        {
                            sp--; 
                            ushort sp2 = sp;
                            r0.value = stack[sp].value;
                            r0.reference = stack[sp].reference;
                            r0.type = stack[sp].type;
                            isCDecl = true;  // trick: make a call to this API to tell the runtime we are usind CDecl (should be in the binary version?)
                        }
                        break;
                    case Instruction.PUSHI:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tUInt;
                                sp++;
                            }
#endif
                        }
                        break;

                    case Instruction.PUSHILE:
                        {

                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;



#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
                            uint top = Pop();
                            uint next = Pop();
                            PushBool(next <= top);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tUInt;
                                sp++;

                                sp--;
                                sp2 = (ushort)((sp) - 1);
                                uint top = stack[sp2 + 1].value;
                                uint next = stack[sp2].value;

                                stack[sp2].value = (uint)((next <= top) ? 1 : 0);
                                stack[sp2].type = HopperType.tBool;
                            }
#endif

                        }
                        break;
                    case Instruction.PUSHIBLE:
                        {

                            operand = (ushort)(code[pc + currentContext.CodeOffset]);
                            pc++;

#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
                            uint top = Pop();
                            uint next = Pop();
                            PushBool(next <= top);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tUInt;
                                sp++;

                                sp--;
                                sp2 = (ushort)((sp) - 1);
                                uint top = stack[sp2 + 1].value;
                                uint next = stack[sp2].value;

                                stack[sp2].value = (uint)((next <= top) ? 1 : 0);
                                stack[sp2].type = HopperType.tBool;
                            }
#endif

                        }
                        break;
                    case Instruction.PUSHIBEQ:
                        {

                            operand = (ushort)(code[pc + currentContext.CodeOffset]);
                            pc++;

#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
                            uint top = Pop();
                            uint next = Pop();
                            PushBool(next == top);
#else
                            {
                                ushort sp2 = sp;
                                stack[sp2].value = operand;
                                stack[sp2].type = HopperType.tUInt;
                                sp++;

                                sp--;
                                sp2 = (ushort)((sp) - 1);
                                uint top = stack[sp2 + 1].value;
                                uint next = stack[sp2].value;

                                stack[sp2].value = (uint)((next == top) ? 1 : 0);
                                stack[sp2].type = HopperType.tBool;
                            }
#endif

                        }
                        break;

                    case Instruction.CALL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            PushCS(pc);
#if PROFILE
                            KeepFnCallLog(currentContext, operand, pc);
#endif
                            if (methodTable.Contains(operand))
                            {
                                pc = methodTable[operand];
                            }
                            else
                            {
                                // probably code built for a small devicd
                                operand = (ushort)(operand & 0x3FFF);
                                pc = methodTable[operand];
                            }
                        }
                        break;

                    case Instruction.J:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            short offset = (short)operand;
                            if (offset > 32767)
                            {
                                offset = (short)(offset - 65536); // 65535 -> -1
                            }
                            pc = (ushort)(pc + offset - 3);
                        }
                        break;
                    case Instruction.JZ:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

#if UNDOINLINED
                            if (Pop() == 0)
#else
                            sp--;
                            uint top = stack[sp].value;
                            if (top == 0)
#endif
                            {
                                short offset = (short)operand;
                                if (offset > 32767)
                                {
                                    offset = (short)(offset - 65536); // 65535 -> -1
                                }
                                pc = (ushort)(pc + offset - 3);
                            }
                        }
                        break;
                    case Instruction.JNZ:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

#if UNDOINLINED
                            if (Pop() != 0)
#else
                            sp--;
                            uint top = stack[sp].value;
                            if (top != 0)
#endif
                            {
                                short offset = (short)operand;
                                if (offset > 32767)
                                {
                                    offset = (short)(offset - 65536); // 65535 -> -1
                                }
                                pc = (ushort)(pc + offset - 3);
                            }
                        }
                        break;
                    case Instruction.PUSHGLOBAL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

#if UNDOINLINED
                            HopperType type = GetStackType((ushort)(operand + gp));
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack((ushort)(operand + gp)), type);
                            }
                            else
                            {
                                Push(GetStackVariant((ushort)(operand + gp)));
                            }
#else
                            ushort address = (ushort)(operand + gp);
                            HopperType type = stack[address].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[address].value;
                            }
                            else
                            {
                                type = stack[address].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[address].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;
                    case Instruction.POPGLOBAL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            ushort address = (ushort)(operand + gp);
                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(address, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(address);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(address);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(address, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(address, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(address, PopVariant(type));
                            }
                        }
                        break;
                    case Instruction.RET:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;

                            bp = PopCS();

                            if (csp != 0) // Main() entry has no return address
                            {
                                // POP address -> PC                
                                pc = PopCS();
#if PROFILE
                                KeepFnReturn(currentContext, pc);
#endif
                            }

                            // clear the locals and arguments off the stack (return value is already dealt with if needed)
                            ClearStack(operand);

                            if (currentISR != 0)
                            {
                                if (preISRcsp == csp)
                                {
                                    currentISR = 0; // ISR returned
                                    inISR = false;
                                }
                            }

                            if (pc == 0)
                            {
                                // RET from child process: pc = 0
                                Halted = true;
                            }
                            if (csp == 0)
                            {
                                Halted = true;
                            }
                        }
                        break;
                        
                    case Instruction.CAST:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            Push(Pop(), (HopperType)operand);
#else
                            ushort sp2 = (ushort)(sp - 1);
                            stack[sp2].type = (HopperType)operand;
#endif
                        }
                        break;

                    case Instruction.INCGLOBALBB:
                        {
                            ushort operand0 = code[pc + currentContext.CodeOffset];
                            pc++;
                            ushort operand1 = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            ushort globalAddress0 = (ushort)(operand0 + gp);
                            ushort globalAddress1 = (ushort)(operand1 + gp);
                            HopperType type0 = GetStackType(globalAddress0);
                            PutStack(globalAddress0, GetStack(globalAddress0) + GetStack(globalAddress1), type0);
#else
                            ushort address0 = (ushort)(operand0 + gp);
                            ushort address1 = (ushort)(operand1 + gp);
                            stack[address0].value += stack[address1].value;
#endif
                        }
                        break;
                    

                    case Instruction.INCGLOBALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            // INCGLOBALB is an optimization of "i = i + 1":
                            // If it were done using ADDI or ADD, then the result pushed on the stack
                            // would be tInt or tUInt, even if i was a tByte.
                            // POPGLOBALB would then supply the type for the resulting expression.
                            //
                            // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
                            // (we need to avoid munting the type if it is currently a -ve tInt)
#if UNDOINLINED
                            ushort globalAddress = (ushort)(operand + gp);
                            HopperType type = GetStackType(globalAddress);
                            if (type == HopperType.tByte)
                            {
                                type = HopperType.tUInt;
                            }
                            PutStack(globalAddress, GetStack(globalAddress) + 1, type);
#else
                            ushort address = (ushort)(operand + gp);
                            stack[address].value++;
                            if (stack[address].type == HopperType.tByte)
                            {
                                stack[address].type = HopperType.tUInt;
                            }
#endif
                        }
                        break;

                    case Instruction.DECGLOBALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            ushort globalAddress = (ushort)(operand + gp);
                            HopperType type = GetStackType(globalAddress);
                            PutStack(globalAddress, GetStack(globalAddress) - 1, type);
#else
                            ushort address = (ushort)(operand + gp);
                            stack[address].value--;
#endif
                        }
                        break;

                    case Instruction.POPCOPYGLOBAL:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;
                            ushort address = (ushort)(operand + gp);
                            HopperType type = GetStackType((ushort)(sp - 1));

                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(address, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(address);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(address);
                                }
                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(address, referenceNew);
                                }
                            }
                        }
                        break;

                    case Instruction.POPCOPYGLOBALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            ushort address = (ushort)(operand + gp);
                            HopperType type = GetStackType((ushort)(sp - 1));

                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(address, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(address);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(address);
                                }
                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(address, referenceNew);
                                }
                            }

                        }
                        break;


                    case Instruction.POPCOPYLOCALB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));

                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(localAddress);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(localAddress);
                                }
                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(localAddress, referenceNew);
                                }
                            }
                        }
                        break;

                    case Instruction.POPCOPYRELB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }
                            ushort referenceAddress = (ushort)(bp + offset);
                            ushort localAddress = (ushort)GetStack(referenceAddress);
                            HopperType type = GetStackType((ushort)(sp - 1));

                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(localAddress);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(localAddress);
                                }

                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(localAddress, referenceNew);
                                }
                            }

                        }
                        break;


                    case Instruction.PUSHGLOBALBB:
                        for (int i = 0; i < 2; i++)
                        {
                            if (lastError != 0)
                            {
                                break;
                            }
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
#if UNDOINLINED
                            HopperType type = GetStackType((ushort)(operand + gp));
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack((ushort)(operand + gp)), type);
                            }
                            else
                            {
                                Push(GetStackVariant((ushort)(operand + gp)));
                            }
#else
                            ushort address = (ushort)(operand + gp);
                            HopperType type = stack[address].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[address].value;
                            }
                            else
                            {
                                type = stack[address].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[address].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;

                    case Instruction.PUSHLOCALBB:
                        for (int i = 0; i < 2; i++)
                        {
                            if (lastError != 0)
                            {
                                break;
                            }
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;

                            short offset = (short)operand;
                            if (offset > 127)
                            {
                                offset = (short)(offset - 256); // 255 -> -1
                            }

#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
#else
                            ushort localAddress2 = (ushort)((bp + offset));
                            HopperType type = stack[localAddress2].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[localAddress2].value;
                            }
                            else
                            {
                                type = stack[localAddress2].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[localAddress2].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif

                        }
                        break;

                    case Instruction.PUSHILT:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;


#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
                            uint top = Pop();
                            uint next = Pop();
                            PushBool(next < top);
#else

                            ushort sp2 = sp;
                            stack[sp2].value = operand;
                            stack[sp2].type = HopperType.tUInt;
                            sp++;

                            sp--;
                            sp2 = (ushort)((sp) - 1);
                            uint top = stack[sp2 + 1].value;
                            uint next = stack[sp2].value;

                            stack[sp2].value = (uint)((next < top) ? 1 : 0);
                            stack[sp2].type = HopperType.tBool;

#endif
                        }
                        break;

                    case Instruction.PUSHILEI:
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;
                                
                            Push(operand, HopperType.tUInt);
                                
                            short topi = PopInt();
                            short nexti = PopInt();
                            PushBool(nexti <= topi);
                        }
                        break;

                    case Instruction.NOP:
                        break;

                    case Instruction.EXIT:
                        {
                            if (null == codeStore)
                            {
                                Diagnostics.Die(0x0B, this); // EXIT not within Inline code?
                                break;
                            }
                            pc = pcStore;
                            code = codeStore;
                            codeStore = null;
                            if (sp != spStore)
                            {
                                Diagnostics.Die(0x0B, this); // stack mismatch from inline code?
                                break;
                            }
                            if (bp != bpStore)
                            {
                                Diagnostics.Die(0x0B, this); // stack mismatch from inline code?
                                break;
                            }
                            if (csp != cspStore)
                            {
                                Diagnostics.Die(0x0B, this); // stack mismatch from inline code?
                                break;
                            }
                            Push(0, HopperType.tUInt); // strictly speaking, this is the result from System.Inline(..)
                        }
                        break;

                    case Instruction.ENTERB:
                        {
                            operand = code[pc + currentContext.CodeOffset];
                            pc++;
                            PushCS(bp);
                            bp = sp;
                            for (ushort i = 0; i < operand; i++)
                            {
#if UNDOINLINED
                                Push(0, HopperType.tByte);
#else
                                {
                                    ushort sp2 = sp;
                                    stack[sp2].value = 0;
                                    stack[sp2].type = HopperType.tByte;
                                    sp++;
                                }
#endif
                            }
                        }
                        break;

                    case Instruction.ENTER:
                        PushCS(bp);
                        bp = sp;
                        break;

                    case Instruction.CALLREL:
                        {
                            PushCS(pc);
                            uint methodIndex = Pop();
                            if (0 == methodIndex)
                            {
                                Diagnostics.Die(0x0F, this); // invalid or uninitialized delegate
                                break;
                            }
#if PROFILE
                            KeepFnCallLog(currentContext, (ushort)methodIndex, pc);
#endif
                            pc = methodTable[methodIndex];
                        }
                        break;

                    case Instruction.JREL:
                        {
                            pc = (ushort)Pop();
                        }
                        break;

                    case Instruction.JIXB:
                    case Instruction.JIX:
                        {
                            uint switchCase = Pop();

                            byte minRange = code[pc + currentContext.CodeOffset];
                            pc++;
                            byte maxRange = code[pc + currentContext.CodeOffset];
                            pc++;

                            byte lsb = code[pc + currentContext.CodeOffset];
                            pc++;
                            byte msb = code[pc + currentContext.CodeOffset];
                            pc++;

                            int jumpBackOffset = lsb + (msb << 8);

                            ushort tpc = pc;

                            pc = (ushort)(pc - jumpBackOffset - 5);

                            ushort tableSize = (ushort)(maxRange - minRange + 1);
                            if (opCode == Instruction.JIX)
                            {
                                tableSize = (ushort)(tableSize << 1);
                            }

                            uint offset = 0;
                            if ((switchCase >= minRange) && (switchCase <= maxRange))
                            {
                                // in the table
                                if (opCode == Instruction.JIX)
                                {
                                    uint index = tpc + (switchCase - minRange)*2;
                                    offset = (uint)(code[index + currentContext.CodeOffset] + (code[index+ 1 + currentContext.CodeOffset] << 8));
                                }
                                else
                                {
                                    uint index = tpc + switchCase - minRange;
                                    offset = code[index + currentContext.CodeOffset];
                                }
                            }

                            if (offset == 0)
                            {
                                // default
                                pc = (ushort)(tpc + tableSize);
                            }
                            else
                            {
                                pc = (ushort)(pc + offset);
                            }

                        }
                        break;


                    case Instruction.RET0:
                        {
                            bp = PopCS();

                            if (csp != 0) // Main() entry has no return address
                            {
                                // POP address -> PC                
                                pc = PopCS();
#if PROFILE
                                KeepFnReturn(currentContext, pc);
#endif
                            }
                            if (currentISR != 0)
                            {
                                if (preISRcsp == csp)
                                {
                                    currentISR = 0; // ISR returned
                                    inISR = false;
                                }
                            }

                            if (pc == 0)
                            {
                                // RET from child process: pc = 0
                                Halted = true;
                            }
                            if (csp == 0)
                            {
                                Halted = true;
                            }
                        }
                        break;
                    case Instruction.RETFAST:
                        {
                            // POP address -> PC                
                            pc = PopCS();
#if PROFILE
                            KeepFnReturn(currentContext, pc);
#endif
                        }
                        break;

                    case Instruction.PUSHDB: // only difference on Windows is a signal to the optimizer
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset]);
                            pc++;
#if UNDOINLINED
                            Push(operand, HopperType.tByte);
#else

                            ushort sp2 = sp;
                            stack[sp2].value = operand;
                            stack[sp2].type = HopperType.tByte;
                            sp++;

#endif
                        }
                        break;

                    case Instruction.PUSHD: // only difference on Windows is a signal to the optimizer
                        {
                            operand = (ushort)(code[pc + currentContext.CodeOffset] + (code[pc + 1 + currentContext.CodeOffset] << 8));
                            pc += 2;
#if UNDOINLINED
                            Push(operand, HopperType.tUInt);
#else

                            ushort sp2 = sp;
                            stack[sp2].value = operand;
                            stack[sp2].type = HopperType.tUInt;
                            sp++;
                            
#endif
                        }
                        break;

                    case Instruction.PUSHI0:
#if UNDOINLINED
                        Push(0, HopperType.tByte);
#else
                        {
                            ushort sp2 = sp;
                            stack[sp2].value = 0;
                            stack[sp2].type = HopperType.tByte;
                            sp++;
                            }
#endif
                        break;
                    case Instruction.PUSHI1:
#if UNDOINLINED
                        Push(1, HopperType.tByte);
#else
                        {
                            ushort sp2 = sp;
                            stack[sp2].value = 1;
                            stack[sp2].type = HopperType.tByte;
                            sp++;
                        }
#endif
                        break;
                    case Instruction.PUSHIM1:
                        {
                            short m1 = -1;
                            PushInt(m1);
                        }
                        break;

                    case Instruction.PUSHLOCALB00:
                        {
                            short offset = 0;
#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
#else
                            ushort localAddress2 = (ushort)((bp + offset));
                            HopperType type = stack[localAddress2].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[localAddress2].value;
                            }
                            else
                            {
                                type = stack[localAddress2].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[localAddress2].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;
                    case Instruction.PUSHLOCALB01:
                        {
                            short offset = 1;
#if UNDOINLINED
                            ushort localAddress = (ushort)(bp + offset);
                            HopperType type = GetStackType(localAddress);
                            if (Type_IsValueType(type))
                            {
                                Push(GetStack(localAddress), type);
                            }
                            else
                            {
                                Push(GetStackVariant(localAddress));
                            }
#else
                            ushort localAddress2 = (ushort)((bp + offset));
                            HopperType type = stack[localAddress2].type;
                            ushort sp2 = sp;
                            if (type <= HopperType.tLong)
                            {
                                stack[sp2].value = stack[localAddress2].value;
                            }
                            else
                            {
                                type = stack[localAddress2].reference.Type;
                                if (type <= HopperType.tLong)
                                {
                                    type = HopperType.tVariant;
                                }
                                stack[sp2].reference = stack[localAddress2].reference;
                            }
                            stack[sp2].type = type;
                            sp++;
#endif
                        }
                        break;

                    case Instruction.POPLOCALB00:
                        {
                            short offset = (short)0;
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;
                    case Instruction.POPLOCALB01:
                        {
                            short offset = 1;
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                            if (copyNextPop)
                            {
                                if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                                {
                                    PutStack(localAddress, Pop(), type);
                                }
                                else
                                {
                                    Variant referenceNew = PopVariant(HopperType.tUndefined);
                                    Variant referenceOld = null;
                                    HopperType targetType = GetStackType(localAddress);
                                    if (!Type_IsValueType(targetType))
                                    {
                                        referenceOld = GetStackVariant(localAddress);
                                    }
                                    if (referenceNew != referenceOld)
                                    {
                                        referenceNew = referenceNew.Clone();
                                        PutStackVariant(localAddress, referenceNew);
                                    }
                                }
                                copyNextPop = false;
                            }
                            else if (Type_IsValueType(type))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                PutStackVariant(localAddress, PopVariant(type));
                            }
                        }
                        break;

                    case Instruction.POPCOPYLOCALB00:
                        {
                            short offset = (short)0;
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                                
                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(localAddress);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(localAddress);
                                }
                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(localAddress, referenceNew);
                                }
                            }
                                
                        }
                        break;
                    case Instruction.POPCOPYLOCALB01:
                        {
                            short offset = 1;
                            ushort localAddress = (ushort)(bp + offset);

                            HopperType type = GetStackType((ushort)(sp - 1));
                                
                            if ((type == HopperType.tLong) || (type == HopperType.tFloat))
                            {
                                PutStack(localAddress, Pop(), type);
                            }
                            else
                            {
                                Variant referenceNew = PopVariant(HopperType.tUndefined);
                                Variant referenceOld = null;
                                HopperType targetType = GetStackType(localAddress);
                                if (!Type_IsValueType(targetType))
                                {
                                    referenceOld = GetStackVariant(localAddress);
                                }
                                if (referenceNew != referenceOld)
                                {
                                    referenceNew = referenceNew.Clone();
                                    PutStackVariant(localAddress, referenceNew);
                                }
                            }
                                
                        }
                        break;

                    case Instruction.PUSHGP:
                        Push(gp, HopperType.tUInt);
                        break;
                    case Instruction.COPYNEXTPOP:
                        copyNextPop = true;
                        break;

                    case Instruction.BOOLNOT:
                        PushBool(Pop() == 0);
                        break;

                    case Instruction.SWAP:
                        {
                            HopperType topType = GetStackType((ushort)(sp - 1));
                            HopperType nextType = GetStackType((ushort)(sp - 2));
                            if (Type_IsValueType(topType) && Type_IsValueType(nextType))
                            {
                                uint top = Pop();
                                uint next = Pop();
                                Push(top, topType);
                                Push(next, nextType);
                            }
                            else if (Type_IsValueType(topType) && !Type_IsValueType(nextType))
                            {
                                uint top = Pop();
                                Variant next = PopVariant(HopperType.tUndefined);
                                Push(top, topType);
                                Push(next);
                            }
                            else if (!Type_IsValueType(topType) && Type_IsValueType(nextType))
                            {
                                Variant top = PopVariant(HopperType.tUndefined);
                                uint next = Pop();
                                Push(top);
                                Push(next, nextType);
                            }
                            else if (!Type_IsValueType(topType) && !Type_IsValueType(nextType))
                            {
                                Variant top = PopVariant(HopperType.tUndefined);
                                Variant next = PopVariant(HopperType.tUndefined);
                                Push(top);
                                Push(next);
                            }
                            else
                            {
                                throw new NotImplementedException();
                            }
                        }
                        break;
                    case Instruction.BITNOT:
                        {
                            HopperType topType = GetStackType((ushort)(sp - 1));
                            uint top = Pop();
                            top = ~top;
                            if (topType == HopperType.tByte)
                            {
                                top = top & 0xFF;
                            }
                            Push(top, topType);
                        }
                        break;

                    default:
                        Diagnostics.Die(0x0A, this); // not implemented
                        break;

                }
#if PROFILE
                KeepOpCodeLog(currentContext, opCode);
#endif

                if ((lastError != 0) || Halted)
                {
                    break;
                }
            } // for (;;)
            waiting = true;
            StepType = StepTypes.None;
            return lastError;
        }



#if PROFILE
        private void KeepOpCodeLog(Context currentContext, Instruction opCode)
        {
            if (!currentContext.CallStats.ContainsKey(opCode))
            {
                currentContext.CallStats[opCode] = 0;
            }
            currentContext.CallStats[opCode]++;
        }
        private void KeepSysCallLog(Context currentContext, SysCall sysCall)
        {
            if (!currentContext.SysCallStats.ContainsKey(sysCall))
            {
                currentContext.SysCallStats[sysCall] = 0;
            }
            currentContext.SysCallStats[sysCall]++;
        }
        private void KeepFnReturn(Context currentContext, ushort returnPC)
        {
            if (returnPC != 0) // return from "main"?
            {
                DateTime returnTime = DateTime.Now;
                ushort len = (ushort)(currentContext.FnCallTimeReturnPCs.Count);
                ushort expectedPC = currentContext.FnCallTimeReturnPCs[len - 1];
                currentContext.FnCallTimeReturnPCs.RemoveAt((ushort)(len - 1));
                if (returnPC != expectedPC)
                {
                    throw new InvalidOperationException();
                }
                DateTime callTime = currentContext.FnCallTimeStack[(ushort)(len - 1)];
                currentContext.FnCallTimeStack.RemoveAt((ushort)(len - 1));
                TimeSpan elapsed = returnTime - callTime;
                ushort fn = currentContext.FnCallTimeStackFn[len - 1];
                currentContext.FnCallTimeStackFn.RemoveAt((ushort)(len - 1));

                if (!currentContext.FnCallTimeStats.ContainsKey(fn))
                {
                    currentContext.FnCallTimeStats[fn] = 0;
                }
                currentContext.FnCallTimeStats[fn] += elapsed.TotalMilliseconds;
                
            }
        }
        private void KeepFnCallLog(Context currentContext, UInt16 fnCall, ushort returnPC)
        {
            if (!currentContext.FnCallStats.ContainsKey(fnCall))
            {
                currentContext.FnCallStats[fnCall] = 0;
            }
            currentContext.FnCallStats[fnCall]++;
            currentContext.FnCallTimeStackFn.Add(fnCall);
            currentContext.FnCallTimeStack.Add(DateTime.Now);
            currentContext.FnCallTimeReturnPCs.Add(returnPC);
        }
#endif
        private void LibraryCall(Context currentContext, LibCall libCall)
        {
            /*
            switch (libCall)
            {
                case LibCall.GraphicsWidthGet:
                    {
                        ushort width = (ushort)Console.CanvasWidth;
                        Push(width, HopperType.tUInt);
                    }
                    break;
                case LibCall.GraphicsHeightGet:
                    {
                        ushort height = (ushort)Console.CanvasHeight;
                        Push(height, HopperType.tUInt);
                    }
                    break;
                case LibCall.GraphicsFlipDisplay:
                    Console.FlipVertical = (bool)(Pop() != 0);
                    break;
                case LibCall.GraphicsClear:
                    {
                        ushort color = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.GraphicsClear(color);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsSetPixel:
                    {
                        ushort colour = (ushort)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.SetPixel(x, y, colour);
                        screen.Resume(false);
                    }
                    break;


                case LibCall.GraphicsLine:
                    {
                        ushort colour = (ushort)Pop();
                        ushort y2 = (ushort)Pop();
                        ushort x2 = (ushort)Pop();
                        ushort y1 = (ushort)Pop();
                        ushort x1 = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.Line(x1, y1, x2, y2, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsHorizontalLine:
                    {
                        ushort colour = (ushort)Pop();
                        ushort y2 = (ushort)Pop();
                        ushort x2 = (ushort)Pop();
                        ushort y1 = (ushort)Pop();
                        ushort x1 = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.HorizontalLine(x1, y1, x2, y2, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsVerticalLine:
                    {
                        ushort colour = (ushort)Pop();
                        ushort y2 = (ushort)Pop();
                        ushort x2 = (ushort)Pop();
                        ushort y1 = (ushort)Pop();
                        ushort x1 = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.VerticalLine(x1, y1, x2, y2, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsRectangle:
                    {
                        ushort colour = (ushort)Pop();
                        ushort h = (ushort)Pop();
                        ushort w = (ushort)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.Rectangle(x, y, w, h, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsFilledRectangle:
                    {
                        ushort colour = (ushort)Pop();
                        ushort h = (ushort)Pop();
                        ushort w = (ushort)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.FillRectangle(x, y, w, h, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsCircle:
                    {
                        ushort colour = (ushort)Pop();
                        ushort r = (ushort)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.Circle(x, y, r, colour);
                        screen.Resume(false);
                    }
                    break;
                case LibCall.GraphicsFilledCircle:
                    {
                        ushort colour = (ushort)Pop();
                        ushort r = (ushort)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        screen.Suspend();
                        screen.Console.FillCircle(x, y, r, colour);
                        screen.Resume(false);
                    }
                    break;

                case LibCall.GraphicsDrawChar:
                    {
                        bool   antiAlias = (bool)(Pop() != 0);
                        byte   scale = (byte)Pop();
                        ushort backColour = (ushort)Pop();
                        ushort foreColour = (ushort)Pop();
                        char   chr = (char)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        //screen.Suspend();
                        throw new NotImplementedException();
                        //screen.Resume(false);
                    }
                    //break;
            }
        }
            */
        switch (libCall)
        {
            case LibCall.TimerStart:
                    // uint Start(uint msInterval, TimerISRDelegate timerISR) library;
                    // uint Start(long msInterval, TimerISRDelegate timerISR) library;
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
            case LibCall.TimerStop:
                    // Stop(uint timerID) library;
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
            case LibCall.TimerAlarm:
                    // uint Alarm(uint msInterval, TimerISRDelegate timerISR) library;
                    // uint Alarm(long msInterval, TimerISRDelegate timerISR) library;
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
            case LibCall.TimerCancel:
                    // Cancel(uint alarmID) library;
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
            default:
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
        }
    }

        private void SystemCall(Context currentContext, SysCall sysCall, byte iOverload)
        {
            bool hasResult = false;
            ushort spBefore = sp;
            if (iOverload == 3)
            {
                iOverload = (byte)Pop();
            }
            switch (sysCall)
            {
                /*
                case SysCall.WebServerMethodGet:
                    Push(new HopperString(WebServer.Method));
                    hasResult = true;
                    break;
                case SysCall.WebServerURLGet:
                    Push(new HopperString(WebServer.URL));
                    hasResult = true;
                    break;
                case SysCall.WebServerArgumentsGet:
                    {
                        HopperStringDictionary arguments = new HopperStringDictionary(HopperType.tString);
                        foreach (KeyValuePair<string, string> arg in WebServer.Arguments)
                        {
                            arguments.Value[arg.Key] = new HopperString(arg.Value);
                        }
                        Push(arguments);
                        hasResult = true;
                    }
                    break;
                case SysCall.WebServerOn:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                // On(string url, RequestHandler requestHandler) system;
                                UInt16 methodIndex = (UInt16)Pop();
                                if (0 == methodIndex)
                                {
                                    Diagnostics.Die(0x0F, this); // invalid or uninitialized delegate
                                    break;
                                }
                                HopperString url = (HopperString)PopVariant(HopperType.tString);
                                WebServer.RegisterHandler(url.Value, "GET", methodIndex);
                            }
                            break;
                        case 1:
                            {
                                // On(string url, string httpMethod, RequestHandler requestHandler) system;
                                UInt16 methodIndex = (UInt16)Pop();
                                if (0 == methodIndex)
                                {
                                    Diagnostics.Die(0x0F, this); // invalid or uninitialized delegate
                                    break;
                                }
                                HopperString method = (HopperString)PopVariant(HopperType.tString);
                                HopperString url = (HopperString)PopVariant(HopperType.tString);
                                WebServer.RegisterHandler(url.Value, method.Value, methodIndex);
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.WebServerClearHandlers:
                    WebServer.ClearHandlers();
                    break;
                case SysCall.WebServerSend:
                    {
                        // Send(uint statusCode, string contentType, string content) system;
                        HopperString content = (HopperString)PopVariant(HopperType.tString);
                        HopperString contentType = (HopperString)PopVariant(HopperType.tString);
                        UInt16 statusCode = (UInt16)Pop();
                        WebServer.Send(statusCode, contentType.Value, content.Value);
                    }
                    break;
                case SysCall.HttpClientGetRequest:
                    {
                        uint reference = Pop();
                        uint address = ((ushort)reference);
                        HopperString url = (HopperString)PopVariant(HopperType.tString);

#if DEBUG
                        Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                        HopperString response = stack[address].reference as HopperString;
                        String responseString = response.Value;
                        bool success = HopperHttpClient.GetResponse(url.Value, ref responseString);
                        if (success)
                        {
                            response.Value = responseString;
                            stack[address].reference = response;
                        }
                        PushBool(success);
                        hasResult = true;
                    }
                    break;
                */
                case SysCall.StringNew:
                    {
                        HopperString str = new HopperString();
                        Push(str);
                        hasResult = true;
                    }
                    break;


                case SysCall.StringNewFromConstant:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                ushort length = (ushort)Pop();
                                ushort location = (ushort)Pop();
                                location = (ushort)(location + currentContext.ConstantsStart);

                                string s = "";
                                for (ushort i = 0; i < length; i++)
                                {
                                    char c = (char)code[location + i];
                                    s += c;
                                }
                                HopperString str = new HopperString(s);
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        case 1:
                            {
                                ushort content = (ushort)Pop();
                                byte lsb = (byte)(content & 0xFF);
                                byte msb = (byte)(content >> 8);
                                string value = ((char)lsb).ToString();
                                if (msb != 0)
                                {
                                    value = value + (char)(msb);
                                }
                                HopperString str = new HopperString(value);
                                Push(str);
                                hasResult = true;
                            }
                            break;
                    }
                    break;
                case SysCall.StringPushImmediate:
                    {
                        string value = "";
                        for (; ; )
                        {
                            ushort content = (ushort)Pop();
                            byte lsb = (byte)(content & 0xFF);
                            byte msb = (byte)(content >> 8);
                            if (lsb == 0)
                            {
                                break;
                            }
                            value = value + (char)lsb;
                            if (msb == 0)
                            {
                                break;
                            }
                            value = value + (char)msb;
                        }
                        HopperString str = new HopperString(value);
                        Push(str);
                        hasResult = true;
                        break;
                    }
                case SysCall.CharToString:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                char more = (char)Pop();
                                HopperString str = new HopperString("" + more);
                                Push(str);
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    hasResult = true;
                    break;
                case SysCall.StringAppend:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                HopperString more = (HopperString)PopVariant(HopperType.tString);
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value + more.Value);
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        case 1:
                            {
                                // String* String_Append(const String * _this_, _CHAR_ more);
                                char more = (char)Pop();
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value + more);
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }

                    break;
                case SysCall.StringBuildFront:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                char more = (char)Pop();
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = more + build.Value;
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.StringTrim:
                    switch (iOverload)
                    {
                        // string Trim(string this)
                        case 0:
                            {
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value.Trim());
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        // Trim(ref string build) system;
                        case 1:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = build.Value.Trim();
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.StringTrimLeft:
                    switch (iOverload)
                    {
                        // string TrimLeft(string this)
                        case 0:
                            {
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value);
                                while (str.Value.StartsWith(" "))
                                {
                                    str.Value = str.Value.Substring(1);
                                }
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        // TrimLeft(ref string build) system;
                        case 1:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                while (build.Value.StartsWith(" "))
                                {
                                    build.Value = build.Value.Substring(1);
                                }
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.StringTrimRight:
                    switch (iOverload)
                    {
                        // TrimRight(ref string build) system;
                        case 0:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                while (build.Value.EndsWith(" "))
                                {
                                    build.Value = build.Value.Substring(0, build.Value.Length - 1);
                                }
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.StringToUpper:
                    switch (iOverload)
                    {
                        // string ToUpper(string this)
                        case 0:
                            {
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value.ToUpper());
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        // ToUpper(ref string build) system;
                        case 1:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = build.Value.ToUpper();
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.StringToLower:
                    switch (iOverload)
                    {
                        // string ToLower(string this)
                        case 0:
                            {
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value.ToLower());
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        // ToLower(ref string build) system;
                        case 1:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = build.Value.ToLower();
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.StringBuild:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                HopperString more = (HopperString)PopVariant(HopperType.tString);
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = build.Value + more.Value;
                                stack[address].reference = build;
                            }
                            break;
                        case 1:
                            {
                                char more = (char)Pop();
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = build.Value + more;
                                stack[address].reference = build;
                            }
                            break;
                        case 2:
                            {
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = "";
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.StringReplace:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                HopperString replace = (HopperString)PopVariant(HopperType.tString);
                                HopperString pattern = (HopperString)PopVariant(HopperType.tString);
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                if (pattern.Value.Length == 0)
                                {
                                    HopperString str = new HopperString(_this_.Value);
                                    Push(str);
                                    hasResult = true;
                                }
                                else
                                {
                                    HopperString str = new HopperString(_this_.Value.Replace(pattern.Value, replace.Value));
                                    Push(str);
                                    hasResult = true;
                                }
                            }
                            break;
                        case 1:
                            {
                                char replace = (char)Pop();
                                char pattern = (char)Pop();
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString(_this_.Value.Replace(pattern, replace));
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.StringEndsWith:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                char pattern = (char)Pop();
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                PushBool(_this_.Value.EndsWith(pattern.ToString()));
                                hasResult = true;
                            }
                            break;
                        case 1:
                            {
                                HopperString pattern = (HopperString)PopVariant(HopperType.tString);
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                PushBool(_this_.Value.EndsWith(pattern.Value));
                                hasResult = true;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif

                            break;
                    }
                    break;
                case SysCall.StringSubstring:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                ushort start = (ushort)Pop();
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString((start >= _this_.Value.Length) ? "" : _this_.Value.Substring(start));
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        case 1:
                            {
                                ushort length = (ushort)Pop();
                                ushort start = (ushort)Pop();
                                HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                                HopperString str = new HopperString();
                                if (start > _this_.Value.Length)
                                {
                                    // empty string
                                }
                                else if (start + length > _this_.Value.Length)
                                {
                                    str = new HopperString(_this_.Value.Substring(start));
                                }
                                else
                                {
                                    str = new HopperString(_this_.Value.Substring(start, length));
                                }
                                Push(str);
                                hasResult = true;
                            }
                            break;
                        // Substring(ref string build, uint start) system;
                        case 2:
                            {
                                ushort start = (ushort)Pop();
                                uint reference = Pop();
                                uint address = ((ushort)reference);
#if DEBUG
                                Diagnostics.ASSERT(stack[address].type == HopperType.tString, "string ref expected");
#endif
                                HopperString build = stack[address].reference as HopperString;
                                build.Value = (start >= build.Value.Length) ? "" : build.Value.Substring(start);
                                stack[address].reference = build;
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif

                            break;
                    }
                    break;
                case SysCall.StringLengthGet:
                    {
                        HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                        Push((ushort)_this_.Value.Length, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.StringInsertChar:
                    {
                        char append = (char)Pop();
                        ushort index = (ushort)Pop();
                        HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                        HopperString str = new HopperString(_this_.Value.Insert(index, append.ToString()));
                        Push(str);
                        hasResult = true;
                    }
                    break;


                case SysCall.StringGetChar:
                    {
                        ushort index = (ushort)Pop();
                        HopperString _this_ = (HopperString)PopVariant(HopperType.tString);
                        if (index >= _this_.Value.Length)
                        {
                            Diagnostics.Die(0x05, this); // string index out of range
                            break;
                        }
                        Push(_this_.Value[index], HopperType.tChar);
                        hasResult = true;
                    }
                    break;

                case SysCall.StringCompare:
                    {
                        HopperString top = (HopperString)PopVariant(HopperType.tString);
                        HopperString next = (HopperString)PopVariant(HopperType.tString);
                        short result = (short)string.Compare(next.Value, top.Value, StringComparison.Ordinal);
                        if (result < 0)
                        {
                            result = -1;
                        }
                        else if (result > 0)
                        {
                            result = 1;
                        }
                        /*
                        short oldresult = (short)string.Compare(next.Value, top.Value);
                        if (oldresult != result)
                        {
                            if (next.Value.ToLower() == top.Value.ToLower())
                            Diagnostics.OutputDebug("\n" + next.Value + " " + top.Value + " " + oldresult.ToString() + " " + result.ToString());
                        }
                        */
                        PushInt(result);
                        hasResult = true;
                    }
                    break;

                case SysCall.StringIndexOf:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                //bool IndexOf(string this, char pattern, ref uint index) system;
                                uint reference = Pop();
                                uint address = ((ushort)reference);
                                char ch = (char)Pop();
                                HopperString top = (HopperString)PopVariant(HopperType.tString);

                                int index = top.Value.IndexOf(ch);

                                if (index != -1)
                                {
                                    stack[address].value = (ushort)index;
                                }
                                PushBool(index != -1);
                                hasResult = true;
                            }
                            break;
                        case 1:
                            {
                                //bool IndexOf(string this, char pattern, uint searchIndex, ref uint index) system;
                                uint reference = Pop();
                                uint address = ((ushort)reference);
                                int searchIndex = (int)Pop();
                                char ch = (char)Pop();
                                HopperString top = (HopperString)PopVariant(HopperType.tString);

                                int index = (searchIndex < top.Value.Length ? top.Value.IndexOf(ch, searchIndex) : -1);

                                if (index != -1)
                                {
                                    stack[address].value = (ushort)index;
                                }
                                PushBool(index != -1);
                                hasResult = true;
                            }
                            break;
                    }
                    break;
                case SysCall.StringContains:
                    {
                        switch (iOverload)
                        {
                            case 0:
                                {
                                    // bool Contains(string this, char needle) system;
                                    char needle = (char)Pop();
                                    HopperString top = (HopperString)PopVariant(HopperType.tString);
                                    PushBool(top.Value.Contains(needle));
                                    hasResult = true;
                                }
                                break;
                            case 1:
                                {
                                    // bool Contains(string this, string needle) system;
                                    HopperString needle = (HopperString)PopVariant(HopperType.tString);
                                    HopperString top = (HopperString)PopVariant(HopperType.tString);
                                    PushBool(top.Value.Contains(needle.Value));
                                    hasResult = true;
                                }
                                break;

                        }
                    }
                    break;
                case SysCall.StringStartsWith:
                    {
                        switch (iOverload)
                        {
                            case 0:
                                {
                                    // bool StartsWith(string this, char pattern) system;
                                    char pattern = (char)Pop();
                                    HopperString top = (HopperString)PopVariant(HopperType.tString);
                                    PushBool((top.Value.Length > 0) && (top.Value[0] == pattern));
                                    hasResult = true;
                                }
                                break;
                            case 1:
                                {
                                    // bool StartsWith(string this, string pattern) system;
                                    HopperString pattern = (HopperString)PopVariant(HopperType.tString);
                                    HopperString top = (HopperString)PopVariant(HopperType.tString);
                                    PushBool(top.Value.StartsWith(pattern.Value));
                                    hasResult = true;
                                }
                                break;

                        }
                    }
                    break;

                case SysCall.CharToLower:
                    {
                        char ch = (char)Pop();
                        Push(Char.ToLower(ch), HopperType.tChar);
                        hasResult = true;
                    }
                    break;
                case SysCall.CharToUpper:
                    {
                        char ch = (char)Pop();
                        Push(Char.ToUpper(ch), HopperType.tChar);
                        hasResult = true;
                    }
                    break;
                case SysCall.CharIsDigit:
                    PushBool(Char.IsDigit((char)Pop()));
                    hasResult = true;
                    break;
                case SysCall.CharIsHexDigit:
                    {
                        uint b = Pop();
                        bool isHex = ((b >= 48) && (b <= 57)) || // 0..9
                                      ((b >= 65) && (b <= 70)) || // A..F
                                      ((b >= 97) && (b <= 102));  // a..f
                        PushBool(isHex);
                        hasResult = true;
                    }
                    break;
                case SysCall.CharIsLower:
                    PushBool(Char.IsLower((char)Pop()));
                    hasResult = true;
                    break;
                case SysCall.CharIsUpper:
                    PushBool(Char.IsUpper((char)Pop()));
                    hasResult = true;
                    break;
                case SysCall.CharIsLetterOrDigit:
                    PushBool(Char.IsLetterOrDigit((char)Pop()));
                    hasResult = true;
                    break;
                case SysCall.ByteToDigit:
                    {
                        uint d = Pop() + 48; // +0
                        Push((char)d, HopperType.tChar);
                        hasResult = true;
                    }
                    break;
                case SysCall.ByteToHex:
                    {
                        uint h = Pop();
                        if (h < 10)
                        {
                            h = h + 48; // +0
                        }
                        else
                        {
                            h = h + 55; // +A - 10
                        }
                        Push((char)h, HopperType.tChar);
                        hasResult = true;
                    }
                    break;


                case SysCall.ArrayNew:
                    {
                        HopperType type = (HopperType)Pop();
                        ushort size = (ushort)Pop();
                        HopperArray array = new HopperArray(type, size);
                        Push(array);
                        hasResult = true;
                    }
                    break;

                case SysCall.ArrayNewFromConstant:
                    {
                        HopperType type = (HopperType)Pop();
                        ushort length = (ushort)Pop();
                        ushort location = (ushort)Pop();
                        location = (ushort)(location + currentContext.ConstantsStart);

                        HopperArray arr = new HopperArray(type, length);

                        for (ushort i = 0; i < length; i++)
                        {
                            arr.Value[i] = code[location + i];
                        }

                        Push(arr);
                        hasResult = true;
                        break;
                    }
                case SysCall.ArraySlice:
                    {
                        ushort length = 0;
                        ushort start = 0;
                        HopperArray _this_ = null;
                        switch (iOverload)
                        {
                            case 0:
                                {
                                    start = (ushort)Pop();
                                    _this_ = (HopperArray)PopVariant(HopperType.tArray);
                                    ushort sLength = (ushort)_this_.Value.Length;
                                    if (sLength > start)
                                    {
                                        length = (ushort)(sLength - start);
                                    }
                                }
                                break;
                            case 1:
                                {
                                    length = (ushort)Pop();
                                    start = (ushort)Pop();
                                    _this_ = (HopperArray)PopVariant(HopperType.tArray);
                                }
                                break;
                        }
                        HopperArray arr = new HopperArray(_this_.VType, length);
                        for (ushort i = 0; i < length; i++)
                        {
                            arr.Value[i] = _this_.Value[start + i];
                        }
                        Push(arr);
                        hasResult = true;
                    }
                    break;

                case SysCall.ArraySetItem:
                    {
#if UNDOINLINED
                        ushort value = (ushort)Pop();
                        ushort index = (ushort)Pop();
                        HopperArray _this_ = (HopperArray)PopVariant(HopperType.tArray);
#else
                        uint sp2 = (uint)(sp);
                        uint value = stack[sp2 - 1].value;
                        uint index = stack[sp2 - 2].value;
                        HopperArray _this_ = stack[sp2 - 3].reference as HopperArray;
                        sp -= 3;
#endif
                        ushort[] array = _this_.Value;
                        if (index >= array.Length)
                        {
                            Diagnostics.Die(0x02, this); // array index out of range
                            break;
                        }
                        array[index] = (ushort)value;
                    }
                    break;
                case SysCall.ArrayGetItem:
                    {
#if UNDOINLINED
                        ushort index = (ushort)Pop();
                        HopperArray _this_ = (HopperArray)PopVariant(HopperType.tArray);
#else
                        uint sp2 = (uint)(sp);
                        uint index = stack[sp2 - 1].value;
                        HopperArray _this_ = stack[sp2 - 2].reference as HopperArray;
                        sp--;
#endif
                        ushort[] array = _this_.Value;
                        if (index >= array.Length)
                        {
                            Diagnostics.Die(0x02, this); // array index out of range
                            break;
                        }
#if UNDOINLINED
                        if (_this_.VType == HopperType.tInt)
                        {
                            // internally in C# we are converting to 32 bits
                            // ushort will not sign extend, short will
                            PushInt((short)(array[index]));
                            hasResult = true;
                        }
                        else
                        {
                            Push(array[index], _this_.VType);
                            hasResult = true;
                        }
#else
                        if (_this_.VType == HopperType.tInt)
                        {
                            // internally in C# we are converting to 32 bits
                            // ushort will not sign extend, short will
                            int v = (short)(array[index]);
                            stack[sp2 - 2].value = (uint)v;
                        }
                        else
                        {
                            stack[sp2 - 2].value = array[index];
                        }
                        stack[sp2 - 2].type = _this_.VType;
#endif
                    }
                    break;
                case SysCall.ArrayItemTypeGet:
                    {
                        HopperArray _this_ = (HopperArray)PopVariant(HopperType.tArray);
                        Push((ushort)_this_.VType, HopperType.tType);
                        hasResult = true;
                    }
                    break;
                case SysCall.ArrayCountGet:
                    {
                        HopperArray _this_ = (HopperArray)PopVariant(HopperType.tArray);
                        Push((ushort)_this_.Value.Length, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.RuntimeInline:
                    {
                        ushort startIndex = (ushort)Pop();
                        HopperArray inlineCodeArray = (HopperArray)PopVariant(HopperType.tArray);
                        if (codeStore != null)
                        {
                            Diagnostics.Die(0x0B, this); // nested call to inline code?
                            break;
                        }
                        pcStore = pc;
                        spStore = sp;
                        bpStore = bp;
                        cspStore = csp;
                        codeStore = code;
                        ushort[] inlineCode = inlineCodeArray.Value;
                        code = new byte[codeStore.Length + inlineCode.Length];
                        for (uint i = 0; i < codeStore.Length; i++)
                        {
                            code[i] = (byte)codeStore[i];
                        }
                        ushort inlineStart = (ushort)codeStore.Length;

                        for (uint i = 0; i < inlineCode.Length; i++)
                        {
                            code[i + inlineStart] = (byte)inlineCode[i];
                        }
                        pc = (ushort)(startIndex + inlineStart);
                        break;
                    }

                case SysCall.PairNew:
                    {
                        HopperType vType = (HopperType)Pop();
                        HopperType kType = (HopperType)Pop();
                        HopperPair pair = new HopperPair(kType, vType);
                        Push(pair);
                        hasResult = true;
                    }
                    break;
                case SysCall.PairKey:
                    {
                        HopperPair pair = (HopperPair)PopVariant(HopperType.tPair);
                        if (Type_IsValueType(pair.KType))
                        {
                            HopperValue value = (HopperValue)pair.Key;
                            Push(value.Value, value.Type);
                            hasResult = true;
                        }
                        else
                        {
                            Push(pair.Key.Clone());
                            hasResult = true;
                        }
                    }
                    break;
                case SysCall.PairValue:
                    {
                        HopperPair pair = (HopperPair)PopVariant(HopperType.tPair);
                        if (Type_IsValueType(pair.VType))
                        {
                            HopperValue value = (HopperValue)pair.Value;
                            Push(value.Value, value.Type);
                            hasResult = true;
                        }
                        else
                        {
                            Push(pair.Value.Clone());
                            hasResult = true;
                        }
                    }
                    break;
                case SysCall.DictionaryNew:
                    {
                        HopperType vType = (HopperType)Pop();
                        HopperType kType = (HopperType)Pop();
                        if (kType == HopperType.tString)
                        {
                            Push(new HopperStringDictionary(vType));
                            hasResult = true;
                        }
                        else if (Type_IsKeyType(kType))
                        {
                            Push(new HopperUIntDictionary(vType));
                            hasResult = true;
                        }
                        else
                        {
                            throw new NotImplementedException();
                        }
                    }
                    break;
                case SysCall.DictionaryCountGet:
                    {
                        Variant _this_ = (Variant)PopVariant(HopperType.tDictionary);
                        if (_this_ as HopperStringDictionary != null)
                        {
                            Push((ushort)((HopperStringDictionary)_this_).Value.Count, HopperType.tUInt);
                            hasResult = true;
                        }
                        else
                        {
                            Push((ushort)((HopperUIntDictionary)_this_).Value.Count, HopperType.tUInt);
                            hasResult = true;
                        }
                    }
                    break;
                case SysCall.DictionaryNext:
                    {
                        ushort iterator = (ushort)Pop();
                        Variant _this_ = (Variant)PopVariant(HopperType.tDictionary);
                        if (_this_ as HopperStringDictionary != null)
                        {
                            HopperStringDictionary dictionary = (HopperStringDictionary)_this_;
                            HopperPair pair = new HopperPair(HopperType.tString, dictionary.VType);
                            bool found = dictionary.Next(pair, ref iterator);
                            PushBool(found);
                            Push(pair);
                            Push(iterator, HopperType.tUInt);
                            hasResult = true; // TODO : push list?
                        }
                        else
                        {
                            HopperUIntDictionary dictionary = (HopperUIntDictionary)_this_;
                            HopperPair pair = new HopperPair(HopperType.tUInt, dictionary.VType);
                            bool found = dictionary.Next(pair, ref iterator);
                            PushBool(found);
                            Push(pair);
                            Push(iterator, HopperType.tUInt);
                            hasResult = true; // TODO : push list?
                        }
                    }
                    break;
                case SysCall.DictionaryClear:
                    {
                        Variant _this_ = (Variant)PopVariant(HopperType.tDictionary);
                        if (_this_ as HopperStringDictionary != null)
                        {
                            ((HopperStringDictionary)_this_).Value.Clear();
                        }
                        else
                        {
                            ((HopperUIntDictionary)_this_).Value.Clear();
                        }
                    }
                    break;
                case SysCall.DictionarySet:
                    {
                        HopperType valueType = GetStackType((ushort)(sp - 1));
                        HopperType keyType = GetStackType((ushort)(sp - 2));
                        if (Type_IsValueType(valueType) && (keyType == HopperType.tString))
                        {
                            uint value = Pop();
                            HopperString key = (HopperString)PopVariant(HopperType.tString);
                            HopperStringDictionary _this_ = (HopperStringDictionary)PopVariant(HopperType.tDictionary);
                            HopperType vType = _this_.VType;
                            if (_this_.VType == HopperType.tVariant)
                            {
                                vType = valueType;
                            }
                            _this_.Value[key.Value] = new HopperValue(value, vType);
                        }
                        else if (Type_IsValueType(valueType) && (keyType != HopperType.tString))
                        {
                            uint value = Pop();
                            ushort key = (ushort)Pop();
                            HopperUIntDictionary _this_ = (HopperUIntDictionary)PopVariant(HopperType.tDictionary);
                            if (_this_.VType == HopperType.tVariant)
                            {
                                _this_.Value[key] = new HopperValue(value, valueType);
                            }
                            else
                            {
                                _this_.Value[key] = new HopperValue(value, _this_.VType);
                            }
                        }
                        else if (!Type_IsValueType(valueType) && (keyType == HopperType.tString))
                        {
                            Variant value = PopVariant(HopperType.tUndefined);
                            HopperString key = (HopperString)PopVariant(HopperType.tString);
                            HopperStringDictionary _this_ = (HopperStringDictionary)PopVariant(HopperType.tDictionary);
                            _this_.Value[key.Value] = value.Clone();

                        }
                        else if (!Type_IsValueType(valueType) && (keyType != HopperType.tString))
                        {
                            Variant value = PopVariant(HopperType.tUndefined);
                            ushort key = (ushort)Pop();
                            HopperUIntDictionary _this_ = (HopperUIntDictionary)PopVariant(HopperType.tDictionary);
                            _this_.Value[key] = value.Clone();
                        }
                        else
                        {
                            throw new NotImplementedException();
                        }
                    }
                    break;
                case SysCall.DictionaryContains:
                    {
                        HopperType keyType = GetStackType((ushort)(sp - 1));
                        if (keyType == HopperType.tString)
                        {
                            HopperString key = (HopperString)PopVariant(HopperType.tString);
                            HopperStringDictionary _this_ = (HopperStringDictionary)PopVariant(HopperType.tDictionary);
                            PushBool(_this_.Value.ContainsKey(key.Value));
                            hasResult = true;
                        }
                        else if (keyType != HopperType.tString)
                        {
                            ushort key = (ushort)Pop();
                            HopperUIntDictionary _this_ = (HopperUIntDictionary)PopVariant(HopperType.tDictionary);
                            PushBool(_this_.Value.ContainsKey(key));
                            hasResult = true;
                        }
                        else
                        {
                            throw new NotImplementedException();
                        }
                    }
                    break;
                case SysCall.DictionaryGet:
                    {
                        HopperType keyType = GetStackType((ushort)(sp - 1));
                        if (keyType == HopperType.tString)
                        {
                            HopperString key = (HopperString)PopVariant(HopperType.tString);
                            HopperStringDictionary _this_ = (HopperStringDictionary)PopVariant(HopperType.tDictionary);
                            if (!_this_.Value.ContainsKey(key.Value))
                            {
                                Diagnostics.Die(0x03, this); // no entry for key in dictionary
                                break;
                            }
                            if (Type_IsValueType(_this_.VType))
                            {
                                HopperValue value = (HopperValue)_this_.Value[key.Value];
                                Push(value.Value, value.Type);
                                hasResult = true;
                            }
                            else
                            {
                                Push(_this_.Value[key.Value].Clone());
                                hasResult = true;
                            }
                        }
                        else if (keyType != HopperType.tString)
                        {
                            ushort key = (ushort)Pop();
                            HopperUIntDictionary _this_ = (HopperUIntDictionary)PopVariant(HopperType.tDictionary);
                            if (!_this_.Value.ContainsKey(key))
                            {
                                Diagnostics.Die(0x03, this); // no entry for key in dictionary
                                break;
                            }
                            if (Type_IsValueType(_this_.VType))
                            {
                                HopperValue value = (HopperValue)_this_.Value[key];
                                Push(value.Value, value.Type);
                                hasResult = true;
                            }
                            else
                            {
                                Push(_this_.Value[key].Clone());
                                hasResult = true;
                            }
                        }
                        else
                        {
                            throw new NotImplementedException();
                        }
                    }
                    break;

                case SysCall.ListNew:
                    {
                        HopperType type = (HopperType)Pop();
                        HopperList list = new HopperList(type);
                        Push(list);
                        hasResult = true;
                    }
                    break;
                case SysCall.ListCountGet:
                    {
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        Push((ushort)_this_.Value.Count, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;
                case SysCall.ListInsert:
                    {
                        HopperType topType = GetStackType((ushort)(sp - 1));
                        if (Type_IsValueType(topType))
                        {
                            uint value = Pop();
                            ushort index = (ushort)Pop();
                            HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                            HopperType valueType = _this_.VType;
                            if (_this_.VType == HopperType.tVariant)
                            {
                                valueType = topType;
                            }
#if DEBUG
                            Diagnostics.ASSERT(_this_.VType == topType, "correct value type for list?");
#endif
                            _this_.Value.Insert(index, new HopperValue(value, valueType));
                        }
                        else
                        {
                            Variant value = PopVariant(HopperType.tUndefined);
                            ushort index = (ushort)Pop();
                            HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
#if DEBUG
                            if (_this_.VType != HopperType.tVariant)
                            {
                                Diagnostics.ASSERT(_this_.VType == value.Type, "correct reference type for list?");
                            }
#endif
                            _this_.Value.Insert(index, value.Clone());
                        }
                    }
                    break;
                case SysCall.ListAppend:
                    {
                        HopperType topType = GetStackType((ushort)(sp - 1));
                        if (Type_IsValueType(topType))
                        {
                            uint value = Pop();
                            HopperList _this_ = (HopperList)PopVariant(HopperType.tList);

                            HopperType valueType = _this_.VType;
                            if (_this_.VType == HopperType.tVariant)
                            {
                                valueType = topType;
                            }
#if DEBUG
                            switch (valueType)
                            {
                                case HopperType.tBool:
                                    switch (topType)
                                    {
                                        case HopperType.tBool:
                                            break;
                                        case HopperType.tByte:
                                        case HopperType.tUInt:
                                        case HopperType.tChar:
                                        case HopperType.tEnum:
                                        case HopperType.tFlags:

                                        // these work because we popped a uint
                                        case HopperType.tLong:
                                        case HopperType.tInt:
                                            if (value > 1)
                                            {
                                                Diagnostics.ASSERT(valueType == topType, "correct value type for tBool list?");
                                            }
                                            break;

                                        default:
                                            Diagnostics.ASSERT(valueType == topType, "correct value type for tBool list?");
                                            break;
                                    }
                                    break;
                                case HopperType.tByte:
                                    switch (topType)
                                    {
                                        case HopperType.tByte:
                                            break;
                                        case HopperType.tUInt:
                                        case HopperType.tChar:
                                        case HopperType.tEnum:
                                        case HopperType.tFlags:

                                        // these work because we popped a uint
                                        case HopperType.tLong:
                                        case HopperType.tInt:
                                            if (value > 255)
                                            {
                                                Diagnostics.ASSERT(valueType == topType, "correct value type for tByte list?");
                                            }
                                            break;
                                        default:
                                            Diagnostics.ASSERT(valueType == topType, "correct value type for tByte list?");
                                            break;
                                    }
                                    break;
                                case HopperType.tUInt:
                                    switch (topType)
                                    {
                                        case HopperType.tUInt:
                                            break;
                                        case HopperType.tByte:
                                        case HopperType.tChar:
                                        case HopperType.tEnum:
                                        case HopperType.tFlags:
                                            // all unsigned types smaller than tUInt
                                            break;
                                        default:
                                            Diagnostics.ASSERT(valueType == topType, "correct value type for list?");
                                            break;
                                    }
                                    break;
                                default:
                                    Diagnostics.ASSERT(valueType == topType, "correct value type for list?");
                                    break;
                            }
#endif

                            _this_.Value.Add(new HopperValue(value, valueType));
                        }
                        else
                        {
                            Variant value = PopVariant(HopperType.tUndefined);
                            HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
#if DEBUG
                            if (_this_.VType != HopperType.tVariant)
                            {
                                Diagnostics.ASSERT(_this_.VType == value.Type, "correct reference type for list?");
                            }
#endif
                            _this_.Value.Add(value.Clone());
                        }
                    }
                    break;
                case SysCall.ListSetItem:
                    {
                        HopperType topType = GetStackType((ushort)(sp - 1));
                        uint value = 0;
                        Variant variant = null;
                        if (Type_IsValueType(topType))
                        {
                            value = Pop();
                        }
                        else
                        {
                            variant = PopVariant(HopperType.tUndefined);
                        }
                        uint index = Pop();
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        if (index >= _this_.Value.Count)
                        {
                            Diagnostics.Die(0x01, this); // list index out of range
                            break;
                        }
                        if (Type_IsValueType(topType))
                        {
                            HopperType valueType = _this_.VType;
                            if (_this_.VType == HopperType.tVariant)
                            {
                                valueType = topType;
                            }
                            _this_.Value[(int)index] = new HopperValue(value, valueType);
                        }
                        else
                        {
                            _this_.Value[(int)index] = variant;
                        }
                    }
                    break;
                case SysCall.ListGetItem:
                    {
                        ushort index = (ushort)Pop();
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        if (index >= _this_.Value.Count)
                        {
                            Diagnostics.Die(0x01, this); // list index out of range
                            break;
                        }
                        if (Type_IsValueType(_this_.VType))
                        {
                            HopperValue value = (HopperValue)_this_.Value[index];
                            Push(value.Value, value.Type);
                            hasResult = true;
                        }
                        else if (_this_.VType == HopperType.tVariant)
                        {
                            HopperValue value = _this_.Value[index] as HopperValue;
                            if (value != null)
                            {
                                Push(value.Value, value.Type);
                                hasResult = true;
                            }
                            else
                            {
                                Push(_this_.Value[index].Clone());
                                hasResult = true;
                            }
                        }
                        else
                        {
                            Push(_this_.Value[index].Clone());
                            hasResult = true;
                        }
                    }
                    break;
                case SysCall.ListGetItemAsVariant:
                    {
                        ushort index = (ushort)Pop();
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        if (index >= _this_.Value.Count)
                        {
                            Diagnostics.Die(0x01, this); // list index out of range
                            break;
                        }
                        Push(_this_.Value[index].Clone());
                        hasResult = true;
                        HopperType topType = GetStackType((ushort)(sp - 1));
                    }
                    break;
                case SysCall.ListRemove:
                    {
                        ushort index = (ushort)Pop();
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        if (index >= _this_.Value.Count)
                        {
                            Diagnostics.Die(0x01, this); // list index out of range
                            break;
                        }
                        _this_.Value.RemoveAt(index);
                    }
                    break;
                case SysCall.ListClear:
                    {
                        HopperList _this_ = (HopperList)PopVariant(HopperType.tList);
                        _this_.Value.Clear();
                    }
                    break;
                case SysCall.ListContains:
                    {
                        HopperType type = GetStackType((ushort)(sp - 1));
                        if (Type_IsValueType(type))
                        {
                            uint value = Pop();
                            HopperValue variant = new HopperValue(value, type);
                            HopperList list = (HopperList)PopVariant(HopperType.tList);
                            PushBool(list.Value.Contains(variant));
                            hasResult = true;
                        }
                        else
                        {
                            Variant variant = PopVariant(HopperType.tUndefined);
                            HopperList list = (HopperList)PopVariant(HopperType.tList);
                            PushBool(list.Value.Contains(variant));
                            hasResult = true;
                        }
                    }
                    break;

                case SysCall.SystemCurrentDirectoryGet:
                    {
                        HopperString str = new HopperString(HopperSystem.CurrentDirectory);
                        Push(str);
                        hasResult = true;
                    }
                    break;
                case SysCall.SystemCurrentDirectorySet:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        HopperSystem.CurrentDirectory = path.Value;
                    }
                    break;
                case SysCall.SystemArgumentsGet:
                    {
                        HopperList arguments = new HopperList(HopperType.tString);
                        foreach (string arg in hopperSystem.Arguments)
                        {
                            arguments.Value.Add(new HopperString(arg));
                        }
                        Push(arguments);
                        hasResult = true;
                    }
                    break;

                case SysCall.RuntimeExecute:
                    {
                        HopperList arguments = (HopperList)PopVariant(HopperType.tList);
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        currentContext.pcBefore = pc;
                        currentContext.spBefore = sp;
                        currentContext.bpBefore = bp;
                        currentContext.gpBefore = gp;
                        currentContext.cspBefore = csp;

                        currentContext.r0Before = r0;
                        currentContext.isCDeclBefore = isCDecl;

                        List<string> args = new List<string>();
                        foreach (Variant v in arguments.Value)
                        {
                            HopperString arg = v as HopperString;
                            args.Add(arg.Value);
                        }

                        ushort setError = 0;
                        Load(path.Value, args);
                        isCDecl = false;
                        int result = Execute(ref setError, true);
                        Halted = false;

                        code = currentContext.Code;
                        methodTable = currentContext.MethodTable;

                        pc = currentContext.pcBefore;
                        gp = currentContext.gpBefore;

                        r0 = currentContext.r0Before;
                        isCDecl = currentContext.isCDeclBefore;


                        if (result != 0) // Die happened
                        {
                            csp = currentContext.cspBefore;
                            sp = currentContext.spBefore;
                            bp = currentContext.bpBefore;
                            lastError = 0;
                        }
#if DEBUG
                        Diagnostics.ASSERTDIE(currentContext.bpBefore == bp, "bp not the same as before System.Execute(..)", this);
                        Diagnostics.ASSERTDIE(currentContext.spBefore == sp, "sp not the same as before System.Execute(..)", this);
#endif
                        if (setError != 0)
                        {
                            Diagnostics.OutputDebug("Runtime.Execute(..) -> " + setError.ToString());
                        }
                        Push(setError, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.ScreenPrint:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                ushort backColour = (ushort)Pop();
                                ushort foreColour = (ushort)Pop();
                                char c = (char)Pop();
                                // Print(char c,     uint foreColour, uint backColour)
                                this.screen.Print(c, foreColour, backColour);
                            }
                            break;
                        case 1:
                            {
                                ushort backColour = (ushort)Pop();
                                ushort foreColour = (ushort)Pop();
                                HopperString str = (HopperString)PopVariant(HopperType.tString);
                                // Print(string s,   uint foreColour, uint backColour)
                                this.screen.Print(str.Value, foreColour, backColour);
                            }
                            break;
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.ScreenColumnsGet:
                    Push(this.screen.Columns, HopperType.tByte);
                    hasResult = true;
                    break;
                case SysCall.ScreenRowsGet:
                    Push(this.screen.Rows, HopperType.tByte);
                    hasResult = true;
                    break;

                case SysCall.ScreenCursorXGet:
                    Push(this.screen.CursorX, HopperType.tByte);
                    hasResult = true;
                    break;
                case SysCall.ScreenCursorYGet:
                    Push(this.screen.CursorY, HopperType.tByte);
                    hasResult = true;
                    break;

                case SysCall.ScreenSuspend:
                    this.screen.Suspend();
                    break;
                case SysCall.ScreenResume:
                    this.screen.Resume(Pop() != 0);
                    break;
                case SysCall.ScreenDrawChar:
                    {
                        ushort backColour = (ushort)Pop();
                        ushort foreColour = (ushort)Pop();
                        char c = (char)Pop();
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        this.screen.DrawChar(x, y, c, foreColour, backColour);
                    }
                    break;
                case SysCall.ScreenPrintLn:
                    this.screen.PrintLn();
                    break;
                case SysCall.ScreenSetCursor:
                    {
                        ushort y = (ushort)Pop();
                        ushort x = (ushort)Pop();
                        this.screen.SetCursor(x, y);
                    }
                    break;
                case SysCall.ScreenShowCursorSet:
                    {
                        ushort show = (ushort)Pop();
                        this.screen.Console.ShowCursor(show != 0);
                    }
                    break;

                case SysCall.ScreenClear:
                    this.screen.Clear();
                    break;

                case SysCall.RuntimePCGet:
                    Push((ushort)this.pc, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.RuntimeSPGet:
                    Push((ushort)this.sp, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.RuntimeBPGet:
                    Push((ushort)this.bp, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.RuntimeCSPGet:
                    Push((ushort)this.bp, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.RuntimeUserCodeGet:
                    Push((ushort)this.codeStore.Length, HopperType.tUInt);
                    hasResult = true;
                    break;

                case SysCall.RuntimeGetStackWord:
                    {

                        ushort offset = (ushort)Pop();
                        ushort value = (ushort)GetStack(offset); // doesn't work for long and float on Windows
                        Push(value, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.RuntimeGetStackType:
                    {
                        ushort offset = (ushort)Pop();
                        HopperType type = GetStackType(offset);
                        Push((ushort)type, HopperType.tType);
                        hasResult = true;
                        break;
                    }
                case SysCall.RuntimeGetCallStackWord:
                    {

                        ushort offset = (ushort)Pop();
                        ushort value = (ushort)GetCallStack(offset);
                        Push(value, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.KeyboardReadKey:
                    Push((ushort)this.keyboard.ReadKey(), HopperType.tEnum);
                    hasResult = true;
                    break;
                case SysCall.KeyboardIsAvailableGet:
                    PushBool(this.keyboard.IsAvailable());
                    hasResult = true;
                    break;
                case SysCall.KeyboardClickXGet:
                    Push((ushort)this.keyboard.ClickX, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.KeyboardClickYGet:
                    Push((ushort)this.keyboard.ClickY, HopperType.tUInt);
                    hasResult = true;
                    break;
                case SysCall.KeyboardClickUpGet:
                    PushBool(this.keyboard.ClickUp);
                    hasResult = true;
                    break;
                case SysCall.KeyboardClickDoubleGet:
                    PushBool(this.keyboard.ClickDouble);
                    hasResult = true;
                    break;
                case SysCall.KeyboardScrollDeltaGet:
                    Push((ushort)this.keyboard.ScrollDelta, HopperType.tInt);
                    hasResult = true;
                    break;

                case SysCall.FileNew:
                    Push(new HopperFile());
                    hasResult = true;
                    break;
                case SysCall.FileExists:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        PushBool(HopperFile.Exists(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileDelete:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        HopperFile.Delete(path.Value);
                    }
                    break;
                case SysCall.FileOpen:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperFile.Open(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileCreate:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperFile.Create(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileIsValid:
                    {
                        HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                        PushBool(file.IsValid());
                        hasResult = true;
                    }
                    break;
                case SysCall.FileRead:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                                Push(file.Read(), HopperType.tByte);
                                hasResult = true;
                                break;
                            }
                        case 1:
                            {
                                Int32 seekpos = PopLong();
                                HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                                Push(file.Read(seekpos), HopperType.tByte);
                                hasResult = true;
                                break;
                            }
                        case 2:
                            {
                                ushort bufferSize = (ushort)Pop();
                                HopperArray buffer = (HopperArray)PopVariant(HopperType.tArray);
                                HopperFile file = (HopperFile)PopVariant(HopperType.tFile);

                                Push(file.Read(buffer, bufferSize), HopperType.tUInt);
                                hasResult = true;
                                break;
                            }
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;

                    }
                    break;
                case SysCall.FileReadLine:
                    {
                        HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                        Push(new HopperString(file.ReadLine()));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileAppend:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                byte b = (byte)Pop();
                                HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                                file.Append(b);
                                break;
                            }
                        case 1:
                            {
                                HopperString content = (HopperString)PopVariant(HopperType.tString);
                                HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                                file.Append(content.Value);
                                break;
                            }
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;
                case SysCall.FileFlush:
                    {
                        HopperFile file = (HopperFile)PopVariant(HopperType.tFile);
                        file.Flush();
                    }
                    break;
                case SysCall.FileGetSize:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        PushLong(HopperFile.GetSize(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileGetTimeStamp:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        PushLong(HopperFile.GetTimeStamp(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileGetTime:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperFile.GetTime(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.FileGetDate:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperFile.GetDate(path.Value));
                        hasResult = true;
                    }
                    break;

                case SysCall.DirectoryNew:
                    Push(new HopperDirectory());
                    hasResult = true;
                    break;
                case SysCall.DirectoryExists:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        PushBool(HopperDirectory.Exists(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.DirectoryOpen:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperDirectory.Open(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.DirectoryIsValid:
                    {
                        HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                        PushBool(directory.IsValid());
                        hasResult = true;
                    }
                    break;

                case SysCall.DirectoryGetDirectory:
                    {
                        ushort index = (ushort)Pop();
                        HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                        Push(directory.GetDirectory(index));
                        hasResult = true;
                    }
                    break;
                case SysCall.DirectoryGetFile:
                    {
                        ushort index = (ushort)Pop();
                        HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                        Push(directory.GetFile(index));
                        hasResult = true;
                    }
                    break;
                case SysCall.DirectoryGetDirectoryCount:
                    {
                        switch (iOverload)
                        {
                            case 0:
                                {
                                    HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                                    Push(directory.GetDirectoryCount(), HopperType.tUInt);
                                    hasResult = true;
                                    break;
                                }
                            case 1:
                                {
                                    uint reference = Pop();
                                    uint address = ((ushort)reference);
#if DEBUG
                                    Diagnostics.ASSERT((stack[address].type == HopperType.tUInt) || (stack[address].type == HopperType.tByte), "uint ref expected, was " + ((byte)(stack[address].type)).ToString("X2"));
#endif
                                    ushort skipped = (ushort)stack[address].value;
                                    HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                                    Push(directory.GetDirectoryCount(ref skipped), HopperType.tUInt);
                                    hasResult = true;
                                    stack[address].value = skipped;
                                    stack[address].type = HopperType.tUInt;
                                    break;
                                }
                        }
                    }
                    break;
                case SysCall.DirectoryGetFileCount:
                    {
                        switch (iOverload)
                        {
                            case 0:
                                {
                                    HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                                    Push(directory.GetFileCount(), HopperType.tUInt);
                                    hasResult = true;
                                    break;
                                }
                            case 1:
                                {
                                    uint reference = Pop();
                                    uint address = ((ushort)reference);
#if DEBUG
                                    Diagnostics.ASSERT((stack[address].type == HopperType.tUInt) || (stack[address].type == HopperType.tByte), "uint ref expected, was " + ((byte)(stack[address].type)).ToString("X2"));
#endif
                                    ushort skipped = (ushort)stack[address].value;
                                    HopperDirectory directory = (HopperDirectory)PopVariant(HopperType.tDirectory);
                                    Push(directory.GetFileCount(ref skipped), HopperType.tUInt);
                                    hasResult = true;

                                    stack[address].value = skipped;
                                    stack[address].type = HopperType.tUInt;
                                    break;
                                }
                        }
                    }
                    break;
                case SysCall.DirectoryGetTime:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperDirectory.GetTime(path.Value));
                        hasResult = true;
                    }
                    break;
                case SysCall.DirectoryGetDate:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        Push(HopperDirectory.GetDate(path.Value));
                        hasResult = true;
                    }
                    break;

                case SysCall.DirectoryDelete:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        HopperDirectory.Delete(path.Value);
                    }
                    break;
                case SysCall.DirectoryCreate:
                    {
                        HopperString path = (HopperString)PopVariant(HopperType.tString);
                        HopperDirectory.Create(path.Value);
                    }
                    break;

                case SysCall.WiFiConnect:
                    {
                        // NOP:
                        HopperString password = (HopperString)PopVariant(HopperType.tString);
                        HopperString ssid = (HopperString)PopVariant(HopperType.tString);
                        PushBool(true);
                        hasResult = true;
                        break;
                    }


                case SysCall.TimeMillisGet:
                    Int32 millis = HopperTime.Millis;
                    PushLong(millis);
                    hasResult = true;
                    break;
                //case SysCall.TimeMicrosGet:
                //    Int32 micros = HopperTime.Micros;
                //    PushLong(micros);
                //    break;
                case SysCall.TimeSecondsGet:
                    UInt16 seconds = HopperTime.Seconds;
                    Push(seconds, HopperType.tUInt);
                    break;

                case SysCall.TimeTime_Get:
                    {
                        HopperString tm = new HopperString();
                        tm.Value = DateTime.Now.ToString("HH:mm:ss");
                        Push(tm);
                        hasResult = true;
                    }
                    break;
                case SysCall.TimeDate_Get:
                    HopperString dt = new HopperString();
                    dt.Value = DateTime.Now.ToString("yyyy-MM-dd");
                    Push(dt);
                    hasResult = true;
                    break;



                case SysCall.LongNew:
                    Push(0, HopperType.tLong);
                    hasResult = true;
                    break;
                case SysCall.LongNewFromConstant:
                    {
                        ushort location = (ushort)Pop();
                        location = (ushort)(location + currentContext.ConstantsStart);
                        Int32 l = BitConverter.ToInt32(currentContext.Code, location);
                        PushLong(l);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongAdd:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushLong(next + top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongSub:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushLong(next - top);
                        hasResult = true;
                    }
                    break;

                case SysCall.LongAddB:
                    {
                        Int32 top = (Int32)Pop();
                        Int32 next = PopLong();
                        PushLong(next + top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongSubB:
                    {
                        Int32 top = (Int32)Pop();
                        Int32 next = PopLong();
                        PushLong(next - top);
                        hasResult = true;
                    }
                    break;

                case SysCall.LongMul:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushLong(next * top);
                        hasResult = true;
                    }
                    break;
                /*
                case SysCall.LongInc:
                    {
#if DEBUG
                        Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
                        ushort sp2 = ((ushort)(sp - 1));
#if DEBUG
                        Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);
                        Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tLong, "tLong expected", this);
#endif
                        Int32 lvalue = BitConverter.ToInt32(BitConverter.GetBytes(stack[sp2].value), 0); ;
                        lvalue++;
                        stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(lvalue), 0);
                    }
                    break;
                case SysCall.LongAddRef:
                    {
                        Int32 top = PopLong();
#if DEBUG
                        Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
                        ushort sp2 = ((ushort)(sp - 1));
#if DEBUG
                        Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);
                        Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tLong, "tLong expected", this);
#endif
                        Int32 lvalue = BitConverter.ToInt32(BitConverter.GetBytes(stack[sp2].value), 0); ;
                        lvalue = lvalue + top;
                        stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(lvalue), 0);
                    }
                    break;
                case SysCall.LongMulRef:
                    {
                        Int32 top = PopLong();
#if DEBUG
                        Diagnostics.ASSERT(sp >= 1, "stack underflow");
#endif
                        ushort sp2 = ((ushort)(sp - 1));
#if DEBUG
                        Diagnostics.ASSERTDIE(stack[sp2].reference == null, "value type", this);
                        Diagnostics.ASSERTDIE(stack[sp2].type == HopperType.tLong, "tLong expected", this);
#endif
                        Int32 lvalue = BitConverter.ToInt32(BitConverter.GetBytes(stack[sp2].value), 0); ;
                        lvalue = lvalue * top;
                        stack[sp2].value = BitConverter.ToUInt32(BitConverter.GetBytes(lvalue), 0);
                    }
                    break;
                */

                case SysCall.LongDiv:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        if (top == 0)
                        {
                            Diagnostics.Die(0x04, this);
                            break;
                        }
                        PushLong(next / top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongMod:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        if (top == 0)
                        {
                            Diagnostics.Die(0x04, this);
                            break;
                        }
                        PushLong(next % top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongEQ:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushBool(next == top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongLT:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushBool(next < top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongLE:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushBool(next <= top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongGT:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushBool(next > top);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongGE:
                    {
                        Int32 top = PopLong();
                        Int32 next = PopLong();
                        PushBool(next >= top);
                        hasResult = true;
                    }
                    break;

                case SysCall.LongToFloat:
                    {
                        Int32 top = PopLong();
                        float f = (float)(top * 1.0);
                        PushFloat(f);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongToUInt:
                    {
                        Int32 top = PopLong();
                        if ((top > 65535) || (top < 0))
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        Push((ushort)top, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;
                case SysCall.UIntToLong:
                    {
                        ushort top = (ushort)Pop();
                        PushLong((Int32)top);
                        hasResult = true;
                    }
                    break;
                case SysCall.IntToLong:
                    {
                        short top = PopInt();
                        PushLong((Int32)top);
                        hasResult = true;
                    }
                    break;
                case SysCall.IntToFloat:
                    {
                        short top = PopInt();
                        PushFloat((float)top);
                        hasResult = true;
                    }
                    break;
                case SysCall.UIntToFloat:
                    {
                        ushort top = (ushort)Pop();
                        PushFloat((float)top);
                        hasResult = true;
                    }
                    break;

                case SysCall.LongToInt:
                    {
                        Int32 top = PopLong();
                        if ((top > 32767) || (top < -32768))
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        short i = (short)top;
                        PushInt(i);
                        hasResult = true;
                    }
                    break;
                case SysCall.UIntToInt:
                    {
                        uint top = Pop();
                        if (top > 32767)
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        short i = (short)top;
                        PushInt(i);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongToString:
                    {
                        Int32 top = PopLong();
                        Push(new HopperString(top.ToString()));
                        hasResult = true;
                    }
                    break;
                case SysCall.LongNegate:
                    {
                        Int32 top = PopLong();
                        PushLong(-top);
                        hasResult = true;
                    }
                    break;
                
                case SysCall.LongGetByte:
                    {
                        uint index = Pop();
                        Int32 top = PopLong();
                        byte[] bytes = BitConverter.GetBytes(top);
                        if (index >= bytes.Length)
                        {
                            Diagnostics.Die(0x02, this); // array index out of range
                            break;
                        }
                        Push(bytes[index], HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.LongFromBytes:
                    {
                        byte[] bytes = new byte[4];
                        bytes[3] = (byte)Pop();
                        bytes[2] = (byte)Pop();
                        bytes[1] = (byte)Pop();
                        bytes[0] = (byte)Pop();

                        Int32 l = BitConverter.ToInt32(bytes, 0);
                        PushLong(l);
                        hasResult = true;
                        break;
                    }
                case SysCall.FloatGetByte:
                    {
                        uint index = Pop();
                        float top = PopFloat();
                        byte[] bytes = BitConverter.GetBytes(top);
                        if (index >= bytes.Length)
                        {
                            Diagnostics.Die(0x02, this); // array index out of range
                            break;
                        }
                        Push(bytes[index], HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatFromBytes:
                    {
                        byte[] bytes = new byte[4];
                        bytes[3] = (byte)Pop();
                        bytes[2] = (byte)Pop();
                        bytes[1] = (byte)Pop();
                        bytes[0] = (byte)Pop();

                        float f = BitConverter.ToSingle(bytes, 0);
                        PushFloat(f);
                        hasResult = true;
                        break;
                    }
                case SysCall.IntGetByte:
                    {
                        uint index = Pop();
                        Int16 top = PopInt();
                        byte[] bytes = BitConverter.GetBytes(top);
                        if (index >= bytes.Length)
                        {
                            Diagnostics.Die(0x02, this); // array index out of range
                            break;
                        }
                        Push(bytes[index], HopperType.tByte);
                        hasResult = true;
                    }
                    break;

                case SysCall.IntFromBytes:
                    {
                        byte[] bytes = new byte[2];
                        bytes[1] = (byte)Pop();
                        bytes[0] = (byte)Pop();

                        Int16 i = BitConverter.ToInt16(bytes, 0);
                        PushInt(i);
                        hasResult = true;
                        break;
                    }
                case SysCall.FloatNew:
                    Push(0, HopperType.tFloat);
                    hasResult = true;
                    break;
                case SysCall.FloatNewFromConstant:
                    {
                        ushort location = (ushort)Pop();
                        location = (ushort)(location + currentContext.ConstantsStart);
                        float f = BitConverter.ToSingle(currentContext.Code, location);
                        PushFloat(f);
                        hasResult = true;
                    }
                    break;

                case SysCall.FloatSin:
                    {
                        float top = PopFloat();
                        PushFloat((float)Math.Sin(top));
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatCos:
                    {
                        float top = PopFloat();
                        PushFloat((float)Math.Cos(top));
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatATan2:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushFloat((float)Math.Atan2(next, top));
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatSqrt:
                    {
                        float top = PopFloat();
                        PushFloat((float)Math.Sqrt(top));
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatAdd:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushFloat(next + top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatSub:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushFloat(next - top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatMul:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushFloat(next * top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatDiv:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        if (top == 0)
                        {
                            Diagnostics.Die(0x04, this);
                            break;
                        }
                        PushFloat(next / top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatEQ:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushBool(next == top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatLT:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushBool(next < top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatLE:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushBool(next <= top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatGT:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushBool(next > top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatGE:
                    {
                        float top = PopFloat();
                        float next = PopFloat();
                        PushBool(next >= top);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatToString:
                    {
                        float top = PopFloat();
                        Push(new HopperString(top.ToString()));
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatToUInt:
                    {
                        float top = PopFloat();
                        if ((top > 65535.0) || (top < 0.0))
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        Push((ushort)top, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;
                case SysCall.FloatToLong:
                    {
                        float top = PopFloat();
                        if ((top > 2147483647) || (top < -2147483648))
                        {
                            Diagnostics.Die(0x0D, this); // numeric type out of range / overflow
                            break;
                        }
                        PushLong((Int32)top);
                        hasResult = true;
                    }
                    break;

                case SysCall.TypesTypeOf:
                    {
                        HopperType type = GetStackType((ushort)(sp - 1));
                        if (Type_IsValueType(type))
                        {
                            Pop();
                        }
                        else
                        {
                            PopVariant(HopperType.tUndefined);
                        }
                        Push((ushort)type, HopperType.tType);
                        hasResult = true;
                    }
                    break;
                case SysCall.TypesKeyTypeOf:
                    {
                        Variant variant = PopVariant(HopperType.tUndefined);
                        switch (variant.Type)
                        {
                            case HopperType.tDictionary:
                                if (variant as HopperUIntDictionary != null)
                                {
                                    HopperUIntDictionary dictionary = (HopperUIntDictionary)variant;
                                    Push((ushort)HopperType.tUInt, HopperType.tType);
                                    hasResult = true;
                                }
                                else if (variant as HopperStringDictionary != null)
                                {
                                    HopperStringDictionary dictionary = (HopperStringDictionary)variant;
                                    Push((ushort)HopperType.tString, HopperType.tType);
                                    hasResult = true;
                                }
                                else
                                {
                                    throw new InvalidOperationException();
                                }
                                break;
                            case HopperType.tPair:
                                {
                                    HopperPair pair = (HopperPair)variant;
                                    Push((ushort)pair.KType, HopperType.tType);
                                    hasResult = true;
                                }
                                break;
                            default:
                                throw new InvalidOperationException();
                        }
                    }
                    break;
                case SysCall.TypesValueTypeOf:
                    {
                        Variant variant = PopVariant(HopperType.tUndefined);
                        switch (variant.Type)
                        {
                            case HopperType.tDictionary:
                                if (variant as HopperUIntDictionary != null)
                                {
                                    HopperUIntDictionary dictionary = (HopperUIntDictionary)variant;
                                    Push((ushort)dictionary.VType, HopperType.tType);
                                    hasResult = true;
                                }
                                else if (variant as HopperStringDictionary != null)
                                {
                                    HopperStringDictionary dictionary = (HopperStringDictionary)variant;
                                    Push((ushort)dictionary.VType, HopperType.tType);
                                    hasResult = true;
                                }
                                else
                                {
                                    throw new InvalidOperationException();
                                }
                                break;
                            case HopperType.tPair:
                                {
                                    HopperPair pair = (HopperPair)variant;
                                    Push((ushort)pair.VType, HopperType.tType);
                                    hasResult = true;
                                }
                                break;
                            case HopperType.tList:
                                {
                                    HopperList list = (HopperList)variant;
                                    Push((ushort)list.VType, HopperType.tType);
                                    hasResult = true;
                                }
                                break;
                            case HopperType.tArray:
                                {
                                    HopperArray array = (HopperArray)variant;
                                    Push((ushort)array.VType, HopperType.tType);
                                    hasResult = true;
                                }
                                break;
                            default:
                                if (Type_IsValueType(variant.Type))
                                {
                                    // box variant
                                    Push((ushort)variant.Type, HopperType.tType);
                                    hasResult = true;
                                }
                                else
                                {
                                    throw new InvalidOperationException();
                                }
                                break;
                        }
                    }
                    break;
                case SysCall.TypesBoxTypeOf:
                    {
                        HopperType type = GetStackType((ushort)(sp - 1));
                        if (Type_IsValueType(type))
                        {
                            Pop();
                        }
                        else
                        {
                            Variant variant = PopVariant(HopperType.tUndefined);
                            if (type == HopperType.tVariant)
                            {
                                HopperValue value = variant as HopperValue;
                                type = value.Type;
                            }
                        }
                        Push((ushort)type, HopperType.tType);
                        hasResult = true;
                    }
                    break;
                case SysCall.TypesVerifyValueTypes:
                    {
                        HopperType memberType = (HopperType)Pop();
                        Variant variant = PopVariant(HopperType.tUndefined);
                        bool success = true;
                        switch (variant.Type)
                        {
                            case HopperType.tList:
                                {
                                    // verify that all members of the list are of type valueType
                                    HopperList list = (HopperList)variant;
                                    ushort length = (ushort)list.Value.Count;
                                    for (ushort i = 0; i < length; i++)
                                    {
                                        Variant item = list.Value[i];
                                        if (item.Type != memberType)
                                        {
                                            success = false;
                                            break;
                                        }
                                    }
                                }
                                break;
                            case HopperType.tDictionary:
                                {
                                    // verify that all members of the dictionary are of type valueType
                                    if (variant as HopperUIntDictionary != null)
                                    {
                                        HopperUIntDictionary dictionary = variant as HopperUIntDictionary;
                                        foreach (KeyValuePair<ushort, Variant> kv in dictionary.Value)
                                        {
                                            if (kv.Value.Type != memberType)
                                            {
                                                success = false;
                                                break;
                                            }
                                        }
                                    }
                                    else
                                    {
                                        HopperStringDictionary dictionary = variant as HopperStringDictionary;
                                        foreach (KeyValuePair<string, Variant> kv in dictionary.Value)
                                        {
                                            if (kv.Value.Type != memberType)
                                            {
                                                success = false;
                                                break;
                                            }
                                        }
                                    }
                                }
                                break;
                            default:
#if DEBUG
                                Diagnostics.ASSERT(false, "unexpected overload");
#endif
                                break;
                        }
                        PushBool(success);
                        hasResult = true;
                    }
                    break;

                case SysCall.VariantBox:
                    {
                        HopperType type = (HopperType)Pop();
                        uint value = Pop();
                        Variant boxed = new HopperValue(value, type);
                        Push(boxed);
                        hasResult = true;
                    }
                    break;
                case SysCall.VariantUnBox:
                    {
                        HopperValue value = (HopperValue)PopVariant(HopperType.tVariant);
                        Push(value.Value, value.Type);
                        hasResult = true;
                    }
                    break;
                case SysCall.DiagnosticsSetError:
                    currentContext.SetError = (ushort)Pop();
                    break;



                case SysCall.DiagnosticsOutputDebug:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                HopperString str = (HopperString)PopVariant(HopperType.tString);
                                Diagnostics.OutputDebug("\n" + str.Value);
                                break;
                            }
                        case 2:
                            {
                                // <string>
                                HopperList list = (HopperList)PopVariant(HopperType.tList);
                                int length = list.Value.Count;
                                Diagnostics.OutputDebug("\n<");
                                for (int i = 0; i < length; i++)
                                {
                                    if (i > 0)
                                    {
                                        Diagnostics.OutputDebug(",");
                                    }
                                    HopperString str = list.Value[i] as HopperString;
                                    Diagnostics.OutputDebug(str.Value);
                                }
                                Diagnostics.OutputDebug(">\n");
                                break;
                            }
                        case 3:
                            {
                                // < <string> >
                                HopperList list = (HopperList)PopVariant(HopperType.tList);
                                int length = list.Value.Count;
                                Diagnostics.OutputDebug("\n<");
                                for (int i = 0; i < length; i++)
                                {
                                    Diagnostics.OutputDebug("\n  <");
                                    HopperList sublist = list.Value[i] as HopperList;
                                    int sublength = sublist.Value.Count;
                                    for (int j = 0; j < sublength; j++)
                                    {
                                        if (j > 0)
                                        {
                                            Diagnostics.OutputDebug(",");
                                        }
                                        HopperString str = sublist.Value[j] as HopperString;
                                        Diagnostics.OutputDebug(str.Value);
                                    }
                                    Diagnostics.OutputDebug(">\n");
                                }
                                Diagnostics.OutputDebug(">\n");
                                break;
                            }
                        case 4:
                            {
                                // < <uint> >
                                HopperList list = (HopperList)PopVariant(HopperType.tList);
                                int length = list.Value.Count;
                                Diagnostics.OutputDebug("\n<");
                                for (int i = 0; i < length; i++)
                                {
                                    Diagnostics.OutputDebug("\n  <");
                                    HopperList sublist = list.Value[i] as HopperList;
                                    int sublength = sublist.Value.Count;
                                    for (int j = 0; j < sublength; j++)
                                    {
                                        if (j > 0)
                                        {
                                            Diagnostics.OutputDebug(",");
                                        }
                                        HopperValue ui = sublist.Value[j] as HopperValue;
                                        Diagnostics.OutputDebug(ui.Value.ToString());
                                    }
                                    Diagnostics.OutputDebug(">\n");
                                }
                                Diagnostics.OutputDebug(">\n");
                                break;
                            }
                        case 5:
                            {
                                // <uint>
                                HopperList list = (HopperList)PopVariant(HopperType.tList);
                                int length = list.Value.Count;
                                Diagnostics.OutputDebug("\n<");
                                for (int i = 0; i < length; i++)
                                {
                                    if (i > 0)
                                    {
                                        Diagnostics.OutputDebug(",");
                                    }
                                    HopperValue ui = list.Value[i] as HopperValue;
                                    Diagnostics.OutputDebug(ui.Value.ToString());
                                }
                                Diagnostics.OutputDebug(">\n");
                                break;
                            }
                        default:
#if DEBUG
                            Diagnostics.ASSERT(false, "unexpected overload");
#endif
                            break;
                    }
                    break;

                case SysCall.DiagnosticsDie:
                    Diagnostics.Die((int)Pop(), this);
                    break;


                case SysCall.SerialConnect:
                    switch (iOverload)
                    {
                        case 0:
                            {
                                Serial.Connect();
                                break;
                            }
                        case 1:
                            {
                                uint port = Pop();
                                Serial.Connect(port);
                                break;
                            }
                    }
                    break;
                case SysCall.SerialClose:
                    Serial.Close();
                    break;
                case SysCall.SerialIsValid:
                    PushBool(Serial.IsValid());
                    hasResult = true;
                    break;

                case SysCall.SerialIsAvailableGet:
                    PushBool(Serial.IsAvailableGet());
                    hasResult = true;
                    break;

                case SysCall.SerialReadChar:
                    Push(Serial.ReadChar(), HopperType.tChar);
                    hasResult = true;
                    break;

                case SysCall.SerialWriteChar:
                    Serial.WriteChar((char)Pop());
                    break;
                case SysCall.SerialWriteString:
                    {
                        HopperString str = (HopperString)PopVariant(HopperType.tString);
                        Serial.WriteString(str);
                    }
                    break;
                case SysCall.SerialPortsGet:
                    {
                        HopperList portList = new HopperList(HopperType.tString);
                        List<string> ports = Serial.GetPorts();
                        foreach (string arg in ports)
                        {
                            portList.Value.Add(new HopperString(arg));
                        }
                        Push(portList);
                        hasResult = true;
                    }
                    break;

                // emulation APIs for small devices
                case SysCall.MemoryReadBit:
                    {
                        uint index = Pop();
                        uint address = Pop();
                        address = address + (index >> 3);
                        byte mask = (byte)(1 << (byte)(index & 0x07));
                        byte value = (byte)(currentContext.memoryArray[address] & mask);
                        Push((byte)((value != 0) ? 1 : 0), HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.MemoryWriteBit:
                    {
                        uint data = Pop();
                        uint index = Pop();
                        uint address = Pop();
                        address = address + (index >> 3);
                        byte mask = (byte)(1 << (byte)(index & 0x07));
                        if (data == 0)
                        {
                            currentContext.memoryArray[address] &= (byte)(~mask);
                        }
                        else
                        {
                            currentContext.memoryArray[address] |= mask;
                        }
                    }
                    break;

                case SysCall.MemoryReadByte:
                    {
                        uint address = Pop();
                        Push(currentContext.memoryArray[address], HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.MemoryReadWord:
                    {
                        uint address = Pop();
                        Push((ushort)(currentContext.memoryArray[address] + (currentContext.memoryArray[address + 1] << 8)), HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.MemoryWriteByte:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryArray[address] = (byte)data;
                    }
                    break;
                case SysCall.MemoryWriteWord:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryArray[address] = (byte)(data & 0xFF);
                        currentContext.memoryArray[address + 1] = (byte)((data >> 8) & 0xFF);
                    }
                    break;
                case SysCall.MemoryAvailable:
                    {
                        Push(0xFFFF, HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.MemoryReadCodeByte:
                    {
                        uint address = Pop();
                        Push(currentContext.memoryCodeArray[address], HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.MemoryReadCodeWord:
                    {
                        uint address = Pop();
                        Push((ushort)(currentContext.memoryCodeArray[address] + (currentContext.memoryCodeArray[address + 1] << 8)), HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.MemoryWriteCodeByte:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryCodeArray[address] = (byte)data;
                    }
                    break;
                case SysCall.MemoryWriteCodeWord:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryCodeArray[address] = (byte)(data & 0xFF);
                        currentContext.memoryCodeArray[address + 1] = (byte)((data >> 8) & 0xFF);
                    }
                    break;

                case SysCall.MemoryReadProgramByte:
                    {
                        uint address = Pop();
                        Push(currentContext.memoryCodeArray[address + currentContext.ProgramOffset], HopperType.tByte);
                        hasResult = true;
                    }
                    break;
                case SysCall.MemoryReadProgramWord:
                    {
                        uint address = Pop();
                        Push((ushort)(currentContext.memoryCodeArray[address + currentContext.ProgramOffset] + (currentContext.memoryCodeArray[address + 1 + currentContext.ProgramOffset] << 8)), HopperType.tUInt);
                        hasResult = true;
                    }
                    break;

                case SysCall.MemoryWriteProgramByte:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryCodeArray[address + currentContext.ProgramOffset] = (byte)data;
                    }
                    break;
                case SysCall.MemoryWriteProgramWord:
                    {
                        uint data = Pop();
                        uint address = Pop();
                        currentContext.memoryCodeArray[address + currentContext.ProgramOffset] = (byte)(data & 0xFF);
                        currentContext.memoryCodeArray[address + 1 + currentContext.ProgramOffset] = (byte)((data >> 8) & 0xFF);
                    }
                    break;
                case SysCall.MemoryProgramOffsetSet:
                    {
                        uint programOffset = Pop();
                        currentContext.ProgramOffset = (ushort)programOffset;
                    }
                    break;


                case SysCall.ClipboardHasTextGet:
                    {
                        uint hasText = (uint)((hopper.HasClipboardText()) ? 1 : 0);
                        currentContext.RemainingClipboardText = hopper.GetClipboardText();
                        Push(hasText, HopperType.tBool);
                        hasResult = true;
                    }
                    break;
                case SysCall.ClipboardGetText:
                    {
                        string clipboardText = hopper.GetClipboardText();
                        HopperString ct = new HopperString(clipboardText);
                        Push(ct);
                        hasResult = true;
                    }
                    break;
                case SysCall.ClipboardGetChar:
                    {
                        char ch = (char)(0);
                        if (currentContext.RemainingClipboardText.Length > 0)
                        {
                            ch = currentContext.RemainingClipboardText[0];
                            currentContext.RemainingClipboardText = currentContext.RemainingClipboardText.Substring(1);
                        }
                        Push(ch, HopperType.tChar);
                        hasResult = true;
                    }
                    break;
                case SysCall.ClipboardSetText:
                    {
                        HopperString ct = (HopperString)PopVariant(HopperType.tString);
                        hopper.SetClipboardText(ct.Value);
                    }
                    break;

                default:
                    Diagnostics.Die(0x0A, this); // not implemented
                    break;
            }
#if PROFILE
            KeepSysCallLog(currentContext, sysCall);
#endif
            // CDECL TODO : Dictionary.Next is a problem : multiple return values
            if (isCDecl)
            {
                if (hasResult)
                {
                    // put the return value where caller expects to find it

                    // POPR0
                    sp--;
                    ushort sp2 = sp;
                    r0.value = stack[sp].value;
                    r0.reference = stack[sp].reference;
                    r0.type = stack[sp].type;
                }

                // replace arguments for caller to pop
                while (spBefore != sp)
                {
                    Push(0, HopperType.tUInt);
                }
            }
        }


        internal void SetError(ushort lastError)
        {
            this.lastError = lastError;
        }
    }
}
