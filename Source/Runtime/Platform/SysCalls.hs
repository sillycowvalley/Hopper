unit SysCalls
{
    enum SysCalls
    {
        StringNewFromConstant = 0x00,
        CharToString       = 0x01,
        StringNew          = 0x02,
        StringAppend       = 0x03,
        StringInsertChar   = 0x04,
        StringCompare      = 0x05,
        StringLengthGet    = 0x06,
        StringEndsWith     = 0x07,
        StringSubstring    = 0x08,
        StringReplace      = 0x09,
        StringGetChar      = 0x0A,
        ArrayNew           = 0x0B,
        ArrayCountGet      = 0x0C,
        ArrayGetItem       = 0x0D,
        ArraySetItem       = 0x0E,
        ListNew            = 0x0F,
        ListCountGet       = 0x10,
        ListAppend         = 0x11,
        ListInsert         = 0x12,
        ListGetItem        = 0x13,
      ListGetItemAsVariant = 0x14,
        ListSetItem        = 0x15,
        ListClear          = 0x16,
        ListRemove         = 0x17,
        ListContains       = 0x18,
        DictionaryNew      = 0x19,
        DictionaryCountGet = 0x1A,
        DictionarySet      = 0x1B,
        DictionaryContains = 0x1C,
        DictionaryGet      = 0x1D,
        DictionaryNext     = 0x1E,
        DictionaryClear    = 0x1F,
        PairNew            = 0x20,
      //PairSet            = 0x21, // unused
        PairKey            = 0x22,
      //PairKeyType        = 0x23, // unused
        PairValue          = 0x24,
      //PairValueType      = 0x25, // unused
      //VariantType        = 0x26, // unused
        VariantBox         = 0x27,  
        VariantUnBox       = 0x28,      
        ScreenPrint        = 0x29,
        ScreenPrintLn      = 0x2A,
        ScreenClear        = 0x2B,
        ScreenSetCursor    = 0x2C,
        ScreenColumnsGet   = 0x2D,
        ScreenRowsGet      = 0x2E,
        ScreenCursorXGet   = 0x2F,
        ScreenCursorYGet   = 0x30,
        ScreenSuspend      = 0x31,
        ScreenResume       = 0x32,
        ScreenDrawChar     = 0x33,
        
        IntToFloat           = 0x34,
        IntToLong            = 0x35,
        UIntToLong           = 0x36,
        UIntToInt            = 0x37,
        LongToString         = 0x38,
        LongToBytes          = 0x39,
        LongToFloat          = 0x3A,
        LongToInt            = 0x3B,
        LongToUInt           = 0x3C,
        LongNew              = 0x3D,
        LongNewFromConstant  = 0x3E,
        LongAdd              = 0x3F,
        LongSub              = 0x40,
        LongDiv              = 0x41,
        LongMul              = 0x42,
        LongMod              = 0x43,
        LongEQ               = 0x44,
        LongLT               = 0x45,
        LongLE               = 0x46,
        LongGT               = 0x47,
        LongGE               = 0x48,
        LongNegate           = 0x49,
        
        FloatToString        = 0x4A,
        FloatToBytes         = 0x4B,
        FloatNew             = 0x4C,
        FloatNewFromConstant = 0x4D,
        FloatAdd             = 0x4E,
        FloatSub             = 0x4F,
        FloatDiv             = 0x50,
        FloatMul             = 0x51,
        FloatEQ              = 0x52,
        FloatLT              = 0x53,
        FloatLE              = 0x54,
        FloatGT              = 0x55,
        FloatGE              = 0x56,
        
        TimeMillis       = 0x57,
        
        SystemArgumentsGet = 0x59,
        SystemCurrentDirectoryGet = 0x5A,
        SystemCurrentDirectorySet = 0x5B,
        
        FileExists   = 0x5F,
        FileNew      = 0x60,
        FileOpen     = 0x61,
        FileCreate   = 0x62,
        FileReadLine = 0x63,
        FileRead     = 0x64,
        FileIsValid  = 0x65,
        FileAppend   = 0x66,
        FileFlush    = 0x67,
        FileDelete   = 0x68,
        FileGetSize  = 0x69,
        
        DirectoryExists            = 0x6A,
        DirectoryNew               = 0x6B,
        DirectoryIsValid           = 0x6C,
        DirectoryOpen              = 0x6D,
        DirectoryGetDirectoryCount = 0x6E,
        DirectoryGetFileCount      = 0x6F,
        DirectoryGetFile           = 0x70,
        DirectoryGetDirectory      = 0x71,
        
        DiagnosticsDie = 0x7C,
        
        TypesTypeOf      = 0x7E,
        TypesValueTypeOf = 0x7F,
        TypesKeyTypeOf   = 0x80,
        TypesBoxTypeOf   = 0x81,
        TypesVerifyValueTypes = 0x82,
        
        StringBuild      = 0x83,
        
        WiFiConnect    = 0x84,
        WiFiIPGet      = 0x85,
        WiFiStatusGet  = 0x86,
        WiFiDisconnect = 0x87,
        
        ArrayNewFromConstant = 0x88,
        
        DirectoryCreate  = 0x89,
        DirectoryDelete  = 0x8A,
        
        RuntimePCGet  = 0x8B,
        RuntimeSPGet  = 0x8C,
        RuntimeBPGet  = 0x8D,
        RuntimeCSPGet = 0x8E,
        
        RuntimeGetStackWord = 0x8F,
        RuntimeGetStackType = 0x90,
        RuntimeGetCallStackWord = 0x91,
        
        RuntimeExecute = 0x92,
        RuntimeInline  = 0x93,
        
        RuntimeUserCodeGet = 0x94,
        
        // Windows only
        //TimeTime_Get = 0x95,
        //TimeDate_Get = 0x96,
        
        RuntimeInDebuggerGet = 0x97,
        RuntimeDateTimeGet = 0x98,
        
        SerialIsAvailableGet = 0xA5,
        SerialReadChar       = 0xA6,
        SerialWriteChar      = 0xA7,
        
        MemoryReadByte = 0xA9,
        MemoryWriteByte = 0xAA,
        MemoryAvailable = 0xAB,
        MemoryMaximum = 0xAC,
        MemoryAllocate = 0xAD,
        MemoryFree = 0xAE,
        
        
        StringBuildFront = 0xB5,
        
        MemoryReadBit = 0xB6,
        MemoryWriteBit = 0xB7,
        
        CharToUpper = 0xB8,
        CharIsUpper = 0xB9,
        CharIsDigit = 0xBA,
        CharIsLetterOrDigit = 0xBB,
        CharIsLower = 0xBC,
        ByteToDigit = 0xBD,
        ByteToHex = 0xBE,
        CharIsHexDigit = 0xBF,
        CharToLower = 0xC0,
        
        TimeDelay        = 0xC6,
        
        IntToBytes       = 0xCD,
        
        FileGetTime = 0xCE,
        DirectoryGetTime = 0xCF,
        
        StringTrim       = 0xD0,
        StringTrimLeft   = 0xD1,
        StringTrimRight  = 0xD2,
        StringPushImmediate = 0xD3,
        
        StringToUpper = 0xD4,
        StringToLower = 0xD5,
        
        MemoryReadWord  = 0xD7,
        MemoryWriteWord = 0xD8,
        
        //MCUPinMode       = 0xD9,
        //MCUDigitalRead   = 0xDA,
        //MCUDigitalWrite  = 0xDB,
        
        LongGetByte      = 0xE0,
        IntGetByte       = 0xE1,
        FloatGetByte     = 0xE2,
        LongFromBytes    = 0xE3,
        IntFromBytes     = 0xE4,
        FloatFromBytes   = 0xE5,
        UIntToFloat      = 0xE6,
        
        FloatToUInt     = 0xEC,
        FloatToLong     = 0xED,
        LongAddB        = 0xEE,
        LongSubB        = 0xEF,
        
    }
}
