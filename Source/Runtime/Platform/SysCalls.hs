unit SysCalls
{
    enum SysCalls
    {
        StringNewFromConstant = 0x00,
        StringBuild           = 0x01,
        StringNew             = 0x02,
        StringBuildFront      = 0x03,
        ArrayNewFromConstant  = 0x04,
        TimeSeconds           = 0x05,
        StringLengthGet       = 0x06,
        TimeDelay             = 0x07,
        DiagnosticsDie        = 0x08,
        SerialConnect         = 0x09,
        StringGetChar         = 0x0A,
        
        ArrayNew              = 0x0B,
        ArrayCountGet         = 0x0C,
        ArrayGetItem          = 0x0D,
        ArraySetItem          = 0x0E,
        
        SerialReadChar        = 0x0F,
        SerialWriteChar       = 0x10,
        SerialIsAvailableGet  = 0x11,
        
        MemoryReadByte        = 0x12,
        MemoryWriteByte       = 0x13,
        MemoryAvailable       = 0x14,
        MemoryMaximum         = 0x15,
        MemoryAllocate        = 0x16,
        MemoryFree            = 0x17,
        
        ByteToHex             = 0x18,
        IntGetByte            = 0x19,
        IntFromBytes          = 0x1A,
        
        ArraySlice            = 0x1B,
        ArrayItemTypeGet      = 0x1C,
        
        LongNew               = 0x1D,
        LongNewFromConstant   = 0x1E,
        LongFromBytes         = 0x1F,
        LongGetByte           = 0x20,
        FloatNew              = 0x21,
        FloatNewFromConstant  = 0x22,
        FloatFromBytes        = 0x23,
        FloatGetByte          = 0x24,
        
        TimeMillis            = 0x25,
        
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
        //LongToBytes          = 0x39,
        LongToFloat          = 0x3A,
        LongToInt            = 0x3B,
        LongToUInt           = 0x3C,
        //LongNew              = 0x3D,
        //LongNewFromConstant  = 0x3E,
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
        //FloatToBytes         = 0x4B,
        //FloatNew             = 0x4C,
        //FloatNewFromConstant = 0x4D,
        FloatAdd             = 0x4E,
        FloatSub             = 0x4F,
        FloatDiv             = 0x50,
        FloatMul             = 0x51,
        FloatEQ              = 0x52,
        FloatLT              = 0x53,
        FloatLE              = 0x54,
        FloatGT              = 0x55,
        FloatGE              = 0x56,
        
        //TimeMillis       = 0x57,
        
        SystemArgumentsGet = 0x59,
        SystemCurrentDirectoryGet = 0x5A,
        SystemCurrentDirectorySet = 0x5B,
        
        PairNew      = 0x5D,
        StringAppend = 0x5E,
        
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
        
        KeyboardReadKey = 0x72,
        KeyboardIsAvailableGet = 0x73,
        KeyboardToKey = 0x74,
        KeyboardClickXGet = 0x75,
        KeyboardClickYGet = 0x76,
        KeyboardClickUpGet = 0x77,
        KeyboardClickDoubleGet = 0x78,
        KeyboardScrollDeltaGet = 0x79,
        
        DiagnosticsSetError = 0x7D,
        
        TypesTypeOf      = 0x7E,
        TypesValueTypeOf = 0x7F,
        TypesKeyTypeOf   = 0x80,
        TypesBoxTypeOf   = 0x81,
        TypesVerifyValueTypes = 0x82,
        
        WiFiConnect    = 0x84,
        WiFiIPGet      = 0x85,
        WiFiStatusGet  = 0x86,
        WiFiDisconnect = 0x87,
        
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
        
        FileGetDate = 0x9D,
        DirectoryGetDate = 0x9E,
        
        SerialWriteString = 0xA0,
        
        PairKey           = 0xA1,
        
        DictionaryNew = 0xA2,
        
        StringInsertChar = 0xA5,
        PairValue        = 0xA6,
        CharToString     = 0xA7,
        
        DictionaryCountGet = 0xA9,
        DictionarySet = 0xAA,
        DictionaryContains = 0xAB,
        DictionaryGet = 0xAC,
        DictionaryNext = 0xAD,
        DictionaryClear = 0xAE,
        
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
        
        FileGetTimeStamp = 0xCC,
        //IntToBytes       = 0xCD,
        
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
        
        //LongGetByte      = 0xE0,
        //FloatGetByte     = 0xE2,
        //LongFromBytes    = 0xE3,
        //FloatFromBytes   = 0xE5,
        UIntToFloat      = 0xE6,
        
        StringCompare   = 0xE8,
        StringEndsWith  = 0xE9,
        StringSubstring = 0xEA,
        StringReplace = 0xEB,
        
        FloatToUInt     = 0xEC,
        FloatToLong     = 0xED,
        LongAddB        = 0xEE,
        LongSubB        = 0xEF,
        
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
        
    }
}
