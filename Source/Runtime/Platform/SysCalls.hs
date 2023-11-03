unit SysCalls
{
    enum SysCall
    {
        StringNewFromConstant = 0x00,
        
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
        ListLengthGet      = 0x10,
        ListAppend         = 0x11,
        ListInsert         = 0x12,
        ListGetItem        = 0x13,
   // ListGetItemAsVariant = 0x14,
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
      //PairSet            = 0x21,
        PairKey            = 0x22,
      //PairKeyType        = 0x23,
        PairValue          = 0x24,
      //PairValueType      = 0x25,
        
        VariantBox         = 0x27,        
        
        ScreenPrint        = 0x29,
        ScreenPrintLn      = 0x2A,
        ScreenClear        = 0x2B,
        
        IntToLong          = 0x35,
        UIntToLong         = 0x36,
        
        LongToBytes        = 0x39,
        
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
        
        TypesTypeOf      = 0x7E,
        
        StringBuild      = 0x83,
        
        SerialWriteChar  = 0xA7,
        
        StringBuildFront = 0xB5,
        
        CharToDigit      = 0xBD,
        
        TimeDelay        = 0xC6,
        
        IntToBytes       = 0xCD,
        
        StringTrim       = 0xD0,
        StringTrimLeft   = 0xD1,
        StringTrimRight  = 0xD2,
        
        MCUPinMode       = 0xD9,
        MCUDigitalRead   = 0xDA,
        MCUDigitalWrite  = 0xDB,
    }
}
