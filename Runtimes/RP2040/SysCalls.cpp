#include "Common.h"

typedef Bool (*SysCallMethod)(Byte iOverload);

SysCallMethod syscallJumps[256];

enum SysCall {
    eStringNewFromConstant = 0x0000,
    eStringNew = 0x0002,
    eStringAppend = 0x0003,
    eStringInsertChar = 0x0004,
    eStringCompare = 0x0005,
    eStringLengthGet = 0x0006,
    eStringEndsWith = 0x0007,
    eStringSubstring = 0x0008,
    eStringReplace = 0x0009,
    eStringGetChar = 0x000A,
    eArrayNew = 0x000B,
    eArrayCountGet = 0x000C,
    eArrayGetItem = 0x000D,
    eArraySetItem = 0x000E,
    eListNew = 0x000F,
    eListLengthGet = 0x0010,
    eListAppend = 0x0011,
    eListInsert = 0x0012,
    eListGetItem = 0x0013,
    eListGetItemAsVariant = 0x0014,
    eListSetItem = 0x0015,
    eListClear = 0x0016,
    eListRemove = 0x0017,
    eListContains = 0x0018,
    eDictionaryNew = 0x0019,
    eDictionaryCountGet = 0x001A,
    eDictionarySet = 0x001B,
    eDictionaryContains = 0x001C,
    eDictionaryGet = 0x001D,
    eDictionaryNext = 0x001E,
    eDictionaryClear = 0x001F,
    ePairNew = 0x0020,
    ePairKey = 0x0022,
    ePairValue = 0x0024,
    eVariantBox = 0x0027,
    eScreenPrint = 0x0029,
    eScreenPrintLn = 0x002A,
    eScreenClear = 0x002B,
    eScreenSetCursor = 0x002C,
    eScreenColumnsGet = 0x002D,
    eScreenRowsGet = 0x002E,
    eScreenCursorXGet = 0x002F,
    eScreenCursorYGet = 0x0030,
    eScreenSuspend = 0x0031,
    eScreenResume = 0x0032,
    eScreenDrawChar = 0x0033,
    eIntToFloat = 0x0034,
    eIntToLong = 0x0035,
    eUIntToLong = 0x0036,
    eUIntToInt = 0x0037,
    eLongToBytes = 0x0039,
    eLongToFloat = 0x003A,
    eLongToInt = 0x003B,
    eLongToUInt = 0x003C,
    eLongNew = 0x003D,
    eLongNewFromConstant = 0x003E,
    eLongAdd = 0x003F,
    eLongSub = 0x0040,
    eLongDiv = 0x0041,
    eLongMul = 0x0042,
    eLongMod = 0x0043,
    eLongEQ = 0x0044,
    eLongLT = 0x0045,
    eLongLE = 0x0046,
    eLongGT = 0x0047,
    eLongGE = 0x0048,
    eLongNegate = 0x0049,
    eFloatToString = 0x004A,
    eFloatToBytes = 0x004B,
    eFloatNew = 0x004C,
    eFloatNewFromConstant = 0x004D,
    eFloatAdd = 0x004E,
    eFloatSub = 0x004F,
    eFloatDiv = 0x0050,
    eFloatMul = 0x0051,
    eFloatEQ = 0x0052,
    eFloatLT = 0x0053,
    eFloatLE = 0x0054,
    eFloatGT = 0x0055,
    eFloatGE = 0x0056,
    eTimeMillis = 0x0057,
    eSystemArgumentsGet = 0x0059,
    eSystemCurrentDirectoryGet = 0x005A,
    eSystemCurrentDirectorySet = 0x005B,
    eFileExists = 0x005F,
    eFileNew = 0x0060,
    eFileOpen = 0x0061,
    eFileCreate = 0x0062,
    eFileReadLine = 0x0063,
    eFileRead = 0x0064,
    eFileIsValid = 0x0065,
    eFileAppend = 0x0066,
    eFileFlush = 0x0067,
    eFileDelete = 0x0068,
    eFileGetSize = 0x0069,
    eDirectoryExists = 0x006A,
    eDirectoryNew = 0x006B,
    eDirectoryIsValid = 0x006C,
    eDirectoryOpen = 0x006D,
    eDirectoryGetDirectoryCount = 0x006E,
    eDirectoryGetFileCount = 0x006F,
    eDirectoryGetFile = 0x0070,
    eDirectoryGetDirectory = 0x0071,
    eTypesTypeOf = 0x007E,
    eTypesValueTypeOf = 0x007F,
    eTypesKeyTypeOf = 0x0080,
    eTypesBoxTypeOf = 0x0081,
    eStringBuild = 0x0083,
    eHttpClientGetRequest = 0x008A,
    eRuntimePCGet = 0x008B,
    eRuntimeSPGet = 0x008C,
    eRuntimeBPGet = 0x008D,
    eRuntimeCSPGet = 0x008E,
    eRuntimeGetStackWord = 0x008F,
    eRuntimeGetStackType = 0x0090,
    eRuntimeGetCallStackWord = 0x0091,
    eRuntimeExecute = 0x0092,
    eRuntimeInline = 0x0093,
    eRuntimeUserCodeGet = 0x0094,
    eSerialIsAvailableGet = 0x00A5,
    eSerialReadChar = 0x00A6,
    eSerialWriteChar = 0x00A7,
    eMemoryReadByte = 0x00A9,
    eMemoryWriteByte = 0x00AA,
    eMemoryAvailable = 0x00AB,
    eMemoryMaximum = 0x00AC,
    eMemoryAllocate = 0x00AD,
    eMemoryFree = 0x00AE,
    eStringBuildFront = 0x00B5,
    eMemoryReadBit = 0x00B6,
    eMemoryWriteBit = 0x00B7,
    eCharToDigit = 0x00BD,
    eTimeDelay = 0x00C6,
    eIntToBytes = 0x00CD,
    eFileGetTime = 0x00CE,
    eDirectoryGetTime = 0x00CF,
    eStringTrim = 0x00D0,
    eStringTrimLeft = 0x00D1,
    eStringTrimRight = 0x00D2,
    eStringPushImmediate = 0x00D3,
    eMemoryReadWord = 0x00D7,
    eMemoryWriteWord = 0x00D8,
    eLongGetByte = 0x00E0,
    eIntGetByte = 0x00E1,
    eFloatGetByte = 0x00E2,
    eLongFromBytes = 0x00E3,
    eIntFromBytes = 0x00E4,
    eFloatFromBytes = 0x00E5,
    eUIntToFloat = 0x00E6,
    eDirectoryCreate = 0x00E9,
    eDirectoryDelete = 0x00EA,
    eWiFiConnect = 0x00EB,
    eFloatToUInt = 0x00EC,
    eFloatToLong = 0x00ED,
    eLongAddB = 0x00EE,
    eLongSubB = 0x00EF,
};

bool undefinedSys(Byte iOverload)
{
    printf("\nundefined syscall at 0x%04X", GetPC()-2);
    SetError(0x0A, (18));
    return false;
}

Bool serialWriteChar(Byte iOverload)
{
    Char value = (Char)VMPop();
    if (value == (Char)0x0D)
    {
        value = (Char)0x0A;
    }
    putchar(value);
    return true;
}
Bool serialReadChar(Byte iOverload)
{
    while (!tud_cdc_available())
    {
        // yield?
    }
    Char value = getchar();
    if (value == (Char)0x0A)
    {
        value = (Char)0x0D;
    }
    VMPush(value, Type::eChar);
    return true;
}
bool serialIsAvailableGet(Byte iOverload)
{
    VMPush((tud_cdc_available() ? 1 : 0), Type::eBool);
    return true;
}

Bool timeDelay(Byte iOverload)
{
    UInt ms = VMPop();
    if (ms != 0)
    {
        sleep_ms(ms);
    }
    return true;
}
Bool timeMillis(Byte iOverload)
{
    UInt32 ms = to_ms_since_boot(get_absolute_time());
    VMPush32(ms, Type::eLong);
    //printf("\nMillis: %d", ms);
    return true;
}

Bool stringNew(Byte iOverload)
{
    UInt address = HRString_New();
    VMPush(address, Type::eString);
    return true;
}
Bool stringNewFromConstant(Byte iOverload)
{
    if (iOverload == 0)
    {
        UInt length   = VMPop();
        UInt location = VMPop();
        UInt address = HRString_NewFromConstant0(GetConstantAddress() + location, length);
        VMPush(address, Type::eString);
        return true;
    }
    else if (iOverload == 1)
    {
        UInt doubleChar = VMPop();
        UInt address = HRString_NewFromConstant1(doubleChar);
        VMPush(address, Type::eString);
        
        return true;
    }
    else
    {
        SetError(0x0A, (19));
        return false;
    }
}
Bool stringInsertChar(Byte iOverload)
{
    UInt ch = VMPop();
    UInt index = VMPop();
    UInt _this = VMPop();
    UInt result = HRString_InsertChar(_this, index, Char(ch));
    GC_Release(_this);
    VMPush(result, Type::eString);
    
    return true;
}
Bool stringLengthGet(Byte iOverload)
{
    UInt _this = VMPop();
    UInt length = HRString_GetLength(_this);
    GC_Release(_this);
    VMPush(length, Type::eUInt);
    return true;
}
Bool stringGetChar(Byte iOverload)
{
    UInt index = VMPop();
    UInt _this = VMPop();
    Char ch = HRString_GetChar(_this, index);
    GC_Release(_this);
    VMPush(ch, Type::eChar);
    return true;
}
Bool stringAppend(Byte iOverload)
{
    if (iOverload == 0)
    {
        UInt append = VMPop();
        UInt _this = VMPop();
        UInt result = HRString_Append(_this, append);
        GC_Release(_this);
        GC_Release(append);
        VMPush(result, Type::eString);
        return true;
    }
    else if (iOverload == 1)
    {
        UInt append = VMPop();
        UInt _this = VMPop();
        UInt result = HRString_Append(_this, Char(append));
        GC_Release(_this);
        VMPush(result, Type::eString);
        return true;
    }
    else
    {
        SetError(0x0A, (19));
        return false;
    }
}
Bool floatToString(Byte iOverload)
{
    Float top = VMPopFloat();
    UInt str = HRString_New();
    char buffer[20];
    sprintf(buffer, "%g", top);
    //printf("\nfloatToString: %s", buffer);
    UInt i = 0;
    while (buffer[i])
    {
        HRString_BuildChar_R(str , (Char)buffer[i]);
        i++;
    }
    VMPush(str, Type::eString);
    return true;
}

Bool longNew(Byte iOverload)
{
    VMPush(0, Type::eLong);
    //Type type;
    //UInt32 ln = VMGet32(GetSP()-2, type);
    //printf("\nlongNew: 0x%08X", ln);
    return true;
}
Bool floatNew(Byte iOverload)
{
    VMPush(0, Type::eFloat);
    return true;
}
Bool floatNewFromConstant(Byte iOverload)
{
    UInt location = VMPop() + GetConstantAddress();
    UInt32 value = Memory_ReadCodeWord(location) + (Memory_ReadCodeWord(location + 0x02) << 16);
    VMPush32(value, Type::eFloat);
    //Type type;
    //value = VMGet32(GetSP()-2, type);
    //Float * f = (Float*)&value;
    //printf("\nfloatNewFromConstant: 0x%08X %g", value, *f);
    return true;
}
Bool floatAdd(Byte iOverload)
{
    Float topf  = VMPopFloat();
    Float nextf = VMPopFloat();
    VMPushFloat(nextf + topf);
    return true;
}
Bool floatSub(Byte iOverload)
{
    Float topf  = VMPopFloat();
    Float nextf = VMPopFloat();
    VMPushFloat(nextf - topf);
    return true;
}
Bool floatMul(Byte iOverload)
{
    Float topf  = VMPopFloat();
    Float nextf = VMPopFloat();
    VMPushFloat(nextf * topf);
    return true;
}
Bool floatDiv(Byte iOverload)
{
    Float topf  = VMPopFloat();
    Float nextf = VMPopFloat();
    if (topf == 0.0)
    {
        SetError(0x04, (20));
        return false;
    }
    VMPushFloat(nextf / topf);
    //printf("\nfloatDiv: %g / %g == %g", nextf, topf, nextf / topf);
    return true;
}

Bool longAdd(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next + *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eLong;
    return true;
}
Bool longSub(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next - *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eLong;
    return true;
}
Bool longMul(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next * *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eLong;
    return true;
}
Bool longDiv(Byte iOverload)
{
    Type ttype;
    Type ntype;
    Long top  = (Long)VMPop32(ttype);
    Long next = (Long)VMPop32(ntype);
    if (top == 0)
    {
        SetError(0x04, (21));
        return false;
    }
    VMPush32((UInt32)(next / top), Type::eLong);
    //printf("\nlongDiv: %d / %d == %d", next, top, next / top);
    return true;
}
Bool longMod(Byte iOverload)
{
    Type ttype;
    Type ntype;
    Long top  = (Long)VMPop32(ttype);
    Long next = (Long)VMPop32(ntype);
    if (top == 0)
    {
        SetError(0x04, (22));
        return false;
    }
    VMPush32((UInt32)(next % top), Type::eLong);
    //printf("\nlongMod: %d %% %d == %d", next, top, next % top);
    return true;
}
Bool longEQ(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next == *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool longLT(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next < *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool longLE(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next <= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool longGT(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next > *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool longGE(Byte iOverload)
{
    sp -= 2;
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    Long * next = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next >= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}


Bool longToFloat(Byte iOverload)
{
    Type ttype;
    Long top  = (Long)VMPop32(ttype);
    Float value = top;
    VMPushFloat(value);
    //printf("\nlongToFloat: %d -> %g", top, value);
    return true;
}
Bool uintToLong(Byte iOverload)
{
    // assumptions:
    // - msb of type  is zero
    // - msw of value is zero
    dataMemoryBlock[typeStack  + sp - 2] = Type::eLong;
    return true;
}
Bool longToUInt(Byte iOverload)
{
    Type ttype;
    UInt32 top  = VMPop32(ttype);
    VMPush((top & 0xFFFF), Type::eUInt);
    //printf("\nlongToUInt: 0x%08X -> 0x%04X", top, top & 0xFFFF);
    return true;
}
Bool screenPrintLn(Byte iOverload)
{
    // TODO HRScreen_PrintLn();
    return true;
}
Bool screenPrint(Byte iOverload)
{
    if (iOverload == 0)
    {
        UInt bc = VMPop();
        UInt fc = VMPop();
        UInt ch = VMPop();
        // TODO HRScreen_Print(Char(ch));
    }
    else if (iOverload == 1)
    {
        UInt bc  = VMPop();
        UInt fc  = VMPop();
        UInt str = VMPop();
        UInt length = HRString_GetLength(str);;
        for (UInt i = 0; i < length; i++)
        {
            Char ch = HRString_GetChar(str, i);
            // TODO HRScreen_Print(ch);
        }
        GC_Release(str);
    }
    return true;
}

void SysCalls_PopulateJumpTable()
{
    for (UInt i = 0; i < 256; i++)
    {
        syscallJumps[i] = undefinedSys;
    }
    syscallJumps[SysCall::eSerialIsAvailableGet]  = serialIsAvailableGet;
    syscallJumps[SysCall::eSerialReadChar]        = serialReadChar;
    syscallJumps[SysCall::eSerialWriteChar]       = serialWriteChar;
    
    syscallJumps[SysCall::eTimeDelay]             = timeDelay;
    syscallJumps[SysCall::eTimeMillis]            = timeMillis;
    
    syscallJumps[SysCall::eStringNew]             = stringNew;
    syscallJumps[SysCall::eStringNewFromConstant] = stringNewFromConstant;
    syscallJumps[SysCall::eStringAppend]          = stringAppend;
    syscallJumps[SysCall::eStringInsertChar]      = stringInsertChar;
    syscallJumps[SysCall::eStringLengthGet]       = stringLengthGet;
    syscallJumps[SysCall::eStringGetChar]         = stringGetChar;
    
    syscallJumps[SysCall::eLongNew]               = longNew;
    syscallJumps[SysCall::eLongAdd]               = longAdd;
    syscallJumps[SysCall::eLongSub]               = longSub;
    syscallJumps[SysCall::eLongMul]               = longMul;
    syscallJumps[SysCall::eLongDiv]               = longDiv;
    syscallJumps[SysCall::eLongMod]               = longMod;
    
    syscallJumps[SysCall::eLongAddB]              = longAdd;
    syscallJumps[SysCall::eLongSubB]              = longSub;
    
    syscallJumps[SysCall::eLongEQ]                = longEQ;
    syscallJumps[SysCall::eLongLT]                = longLT;
    syscallJumps[SysCall::eLongLE]                = longLE;
    syscallJumps[SysCall::eLongGT]                = longGT;
    syscallJumps[SysCall::eLongGE]                = longGE;
    
    
    syscallJumps[SysCall::eFloatNew]              = floatNew;
    syscallJumps[SysCall::eFloatNewFromConstant]  = floatNewFromConstant;
    
    syscallJumps[SysCall::eFloatAdd]              = floatAdd;
    syscallJumps[SysCall::eFloatSub]              = floatSub;
    syscallJumps[SysCall::eFloatMul]              = floatMul;
    syscallJumps[SysCall::eFloatDiv]              = floatDiv;
    
    syscallJumps[SysCall::eLongToFloat]           = longToFloat;
    syscallJumps[SysCall::eUIntToLong]            = uintToLong;
    syscallJumps[SysCall::eLongToUInt]            = longToUInt;
    syscallJumps[SysCall::eFloatToString]         = floatToString;
    
    syscallJumps[SysCall::eScreenPrint]           = screenPrint;
    syscallJumps[SysCall::eScreenPrintLn]         = screenPrintLn;
    
    // TODO
}

bool SysCall()
{
    return syscallJumps[codeMemoryBlock[pc++]](VMPop());
}
bool SysCall0()
{
    return syscallJumps[codeMemoryBlock[pc++]](0);
}
bool SysCall1()
{
    return syscallJumps[codeMemoryBlock[pc++]](1);
}


