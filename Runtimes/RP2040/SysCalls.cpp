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
    eLongToString = 0x38,
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
    eDiagnosticsDie = 0x007C,
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
Bool diagnosticsDie(Byte iOverload)
{
    Byte err = (Byte)VMPop();
    SetError(err, (38));
    return false;
}

Bool serialWriteChar(Byte iOverload)
{
    Char value = (Char)VMPop();
    //if (value == (Char)0x0D)
    //{
    //    value = (Char)0x0A;
    //}
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
Bool stringCompare(Byte iOverload)
{
    UInt right = VMPop();
    UInt left = VMPop();
    Int result = HRString_Compare(left, right);
    GC_Release(right);
    GC_Release(left);
    VMPushInt(result);
    return true;
}
Bool stringBuildFront(Byte iOverload)
{
    // BuildFront(ref string build, char insert)
    Char ch      = (Char)VMPop();
    Type htype   = (Type)0;
    UInt address = VMPop(htype);
    UInt str = VMGet(address, htype);
    HRString_BuildFront_R(str, ch);
    VMPut(address, str, Type::eString);
    return true;
}
Bool stringBuild(Byte iOverload)
{
    Type type;
    switch (iOverload)
    {
        case 0x00:
        {
            UInt append = VMPop();
            UInt address = VMPop();
            UInt str = VMGet(address, type);
            HRString_BuildString_R(str, append);
            VMPut(address, str, Type::eString);
            GC_Release(append);
            break;
        }
        case 0x01:
        {
            Char ch = (Char)VMPop();
            UInt address = VMPop();
            UInt str = VMGet(address, type);
            HRString_BuildChar_R(str, ch);
            VMPut(address, str, Type::eString);
            break;
        }
        case 0x02:
        {
            UInt address = VMPop();
            UInt str = VMGet(address, type);
            HRString_BuildClear_R(str);
            VMPut(address, str, Type::eString);
            break;
        }
        default:
        {
            SetError(0x0B, (37));
            break;
        }
    } // switch
    return true;
}

Bool stringSubstring(Byte iOverload)
{
    switch (iOverload)
    {
        case 0:
        {
            UInt start = VMPop();
            UInt _this = VMPop();
            UInt result = HRString_Substring(_this, start);
            GC_Release(_this);
            VMPush(result, Type::eString);
            break;
        }
        case 1:
        {
            UInt limit = VMPop();
            UInt start = VMPop();
            UInt _this = VMPop();
            UInt result = HRString_Substring(_this, start, limit);
            GC_Release(_this);
            VMPush(result, Type::eString);
            break;
        }
        case 2:
        {
            UInt start = VMPop();
            Type htype = (Type)0;
            UInt address = VMPop(htype);
            UInt str = VMGet(address, htype);
            HRString_Substring(str, start);
            VMPut(address, str, Type::eString);
            break;
        }
        default:
        {
            SetError(0x12, (35));
            break;
        }
    } // switch
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
void stripTrailingZeros(char * buffer)
{
    int i = strlen(buffer) - 1;
    for (;;)
    {
        if (i == 0)
        {
            break;
        }
        if (buffer[i] != '0')
        {
            break;
        }
        // buffer[i] == 0
        if ((i >= 1) && (buffer[i-1] != '.'))
        {
            buffer[i] = 0;
        }
        i--;
    }
    if (strcmp(buffer, "0.0") == 0)
    {
        buffer[1] = 0; // "0.0" -> "0"
    }
}
Bool floatToString(Byte iOverload)
{
    Float top = VMPopFloat();
    UInt str = HRString_New();
    char buffer[20];
    sprintf(buffer, "%g", top);
    stripTrailingZeros(buffer);
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
Bool longToString(Byte iOverload)
{
    Long top = VMPopLong();
    UInt str = HRString_New();
    char buffer[20];
    sprintf(buffer, "%ld", top);
    //printf("\nlongToString: %s", buffer);
    UInt i = 0;
    while (buffer[i])
    {
        HRString_BuildChar_R(str , (Char)buffer[i]);
        i++;
    }
    VMPush(str, Type::eString);
    return true;
}

Bool longGetByte(Byte iOverload)
{
    UInt index = VMPop();
    Type type;
    UInt32 top   = VMPop32(type);
    Byte b = (Byte)((top >> (8*index)) & 0xFF);
    VMPush(b, Type::eByte);
    return true;
}
Bool intGetByte(Byte iOverload)
{
    UInt index = VMPop();
    UInt top   = VMPop();
    Byte b = (Byte)((top >> (8*index)) & 0xFF);
    VMPush(b, Type::eByte);
    return true;
}
Bool longFromBytes(Byte iOverload)
{
    Byte b3 = (Byte)VMPop();
    Byte b2 = (Byte)VMPop();
    Byte b1 = (Byte)VMPop();
    Byte b0 = (Byte)VMPop();
    UInt32 l = b0 + (b1 << 8) + (b2 << 16) + (b3 << 24);
    VMPush32(l, Type::eLong);
    return true;
}
Bool floatFromBytes(Byte iOverload)
{
    Byte b3 = (Byte)VMPop();
    Byte b2 = (Byte)VMPop();
    Byte b1 = (Byte)VMPop();
    Byte b0 = (Byte)VMPop();
    UInt32 l = b0 + (b1 << 8) + (b2 << 16) + (b3 << 24);
    VMPush32(l, Type::eFloat);
    return true;
}
Bool intFromBytes(Byte iOverload)
{
    Byte b1 = (Byte)VMPop();
    Byte b0 = (Byte)VMPop();
    UInt ui = b0 + (b1 << 8);
    VMPush(ui, Type::eInt);
    return true;
}
bool longToBytes(Byte iOverload)
{
    Type type;
    UInt32 l = VMPop32(type);
    UInt lst = HRList_New(Type::eByte);
    HRList_Append(lst, (l & 0xFF), Type::eByte);
    HRList_Append(lst, ((l >> 8) & 0xFF), Type::eByte);
    HRList_Append(lst, ((l >> 16) & 0xFF), Type::eByte);
    HRList_Append(lst, (l >> 24), Type::eByte);
    VMPush(lst, Type::eList);
    return true;
}
bool intToBytes(Byte iOverload)
{
    UInt ui = VMPop();
    UInt lst = HRList_New(Type::eByte);;
    HRList_Append(lst, (ui & 0xFF), Type::eByte);
    HRList_Append(lst, (ui >> 8), Type::eByte);
    VMPush(lst, Type::eList);
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
    return true;
}
Bool longNewFromConstant(Byte iOverload)
{
    UInt location = VMPop() + GetConstantAddress();
    UInt32 value = Memory_ReadCodeWord(location) + (Memory_ReadCodeWord(location + 0x02) << 16);
    VMPush32(value, Type::eLong);
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
Bool longNegate(Byte iOverload)
{
    Long * top = (Long*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *top = - *top;
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

Bool floatEQ(Byte iOverload)
{
    sp -= 2;
    Float * top  = (Float*)&dataMemoryBlock[valueStack + (sp << 1)];
    Float * next = (Float*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    //printf("\nfloatEQ: %g == %g -> %c", *next, *top, (*next == *top) ? 't' : 'f');
    *((UInt32*)next) = (*next == *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool floatLT(Byte iOverload)
{
    sp -= 2;
    Float * top  = (Float*)&dataMemoryBlock[valueStack + (sp << 1)];
    Float * next = (Float*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *((UInt32*)next) = (*next < *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool floatLE(Byte iOverload)
{
    sp -= 2;
    Float * top  = (Float*)&dataMemoryBlock[valueStack + (sp << 1)];
    Float * next = (Float*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *((UInt32*)next) = (*next <= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool floatGT(Byte iOverload)
{
    sp -= 2;
    Float * top  = (Float*)&dataMemoryBlock[valueStack + (sp << 1)];
    Float * next = (Float*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *((UInt32*)next) = (*next > *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
Bool floatGE(Byte iOverload)
{
    sp -= 2;
    Float * top  = (Float*)&dataMemoryBlock[valueStack + (sp << 1)];
    Float * next = (Float*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *((UInt32*)next) = (*next >= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}


Bool longToFloat(Byte iOverload)
{
    Type ttype;
    Long top  = (Long)VMPop32(ttype);
    Float value = top;
    VMPushFloat(value);
    //printf("\nlongToFloat: %ld -> %g", top, value);
    return true;
}
Bool uintToFloat(Byte iOverload)
{
    UInt top  = VMPop();
    Float value = top;
    VMPushFloat(value);
    //printf("\nuintToFloat: %d -> %g", top, value);
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
Bool intToLong(Byte iOverload)
{
    Int top  = VMPopInt();
    Long value = top;
    VMPushLong(value);
    return true;
}
Bool intToFloat(Byte iOverload)
{
    Int top  = VMPopInt();
    Float value = top;
    VMPushFloat(value);
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

Bool screenClear(Byte iOverload)
{
    // TODO HRScreen_Clear();
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
        /*UInt bc =*/ VMPop();
        /*UInt fc =*/ VMPop();
        /*UInt ch =*/ VMPop();
        // TODO HRScreen_Print(Char(ch));
    }
    else if (iOverload == 1)
    {
        /*UInt bc  =*/ VMPop();
        /*UInt fc  =*/ VMPop();
        UInt str = VMPop();
        UInt length = HRString_GetLength(str);;
        for (UInt i = 0; i < length; i++)
        {
            /*Char ch =*/ HRString_GetChar(str, i);
            // TODO HRScreen_Print(ch);
        }
        GC_Release(str);
    }
    return true;
}

Bool arrayNew(Byte iOverload)
{
    Type htype = Type(VMPop());
    UInt count = VMPop();
    UInt address = HRArray_New(htype, count);
    VMPush(address, Type::eArray);
    return true;
}
Bool arrayCountGet(Byte iOverload)
{
    UInt _this = VMPop();
    UInt length = HRArray_GetCount(_this);
    GC_Release(_this);
    VMPush(length, Type::eUInt);
    return true;
}
Bool arrayGetItem(Byte iOverload)
{
    UInt index = VMPop();
    UInt _this = VMPop();
    Type etype = (Type)0;
    UInt item = HRArray_GetItem(_this, index, etype);
    GC_Release(_this);
    VMPush(item, etype);
    return true;
}
Bool arraySetItem(Byte iOverload)
{
    UInt item  = VMPop();
    UInt index = VMPop();
    UInt _this = VMPop();
    HRArray_SetItem(_this, index, item);
    GC_Release(_this);
    return true;
}

Bool listNew(Byte iOverload)
{
    Type htype = (Type)VMPop();
    UInt address = HRList_New(htype);
    VMPush(address, Type::eList);
    return true;
}
Bool listLengthGet(Byte iOverload)
{
    UInt _this = VMPop();
    UInt length = HRList_GetLength(_this);
    GC_Release(_this);
    VMPush(length, Type::eUInt);
    return true;
}

Bool listAppend(Byte iOverload)
{
    Type itype = (Type)0;
    UInt32 item = VMPop32(itype);
    UInt _this = VMPop();
    HRList_Append(_this, item, itype);
    if (IsReferenceType(itype))
    {
        GC_Release(item);
    }
    GC_Release(_this);
    return true;
}

Bool listInsert(Byte iOverload)
{
    Type itype  = (Type)0;
    UInt32 item = VMPop(itype);
    UInt index  = VMPop();
    UInt _this  = VMPop();
    HRList_Insert(_this, index, item, itype);
    if (IsReferenceType(itype))
    {
        GC_Release(item);
    }
    GC_Release(_this);
    return true;
}

Bool listGetItem(Byte iOverload)
{
    UInt index = VMPop();
    UInt _this = VMPop();
    Type itype = (Type)0;
    UInt32 item = HRList_GetItem(_this, index, itype);
    GC_Release(_this);
    VMPush32(item, itype);
    return true;
}

Bool listGetItemAsVariant(Byte iOverload)
{
    UInt index  = VMPop();
    UInt _this  = VMPop();
    Type itype  = (Type)0;
    UInt32 item = HRList_GetItem(_this, index, itype);
    if (!IsReferenceType(itype))
    {
        item = HRVariant_CreateValueVariant(item, itype);
        itype = Type::eVariant;
    }
    GC_Release(_this);
    VMPush(item, itype);
    return true;
}

Bool listSetItem(Byte iOverload)
{
    Type itype  = (Type)0;
    UInt32 item = VMPop32(itype);
    UInt index  = VMPop();
    UInt _this  = VMPop();
    HRList_SetItem(_this, index, item, itype);
    if (IsReferenceType(itype))
    {
        GC_Release(item);
    }
    GC_Release(_this);
    return true;
}

Bool listClear(Byte iOverload)
{
    UInt _this = VMPop();
    HRList_Clear(_this);
    GC_Release(_this);
    return true;
}

Bool listRemove(Byte iOverload)
{
    UInt index = VMPop();
    UInt _this = VMPop();
    HRList_Remove(_this, index);
    GC_Release(_this);
    return true;
}

Bool listContains(Byte iOverload)
{
    Type   itype  = (Type)0;
    UInt32 item   = VMPop32(itype);
    UInt   _this  = VMPop();
    Bool contains = HRList_Contains(_this, item, itype);
    if (IsReferenceType(itype))
    {
        GC_Release(item);
    }
    GC_Release(_this);
    VMPush(contains ? 1 : 0, Type::eBool);
    return true;
}

Bool dictionaryNew(Byte iOverload)
{
    Type vtype = (Type)VMPop();
    Type ktype = (Type)VMPop();
    UInt address = HRDictionary_New(ktype, vtype);
    VMPush(address, Type::eDictionary);
    return true;
}
Bool dictionaryClear(Byte iOverload)
{
    UInt _this = VMPop();
    HRDictionary_Clear(_this);
    GC_Release(_this);
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
    syscallJumps[SysCall::eStringSubstring]       = stringSubstring;
    syscallJumps[SysCall::eStringCompare]         = stringCompare;
    syscallJumps[SysCall::eStringBuildFront]      = stringBuildFront;
    syscallJumps[SysCall::eStringBuild]           = stringBuild;
    
    syscallJumps[SysCall::eArrayNew]              = arrayNew;
    syscallJumps[SysCall::eArrayCountGet]         = arrayCountGet;
    syscallJumps[SysCall::eArrayGetItem]          = arrayGetItem;
    syscallJumps[SysCall::eArraySetItem]          = arraySetItem;
    
    syscallJumps[SysCall::eListNew]               = listNew;
    syscallJumps[SysCall::eListLengthGet]         = listLengthGet;
    syscallJumps[SysCall::eListAppend]            = listAppend;
    syscallJumps[SysCall::eListInsert]            = listInsert;
    syscallJumps[SysCall::eListGetItem]           = listGetItem;
    syscallJumps[SysCall::eListGetItemAsVariant]  = listGetItemAsVariant;
    syscallJumps[SysCall::eListSetItem]           = listSetItem;
    syscallJumps[SysCall::eListClear]             = listClear;
    syscallJumps[SysCall::eListRemove]            = listRemove;
    syscallJumps[SysCall::eListContains]          = listContains;
    
    syscallJumps[SysCall::eDictionaryNew]         = dictionaryNew;
    syscallJumps[SysCall::eDictionaryClear]       = dictionaryClear;
    
    syscallJumps[SysCall::eLongNew]               = longNew;
    syscallJumps[SysCall::eLongNewFromConstant]   = longNewFromConstant;
    syscallJumps[SysCall::eLongAdd]               = longAdd;
    syscallJumps[SysCall::eLongSub]               = longSub;
    syscallJumps[SysCall::eLongMul]               = longMul;
    syscallJumps[SysCall::eLongDiv]               = longDiv;
    syscallJumps[SysCall::eLongMod]               = longMod;
    
    syscallJumps[SysCall::eLongAddB]              = longAdd;
    syscallJumps[SysCall::eLongSubB]              = longSub;
    syscallJumps[SysCall::eLongNegate]            = longNegate;
    
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
    
    syscallJumps[SysCall::eFloatEQ]                = floatEQ;
    syscallJumps[SysCall::eFloatLT]                = floatLT;
    syscallJumps[SysCall::eFloatLE]                = floatLE;
    syscallJumps[SysCall::eFloatGT]                = floatGT;
    syscallJumps[SysCall::eFloatGE]                = floatGE;
    
    
    syscallJumps[SysCall::eLongGetByte]           = longGetByte;
    syscallJumps[SysCall::eFloatGetByte]          = longGetByte;
    syscallJumps[SysCall::eIntGetByte]            = intGetByte;
    syscallJumps[SysCall::eLongFromBytes]         = longFromBytes;
    syscallJumps[SysCall::eFloatFromBytes]        = floatFromBytes;
    syscallJumps[SysCall::eIntFromBytes]          = intFromBytes;
    syscallJumps[SysCall::eLongToBytes]           = longToBytes;
    syscallJumps[SysCall::eFloatToBytes]          = longToBytes;
    syscallJumps[SysCall::eIntToBytes]            = intToBytes;
    
    
    
    syscallJumps[SysCall::eLongToFloat]           = longToFloat;
    syscallJumps[SysCall::eLongToString]          = longToString;
    syscallJumps[SysCall::eUIntToLong]            = uintToLong;
    syscallJumps[SysCall::eUIntToFloat]           = uintToFloat;
    syscallJumps[SysCall::eIntToFloat]            = intToFloat;
    syscallJumps[SysCall::eIntToLong]             = intToLong;
    syscallJumps[SysCall::eLongToUInt]            = longToUInt;
    syscallJumps[SysCall::eFloatToString]         = floatToString;
    
    syscallJumps[SysCall::eScreenClear]           = screenClear;
    syscallJumps[SysCall::eScreenPrint]           = screenPrint;
    syscallJumps[SysCall::eScreenPrintLn]         = screenPrintLn;
    
    syscallJumps[SysCall::eDiagnosticsDie]        = diagnosticsDie;
    
    
    // TODO
}

bool SysCall()
{
    //printf("\nSysCall: 0x%04X 0x%02X", GetPC()-1, codeMemoryBlock[pc]);
    return syscallJumps[codeMemoryBlock[pc++]](VMPop());
}
bool SysCall0()
{
    //printf("\nSysCall0: 0x%04X 0x%02X", GetPC()-1, codeMemoryBlock[pc]);
    return syscallJumps[codeMemoryBlock[pc++]](0);
}
bool SysCall1()
{
    //printf("\nSysCall1: 0x%04X 0x%02X", GetPC()-1, codeMemoryBlock[pc]);
    return syscallJumps[codeMemoryBlock[pc++]](1);
}


