#ifndef HOPPERRUNTIME_H
#define HOPPERRUNTIME_H

#include <Arduino.h>
#include "HopperConfiguration.h"

typedef unsigned char  Byte;
typedef unsigned char  Char;
typedef unsigned short UInt;
typedef   signed short Int;
typedef           bool Bool;

typedef Bool (*InstructionDelegate)();
typedef UInt ISRDelegate;

// method definitions
enum OpCode {
    eADD = 0x0000,
    eSUB = 0x0001,
    eDIV = 0x0002,
    eMUL = 0x0003,
    eMOD = 0x0004,
    eEQ = 0x0005,
    eNE = 0x0006,
    eGT = 0x0007,
    eLT = 0x0008,
    eGE = 0x0009,
    eLE = 0x000A,
    eBOOLOR = 0x000B,
    eBOOLAND = 0x000C,
    eBITOR = 0x000D,
    eBITAND = 0x000E,
    eBITSHL = 0x000F,
    eBITSHR = 0x0010,
    eADDI = 0x0011,
    eSUBI = 0x0012,
    eDIVI = 0x0013,
    eMULI = 0x0014,
    eMODI = 0x0015,
    eGTI = 0x0016,
    eLTI = 0x0017,
    eGEI = 0x0018,
    eLEI = 0x0019,
    ePUSHIB = 0x001A,
    ePOPLOCALB = 0x001B,
    ePUSHLOCALB = 0x001C,
    ePOPRELB = 0x001D,
    ePUSHRELB = 0x001E,
    ePOPGLOBALB = 0x001F,
    ePUSHGLOBALB = 0x0020,
    ePUSHSTACKADDRB = 0x0021,
    eINCLOCALB = 0x0022,
    eDECLOCALB = 0x0023,
    eDUP = 0x0027,
    eDECSP = 0x0028,
    eRETB = 0x002A,
    eRETRETB = 0x002B,
    eCALLB = 0x002C,
    eTESTBPB = 0x002D,
    eJZB = 0x002E,
    eJNZB = 0x002F,
    eJB = 0x0030,
    eJZW = 0x0031,
    eJNZW = 0x0032,
    eJW = 0x0033,
    eCALLW = 0x0034,
    ePUSHIW = 0x0037,
    eINCLOCALBB = 0x003F,
    ePUSHIWLE = 0x0040,
    eBOOLNOT = 0x0041,
    eBITNOT = 0x0042,
    eSWAP = 0x0043,
    ePUSHI0 = 0x0044,
    ePUSHI1 = 0x0045,
    ePUSHIM1 = 0x0046,
    ePUSHGP = 0x0047,
    eRET0 = 0x004A,
    eCALLREL = 0x004B,
    ePOPLOCALB00 = 0x004C,
    ePOPLOCALB02 = 0x004D,
    ePUSHLOCALB00 = 0x004E,
    ePUSHLOCALB02 = 0x004F,
    eSYSCALL0 = 0x0024,
    eSYSCALL1 = 0x0025,
    eSYSCALL = 0x0026,
    eCOPYNEXTPOP = 0x0048,
    eENTER = 0x0049,
    eNOP = 0x0050,
    eCAST = 0x0051,
    ePUSHGLOBALBB = 0x0052,
    eINCGLOBALB = 0x0053,
    eDECGLOBALB = 0x0054,
    ePUSHIWLT = 0x0055,
    ePUSHLOCALBB = 0x0056,
    ePOPCOPYLOCALB = 0x0057,
    ePOPCOPYRELB = 0x0058,
    ePOPCOPYGLOBALB = 0x0059,
    ePOPCOPYLOCALB00 = 0x005D,
    ePOPCOPYLOCALB02 = 0x005E,
    eENTERB = 0x005F,
    ePUSHDW = 0x0060,
    eRETFAST = 0x0061,
    ePUSHDB = 0x0062,
    eEXIT = 0x0063,
    eBITXOR = 0x0064,
    ePUSHIWLEI = 0x0065,
    eINCGLOBALBB = 0x0066,
    eJREL = 0x0067,
    eJIXB = 0x0068,
    eJIXW = 0x0069,
    eCALLIW = 0x006A,
    ePUSHIBLE = 0x006B,
    ePUSHIBEQ = 0x006C,
    eADDB = 0x006D,
    eSUBB = 0x006E,
    eLIBCALL = 0x006F,
};

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
};

enum Type {
    eUndefined = 0x0000,
    eChar = 0x0001,
    eInt = 0x0002,
    eByte = 0x0003,
    eUInt = 0x0004,
    eReference = 0x0005,
    eBool = 0x0006,
    eType = 0x000C,
    eFloat = 0x000D,
    eLong = 0x000E,
    eString = 0x000F,
    ePair = 0x0010,
    eArray = 0x0012,
    eDictionary = 0x0013,
    eVariant = 0x0014,
    eFile = 0x0015,
    eDirectory = 0x0016,
    eList = 0x0019,
    eListItem = 0x001A,
};

enum LibCall {
    eWireBegin = 0x0000,
    eWireBeginTx = 0x0001,
    eWireEndTx = 0x0002,
    eWireWrite = 0x0003,
    eMCUPinMode = 0x0004,
    eMCUDigitalRead = 0x0005,
    eMCUDigitalWrite = 0x0006,
    eMCUAnalogRead = 0x0007,
    eMCUAnalogWrite = 0x0008,
    eMCUAnalogWriteResolution = 0x0009,
    eMCUAttachToPin = 0x000A,
    eGraphicsConfigureDisplay = 0x000B,
    eGraphicsConfigureSPI = 0x000C,
    eGraphicsConfigureSPIPort = 0x000D,
    eGraphicsConfigureReset = 0x000E,
    eGraphicsConfigureI2C = 0x000F,
    eGraphicsConfigureMatrix = 0x0010,
    eGraphicsBegin = 0x0011,
    eGraphicsEnd = 0x0012,
    eGraphicsInvertDisplay = 0x0013,
    eGraphicsFlipDisplay = 0x0014,
    eGraphicsClear = 0x0015,
    eGraphicsWidthGet = 0x0016,
    eGraphicsHeightGet = 0x0017,
    eGraphicsSetPixel = 0x0018,
    eGraphicsLine = 0x0019,
    eGraphicsHorizontalLine = 0x001A,
    eGraphicsVerticalLine = 0x001B,
    eGraphicsRectangle = 0x001C,
    eGraphicsFilledRectangle = 0x001D,
    eGraphicsCircle = 0x001E,
    eGraphicsFilledCircle = 0x001F,
    eGraphicsShow = 0x0020,
    eGraphicsDrawChar = 0x0021,
};

enum Display {
    eNoDisplay = 0x0000,
    eILI9341 = 0x0001,
    eST7735 = 0x0002,
    eST7796 = 0x0003,
    eSSD1306 = 0x0004,
    eLedMatrix = 0x0005,
};

enum DisplayState {
    eOK = 0x0000,
    eDisplayNotSet = 0x0001,
    eBadWidth = 0x0002,
    eBadHeight = 0x0003,
    eSPIPinsNotSet = 0x0004,
    eI2CAddressNotSet = 0x0005,
    eMatrixNotConfigured = 0x0006,
};

enum HopperFlags {
    eTraceOn = 0x0001,
    eWarpSpeed = 0x0002,
    eCheckedBuild = 0x0004,
    eStack8Bit = 0x0008,
    eProfileBuild = 0x0010,
    eBreakpointsSet = 0x0020,
    eSingleStep = 0x0040,
    eMCUPlatform = 0x0080,
};

enum Key {
    eNoKey = 0x0000,
    eAlt = 0x0100,
    eControl = 0x0200,
    eShift = 0x0400,
    eMask = 0xF0FF,
    eClick = 0xE0FF,
    eScroll = 0xE0FE,
    eClickRight = 0xE0FD,
    eSpace = 0x0020,
    eDelete = 0xE071,
    eBackspace = 0x0008,
    eTab = 0xE00D,
    eEnter = 0x000D,
    eEscape = 0x001B,
    eModSpace = 0xE029,
    eModEscape = 0xE076,
    eModInsert = 0xE070,
    eModBackspace = 0xE066,
    eModEnter = 0xE05A,
    eModEsc = 0xE076,
    eF1 = 0xE005,
    eF2 = 0xE006,
    eF3 = 0xE004,
    eF4 = 0xE00C,
    eF5 = 0xE003,
    eF6 = 0xE00B,
    eF7 = 0xE083,
    eF8 = 0xE00A,
    eF9 = 0xE001,
    eF10 = 0xE009,
    eF11 = 0xE078,
    eF12 = 0xE007,
    eLeft = 0xE06B,
    eRight = 0xE074,
    eUp = 0xE075,
    eDown = 0xE072,
    eHome = 0xE06C,
    eEnd = 0xE069,
    ePageDown = 0xE07A,
    ePageUp = 0xE07D,
    eModPeriod = 0xE049,
    eMod0 = 0xE045,
    eMod1 = 0xE016,
    eMod2 = 0xE01E,
    eMod3 = 0xE026,
    eMod4 = 0xE025,
    eMod5 = 0xE02E,
    eMod6 = 0xE036,
    eMod7 = 0xE03D,
    eMod8 = 0xE03E,
    eMod9 = 0xE046,
    eModA = 0xE01C,
    eModB = 0xE032,
    eModC = 0xE021,
    eModD = 0xE023,
    eModE = 0xE014,
    eModF = 0xE02B,
    eModG = 0xE034,
    eModH = 0xE033,
    eModI = 0xE043,
    eModJ = 0xE03B,
    eModK = 0xE042,
    eModL = 0xE04B,
    eModM = 0xE03A,
    eModN = 0xE031,
    eModO = 0xE044,
    eModP = 0xE04D,
    eModQ = 0xE015,
    eModR = 0xE02D,
    eModS = 0xE01B,
    eModT = 0xE02C,
    eModU = 0xE03C,
    eModV = 0xE02A,
    eModW = 0xE01D,
    eModX = 0xE022,
    eModY = 0xE035,
    eModZ = 0xE01A,
    eControlA = 0xE21C,
    eControlC = 0xE221,
    eControlV = 0xE22A,
    eA = 0x0041,
    ea = 0x0061,
    eB = 0x0042,
    eb = 0x0062,
    eC = 0x0043,
    ec = 0x0063,
    eD = 0x0044,
    ed = 0x0064,
    eE = 0x0045,
    ee = 0x0065,
    eF = 0x0046,
    ef = 0x0066,
    eG = 0x0047,
    eg = 0x0067,
    eH = 0x0048,
    eh = 0x0068,
    eI = 0x0049,
    ei = 0x0069,
    eJ = 0x004A,
    ej = 0x006A,
    eK = 0x004B,
    ek = 0x006B,
    eL = 0x004C,
    el = 0x006C,
    eM = 0x004D,
    em = 0x006D,
    eN = 0x004E,
    en = 0x006E,
    eO = 0x004F,
    eo = 0x006F,
    eP = 0x0050,
    ep = 0x0070,
    eQ = 0x0051,
    eq = 0x0071,
    eR = 0x0052,
    er = 0x0072,
    eS = 0x0053,
    es = 0x0073,
    eT = 0x0054,
    et = 0x0074,
    eU = 0x0055,
    eu = 0x0075,
    eV = 0x0056,
    ev = 0x0076,
    eW = 0x0057,
    ew = 0x0077,
    eX = 0x0058,
    ex = 0x0078,
    eY = 0x0059,
    ey = 0x0079,
    eZ = 0x005A,
    ez = 0x007A,
    eN0 = 0x0030,
    eN1 = 0x0031,
    eN2 = 0x0032,
    eN3 = 0x0033,
    eN4 = 0x0034,
    eN5 = 0x0035,
    eN6 = 0x0036,
    eN7 = 0x0037,
    eN8 = 0x0038,
    eN9 = 0x0039,
};

void HopperEntryPoint();
void Runtime_MCU();
Bool Runtime_LoadAuto_R(UInt & loadedAddress, UInt & codeLength);
Byte Runtime_FromHex(Char ch);
void Runtime_WaitForEnter();
void Runtime_DumpPage(Byte iPage, Bool includeAddresses);
void Runtime_Out4Hex(UInt value);
void Runtime_Out2Hex(Byte value);
Bool Runtime_SerialLoadIHex_R(UInt & loadedAddress, UInt & codeLength);
Bool Runtime_TryReadSerialByte_R(Byte & data);
void HopperVM_Restart();
void HopperVM_Initialize(UInt loadedAddress, UInt loadedSize);
void HopperVM_ExecuteWarp();
void HopperVM_ClearBreakpoints(Bool includingZero);
void HopperVM_SetBreakpoint(Byte n, UInt address);
UInt HopperVM_PC_Get();
UInt HopperVM_CSP_Get();
UInt HopperVM_SP_Get();
UInt HopperVM_BP_Get();
void HopperVM_FlashProgram(UInt codeLocation, UInt codeLength);
void HopperVM_Execute();
void HopperVM_ExecuteStepTo();
void HopperVM_Release();
UInt HopperVM_GetAppName();
Bool HopperVM_BreakpointExists_Get();
UInt HopperVM_GetBreakpoint(Byte n);
UInt HopperVM_GetCS(UInt address);
UInt HopperVM_Get_R(UInt address, Type & htype);
void HopperVM_DataMemoryReset();
void HopperVM_DiskSetup();
Bool HopperVM_ExecuteOpCode();
UInt HRString_New();
void HRString_BuildChar_R(UInt & _this, Char ch);
void HRString_BuildClear_R(UInt & _this);
UInt HRString_new(UInt size);
UInt HRString_getCapacity(UInt _this);
UInt HRString_GetLength(UInt _this);
UInt HRString_clone(UInt original, UInt extra);
void HRDirectory_Create(UInt hrpath);
Bool HRDirectory_Exists(UInt hrpath);
UInt HRFile_Create(UInt hrpath);
void HRFile_Append(UInt _this, Byte b);
void HRFile_Flush(UInt _this);
Bool HRFile_Exists(UInt str);
UInt HRFile_CreateFromCode(UInt hrpath, UInt codeStart, UInt codeLength);
void HRFile_Delete(UInt path);
UInt HRFile_New();
UInt Memory_HeapStart_Get();
UInt Memory_HeapSize_Get();
UInt Memory_FreeList_Get();
void Memory_Free(UInt address);
void Memory_Initialize(UInt start, UInt size);
UInt Memory_Allocate(UInt size);
void Memory_Set(UInt memory, Byte value, UInt size);
void GC_Release(UInt address);
UInt GC_New(UInt size, Type htype);
Char Char_ToHex(Byte h);
void Minimal_Error_Set(Byte value);
Byte Minimal_Error_Get();
Bool IO_IsBreak();
void IO_WriteLn();
void IO_Write(Char c);
void IO_AssignKeyboardBuffer(UInt buffer);
void IO_WriteHex(UInt u);
void IO_PushKey(Char c);
void IO_WriteHex(Byte b);
void HRArray_Release();
void HRArray_Initialize();
void Instructions_PopulateJumpTable(UInt jumpTable);
Bool Instructions_Undefined();
Bool Instructions_Add();
Bool Instructions_Sub();
Bool Instructions_Div();
Bool Instructions_Mul();
Bool Instructions_Mod();
Bool Instructions_EQ();
Bool Instructions_NE();
Bool Instructions_GT();
Bool Instructions_LT();
Bool Instructions_GE();
Bool Instructions_LE();
Bool Instructions_BoolOr();
Bool Instructions_BoolAnd();
Bool Instructions_BitOr();
Bool Instructions_BitAnd();
Bool Instructions_BitShl();
Bool Instructions_BitShr();
Bool Instructions_AddI();
Bool Instructions_SubI();
Bool Instructions_DivI();
Bool Instructions_MulI();
Bool Instructions_ModI();
Bool Instructions_GTI();
Bool Instructions_LTI();
Bool Instructions_GEI();
Bool Instructions_LEI();
Bool Instructions_PushIB();
Bool Instructions_PopLocalB();
Bool Instructions_PushLocalB();
Bool Instructions_PopRelB();
Bool Instructions_PushRelB();
Bool Instructions_PopGlobalB();
Bool Instructions_PushGlobalB();
Bool Instructions_PushStackAddrB();
Bool Instructions_IncLocalB();
Bool Instructions_DecLocalB();
Bool Instructions_Dup();
Bool Instructions_DecSP();
Bool Instructions_RetB();
Bool Instructions_RetRetB();
Bool Instructions_CallB();
Bool Instructions_TestBPB();
Bool Instructions_Exit();
Bool Instructions_JZB();
Bool Instructions_JNZB();
Bool Instructions_JB();
Bool Instructions_JZW();
Bool Instructions_JNZW();
Bool Instructions_JW();
Bool Instructions_CallW();
Bool Instructions_PushIW();
Bool Instructions_IncLocalBB();
Bool Instructions_PushIWLE();
Bool Instructions_BoolNot();
Bool Instructions_BitNot();
Bool Instructions_Swap();
Bool Instructions_PushI0();
Bool Instructions_PushI1();
Bool Instructions_PushIM1();
Bool Instructions_PushGP();
Bool Instructions_Ret0();
Bool Instructions_CallRel();
Bool Instructions_PopLocalB00();
Bool Instructions_PopLocalB02();
Bool Instructions_PushLocalB00();
Bool Instructions_PushLocalB02();
Bool Instructions_SysCall0();
Bool Instructions_SysCall1();
Bool Instructions_SysCall();
Bool Instructions_CNP();
Bool Instructions_Enter();
Bool Instructions_NOP();
Bool Instructions_Cast();
Bool Instructions_PushGlobalBB();
Bool Instructions_IncGlobalB();
Bool Instructions_DecGlobalB();
Bool Instructions_IncGlobalBB();
Bool Instructions_PushIWLT();
Bool Instructions_PushLocalBB();
Bool Instructions_PopCopyLocalB();
Bool Instructions_PopCopyRelB();
Bool Instructions_PopCopyGlobalB();
Bool Instructions_PopCopyLocalB00();
Bool Instructions_PopCopyLocalB02();
Bool Instructions_EnterB();
Bool Instructions_RetFast();
Bool Instructions_BitXor();
Bool Instructions_PushIWLEI();
Bool Instructions_JREL();
Bool Instructions_JIXB();
Bool Instructions_JIXW();
Bool Instructions_CallIW();
Bool Instructions_PushIBLE();
Bool Instructions_PushIBEQ();
Bool Instructions_AddB();
Bool Instructions_SubB();
Bool Instructions_LibCall();
UInt HRString_Clone(UInt original);
void Runtime_ErrorDump(UInt number);
void HRDirectory_Clear(UInt _this);
void HRFile_Clear(UInt _this);
Bool HRFile_IsCode(UInt _this);
void HRList_Clear(UInt _this);
void HRList_clearAllItems(UInt pCurrent, Type etype);
void HRList_clearItem(UInt pCurrent, Type etype);
void HRDictionary_Clear(UInt _this);
Bool HRDictionary_next_R(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt & value);
void HRPair_Clear(UInt _this);
void HRVariant_Clear(UInt _this);
OpCode HopperVM_CurrentOpCode_Get();
UInt HopperVM_Pop();
void HopperVM_Push(UInt value, Type htype);
UInt HopperVM_Pop_R(Type & htype);
Int HopperVM_PopI();
void HopperVM_PushI(Int ivalue);
Byte HopperVM_ReadByteOperand();
Bool HopperVM_CNP_Get();
void HopperVM_CNP_Set(Bool value);
Int HopperVM_ReadByteOffsetOperand();
UInt HopperVM_TypeStack_Get();
UInt HopperVM_ValueStack_Get();
void HopperVM_Put(UInt address, UInt value, Type htype);
void HopperVM_BP_Set(UInt value);
UInt HopperVM_PopCS();
void HopperVM_PC_Set(UInt value);
void HopperVM_PushCS(UInt value);
UInt HopperVM_LookupMethod(UInt methodIndex);
Bool HopperVM_ExitInline();
Int HopperVM_ReadWordOffsetOperand();
UInt HopperVM_ReadWordOperand();
Bool HopperVM_ExecuteSysCall(Byte iSysCall, UInt iOverload);
UInt HopperVM_Get(UInt address);
Int HopperVM_PopI_R(Type & htype);
Bool HopperVM_RunInline();
Bool Types_IsReferenceType(Type htype);
void GC_AddReference(UInt address);
UInt GC_Clone(UInt original);
Bool Library_ExecuteLibCall(Byte iLibCall);
void IO_WriteUInt(UInt _this);
void IO_writeDigit(UInt uthis);
UInt Memory_Available();
UInt Memory_Maximum();
Bool HRFile_IsValid(UInt _this);
UInt HRFile_ReadLine(UInt _this);
Byte HRFile_Read(UInt _this);
Byte HRFile_Read(UInt _this, UInt hrseekpos);
void HRFile_Append(UInt _this, UInt hrstr);
UInt HRFile_Open(UInt hrpath);
UInt HRFile_GetTime(UInt path);
UInt HRFile_GetSize(UInt path);
UInt HRFile_Clone(UInt original);
UInt HRDirectory_New();
UInt HRDirectory_Open(UInt hrpath);
Bool HRDirectory_IsValid(UInt _this);
UInt HRDirectory_GetFileCount(UInt hrdir);
UInt HRDirectory_GetDirectoryCount(UInt hrdir);
UInt HRDirectory_GetFile(UInt hrdir, UInt index);
UInt HRDirectory_GetDirectory(UInt hrdir, UInt index);
void HRDirectory_Delete(UInt hrpath);
UInt HRDirectory_GetTime(UInt hrpath);
UInt HRDirectory_Clone(UInt original);
Char HRString_GetChar(UInt _this, UInt index);
UInt HRString_NewFromConstant0(UInt location, UInt length);
UInt HRString_NewFromConstant1(UInt doubleChar);
UInt HRString_InsertChar(UInt _this, UInt index, Char ch);
Bool HRString_EndsWith(UInt _this, Char with);
Bool HRString_EndsWith(UInt _this, UInt with);
Int HRString_Compare(UInt left, UInt right);
UInt HRString_Replace(UInt _this, UInt pattern, UInt replace);
UInt HRString_Replace(UInt _this, Char from, Char to);
UInt HRString_Append(UInt _this, UInt append);
UInt HRString_Append(UInt _this, Char ch);
UInt HRString_Substring(UInt _this, UInt start);
UInt HRString_Substring(UInt _this, UInt start, UInt limit);
void HRString_Substring_R(UInt & _this, UInt start);
void HRString_BuildString_R(UInt & _this, UInt append);
void HRString_BuildFront_R(UInt & _this, Char ch);
UInt HRString_Trim(UInt _this);
void HRString_TrimRight_R(UInt & _this);
void HRString_TrimLeft_R(UInt & _this);
UInt HRString_TrimLeft(UInt _this);
UInt HRLong_NewFromConstant(UInt location);
UInt HRLong_ToBytes(UInt ichunk);
Byte HRLong_GetByte(UInt ichunk, UInt i);
UInt HRLong_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3);
UInt HRLong_ToUInt(UInt _this);
UInt HRLong_LongNegate(UInt top);
UInt HRLong_New();
UInt HRLong_Clone(UInt original);
UInt HRFloat_NewFromConstant(UInt location);
UInt HRFloat_ToBytes(UInt ichunk);
Byte HRFloat_GetByte(UInt ichunk, UInt i);
UInt HRFloat_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3);
UInt HRFloat_New();
UInt HRFloat_Clone(UInt original);
Bool HRWiFi_Connect(UInt ssid, UInt password);
Bool HRHttpClient_GetRequest_R(UInt url, UInt & content);
UInt HRArray_New(Type htype, UInt count);
UInt HRArray_GetItem_R(UInt _this, UInt index, Type & etype);
void HRArray_SetItem(UInt _this, UInt index, UInt value);
UInt HRArray_GetCount(UInt _this);
UInt HRList_New(Type htype);
UInt HRList_GetLength(UInt _this);
void HRList_Append(UInt _this, UInt item, Type itype);
void HRList_SetItem(UInt _this, UInt index, UInt item, Type itype);
void HRList_Insert(UInt _this, UInt index, UInt item, Type itype);
UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype);
void HRList_Remove(UInt _this, UInt index);
Bool HRList_Contains(UInt _this, UInt item, Type itype);
UInt HRList_Clone(UInt original);
UInt HRList_createItem(UInt itemData, Type etype, Type itype);
UInt HRVariant_CreateValueVariant(UInt value, Type vtype);
UInt HRVariant_New(UInt value, Type vtype);
UInt HRVariant_Clone(UInt original);
UInt HRVariant_GetValue_R(UInt _this, Type & vtype);
Bool HRVariant_IsEqual(UInt left, Type ltype, UInt right, Type rtype);
UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt value);
UInt HRPair_GetValue_R(UInt _this, Type & vtype);
UInt HRPair_GetKey_R(UInt _this, Type & ktype);
UInt HRPair_Clone(UInt original);
UInt HRDictionary_New(Type ktype, Type vtype);
UInt HRDictionary_GetCount(UInt _this);
void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt value, Type vtype);
Bool HRDictionary_Next_R(UInt _this, UInt & iterator, UInt & hrpair);
Bool HRDictionary_Contains(UInt _this, UInt key);
UInt HRDictionary_Get_R(UInt _this, UInt key, Type & vtype);
UInt HRDictionary_Clone(UInt original);
void HRDictionary_adjustCapacity(UInt _this, UInt newCapacity);
UInt HRDictionary_hashKey16(UInt key);
UInt HRDictionary_findEntry(UInt pEntries, UInt capacity, UInt key, UInt hash, Bool valueKeys);
Bool HRDictionary_validEntry(UInt pEntry, Bool valueKeys);
UInt HRUInt_ToLong(UInt ui);
UInt HRInt_ToLong(UInt ichunk);
UInt HRInt_ToBytes(UInt ichunk);
Byte HRInt_GetByte(UInt ichunk, UInt i);
UInt HRInt_FromBytes(Byte b0, Byte b1);
Char Char_ToDigit(Byte d);
UInt HRVariant_UnBox_R(UInt _this, Type & vtype);


#endif // HOPPERRUNTIME_H