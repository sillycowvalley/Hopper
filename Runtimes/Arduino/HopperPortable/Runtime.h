#ifndef HOPPERRUNTIME_H
#define HOPPERRUNTIME_H

#include <Arduino.h>
#include "HopperConfiguration.h"

typedef unsigned char  Byte;
typedef unsigned char  Char;
typedef unsigned short UInt;
typedef   signed short Int;
typedef   signed char  Int8;
typedef           bool Bool;

typedef Bool (*InstructionDelegate)();
typedef UInt PinISRDelegate;
typedef UInt TimerISRDelegate;
typedef UInt HandlerDelegate;

// method definitions
enum OpCode {
    eLIBCALL = 0x0008,
    eLIBCALL0 = 0x0009,
    eLIBCALL1 = 0x000A,
    ePUSHI = 0x0037,
    ePUSHD = 0x0060,
    ePUSHLOCAL = 0x0039,
    ePUSHREL = 0x003B,
    ePUSHGLOBAL = 0x003D,
    ePUSHSTACKADDR = 0x003E,
    ePUSHGP = 0x0047,
    ePOPLOCAL = 0x0038,
    ePOPREL = 0x003A,
    ePOPGLOBAL = 0x003C,
    eCOPYNEXTPOP = 0x0048,
    eBOOLNOT = 0x0041,
    eBITNOT = 0x0042,
    eSWAP = 0x0043,
    eDUP = 0x0027,
    eDECSP = 0x0028,
    eDIE = 0x0029,
    eENTER = 0x0049,
    eNOP = 0x0050,
    eCAST = 0x0051,
    eJZ = 0x0031,
    eJNZ = 0x0032,
    eJW = 0x0033,
    eJREL = 0x0067,
    eRET = 0x0035,
    eRETRES = 0x0036,
    eCALLI = 0x006A,
    eCALL = 0x0034,
    eCALLREL = 0x004B,
    eSYSCALL = 0x0026,
    eADD = 0x0080,
    eADDI = 0x0081,
    eSUB = 0x0082,
    eSUBI = 0x0083,
    eDIV = 0x0084,
    eDIVI = 0x0085,
    eMUL = 0x0086,
    eMULI = 0x0087,
    eMOD = 0x0088,
    eMODI = 0x0089,
    eGT = 0x008A,
    eGTI = 0x008B,
    eLT = 0x008C,
    eLTI = 0x008D,
    eGE = 0x008E,
    eGEI = 0x008F,
    eLE = 0x0090,
    eLEI = 0x0091,
    eEQ = 0x0092,
    eNE = 0x0094,
    eBOOLOR = 0x0096,
    eBOOLAND = 0x0098,
    eBITAND = 0x009A,
    eBITOR = 0x009C,
    eBITXOR = 0x009E,
    eBITSHR = 0x00A0,
    eBITSHL = 0x00A2,
    ePUSHIB = 0x001A,
    ePOPLOCALB = 0x001B,
    ePUSHLOCALB = 0x001C,
    ePOPRELB = 0x001D,
    ePUSHRELB = 0x001E,
    ePOPGLOBALB = 0x001F,
    ePUSHGLOBALB = 0x0020,
    ePUSHSTACKADDRB = 0x0021,
    eRETB = 0x002A,
    eRETRESB = 0x002B,
    eCALLB = 0x002C,
    eTESTBPB = 0x002D,
    eJZB = 0x002E,
    eJNZB = 0x002F,
    eJB = 0x0030,
    eJIXB = 0x0068,
    eJIX = 0x0069,
    ePUSHILE = 0x0040,
    ePUSHI0 = 0x0044,
    ePUSHI1 = 0x0045,
    ePUSHIM1 = 0x0046,
    eRET0 = 0x004A,
    ePOPLOCALB00 = 0x004C,
    ePOPLOCALB02 = 0x004D,
    ePUSHLOCALB00 = 0x004E,
    ePUSHLOCALB02 = 0x004F,
    eSYSCALL0 = 0x0024,
    eSYSCALL1 = 0x0025,
    ePUSHGLOBALBB = 0x0052,
    eINCLOCALB = 0x0022,
    eINCLOCALIB = 0x00A4,
    eDECLOCALB = 0x0023,
    eDECLOCALIB = 0x00A6,
    eINCGLOBALB = 0x0053,
    eINCGLOBALIB = 0x00A5,
    eDECGLOBALB = 0x0054,
    eDECGLOBALIB = 0x00A7,
    eINCLOCALBB = 0x003F,
    eINCLOCALIBB = 0x00A3,
    eSYSCALLB0 = 0x00A8,
    eSYSCALL00 = 0x00A9,
    ePUSHIBB = 0x00AA,
    eSYSCALLB1 = 0x00AB,
    eSYSCALL01 = 0x00AC,
    eSYSCALL10 = 0x00AD,
    ePUSHILT = 0x0055,
    ePUSHLOCALBB = 0x0056,
    ePOPCOPYLOCALB = 0x0057,
    ePOPCOPYRELB = 0x0058,
    ePOPCOPYGLOBALB = 0x0059,
    ePOPCOPYLOCALB00 = 0x005D,
    ePOPCOPYLOCALB02 = 0x005E,
    eENTERB = 0x005F,
    eEXIT = 0x0063,
    ePUSHDB = 0x0062,
    ePUSHILEI = 0x0065,
    eINCGLOBALBB = 0x0066,
    eRETFAST = 0x0061,
    ePUSHIBLE = 0x006B,
    ePUSHIBEQ = 0x006C,
    eADDB = 0x006D,
    eSUBB = 0x006E,
};

enum SysCall {
    eStringNewFromConstant = 0x0000,
    eCharToString = 0x0001,
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
    eListCountGet = 0x0010,
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
    eVariantUnBox = 0x0028,
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
    eLongToString = 0x0038,
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
    eTypesVerifyValueTypes = 0x0082,
    eStringBuild = 0x0083,
    eWiFiConnect = 0x0084,
    eWiFiIPGet = 0x0085,
    eWiFiStatusGet = 0x0086,
    eWiFiDisconnect = 0x0087,
    eArrayNewFromConstant = 0x0088,
    eDirectoryCreate = 0x0089,
    eDirectoryDelete = 0x008A,
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
    eCharToUpper = 0x00B8,
    eCharIsUpper = 0x00B9,
    eCharIsDigit = 0x00BA,
    eCharIsLetterOrDigit = 0x00BB,
    eCharIsLower = 0x00BC,
    eByteToDigit = 0x00BD,
    eByteToHex = 0x00BE,
    eCharIsHexDigit = 0x00BF,
    eCharToLower = 0x00C0,
    eTimeDelay = 0x00C6,
    eIntToBytes = 0x00CD,
    eFileGetTime = 0x00CE,
    eDirectoryGetTime = 0x00CF,
    eStringTrim = 0x00D0,
    eStringTrimLeft = 0x00D1,
    eStringTrimRight = 0x00D2,
    eStringPushImmediate = 0x00D3,
    eStringToUpper = 0x00D4,
    eStringToLower = 0x00D5,
    eMemoryReadWord = 0x00D7,
    eMemoryWriteWord = 0x00D8,
    eLongGetByte = 0x00E0,
    eIntGetByte = 0x00E1,
    eFloatGetByte = 0x00E2,
    eLongFromBytes = 0x00E3,
    eIntFromBytes = 0x00E4,
    eFloatFromBytes = 0x00E5,
    eUIntToFloat = 0x00E6,
    eFloatToUInt = 0x00EC,
    eFloatToLong = 0x00ED,
    eLongAddB = 0x00EE,
    eLongSubB = 0x00EF,
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

enum HopperPinStatus {
    eLow = 0x0000,
    eHigh = 0x0001,
    eChange = 0x0002,
    eFalling = 0x0003,
    eRising = 0x0004,
};

enum LibCall {
    eWireBegin = 0x0000,
    eWireBeginTx = 0x0001,
    eWireEndTx = 0x0002,
    eWireWrite = 0x0003,
    eWireConfigure = 0x0004,
    eWireRead = 0x0005,
    eWireRequestFrom = 0x0006,
    eMCUPinMode = 0x0007,
    eMCUDigitalRead = 0x0008,
    eMCUDigitalWrite = 0x0009,
    eMCUAnalogRead = 0x000A,
    eMCUAnalogWrite = 0x000B,
    eMCUAnalogWriteResolution = 0x000C,
    eMCUAttachToPin = 0x000D,
    eMCUInterruptsEnabledGet = 0x000E,
    eMCUInterruptsEnabledSet = 0x000F,
    eMCUReboot = 0x0010,
    eMCUHeapFree = 0x0011,
    eMCUStackFree = 0x0012,
    eTimerStart = 0x0013,
    eTimerStop = 0x0014,
    eTimerAlarm = 0x0015,
    eTimerCancel = 0x0016,
    eSPISettings = 0x0017,
    eSPIBegin = 0x0018,
    eSPIBeginTransaction = 0x0019,
    eSPIEndTransaction = 0x001A,
    eSPIReadByte = 0x001B,
    eSPIReadWord = 0x001C,
    eSPIReadBuffer = 0x001D,
    eSPIWriteByte = 0x001E,
    eSPIWriteWord = 0x001F,
    eSPIWriteBuffer = 0x0020,
    eSPISetCSPin = 0x0021,
    eSPIGetCSPin = 0x0022,
    eSPISetClkPin = 0x0023,
    eSPISetTxPin = 0x0024,
    eSPISetRxPin = 0x0025,
    eSPICSPinGet = 0x0026,
    eSPICSPinSet = 0x0027,
    eSPIClkPinSet = 0x0028,
    eSPITxPinSet = 0x0029,
    eSPIRxPinSet = 0x002A,
    eNeoPixelBegin = 0x002B,
    eNeoPixelBrightnessSet = 0x002C,
    eNeoPixelBrightnessGet = 0x002D,
    eNeoPixelSetColor = 0x002E,
    eNeoPixelShow = 0x002F,
    eNeoPixelLengthGet = 0x0030,
    eWebClientGetRequest = 0x0031,
    eWebServerBegin = 0x0032,
    eWebServerOn = 0x0033,
    eWebServerOnNotFound = 0x0034,
    eWebServerEvents = 0x0035,
    eWebServerClose = 0x0036,
    eWebServerSend = 0x0037,
};

enum DataOrder {
    eLSBFirst = 0x0000,
    eMSBFirst = 0x0001,
};

enum DataMode {
    eMode0 = 0x0000,
    eMode1 = 0x0001,
    eMode2 = 0x0002,
    eMode3 = 0x0003,
};

enum HopperFlags {
    eTraceOn = 0x0001,
    eWarpSpeed = 0x0002,
    eStackSlot32Bit = 0x0002,
    eCheckedBuild = 0x0004,
    eSP8Bit = 0x0008,
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
UInt Colour_MatrixGreen_Get();
UInt Colour_Black_Get();
void HopperVM_Restart();
void HopperVM_Initialize(UInt loadedAddress, UInt loadedSize);
void HopperVM_ClearBreakpoints(Bool includingZero);
void HopperVM_SetBreakpoint(Byte n, UInt address);
UInt HopperVM_PC_Get();
UInt HopperVM_CSP_Get();
UInt HopperVM_SP_Get();
UInt HopperVM_BP_Get();
void HopperVM_FlashProgram(UInt codeLocation, UInt codeLength, UInt crc);
Bool HopperVM_Execute();
Bool HopperVM_ExecuteStepTo();
void HopperVM_DumpStack(UInt limit);
void HopperVM_DumpHeap(Bool display, UInt accountedFor);
void HopperVM_Release();
UInt HopperVM_GetAppName(Bool crc);
Bool HopperVM_BreakpointExists_Get();
UInt HopperVM_GetBreakpoint(Byte n);
UInt HopperVM_GetCS(UInt address);
UInt HopperVM_Get_R(UInt address, Type & htype);
void HopperVM_DataMemoryReset();
void HopperVM_DiskSetup();
Bool HopperVM_ExecuteOpCode();
void HopperVM_WriteERROR();
void HopperVM_WriteBREAK();
Bool HopperVM_IsOnFreeList(UInt pCandidate);
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
UInt HRFile_Open(UInt hrpath);
Byte HRFile_Read(UInt _this);
UInt HRFile_CreateFromCode(UInt hrpath, UInt codeStart, UInt codeLength);
void HRFile_Delete(UInt path);
UInt HRFile_New();
UInt HRFile_GetSize(UInt path);
Bool HRFile_lt32(UInt nextLSW, UInt nextMSW, UInt topLSW, UInt topMSW);
UInt Memory_HeapStart_Get();
UInt Memory_HeapSize_Get();
UInt Memory_FreeList_Get();
void Memory_Free(UInt address);
void Memory_Initialize(UInt start, UInt size);
UInt Memory_Allocate(UInt size);
void Memory_Set(UInt memory, Byte value, UInt size);
void GC_Release(UInt address);
void GC_Dump(UInt address);
void GC_Dump(UInt address, UInt indent);
UInt GC_New(UInt size, Type htype);
Char HRByte_ToHex(Byte h);
void Minimal_Error_Set(Byte value);
Byte Minimal_Error_Get();
Bool IO_IsBreak();
void IO_WriteLn();
void IO_Write(Char c);
void IO_WriteHex(UInt u);
void IO_WriteHex(Byte b);
void IO_WriteUInt(UInt _this);
void IO_WriteInt(Int _this);
void IO_AssignKeyboardBuffer(UInt buffer);
void IO_PushKey(Char c);
void IO_writeDigit(UInt uthis);
Bool Types_IsReferenceType(Type htype);
void Runtime_ErrorDump(UInt number);
void HRArray_Release();
void HRArray_Initialize();
void HRArray_Dump(UInt address, UInt indent);
void Instructions_PopulateJumpTable(UInt jumpTable);
Bool Instructions_Undefined();
Bool Instructions_Die();
Bool Instructions_PopLocalB();
Bool Instructions_PopRelB();
Bool Instructions_PushRelB();
Bool Instructions_PopGlobalB();
Bool Instructions_PushGlobalB();
Bool Instructions_PushStackAddrB();
Bool Instructions_CallB();
Bool Instructions_JNZB();
Bool Instructions_JB();
Bool Instructions_Ret0();
Bool Instructions_PopLocalB00();
Bool Instructions_PopLocalB02();
Bool Instructions_SysCall0();
Bool Instructions_SysCall1();
Bool Instructions_SysCall00();
Bool Instructions_SysCall01();
Bool Instructions_SysCall10();
Bool Instructions_SysCallB0();
Bool Instructions_SysCallB1();
Bool Instructions_PushGlobalBB();
Bool Instructions_PushLocalBB();
Bool Instructions_PopCopyLocalB();
Bool Instructions_PopCopyRelB();
Bool Instructions_PopCopyGlobalB();
Bool Instructions_PopCopyLocalB00();
Bool Instructions_PopCopyLocalB02();
Bool Instructions_EnterB();
Bool Instructions_JIXB();
Bool Instructions_PushILE();
Bool Instructions_PushILT();
Bool Instructions_PushILEI();
Bool Instructions_PushIBEQ();
Bool Instructions_RetB();
Bool Instructions_RetFast();
Bool Instructions_PopLocal();
Bool Instructions_PushLocal();
Bool Instructions_PopRel();
Bool Instructions_PushRel();
Bool Instructions_PopGlobal();
Bool Instructions_PushGlobal();
Bool Instructions_PushStackAddr();
Bool Instructions_Dup();
Bool Instructions_DecSP();
Bool Instructions_Ret();
Bool Instructions_RetRes();
Bool Instructions_TestBPB();
Bool Instructions_Exit();
Bool Instructions_JZ();
Bool Instructions_JNZ();
Bool Instructions_J();
Bool Instructions_PushIW();
Bool Instructions_Swap();
Bool Instructions_PushIM1();
Bool Instructions_PushGP();
Bool Instructions_CNP();
Bool Instructions_NOP();
Bool Instructions_JREL();
Bool Instructions_JIX();
Bool Instructions_Call();
Bool Instructions_CallRel();
Bool Instructions_SysCall();
Bool Instructions_LibCall0();
Bool Instructions_LibCall1();
Bool Instructions_LibCall();
Bool Instructions_IncLocalBB();
Bool Instructions_IncGlobalBB();
Bool Instructions_PopCopyLocal();
Bool Instructions_PopCopyRel();
Bool Instructions_PopCopyGlobal();
UInt HRString_Clone(UInt original);
void HRString_Dump(UInt address, UInt indent);
Byte HRLong_GetByte(UInt ichunk, UInt i);
UInt HRLong_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3);
void HRLong_Dump(UInt address, UInt indent);
void HRDirectory_Clear(UInt _this);
void HRFile_Clear(UInt _this);
Bool HRFile_IsCode(UInt _this);
void HRList_Clear(UInt _this);
void HRList_Dump(UInt address, UInt indent);
void HRList_clearAllItems(UInt pCurrent, Type etype);
void HRList_clearItem(UInt pCurrent, Type etype);
void HRDictionary_Clear(UInt _this);
void HRDictionary_Dump(UInt address, UInt indent);
Bool HRDictionary_next_R(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt & value);
void HRPair_Clear(UInt _this);
void HRPair_Dump(UInt address, UInt indent);
void HRVariant_Clear(UInt _this);
void HRVariant_Dump(UInt address, UInt indent);
Char HRByte_ToDigit(Byte d);
OpCode HopperVM_CurrentOpCode_Get();
UInt HopperVM_Pop_R(Type & htype);
Bool HopperVM_CNP_Get();
void HopperVM_CNP_Set(Bool value);
Int HopperVM_ReadByteOffsetOperand();
UInt HopperVM_TypeStack_Get();
UInt HopperVM_ValueStack_Get();
void HopperVM_Put(UInt address, UInt value, Type htype);
void HopperVM_Push(UInt value, Type htype);
Byte HopperVM_ReadByteOperand();
UInt HopperVM_GP_Get();
void HopperVM_PushCS(UInt value);
void HopperVM_PC_Set(UInt value);
UInt HopperVM_LookupMethod(UInt methodIndex);
UInt HopperVM_Pop();
void HopperVM_BP_Set(UInt value);
UInt HopperVM_PopCS();
Bool HopperVM_ExecuteSysCall(Byte iSysCall, UInt iOverload);
UInt HopperVM_ReadWordOperand();
Int HopperVM_PopI_R(Type & htype);
Int HopperVM_ReadWordOffsetOperand();
Bool HopperVM_ExitInline();
void HopperVM_PushI(Int ivalue);
Bool HopperVM_RunInline();
void GC_AddReference(UInt address);
UInt GC_Clone(UInt original);
Bool Library_ExecuteLibCall(Byte iLibCall, UInt iOverload);
UInt Memory_Available();
UInt Memory_Maximum();
UInt HRList_New(Type htype);
UInt HRList_GetCount(UInt _this);
void HRList_Append(UInt _this, UInt item, Type itype);
void HRList_SetItem(UInt _this, UInt index, UInt item, Type itype);
void HRList_Insert(UInt _this, UInt index, UInt item, Type itype);
UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype);
void HRList_Remove(UInt _this, UInt index);
Bool HRList_Contains(UInt _this, UInt item, Type itype);
Type HRList_GetValueType(UInt _this);
UInt HRList_Clone(UInt original);
UInt HRList_createItem(UInt itemData, Type etype, Type itype);
Bool HRFile_IsValid(UInt _this);
UInt HRFile_ReadLine(UInt _this);
Byte HRFile_Read(UInt _this, UInt hrseekpos);
void HRFile_Append(UInt _this, UInt hrstr);
UInt HRFile_GetTime(UInt path);
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
UInt HRLong_NewFromConstant(UInt location);
UInt HRLong_ToBytes(UInt ichunk);
UInt HRLong_ToUInt(UInt _this);
UInt HRLong_LongNegate(UInt top);
UInt HRLong_LongAddB(UInt next, UInt top);
UInt HRLong_LongSubB(UInt next, UInt top);
UInt HRLong_New();
UInt HRLong_Clone(UInt original);
UInt HRFloat_NewFromConstant(UInt location);
UInt HRFloat_ToBytes(UInt ichunk);
Byte HRFloat_GetByte(UInt ichunk, UInt i);
UInt HRFloat_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3);
UInt HRFloat_New();
UInt HRFloat_Clone(UInt original);
UInt HRString_NewFromConstant0(UInt location, UInt length);
UInt HRString_NewFromConstant1(UInt doubleChar);
Char HRString_GetChar(UInt _this, UInt index);
UInt HRString_InsertChar(UInt _this, UInt index, Char ch);
UInt HRString_ToUpper(UInt _this);
void HRString_ToUpper_R(UInt & _this);
UInt HRString_ToLower(UInt _this);
void HRString_ToLower_R(UInt & _this);
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
UInt HRArray_New(Type htype, UInt count);
UInt HRArray_NewFromConstant(UInt location, Type htype, UInt length);
UInt HRArray_GetItem_R(UInt _this, UInt index, Type & etype);
void HRArray_SetItem(UInt _this, UInt index, UInt value);
UInt HRArray_GetCount(UInt _this);
Type HRArray_GetValueType(UInt _this);
UInt HRArray_Clone(UInt original);
UInt HRVariant_CreateValueVariant(UInt value, Type vtype);
UInt HRVariant_New(UInt value, Type vtype);
UInt HRVariant_UnBox_R(UInt _this, Type & vtype);
Type HRVariant_GetValueType(UInt _this);
UInt HRVariant_Clone(UInt original);
UInt HRVariant_GetValue_R(UInt _this, Type & vtype);
Bool HRVariant_IsEqual(UInt left, Type ltype, UInt right, Type rtype);
UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt value);
UInt HRPair_GetValue_R(UInt _this, Type & vtype);
UInt HRPair_GetKey_R(UInt _this, Type & ktype);
Type HRPair_GetValueType(UInt _this);
Type HRPair_GetKeyType(UInt _this);
UInt HRPair_Clone(UInt original);
Bool HRDictionary_Next_R(UInt _this, UInt & iterator, UInt & hrpair);
Type HRDictionary_GetKeyType(UInt _this);
Type HRDictionary_GetValueType(UInt _this);
UInt HRDictionary_New(Type ktype, Type vtype);
UInt HRDictionary_GetCount(UInt _this);
void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt value, Type vtype);
Bool HRDictionary_Contains(UInt _this, UInt key);
UInt HRDictionary_Get_R(UInt _this, UInt key, Type & vtype);
UInt HRDictionary_Clone(UInt original);
void HRDictionary_adjustCapacity(UInt _this, UInt newCapacity);
UInt HRDictionary_hashKey16(UInt key);
UInt HRDictionary_findEntry(UInt pEntries, UInt capacity, UInt key, UInt hash, Bool valueKeys);
Bool HRDictionary_validEntry(UInt pEntry, Bool valueKeys);
Char HRChar_ToUpper(Char _this);
Char HRChar_ToLower(Char _this);
Bool HRChar_IsUpper(Char _this);
Bool HRChar_IsLower(Char _this);
Bool HRChar_IsDigit(Char _this);
Bool HRChar_IsLetterOrDigit(Char _this);
Bool HRChar_IsHexDigit(Char _this);
UInt HRUInt_ToLong(UInt ui);
UInt HRInt_ToLong(UInt ichunk);
UInt HRInt_ToBytes(UInt ichunk);
Byte HRInt_GetByte(UInt ichunk, UInt i);
UInt HRInt_FromBytes(Byte b0, Byte b1);



#endif // HOPPERRUNTIME_H