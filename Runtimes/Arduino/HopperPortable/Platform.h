#ifndef HOPPERPLATFORM_H
#define HOPPERPLATFORM_H

#include <queue>

#include <Arduino.h>
#include "Runtime.h"

enum InterruptType 
{
    eUndefinedInterruptType = 0,
    ePin,
    eTimer,
};

struct HopperISRStruct
{
    InterruptType interruptType;
    Byte pin;
    UInt timerID;
    Byte status;
    UInt isrDelegate;
};


extern std::queue<HopperISRStruct> isrQueue;

// Machine

extern unsigned char * dataMemoryBlock;
extern unsigned char * codeMemoryBlock;
extern unsigned char * codeStartAddress;

void Machine_Initialize();
Bool Machine_GetExited();
void Machine_SetExited(Bool value);

void External_WebServerRelease();

void HRString_FromString(UInt & hrstr, const String & str);
void HRString_ToString(UInt hrstr, String & str);


// lastError codes:
//   0x00 - ok
//   0x01 - list index out of range
//   0x02 - array index out of range
//   0x03 - no entry for key in dictionary
//   0x04 - division by zero attempted
//   0x05 - string index out of range
//   0x06 - call stack overflow
//   0x07 - argument stack overflow
//   0x08 - failed dynamic cast
//   0x09 - invalid variant type
//   0x0A - feature not implemented
//   0x0B - system failure (internal error)
//   0x0C - memory allocation failure
//   0x0D - numeric type out of range / overflow
//   0x0E - error returned from a failing child exe

void Error(Byte error, UInt pc);
void Error(Byte error);
void Error(Byte error, char * comment);
void Diagnostics_SetError(Byte value);


// Platform

void Platform_Release();
void Platform_Initialize();
void FileSystem_Initialize();

bool External_LoadAuto_Get();
UInt External_GetSegmentPages();
void External_WatchDog();

void External_WriteToJumpTable(UInt jumpTable, Byte opCode, InstructionDelegate instructionDelegate);
bool External_FunctionCall(UInt jumpTable, Byte opCode);

UInt External_GetMillis();
void External_Delay(UInt ms);
void External_PinMode(Byte pin, Byte value);
void External_DigitalWrite(Byte pin, Byte value);
Byte External_DigitalRead(Byte pin);
UInt External_AnalogRead(Byte pin);
void External_AnalogWrite(Byte pin, UInt value);
void External_AnalogWriteResolution(Byte bits);
Bool External_AttachToPin(Byte value, PinISRDelegate gpioISRDelegate, Byte status);
Bool External_MCUInterruptsEnabledGet();
void External_MCUInterruptsEnabledSet(Bool value);
void External_ServiceInterrupts();
void External_MCUReboot(Bool bootsel);
UInt External_MCUHeapFree();
UInt External_MCUStackFree();

UInt External_MCUClockSpeedGet();
void External_MCUClockSpeedSet(UInt value);


Bool Serial_IsAvailable_Get();
Char Serial_ReadChar();
void Serial_WriteChar(Char value);

Byte Memory_ReadCodeByte(UInt address);
void Memory_WriteCodeByte(UInt address, Byte value);
Byte Memory_ReadByte(UInt address);
void Memory_WriteByte(UInt address, Byte value);

UInt Memory_ReadWord(UInt address);
UInt Memory_ReadCodeWord(UInt address);
void Memory_WriteWord(UInt address, UInt value);
void Memory_WriteCodeWord(UInt address, UInt value);

Byte Memory_ReadProgramByte(UInt address);
void Memory_WriteProgramByte(UInt address, Byte value);
UInt Memory_ReadProgramWord(UInt address);
void Memory_WriteProgramWord(UInt address, UInt value);

void External_SetCodeStartAddress(UInt codeAddress);

UInt External_LongToFloat(UInt hrlong);
UInt External_FloatToLong(UInt hrfloat);
UInt External_FloatToUInt(UInt hrfloat);
UInt External_IntToFloat(Int i);
UInt External_UIntToFloat(UInt ui);
UInt External_FloatToString(UInt hrfloat);
UInt External_LongToString(UInt hrlong);
Int  External_UIntToInt(UInt ui);
UInt External_IntToUInt(Int i);
Int  External_LongToInt(UInt hrlong);

bool External_FloatEQ(UInt n, UInt t);
bool External_FloatLT(UInt n, UInt t);
bool External_FloatLE(UInt n, UInt t);
bool External_FloatGE(UInt n, UInt t);
bool External_FloatGT(UInt n, UInt t);
UInt External_FloatAdd(UInt n, UInt t);
UInt External_FloatSub(UInt n, UInt t);
UInt External_FloatMul(UInt n, UInt t);
UInt External_FloatDiv(UInt n, UInt t);

bool External_LongEQ(UInt n, UInt t);
bool External_LongGT(UInt n, UInt t);
bool External_LongGE(UInt n, UInt t);
bool External_LongLT(UInt n, UInt t);
bool External_LongLE(UInt n, UInt t);
UInt External_LongAdd(UInt n, UInt t);
UInt External_LongSub(UInt n, UInt t);
UInt External_LongDiv(UInt n, UInt t);
UInt External_LongMod(UInt n, UInt t);
UInt External_LongMul(UInt n, UInt t);

bool HRWire_Begin(Byte controller);
void HRWire_BeginTx(Byte controller, Byte address);
Byte HRWire_EndTx(Byte controller);
void HRWire_Write(Byte controller, Byte data);
void HRWire_Write(Byte controller, UInt hrarray, UInt startIndex, UInt length);
void HRWire_Configure(Byte controller, Byte sdaPin, Byte sclPin, UInt freqkHz);
Byte HRWire_RequestFrom(Byte controller, Byte address, Byte bytes);
Byte HRWire_Read(Byte controller);

Bool HRSPI_Begin(Byte spiController);
void HRSPI_BeginTransaction(Byte spiController);
void HRSPI_EndTransaction(Byte spiController);
void HRSPI_SetCSPin(Byte spiController, Byte csPin);
void HRSPI_SetClkPin(Byte spiController, Byte clkPin);
void HRSPI_SetTxPin(Byte spiController, Byte txPin);
void HRSPI_SetRxPin(Byte spiController, Byte rxPin);
Byte HRSPI_GetCSPin(Byte spiController);
Byte HRSPI_ReadByte(Byte spiController);
UInt HRSPI_ReadWord(Byte spiController);
void HRSPI_ReadBuffer(Byte spiController, UInt hrdata, UInt startIndex, UInt length);
void HRSPI_WriteByte(Byte spiController, Byte data);
void HRSPI_WriteWord(Byte spiController, UInt data);
void HRSPI_WriteBytes(Byte spiController, Byte data, UInt count);
void HRSPI_WriteWords(Byte spiController, UInt data, UInt count);
void HRSPI_WriteBuffer(Byte spiController, UInt hrdata, UInt startIndex, UInt length);
void HRSPI_Settings(Byte spiController, UInt hrspeedMaximum, DataOrder dataOrder, DataMode dataMode);

void HRNeoPixel_Begin(UInt length, Byte pin, UInt pixelType);
void HRNeoPixel_SetBrightness(Byte brightness);
Byte HRNeoPixel_GetBrightness();
void HRNeoPixel_SetColor(UInt pixel, Byte r, Byte g, Byte b, Byte w);
void HRNeoPixel_Show();
UInt HRNeoPixel_GetLength();

Bool External_WebClientGetRequest_R(UInt hrurl, UInt& hrcontent);

Bool External_WiFiConnect(UInt hrssid, UInt hrpassword);
void External_WiFiDisconnect();
UInt External_WiFiIP();
UInt External_WiFiStatus();

void External_WebServerBegin(UInt port);
void External_WebServerClose();
void External_WebServerEvents();
void External_WebServerSend(UInt httpCode, UInt hrheaderContent, UInt content);
void External_WebServerOn(UInt uri, UInt handler);
void External_WebServerOnNotFound(UInt handler);


#endif // HOPPERPLATFORM_H