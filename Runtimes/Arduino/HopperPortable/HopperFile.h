#ifndef HOPPERFILE_H
#define HOPPERFILE_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

Bool External_FileExists(UInt hrpath);
void External_FileDelete(UInt hrpath);
void External_FileWriteAllBytes(UInt hrpath, UInt content, Bool append);
void External_FileWriteAllCodeBytes(UInt hrpath, UInt codeStart, UInt codeLength);
Bool External_TryFileReadByte_R(UInt hrpath, UInt seekpos, Byte & b);
UInt External_TryFileReadBuffer(UInt hrpath, UInt seekpos, UInt hrbuffer, UInt bufferSize);
UInt External_FileGetSize(UInt hrpath);
Bool External_DirectoryExists(UInt hrpath);
void External_DirectoryDelete(UInt hrpath);
void External_DirectoryCreate(UInt hrpath);
UInt External_DirectoryGetFileCount_R(UInt hrpath, UInt & skipped);
UInt External_DirectoryGetDirectoryCount_R(UInt hrpath, UInt & skipped);
UInt External_DirectoryGetFile(UInt hrpath, UInt index);
UInt External_DirectoryGetDirectory(UInt hrpath, UInt index);

UInt External_FileGetTimeStamp(UInt hrpath);
UInt External_FileGetTime(UInt hrpath);
UInt External_FileGetDate(UInt hrpath);
UInt External_DirectoryGetTime(UInt hrpath);
UInt External_DirectoryGetDate(UInt hrpath);


Bool External_ReadAllCodeBytes_R(UInt hrpath, UInt loadAddress, UInt & codeLength);
UInt External_ReadLine(UInt _this);

Byte External_SDSPIControllerGet();
void External_SDSPIControllerSet(Byte iController);
Byte External_SDCSPinGet();
void External_SDCSPinSet(Byte pin);
Byte External_SDClkPinGet();
void External_SDClkPinSet(Byte pin);
Byte External_SDTxPinGet();
void External_SDTxPinSet(Byte pin);
Byte External_SDRxPinGet();
void External_SDRxPinSet(Byte pin);
bool External_SDMount();
void External_SDEject();



#endif // HOPPERFILE_H