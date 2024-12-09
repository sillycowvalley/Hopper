#ifndef HOPPERFILE_H
#define HOPPERFILE_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

// File memory map:
//   0000 heap allocator size
//   0F   type = tFile
//   00   GC reference count
//   00   bool:   isValid
//   00   bool:   isReading: 32 bit pos is position of next byte to read (Read and ReadLine)
//   00   bool:   isWriting
//   00   bool:   isCode:    16 bit pos is length and buffer is start in codeSegment
//   0000     string: path
//   00000000 uint32: pos
//   0000     uint:   string (buffer)
//   00000000 uint32: size: 32 bit file size in bytes used to read (Read and ReadLine)

// Directory memory map:
//   0000 heap allocator size
//   10   type = tDirectory
//   00   GC reference count
//   00   bool:   isValid
//   0000 string: path


Bool External_FileExists(UInt hrpath);
void External_FileDelete(UInt hrpath);
UInt External_FileWriteAllBytes(UInt hrpath, UInt content, Bool append);
UInt External_FileWriteAllCodeBytes(UInt hrpath, UInt codeStart, UInt codeLength);
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

void FileSystem_End();



#endif // HOPPERFILE_H