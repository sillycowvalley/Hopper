#ifndef HOPPERFILE_H
#define HOPPERFILE_H

#include <Arduino.h>
#include "Runtime.h"
#include "Platform.h"

Bool External_FileExists(UInt hrpath);
void External_FileDelete(UInt hrpath);
void External_FileWriteAllBytes(UInt hrpath, UInt content);
void External_FileWriteAllCodeBytes(UInt hrpath, UInt codeStart, UInt codeLength);
Bool External_TryFileReadByte_R(UInt hrpath, UInt seekpos, Byte & b);
UInt External_FileGetTime(UInt hrpath);
UInt External_FileGetSize(UInt hrpath);
Bool External_DirectoryExists(UInt hrpath);
void External_DirectoryDelete(UInt hrpath);
void External_DirectoryCreate(UInt hrpath);
UInt External_DirectoryGetTime(UInt hrpath);
UInt External_DirectoryGetFileCount(UInt hrpath);
UInt External_DirectoryGetDirectoryCount(UInt hrpath);
UInt External_DirectoryGetFile(UInt hrpath, UInt index);
UInt External_DirectoryGetDirectory(UInt hrpath, UInt index);

Bool External_ReadAllCodeBytes_R(UInt hrpath, UInt loadAddress, UInt & codeLength);


#endif // HOPPERFILE_H