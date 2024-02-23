#include "HopperFile.h"

long nativeLongFromHopperLong(UInt hopperLong);

#ifdef RP2040
#define USELITTLEFS
#endif

#ifdef SEEEDESP32C3
#define ESP32LITTLEFS
#endif

#ifdef LOLIND1MINI
#define USELITTLEFS
#endif

#ifdef LOLIN_S2_PICO
#define ESP32LITTLEFS
#endif

#ifdef ESP32LITTLEFS
#include "FS.h"
#include "LittleFS.h"
#endif

#ifdef USELITTLEFS
#include "LittleFS.h" // https://arduino-pico.readthedocs.io/en/latest/fs.html
#endif

void FileSystem_Initialize()
{
      //Serial.println("FileSystem_Initialize");
#ifdef ESP32LITTLEFS
      LittleFS.begin(true); // mount the file system, format if failed to mount
#endif
#ifdef USELITTLEFS  
      
      // LittleFS will automatically format the filesystem if one is not detected.
      LittleFSConfig cfg;
      LittleFS.setConfig(cfg);
      if (!LittleFS.begin()) // mount the file system
      {
          Serial.println("LittleFS NOT ok: did you configure Flash Size correctly on the Tools menu?");
      }

#ifdef DIAGNOSTICS
      FSInfo info;
      if (LittleFS.info(info))
      {
        Serial.println("LittleFS.info ok");
        Serial.print("  blockSize:    "); Serial.println(info.blockSize);
        Serial.print("  pageSize:     "); Serial.println(info.pageSize);
        Serial.print("  maxOpenFiles: "); Serial.println(info.maxOpenFiles);
        Serial.print("  totalBytes:   "); Serial.println(info.totalBytes);
        Serial.print("  usedBytes:    "); Serial.println(info.usedBytes);
      }
      else
      {
        Serial.println("LittleFS.info NOT ok");
      }
#endif

#endif
}



const UInt pathBufferSize = 40;
void HRPathToBuffer(UInt hrpath, char * buffer)
{
    UInt length = HRString_GetLength(hrpath);
    if (length >= pathBufferSize)
    {
        length = pathBufferSize-1;
    }
    for (UInt i=0; i < length; i++)
    {
        buffer[i] = tolower(HRString_GetChar(hrpath, i));
    }
    buffer[length] = 0;
}

Bool External_FileExists(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    if(LittleFS.exists(buffer))
    {
        File f = LittleFS.open(buffer, "r");
        return !f.isDirectory();
    }
#endif    
    return false;
}
void External_FileDelete(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    LittleFS.remove(buffer);
#endif    
}
void External_FileWriteAllBytes(UInt hrpath, UInt hrcontent, bool append)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    
    // "a+"
    // Open for reading and appending (writing at end of file).  The
    // file is created if it does not exist.  The initial file
    // position for reading is at the beginning of the file, but
    // output is always appended to the end of the file.
    // "w"
    // Truncate file to zero length or create text file for writing.
    // The stream is positioned at the beginning of the file.
    File f = LittleFS.open(buffer, append ? "a+" : "w");
    if (f) 
    {
        UInt length = HRString_GetLength(hrcontent);
        for (UInt i=0; i < length; i++)
        {
            Type itype;
            Byte b = (Byte)HRString_GetChar(hrcontent, i);
            f.write(b);
        }
        f.close();
    }
#endif    
}
void External_FileWriteAllCodeBytes(UInt hrpath, UInt codeStart, UInt codeLength)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)

    // "w"
    // Truncate file to zero length or create text file for writing.
    // The stream is positioned at the beginning of the file.
    File f = LittleFS.open(buffer, "w");
    if (f) 
    {
        for (UInt i=0; i < codeLength; i++)
        {
            Type itype;
            Byte b = Memory_ReadCodeByte(codeStart+i); // External_FileWriteAllCodeBytes
            f.write(b);
        }
        f.close();
    }
#endif    
}

Bool External_ReadAllCodeBytes_R(UInt hrpath, UInt loadAddress, UInt & codeLength)
{
    Bool success = false;
    codeLength = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && !f.isDirectory())
    {
        while(f.available())
        {
            uint bytesRead = f.readBytes(buffer, pathBufferSize);
            for (uint i=0; i < bytesRead; i++)
            {
                Memory_WriteCodeByte(loadAddress, buffer[i]);        // ReadAllCodeBytes (only used by Runtime_LoadAuto)
                loadAddress++;
                codeLength++;
            }
        }
        f.close();
        success = true;
    }
#endif
    return success;
}

Bool External_TryFileReadByte_R(UInt hrpath, UInt hrseekpos, Byte & b)
{
    bool success = false;    
    long seekpos = nativeLongFromHopperLong(hrseekpos);
    b = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && !f.isDirectory()) 
    {
        if (f.seek(seekpos))
        {
            b = (Byte)(f.read());
            f.close();
            success = true;    
        }
    }
#endif
    return success;
}

UInt External_FileGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && !f.isDirectory())
    {
        time_t lw = f.getLastWrite();
        unsigned int t = (unsigned int)lw;
        UInt result = HRLong_FromBytes(t & 0xFF, (t >> 8) & 0xFF, (t >> 16) & 0xFF, t >> 24);
        return result;
    }
#endif    
    return HRLong_New();
}
UInt External_FileGetSize(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && !f.isDirectory())
    {
        size_t s = f.size();
        uint32_t ui = s;
        UInt result = HRLong_FromBytes(ui & 0xFF, (ui >> 8) & 0xFF, (ui >> 16) & 0xFF, ui >> 24);
        return result;
    }
#endif
    return HRLong_New();
}

Bool External_DirectoryExists(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    if(LittleFS.exists(buffer))
    {
      File f = LittleFS.open(buffer, "r");
        return f.isDirectory();
    }
#endif    
    return false;
}
void External_DirectoryDelete(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    char buffer2[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer2);
    uint plen = strlen(buffer2);
    if ((plen == 0) || (buffer2[plen-1] != '/'))
    {
        buffer2[plen]   = '/';  
        buffer2[plen+1] = 0;
    }
    plen = strlen(buffer2);
    buffer2[plen]   = '_';  
    buffer2[plen+1] = 0;
    
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    if(LittleFS.exists(buffer2))
    {
        LittleFS.remove(buffer2);
    }
    LittleFS.rmdir(buffer);
#endif
   
}
void External_DirectoryCreate(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)

    LittleFS.mkdir(buffer);
    uint plen = strlen(buffer);
    if ((plen == 0) || (buffer[plen-1] != '/'))
    {
        buffer[plen]   = '/';  
        buffer[plen+1] = 0;
    }
    plen = strlen(buffer);
    buffer[plen]   = '_';  
    buffer[plen+1] = 0;
    File f = LittleFS.open(buffer, "w");
    f.print('.');
    f.close();

#endif
}
UInt External_DirectoryGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && f.isDirectory())
    {
#ifdef ESP32LITTLEFS
        time_t lw = f.getLastWrite();
#endif        
#ifdef USELITTLEFS      
        time_t lw = f.getCreationTime();
#endif        
        unsigned int t = (unsigned int)lw;
        UInt result = HRLong_FromBytes(t & 0xFF, (t >> 8) & 0xFF, (t >> 16) & 0xFF, t >> 24);
        return result;
    }
#endif    
    return HRLong_New();
}
UInt External_DirectoryGetFileCount_R(UInt hrpath, UInt & skipped)
{
    skipped = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;
    
#ifdef ESP32LITTLEFS
    File dir = LittleFS.open(buffer);
    if (dir.isDirectory())
    {
        for (;;)
        {
            File file = dir.openNextFile();
            if (!file)
            {
                break;
            }
            if (!file.isDirectory())
            {
                if (file.name() != "_")
                {
                    count++;
                }
            }
        } // loop
    }
#endif    
#ifdef USELITTLEFS      
    Dir dir = LittleFS.openDir(buffer);
    while (dir.next()) 
    {
        if(!dir.isDirectory())  
        {
            String name = dir.fileName();
            if (name == "_") continue;
            count++;
        }
    }
#endif    
    return count;
}
UInt External_DirectoryGetDirectoryCount_R(UInt hrpath, UInt & skipped)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;
    skipped = 0;
#ifdef ESP32LITTLEFS
    File dir = LittleFS.open(buffer);
    if (dir.isDirectory())
    {
        for (;;)
        {
            File file = dir.openNextFile();
            if (!file)
            {
                break;
            }
            if (file.isDirectory())
            {
                count++;
            }
        } // loop
    }
#endif    
#ifdef USELITTLEFS  
    Dir dir = LittleFS.openDir(buffer);
    while (dir.next()) 
    {
        if(dir.isDirectory())  
        {
            String name = dir.fileName();
            count++;
        }
    }
#endif    
    return count;
}
UInt External_DirectoryGetFile(UInt hrpath, UInt index)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;
#ifdef ESP32LITTLEFS
    File dir = LittleFS.open(buffer);
    if (dir.isDirectory())
    {
        for (;;)
        {
            File file = dir.openNextFile();
            if (!file)
            {
                break;
            }
            if (!file.isDirectory())
            {
                String name = file.name();
                if (name == "_") { continue; }
                if (count == index)
                {
                    UInt str = HRString_New();
                    uint plen = strlen(buffer);
                    for (uint i=0; i < plen; i++)
                    {
                        HRString_BuildChar_R(str, buffer[i]);
                    }
                    if ((plen > 0) && (buffer[plen-1] != '/'))
                    {
                        HRString_BuildChar_R(str, '/');
                    }
                    for (uint i=0; i < name.length(); i++)
                    {
                        HRString_BuildChar_R(str, name[i]);
                    }
                    return str;
                }
                count++;
            }
        } // loop
    }
#endif    
#ifdef USELITTLEFS  
    Dir dir = LittleFS.openDir(buffer);
    while (dir.next()) 
    {
        if (!dir.isDirectory())  
        {
            String name = dir.fileName();
            if (name == "_") continue; // ignore
            if (count == index)
            {
                UInt str = HRString_New();
                uint plen = strlen(buffer);
                for (uint i=0; i < plen; i++)
                {
                    HRString_BuildChar_R(str, buffer[i]);
                }
                if ((plen > 0) && (buffer[plen-1] != '/'))
                {
                    HRString_BuildChar_R(str, '/');
                }
                for (uint i=0; i < name.length(); i++)
                {
                    HRString_BuildChar_R(str, name[i]);
                }
                return str;
            }
            count++;
        }
    }
#endif    
    return HRString_New();
}
UInt External_DirectoryGetDirectory(UInt hrpath, UInt index)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;
#ifdef ESP32LITTLEFS
    File dir = LittleFS.open(buffer);
    if (dir.isDirectory())
    {
        for (;;)
        {
            File file = dir.openNextFile();
            if (!file)
            {
                break;
            }
            if (file.isDirectory())
            {
                if (count == index)
                {
                    String name = file.name();
                    UInt str = HRString_New();
                    uint plen = strlen(buffer);
                    for (uint i=0; i < plen; i++)
                    {
                        HRString_BuildChar_R(str, buffer[i]);
                    }
                    if ((plen > 0) && (buffer[plen-1] != '/'))
                    {
                        HRString_BuildChar_R(str, '/');
                    }
                    for (uint i=0; i < name.length(); i++)
                    {
                        HRString_BuildChar_R(str, name[i]);
                    }
                    return str;
                }
                count++;
            }
        } // loop
    }
#endif    
#ifdef USELITTLEFS  
    Dir dir = LittleFS.openDir(buffer);
    while (dir.next()) 
    {
        if (dir.isDirectory())  
        {
            if (count == index)
            {
                UInt str = HRString_New();
                String name = dir.fileName();
                uint plen = strlen(buffer);
                for (uint i=0; i < plen; i++)
                {
                    HRString_BuildChar_R(str, buffer[i]);
                }
                if ((plen > 0) && (buffer[plen-1] != '/'))
                {
                    HRString_BuildChar_R(str, '/');
                }
                for (uint i=0; i < name.length(); i++)
                {
                    HRString_BuildChar_R(str, name[i]);
                }
                return str;
            }
            count++;
        }
    }
#endif    
    return HRString_New();
}

UInt External_ReadLine(UInt _this)
{
    UInt str = HRString_New();
    Bool isValid = false;
    
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00)) // IsValid() && IsReading()
        {
            uint32_t pos  = Memory_ReadWord(_this + 8) + (Memory_ReadWord(_this + 8 + 0x02) << 16);
            uint32_t size = Memory_ReadWord(_this + 14) + (Memory_ReadWord(_this + 14 + 0x02) << 16);

            if (pos < size)
            {
                isValid = true;
                char buffer[pathBufferSize];
                HRPathToBuffer(Memory_ReadWord(_this + 6), (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
                File f = LittleFS.open(buffer, "r");
                bool isOpen = false;
                if (f)
                {
                    isOpen = true;
                }
                else
                {
                    break;
                }
                if (f.isDirectory())
                {
                    f.close();
                    break;
                }
                if (!f.seek(pos))
                {
                    f.close();
                    break;
                }
#endif
                for (;;)
                {
                    if (pos == size)
                    {
                        if (HRString_GetLength(str) == 0) { isValid = false; } // empty string means nothing was read
                        break;
                    }
                    Byte b = 0;
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
                    int i = f.read();
                    if (i == -1) { isValid = false; break; }
                    b = (Byte)i;
#endif
                    pos++;
                    if (b == 0x0D) { continue; }
                    if (b == 0x0A) { break;    }
                    HRString_BuildChar_R(str, Char(b)); // append to string
                } // for (;;)

#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
                if (isOpen)
                {
                    f.close();
                    Memory_WriteWord(_this + 8,        (pos & 0xFFFF));
                    Memory_WriteWord(_this + 8 + 0x02, (pos >> 16));
                }
#endif
                break;
            }
        }
        break;
    } // for (;;)
    if (!isValid)
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
    return str;
}

