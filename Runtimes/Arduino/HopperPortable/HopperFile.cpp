#include "HopperFile.h"


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

      /*
      FSInfo info;
      if (LittleFS.info(info))
      {
        Serial.println("LittleFS.info ok");
        Serial.println(info.blockSize);
        Serial.println(info.pageSize);
        Serial.println(info.maxOpenFiles);
        Serial.println(info.totalBytes);
        Serial.println(info.usedBytes);
      }
      else
      {
        Serial.println("LittleFS.info NOT ok");
      }
      */
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
void External_FileWriteAllBytes(UInt hrpath, UInt content)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "a"); // creates file if it does not exist
    if (f) 
    {
        UInt length = HRString_GetLength(content);
        for (UInt i=0; i < length; i++)
        {
            Type itype;
            Byte b = (Byte)HRString_GetChar(content, i);
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
    File f = LittleFS.open(buffer, "w");
    if (f) 
    {
        for (UInt i=0; i < codeLength; i++)
        {
            Type itype;
            Byte b = Memory_ReadCodeByte(codeStart+i);
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
                Memory_WriteCodeByte(loadAddress, buffer[i]);
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

Bool External_TryFileReadByte_R(UInt hrpath, UInt seekpos, Byte & b)
{
    bool success = false;    
    b = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
#if defined(USELITTLEFS) || defined(ESP32LITTLEFS)
    File f = LittleFS.open(buffer, "r");
    if (f && !f.isDirectory() && f.seek(seekpos)) 
    {
        b = (Byte)(f.read());
        f.close();
        success = true;    
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
        unsigned int s = f.size();
        UInt result = HRLong_FromBytes(s & 0xFF, (s >> 8) & 0xFF, (s >> 16) & 0xFF, s >> 24);
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
UInt External_DirectoryGetFileCount(UInt hrpath)
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
UInt External_DirectoryGetDirectoryCount(UInt hrpath)
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
                if (name == "_") { continue; } // no count++
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
            if (name == "_") { continue; } // no count++
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



