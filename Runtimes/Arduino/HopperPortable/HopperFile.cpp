#include "HopperFile.h"

long nativeLongFromHopperLong(UInt hopperLong);


#include "LittleFS.h" // https://arduino-pico.readthedocs.io/en/latest/fs.html
#include <FS.h>
#include <SDFS.h>

#include <time.h>


void FileSystem_Initialize()
{
      
      // LittleFS will automatically format the filesystem if one is not detected.
      LittleFSConfig cfg;
      LittleFS.setConfig(cfg);
      if (!LittleFS.begin()) // mount the file system
      {
          Serial.println("Did you configure Flash Size correctly on the Tools menu in Arduino IDE?");
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

}

Byte sdController = 0;
Byte sdCSPin;
Byte sdClkPin;
Byte sdTxPin;
Byte sdRxPin;
bool sdMounted = false;

char * isSDPath(char * buffer)
{
    if (sdMounted)
    {
        if ((buffer[0] == '/') && (buffer[1] == 's') && (buffer[2] == 'd') && (buffer[3] == '/'))
        {
            char* sdpath = &buffer[3];
            return sdpath;
        }
        if ((buffer[0] == '/') && (buffer[1] == 's') && (buffer[2] == 'd') && (buffer[3] == 0))
        {
            buffer[3] = '/';
            buffer[4] = 0;
            char* sdpath = &buffer[3];
            return sdpath;
        }
    }
    return nullptr;
}
bool isSDRoot(char * buffer)
{
    if (sdMounted)
    {
        if ((buffer[0] == '/') && (buffer[1] == 's') && (buffer[2] == 'd') && (buffer[3] == '/') && (buffer[4] == 0)) // '/sd/'
        {
            return true;
        }
        if ((buffer[0] == '/') && (buffer[1] == 's') && (buffer[2] == 'd') && (buffer[3] == 0)) // '/sd'
        {
            return true;
        }
    }
    return false;
}

const UInt pathBufferSize = 128;
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
    char * sdpath = isSDPath(buffer);
    File f;
    if (nullptr != sdpath)
    {
        if (SDFS.exists(sdpath))
        {
            f = SDFS.open(sdpath, "r");
        }
    }
    else if(LittleFS.exists(buffer))
    {
        f = LittleFS.open(buffer, "r");
    }
    return f && !f.isDirectory();
}
void External_FileDelete(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        SDFS.remove(sdpath);
    }
    else
    {
        LittleFS.remove(buffer);
    }
}
void External_FileWriteAllBytes(UInt hrpath, UInt hrcontent, bool append)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f;
    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, append ? "a+" : "w");
    }
    else
    {
        // "a+"
        // Open for reading and appending (writing at end of file).  The
        // file is created if it does not exist.  The initial file
        // position for reading is at the beginning of the file, but
        // output is always appended to the end of the file.
        // "w"
        // Truncate file to zero length or create text file for writing.
        // The stream is positioned at the beginning of the file.
        f = LittleFS.open(buffer, append ? "a+" : "w");
    }
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
}
void External_FileWriteAllCodeBytes(UInt hrpath, UInt codeStart, UInt codeLength)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    File f;
    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "w");
    }
    else
    {
        // "w"
        // Truncate file to zero length or create text file for writing.
        // The stream is positioned at the beginning of the file.
        f = LittleFS.open(buffer, "w");
    }
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
}

Bool External_ReadAllCodeBytes_R(UInt hrpath, UInt loadAddress, UInt & codeLength)
{
    Bool success = false;
    codeLength = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    File f;
    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
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
    return success;
}

Bool External_TryFileReadByte_R(UInt hrpath, UInt hrseekpos, Byte & b)
{
    bool success = false;    
    long seekpos = nativeLongFromHopperLong(hrseekpos);
    b = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    File f;
    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && !f.isDirectory()) 
    {
        if (f.seek(seekpos))
        {
            b = (Byte)(f.read());
            f.close();
            success = true;    
        }
    }
    return success;
}


UInt External_FileGetSize(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && !f.isDirectory())
    {
        size_t s = f.size();
        uint32_t ui = s;
        UInt result = HRLong_FromBytes(ui & 0xFF, (ui >> 8) & 0xFF, (ui >> 16) & 0xFF, ui >> 24);
        return result;
    }
    return HRLong_New();
}

Bool External_DirectoryExists(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    if (isSDRoot(buffer))
    {
        return true; // not a real directory
    }
    File f;
    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        if (SDFS.exists(sdpath))
        {
            f = SDFS.open(sdpath, "r");
        }
    }
    else
    {
        if(LittleFS.exists(buffer))
        {
            f = LittleFS.open(buffer, "r");
        }
        else if ((buffer[1] == 0) && (buffer[0] == '/'))
        {
            Serial.println("Did you configure Flash Size correctly on the Tools menu in Arduino IDE?");
        }
    }
    return f && f.isDirectory();
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

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        if (!isSDRoot(buffer))
        {
            SDFS.rmdir(sdpath);
        }
    }
    else
    {    
        if(LittleFS.exists(buffer2))
        {
            LittleFS.remove(buffer2);
        }
        LittleFS.rmdir(buffer);
    }
   
}
void External_DirectoryCreate(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        SDFS.mkdir(sdpath);
    }
    else
    {
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
    }
}

UInt External_DirectoryGetFileCount_R(UInt hrpath, UInt & skipped)
{
    skipped = 0;
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;

    Dir dir;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        dir = SDFS.openDir(sdpath);
    }
    else
    {
        dir = LittleFS.openDir(buffer);
    }
    while (dir.next()) 
    {
        if(!dir.isDirectory())  
        {
            String name = dir.fileName();
            if (name == "_") continue;
            count++;
        }
    }
    return count;
}
UInt External_DirectoryGetDirectoryCount_R(UInt hrpath, UInt & skipped)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;
    skipped = 0;

    Dir dir;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        dir = SDFS.openDir(sdpath);
    }
    else
    {
        if (sdMounted && (buffer[0] == '/') && (buffer[1] == 0))
        {
            count++; // /sd/ is in the root
        }
        dir = LittleFS.openDir(buffer);
    }
    while (dir.next()) 
    {
        if (dir.isDirectory())  
        {
            String name = dir.fileName();
            count++;
        }
    }
    return count;
}
UInt External_DirectoryGetFile(UInt hrpath, UInt index)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;

    Dir dir;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        dir = SDFS.openDir(sdpath);
    }
    else
    {
        dir = LittleFS.openDir(buffer);
    }
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
    return HRString_New();
}
UInt External_DirectoryGetDirectory(UInt hrpath, UInt index)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    UInt count = 0;

    Dir dir;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        dir = SDFS.openDir(sdpath);
    }
    else
    {
        dir = LittleFS.openDir(buffer);
    }
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
    if (sdMounted)
    {
        // /sd/ is in the root
        UInt str = HRString_New();
        HRString_BuildChar_R(str, '/');
        HRString_BuildChar_R(str, 's');
        HRString_BuildChar_R(str, 'd');
        HRString_BuildChar_R(str, '/');
        return str;
    }
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

                File f;
                char * sdpath = isSDPath(buffer);
                if (nullptr != sdpath)
                {
                    f = SDFS.open(sdpath, "r");
                }
                else
                { 
                    f = LittleFS.open(buffer, "r");
                }
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

                for (;;)
                {
                    if (pos == size)
                    {
                        if (HRString_GetLength(str) == 0) { isValid = false; } // empty string means nothing was read
                        break;
                    }
                    Byte b = 0;

                    int i = f.read();
                    if (i == -1) { isValid = false; break; }
                    b = (Byte)i;

                    pos++;
                    if (b == 0x0D) { continue; }
                    if (b == 0x0A) { break;    }
                    HRString_BuildChar_R(str, Char(b)); // append to string
                } // for (;;)

                if (isOpen)
                {
                    f.close();
                    Memory_WriteWord(_this + 8,        (pos & 0xFFFF));
                    Memory_WriteWord(_this + 8 + 0x02, (pos >> 16));
                }
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

Byte External_SDSPIControllerGet()
{
    return sdController;
}
void External_SDSPIControllerSet(Byte iController)
{
    sdController = iController;
}
Byte External_SDCSPinGet()
{
    return sdCSPin;
}
void External_SDCSPinSet(Byte pin)
{
    sdCSPin = pin;
}
Byte External_SDClkPinGet()
{
    return sdClkPin;
}
void External_SDClkPinSet(Byte pin)
{
    sdClkPin = pin;
}
Byte External_SDTxPinGet()
{
    return sdTxPin;
}
void External_SDTxPinSet(Byte pin)
{
    sdTxPin = pin;
}
Byte External_SDRxPinGet()
{
    return sdRxPin;
}
void External_SDRxPinSet(Byte pin)
{
    sdRxPin = pin;
}
bool External_SDMount()
{
    //pinMode(sdCSPin, OUTPUT);
    SPIClassRP2040* sdSPI = (sdController == 0) ? &SPI : &SPI1;
    sdSPI->setSCK(sdClkPin);
    sdSPI->setTX(sdTxPin);
    sdSPI->setRX(sdRxPin);
    //sdSPI->setCS(sdCSPin);
        
    SDFS.setConfig(SDFSConfig(sdCSPin, SPI_HALF_SPEED, *sdSPI));
    sdMounted = SDFS.begin();
    return sdMounted;
}
void External_SDEject()
{
    if (sdMounted)
    {
        SDFS.end();
        SPIClassRP2040* sdSPI = (sdController == 0) ? &SPI : &SPI1;
        sdSPI->end();
        sdMounted = false;
    }
}

UInt External_DirectoryGetDate(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    UInt result = HRString_New();

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && f.isDirectory())
    {
        time_t lw = f.getCreationTime();
        struct tm ts = *localtime(&lw);

        UInt year = ts.tm_year + 1900;
        UInt month = ts.tm_mon+1;
        
        HRString_BuildChar_R(result, (Char)((year / 1000) + 48));
        HRString_BuildChar_R(result, (Char)(((year / 100) % 10) + 48));
        HRString_BuildChar_R(result, (Char)(((year / 10) % 10) + 48));
        HRString_BuildChar_R(result, (Char)((year % 10) + 48));
        HRString_BuildChar_R(result, '-');
        HRString_BuildChar_R(result, (Char)((month / 10) + 48));
        HRString_BuildChar_R(result, (Char)((month % 10) + 48));
        HRString_BuildChar_R(result, '-');
        HRString_BuildChar_R(result, (Char)((ts.tm_mday / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_mday % 10) + 48));
    }
    return result;
}
UInt External_DirectoryGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    UInt result = HRString_New();

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && f.isDirectory())
    {
        time_t lw = f.getCreationTime();
        struct tm ts = *localtime(&lw);

        HRString_BuildChar_R(result, (Char)((ts.tm_hour / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_hour % 10) + 48));
        HRString_BuildChar_R(result, ':');
        HRString_BuildChar_R(result, (Char)((ts.tm_min / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_min % 10) + 48));
        HRString_BuildChar_R(result, ':');
        HRString_BuildChar_R(result, (Char)((ts.tm_sec / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_sec % 10) + 48));
    }
    return result;
}
UInt External_FileGetDate(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    UInt result = HRString_New();

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && !f.isDirectory())
    {
        time_t lw = f.getLastWrite();
        struct tm ts = *localtime(&lw);

        UInt year = ts.tm_year + 1900;
        UInt month = ts.tm_mon+1;
        
        HRString_BuildChar_R(result, (Char)((year / 1000) + 48));
        HRString_BuildChar_R(result, (Char)(((year / 100) % 10) + 48));
        HRString_BuildChar_R(result, (Char)(((year / 10) % 10) + 48));
        HRString_BuildChar_R(result, (Char)((year % 10) + 48));
        HRString_BuildChar_R(result, '-');
        HRString_BuildChar_R(result, (Char)((month / 10) + 48));
        HRString_BuildChar_R(result, (Char)((month % 10) + 48));
        HRString_BuildChar_R(result, '-');
        HRString_BuildChar_R(result, (Char)((ts.tm_mday / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_mday % 10) + 48));
    }
    return result;
}
UInt External_FileGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    UInt result = HRString_New();

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && !f.isDirectory())
    {
        time_t lw = f.getLastWrite();
        struct tm ts = *localtime(&lw);

        HRString_BuildChar_R(result, (Char)((ts.tm_hour / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_hour % 10) + 48));
        HRString_BuildChar_R(result, ':');
        HRString_BuildChar_R(result, (Char)((ts.tm_min / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_min % 10) + 48));
        HRString_BuildChar_R(result, ':');
        HRString_BuildChar_R(result, (Char)((ts.tm_sec / 10) + 48));
        HRString_BuildChar_R(result, (Char)((ts.tm_sec % 10) + 48));
    }
    return result;
}
UInt External_FileGetTimeStamp(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    File f;

    char * sdpath = isSDPath(buffer);
    if (nullptr != sdpath)
    {
        f = SDFS.open(sdpath, "r");
    }
    else
    {
        f = LittleFS.open(buffer, "r");
    }
    if (f && !f.isDirectory())
    {
        time_t lw = f.getLastWrite();
        unsigned int t = (unsigned int)lw;
        UInt result = HRLong_FromBytes(t & 0xFF, (t >> 8) & 0xFF, (t >> 16) & 0xFF, t >> 24);
        return result;
    }
    return HRLong_New();
}