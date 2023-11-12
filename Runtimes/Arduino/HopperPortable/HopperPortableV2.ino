#include "LittleFS.h" // https://arduino-pico.readthedocs.io/en/latest/fs.html

const bool loadAuto = true; // set this to false if you are booting into a bad flashed Hopper program

// Platform

#define PI_PICO_W
//#define NANO_RP2040
//#define LOLIN_D1_MINI
//#define LOLIN_C3_MINI
//#define LOLIN_S2_PICO

//Lolin S2 & C3 boards:
// To put S2 and C3 boards into Device Firmware Upgrade (DFU) mode:
// - Hold on Button 0|9
// - Press Button Reset
// - Release Button 0|9 When you hear the prompt tone on usb reconnection
//
// The steps to then get the C3 Mini working in Hopper are currently nuts:
// - after uploading, press the reset button on the Mini
// - in Hopper, run TERM to drain the diagnostic info from serial
// - exit TERM then run HM!

#ifdef PI_PICO_W
const size_t segmentSizes = 0x8000; // size in words, 64K x2 on the Pi Pico
#endif

#ifdef NANO_RP2040
const size_t segmentSizes = 0x8000; // size in words, 64K x2 on the Arduino Nano RP2040
#endif

#ifdef LOLIN_D1_MINI
const size_t segmentSizes = 0x1800;  // size in words, 12K x2 on the Lolin D1 Mini
#endif

#ifdef LOLIN_S2_PICO
const size_t segmentSizes = 0x8000;  // size in words, 64K x2 on the Lolin S2 Pico
#endif

#ifdef LOLIN_C3_MINI
const size_t segmentSizes = 0x8000;  // size in words, 64K x2 on the Lolin C3 Mini
#endif

unsigned char * dataMemoryBlock = nullptr;
unsigned char * codeMemoryBlock = nullptr;

bool exited;

// Machine

typedef unsigned char  Byte;
typedef unsigned char  Char;
typedef unsigned short UInt;
typedef   signed short Int;
typedef           bool Bool;

typedef Bool (*InstructionDelegate)();

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

void GC_Release(UInt address);
UInt HRLong_New();
UInt HRFloat_New();
UInt HRString_New();
void HRString_Build_R(UInt& _this, Char ch);
UInt HRString_GetLength(UInt _this);
Char HRString_GetChar(UInt _this, UInt index);

UInt HRList_GetLength(UInt _this);
UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype);
void HRList_Append(UInt _this, UInt item, Type itype);
UInt HRLong_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3);


Byte lastError;

// Platform

bool External_LoadAuto_Get()
{
    return loadAuto;
}
UInt External_GetSegmentSizes()
{
    return segmentSizes;
}
void External_WatchDog()
{
#ifndef PI_PICO_W  // Pi Pico appears to not need this
    yield();
#endif
}

void Platform_Initialize()
{
    if (nullptr != dataMemoryBlock)
    {
        Platform_Release();
    }
    //Serial.println("Platform_Initialize");
    //Serial.println((unsigned int)(segmentSizes << 1), HEX);
    codeMemoryBlock = (unsigned char*)malloc((segmentSizes << 1)); // calloc does not work on the Lolin D1 Mini
    if (nullptr != codeMemoryBlock)
    {
        memset(codeMemoryBlock, (segmentSizes << 1), 0);
    }
    //Serial.println((unsigned int)codeMemoryBlock, HEX);
    dataMemoryBlock = (unsigned char*)malloc((segmentSizes << 1));
    if (nullptr != dataMemoryBlock)
    {
        memset(dataMemoryBlock, (segmentSizes << 1), 0);
    }
    //Serial.println((unsigned int)dataMemoryBlock, HEX);

    // LittleFS will automatically format the filesystem if one is not detected.
    LittleFSConfig cfg;
    LittleFS.setConfig(cfg);

    LittleFS.begin(); // mount the file system
}

void Platform_Release()
{
    LittleFS.end(); // unmount the file system

    free(dataMemoryBlock);
    dataMemoryBlock = nullptr;
    free(codeMemoryBlock);
    codeMemoryBlock = nullptr;
}

void Serial_WriteChar(Char value)
{
    if (value == (Char)0x0D)
    {
        value = (Char)0x0A;
    }
    char str[2];
    str[0] = value;
    str[1] = 0;
    Serial.print(str);
}

Char Serial_ReadChar()
{
    while (Serial.available() == 0)
    {
        External_WatchDog();
    }
    int ch = Serial.read();
    if (ch == (Char)0x0A)
    {
        ch = 0x0D;
    }
    return (Char)ch;
}
Bool Serial_IsAvailable_Get()
{
    External_WatchDog();
    return (Bool)(Serial.available() != 0);
}


Byte Memory_ReadCodeByte(UInt address)
{
    return codeMemoryBlock[address];
}

void Memory_WriteCodeByte(UInt address, Byte value)
{
    codeMemoryBlock[address] = value;
}


Byte Memory_ReadByte(UInt address)
{
    return dataMemoryBlock[address];
}

void Memory_WriteByte(UInt address, Byte value)
{
    dataMemoryBlock[address] = value;
}

UInt Memory_ReadWord(UInt address)
{
    return dataMemoryBlock[address] + (dataMemoryBlock[address+1] << 8);
}

UInt Memory_ReadCodeWord(UInt address)
{
    return codeMemoryBlock[address] + (codeMemoryBlock[address + 1] << 8);
}

void Memory_WriteWord(UInt address, UInt value)
{
    dataMemoryBlock[address] = value & 0xFF;
    dataMemoryBlock[address+1] = value >> 8;
}

void Memory_WriteCodeWord(UInt address, UInt value)
{
    codeMemoryBlock[address] = value & 0xFF;
    codeMemoryBlock[address + 1] = value >> 8;
}

void External_Delay(UInt ms)
{
    delay(ms);
}

void External_PinMode(Byte pin, Byte value)
{
    pinMode(pin, value);
}

void External_DigitalWrite(Byte pin, Byte value)
{
    digitalWrite(pin, value);
}

Byte External_DigitalRead(Byte pin)
{
    Byte value = digitalRead(pin);
    return value;
}




// Machine
void Machine_Initialize()
{
    lastError = 0;
    exited    = false;
}

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

void Error(Byte error, UInt pc)
{
    lastError = error;
    if (0 != error)
    {
        char buffer[40];
        sprintf(buffer, "Error: 0x%02x (PC=0x%04x)\n", error, pc);
        Serial.print(buffer);
        exited = true;
    }
}
void Error(Byte error)
{
    lastError = error;
    if (0 != error)
    {
        char buffer[40];
        sprintf(buffer, "Error: 0x%02x\n", error);
        Serial.print(buffer);
        exited = true;
    }
}
void Error(Byte error, char * comment)
{
    lastError = error;
    if (0 != error)
    {
        char buffer[40];
        sprintf(buffer, "Error: 0x%02x (%s)\n", error, comment);
        Serial.print(buffer);
        exited = true;
    }
}

void Diagnostics_SetError(Byte value)
{
    Error(value);
}

void External_WriteToJumpTable(UInt jumpTable, Byte opCode, InstructionDelegate instructionDelegate)
{
    UInt opOffset = (opCode << 2);
    InstructionDelegate * jumpLocation = (InstructionDelegate * )(&dataMemoryBlock[jumpTable] + opOffset);
    *jumpLocation = instructionDelegate;
}

bool External_FunctionCall(UInt jumpTable, Byte opCode)
{
    UInt opOffset = (opCode << 2);
    InstructionDelegate instructionDelegate = *((InstructionDelegate*)(&dataMemoryBlock[jumpTable] + opOffset));
    return instructionDelegate();
}


UInt hopperLongFromNativeLong(long nativeLong)
{
    Byte* bytes = (Byte*)(&nativeLong);
    UInt hopperLong = HRLong_New();

    dataMemoryBlock[hopperLong + 2] = *(bytes + 0);
    dataMemoryBlock[hopperLong + 3] = *(bytes + 1);
    dataMemoryBlock[hopperLong + 4] = *(bytes + 2);
    dataMemoryBlock[hopperLong + 5] = *(bytes + 3);

    return hopperLong;
}
long nativeLongFromHopperLong(UInt hopperLong)
{
    long nativeLong = 0;
    Byte* bytes = (Byte*)(&nativeLong);

    *(bytes + 0) = dataMemoryBlock[hopperLong + 2];
    *(bytes + 1) = dataMemoryBlock[hopperLong + 3];
    *(bytes + 2) = dataMemoryBlock[hopperLong + 4];
    *(bytes + 3) = dataMemoryBlock[hopperLong + 5];

    return nativeLong;
}

UInt hopperFloatFromNativeFloat(float nativeFloat)
{
    Byte* bytes = (Byte*)(&nativeFloat);
    UInt hopperFloat = HRFloat_New();

    dataMemoryBlock[hopperFloat + 2] = *(bytes + 0);
    dataMemoryBlock[hopperFloat + 3] = *(bytes + 1);
    dataMemoryBlock[hopperFloat + 4] = *(bytes + 2);
    dataMemoryBlock[hopperFloat + 5] = *(bytes + 3);

    return hopperFloat;
}
float nativeFloatFromHopperFloat(UInt hopperFloat)
{
    float nativeFloat = 0;
    Byte* bytes = (Byte*)(&nativeFloat);

    *(bytes + 0) = dataMemoryBlock[hopperFloat + 2];
    *(bytes + 1) = dataMemoryBlock[hopperFloat + 3];
    *(bytes + 2) = dataMemoryBlock[hopperFloat + 4];
    *(bytes + 3) = dataMemoryBlock[hopperFloat + 5];

    return nativeFloat;
}

UInt External_GetMillis()
{
    unsigned long ticks = millis();
    return hopperLongFromNativeLong((long)ticks);
}

bool External_LongEQ(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return (next == top);
}
bool External_LongGT(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return(next > top);
}
bool External_LongLT(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return(next < top);
}
bool External_LongGE(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return(next >= top);
}
bool External_LongLE(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return(next <= top);
}

UInt External_LongAdd(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return hopperLongFromNativeLong(next + top);
}
UInt External_LongSub(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return hopperLongFromNativeLong(next - top);
}
UInt External_LongDiv(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return hopperLongFromNativeLong(next / top); // TODO : division by zero
}
UInt External_LongMul(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return hopperLongFromNativeLong(next * top);
}
UInt External_LongMod(UInt n, UInt t)
{
    long top = nativeLongFromHopperLong(t);
    long next = nativeLongFromHopperLong(n);
    return hopperLongFromNativeLong(next % top); // TODO : division by zero
}

UInt External_LongToFloat(UInt hrlong)
{
    long ln = nativeLongFromHopperLong(hrlong);
    return hopperFloatFromNativeFloat(float(ln));
}

UInt External_IntToFloat(Int i)
{
    return hopperFloatFromNativeFloat(float(i));
}

UInt External_UIntToFloat(UInt ui)
{
    return hopperFloatFromNativeFloat(float(ui));
}

UInt External_FloatToString(UInt hrfloat)
{
    UInt hrstring = HRString_New();
    char buffer[20];
    float fl = nativeFloatFromHopperFloat(hrfloat);
    sprintf(buffer, "%f", fl);
    UInt i = 0;
    while (buffer[i])
    {
        HRString_Build_R(hrstring , (Char)buffer[i]);
        i++;
    }
    return hrstring;
}

bool External_FloatEQ(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return (next == top);
}
bool External_FloatGT(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return(next > top);
}
bool External_FloatLT(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return(next < top);
}
bool External_FloatGE(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return(next >= top);
}
bool External_FloatLE(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return(next <= top);
}

UInt External_FloatAdd(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return hopperFloatFromNativeFloat(next + top);
}
UInt External_FloatSub(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return hopperFloatFromNativeFloat(next - top);
}
UInt External_FloatDiv(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return hopperFloatFromNativeFloat(next / top); // TODO : division by zero
}
UInt External_FloatMul(UInt n, UInt t)
{
    float top = nativeFloatFromHopperFloat(t);
    float next = nativeFloatFromHopperFloat(n);
    return hopperFloatFromNativeFloat(next * top);
}

Int  External_UIntToInt(UInt ui)
{
    return (Int)ui;
}

UInt External_IntToUInt(Int i)
{
    return (UInt)i;
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
    
    if(LittleFS.exists(buffer))
    {
        File f = LittleFS.open(buffer, "r");
        return f.isFile();
    }
    return false;
}
void External_FileDelete(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    LittleFS.remove(buffer);
}
void External_FileWriteAllBytes(UInt hrpath, UInt content)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f = LittleFS.open(buffer, "w");
    if (f) 
    {
        UInt length = HRList_GetLength(content);
        for (UInt i=0; i < length; i++)
        {
            Type itype;
            Byte b = (Byte)HRList_GetItem_R(content, i, itype);
            f.write(b);
        }
        f.close();
    }
}
void External_FileReadAllBytes(UInt hrpath, UInt content)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f = LittleFS.open(buffer, "r");
    if (f && f.isFile()) 
    {
        while(f.available())
        {
            uint bytesRead = f.readBytes(buffer, pathBufferSize);
            for (uint i=0; i < bytesRead; i++)
            {
                HRList_Append(content, buffer[i], Type::eByte);
            }
        }
        f.close();
    }
}
UInt External_FileGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f = LittleFS.open(buffer, "r");
    if (f && f.isFile())
    {
        time_t lw = f.getLastWrite();
        unsigned int t = (unsigned int)lw;
        UInt result = HRLong_FromBytes(t & 0xFF, (t >> 8) & 0xFF, (t >> 16) & 0xFF, t >> 24);
        return result;
    }
    return HRLong_New();
}
UInt External_FileGetSize(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f = LittleFS.open(buffer, "r");
    if (f && f.isFile())
    {
        unsigned int s = f.size();
        UInt result = HRLong_FromBytes(s & 0xFF, (s >> 8) & 0xFF, (s >> 16) & 0xFF, s >> 24);
        return result;
    }
    return HRLong_New();
}

Bool External_DirectoryExists(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);

    if(LittleFS.exists(buffer))
    {
        File f = LittleFS.open(buffer, "r");
        return f.isDirectory();
    }
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
    if (LittleFS.exists(buffer2))
    {
        LittleFS.remove(buffer2);
    }
    LittleFS.rmdir(buffer);
}
void External_DirectoryCreate(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
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
UInt External_DirectoryGetTime(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    File f = LittleFS.open(buffer, "r");
    if (f && f.isDirectory())
    {
        time_t lw = f.getCreationTime();
        unsigned int t = (unsigned int)lw;
        Serial.println(t);
        UInt result = HRLong_FromBytes(t & 0xFF, (t >> 8) & 0xFF, (t >> 16) & 0xFF, t >> 24);
        return result;
    }
    return HRLong_New();
}
UInt External_DirectoryGetFileCount(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    Dir dir = LittleFS.openDir(buffer);
    UInt count = 0;
    while (dir.next()) 
    {
        if(dir.isFile())  
        {
            String name = dir.fileName();
            if (name == "_") continue;
            count++;
        }
    }
    return count;
}
UInt External_DirectoryGetDirectoryCount(UInt hrpath)
{
    char buffer[pathBufferSize];
    HRPathToBuffer(hrpath, (char*)&buffer);
    Dir dir = LittleFS.openDir(buffer);
    UInt count = 0;
    while (dir.next()) 
    {
        if(dir.isDirectory())  
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
    Dir dir = LittleFS.openDir(buffer);
    UInt count = 0;
    while (dir.next()) 
    {
        if (dir.isFile())  
        {
            if (count == index)
            {
                String name = dir.fileName();
                if (name == "_") continue;

                UInt str = HRString_New();
                uint plen = strlen(buffer);
                for (uint i=0; i < plen; i++)
                {
                    HRString_Build_R(str, buffer[i]);
                }
                if ((plen > 0) && (buffer[plen-1] != '/'))
                {
                    HRString_Build_R(str, '/');
                }
                for (uint i=0; i < name.length(); i++)
                {
                    HRString_Build_R(str, name[i]);
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
    Dir dir = LittleFS.openDir(buffer);
    UInt count = 0;
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
                    HRString_Build_R(str, buffer[i]);
                }
                if ((plen > 0) && (buffer[plen-1] != '/'))
                {
                    HRString_Build_R(str, '/');
                }
                for (uint i=0; i < name.length(); i++)
                {
                    HRString_Build_R(str, name[i]);
                }
                return str;
            }
            count++;
        }
    }
    return HRString_New();
}







void HopperEntryPoint();



void setup() 
{
  Serial.begin(57600);
  delay(100);

  codeMemoryBlock = nullptr;
  dataMemoryBlock = nullptr;
  
  Machine_Initialize();
  Platform_Initialize();
  //Serial.println("Initialized");

  // flicker LED_BUILTIN to show that initialization completed
  pinMode(LED_BUILTIN, OUTPUT);
  for (int i = 0; i < 5; i++)
  {
    digitalWrite(LED_BUILTIN, HIGH);
    delay(50);
    digitalWrite(LED_BUILTIN, LOW);
    delay(50);
  }
#ifdef LOLIN_D1_MINI
  digitalWrite(LED_BUILTIN, HIGH); // high is "off" on the D1 Mini
#endif  
}

void loop() 
{
  if (!exited)
  {
    HopperEntryPoint();

    Platform_Release();

    exited = true;
  }
}





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
    eCAST = 0x0051,
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
    eBITXOR = 0x0064,
    ePUSHIWLEI = 0x0065,
    eJIXB = 0x0068,
    eJIXW = 0x0069,
    eCALLIW = 0x006A,
    ePUSHIBLE = 0x006B,
    ePUSHIBEQ = 0x006C,
    eADDB = 0x006D,
    eSUBB = 0x006E,
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
    eIntToFloat = 0x0034,
    eIntToLong = 0x0035,
    eUIntToLong = 0x0036,
    eLongToBytes = 0x0039,
    eLongToFloat = 0x003A,
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
    eStringBuild = 0x0083,
    eSerialIsAvailableGet = 0x00A5,
    eSerialReadChar = 0x00A6,
    eSerialWriteChar = 0x00A7,
    eStringBuildFront = 0x00B5,
    eCharToDigit = 0x00BD,
    eTimeDelay = 0x00C6,
    eIntToBytes = 0x00CD,
    eFileGetTime = 0x00CE,
    eDirectoryGetTime = 0x00CF,
    eStringTrim = 0x00D0,
    eStringTrimLeft = 0x00D1,
    eStringTrimRight = 0x00D2,
    eMCUPinMode = 0x00D9,
    eMCUDigitalRead = 0x00DA,
    eMCUDigitalWrite = 0x00DB,
    eLongGetByte = 0x00E0,
    eIntGetByte = 0x00E1,
    eFloatGetByte = 0x00E2,
    eLongFromBytes = 0x00E3,
    eIntFromBytes = 0x00E4,
    eFloatFromBytes = 0x00E5,
    eUIntToFloat = 0x00E6,
    eDirectoryCreate = 0x00E9,
    eDirectoryDelete = 0x00EA,
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
Bool Runtime_LoadAuto_R(UInt & loadedAddress);
Byte Runtime_FromHex(Char ch);
void Runtime_WaitForEnter();
void Runtime_DumpPage(Byte iPage, Bool includeAddresses);
void Runtime_Out4Hex(UInt value);
void Runtime_Out2Hex(Byte value);
Bool Runtime_SerialLoadIHex_R(UInt & loadedAddress, UInt & codeLength);
Bool Runtime_TryReadSerialByte_R(Byte & data);
void HopperVM_Restart();
void HopperVM_Initialize(UInt loadedAddress);
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
UInt Memory_HeapStart_Get();
UInt Memory_HeapSize_Get();
UInt Memory_FreeList_Get();
void Memory_Free(UInt address);
void Memory_Initialize(UInt start, UInt size);
UInt Memory_Allocate(UInt size);
void Memory_Set(UInt memory, Byte value, UInt size);
Bool HRFile_Exists(UInt str);
UInt HRFile_Open(UInt hrpath);
Bool HRFile_IsValid(UInt _this);
Byte HRFile_Read(UInt _this);
UInt HRFile_Create(UInt hrpath);
void HRFile_Append(UInt _this, Byte b);
void HRFile_Flush(UInt _this);
UInt HRFile_New();
void HRFile_Delete(UInt path);
void GC_Release(UInt address);
UInt GC_New(UInt size, Type htype);
Char Char_ToHex(Byte h);
void Minimal_Error_Set(Byte value);
Byte Minimal_Error_Get();
Bool IO_IsBreak();
void IO_WriteLn();
void IO_Write(Char c);
void IO_PushKey(Char c);
void HRArray_Release();
void HRArray_Initialize();
UInt HRString_New();
void HRString_Build_R(UInt & _this, Char ch);
void HRString_Build_R(UInt & _this);
UInt HRString_Clone(UInt original);
UInt HRString_getCapacity(UInt _this);
UInt HRString_GetLength(UInt _this);
UInt HRString_clone(UInt original, UInt extra);
void Instructions_PopulateJumpTable(UInt jumpTable);
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
Bool Instructions_Cast();
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
Bool Instructions_JIXB();
Bool Instructions_JIXW();
Bool Instructions_CallIW();
Bool Instructions_PushIBLE();
Bool Instructions_PushIBEQ();
Bool Instructions_AddB();
Bool Instructions_SubB();
Bool HRDirectory_Exists(UInt hrpath);
void HRDirectory_Create(UInt hrpath);
void HRDirectory_Clear(UInt _this);
void Runtime_ErrorDump(UInt number);
UInt HRList_GetLength(UInt _this);
UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype);
void HRList_Append(UInt _this, UInt item, Type itype);
UInt HRList_New(Type htype);
void HRList_Clear(UInt _this);
UInt HRList_createItem(UInt itemData, Type etype, Type itype);
void HRList_clearAllItems(UInt pCurrent, Type etype);
void HRList_clearItem(UInt pCurrent, Type etype);
void HRFile_Clear(UInt _this);
void HRDictionary_Clear(UInt _this);
Bool HRDictionary_next_R(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt & value);
void HRPair_Clear(UInt _this);
void HRVariant_Clear(UInt _this);
UInt HRVariant_GetValue_R(UInt _this, Type & vtype);
UInt HRVariant_CreateValueVariant(UInt value, Type vtype);
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
Int HopperVM_ReadWordOffsetOperand();
UInt HopperVM_ReadWordOperand();
Bool HopperVM_ExecuteSysCall(Byte iSysCall, UInt iOverload);
UInt HopperVM_Get(UInt address);
Int HopperVM_PopI_R(Type & htype);
Bool Types_IsReferenceType(Type htype);
void GC_AddReference(UInt address);
UInt GC_Clone(UInt original);
void IO_WriteUInt(UInt _this);
void IO_Clear();
void IO_WriteHex(UInt u);
void IO_WriteHex(Byte b);
void IO_writeDigit(UInt uthis);
UInt HRFile_ReadLine(UInt _this);
Byte HRFile_Read(UInt _this, UInt hrseekpos);
void HRFile_Append(UInt _this, UInt hrstr);
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
void HRString_Build_R(UInt & _this, UInt append);
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
UInt HRArray_New(Type htype, UInt count);
UInt HRArray_GetItem_R(UInt _this, UInt index, Type & etype);
void HRArray_SetItem(UInt _this, UInt index, UInt value);
UInt HRArray_GetCount(UInt _this);
void HRList_SetItem(UInt _this, UInt index, UInt item, Type itype);
void HRList_Insert(UInt _this, UInt index, UInt item, Type itype);
void HRList_Remove(UInt _this, UInt index);
Bool HRList_Contains(UInt _this, UInt item, Type itype);
UInt HRList_Clone(UInt original);
UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt value);
UInt HRPair_GetValue_R(UInt _this, Type & vtype);
UInt HRPair_GetKey_R(UInt _this, Type & ktype);
UInt HRPair_Clone(UInt original);
UInt HRVariant_New(UInt value, Type vtype);
UInt HRVariant_Clone(UInt original);
Bool HRVariant_IsEqual(UInt left, Type ltype, UInt right, Type rtype);
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

Bool Runtime_loaded = false;
Byte Minimal_error = 0;
UInt Memory_heapStart = 0x8000;
UInt Memory_heapSize = 0x4000;
UInt Memory_freeList = 0;
UInt HRArray_setSlots = 0;
UInt HRArray_clearSlots = 0;
UInt HopperVM_binaryAddress = 0;
UInt HopperVM_constAddress = 0;
UInt HopperVM_methodTable = 0;
UInt HopperVM_valueStack = 0;
UInt HopperVM_typeStack = 0;
UInt HopperVM_callStack = 0;
UInt HopperVM_dataMemory = 0;
UInt HopperVM_breakpoints = 0;
Bool HopperVM_breakpointExists = false;
UInt HopperVM_currentDirectory = 0;
UInt HopperVM_pc = 0;
UInt HopperVM_sp = 0;
UInt HopperVM_bp = 0;
UInt HopperVM_csp = 0;
Bool HopperVM_cnp = false;
OpCode HopperVM_opCode = (OpCode)0;
UInt HopperVM_jumpTable = 0;
Bool IO_echoToLCD = false;
UInt IO_keyboardInPointer = 0;
UInt IO_keyboardOutPointer = 0;
    
void HopperEntryPoint()
{
    Runtime_MCU();
}

void Runtime_MCU()
{
    Runtime_loaded = false;
    HopperVM_Restart();
    Bool refresh = true;
    UInt loadedAddress = 0;
    if (External_LoadAuto_Get() && Runtime_LoadAuto_R(loadedAddress))
    {
        HopperVM_Initialize(loadedAddress);
        HopperVM_Restart();
        Runtime_loaded = true;
        HopperVM_ExecuteWarp();
    }
    Serial_WriteChar(Char(92));
    for (;;)
    {
        Char ch = 0;
        if (Serial_IsAvailable_Get())
        {
            ch = Serial_ReadChar();
        }
        if (ch == Char(27))
        {
            Serial_WriteChar(Char(92));
            ch = Serial_ReadChar();
            switch (ch)
            {
            case 'F':
            {
                Byte msn = Runtime_FromHex(Serial_ReadChar());
                Byte lsn = Runtime_FromHex(Serial_ReadChar());
                Runtime_WaitForEnter();
                Byte iPage = (msn << 0x04) + lsn;
                Runtime_DumpPage(iPage, false);
                Serial_WriteChar(Char(92));
                break;
            }
            case 'M':
            {
                Byte msn = Runtime_FromHex(Serial_ReadChar());
                Byte lsn = Runtime_FromHex(Serial_ReadChar());
                Runtime_WaitForEnter();
                Byte iPage = (msn << 0x04) + lsn;
                Runtime_DumpPage(iPage, true);
                Serial_WriteChar(Char(92));
                break;
            }
            case 'B':
            {
                Char arg = Serial_ReadChar();
                if (arg == 'X')
                {
                    HopperVM_ClearBreakpoints(false);
                }
                else
                {
                    Byte n = Runtime_FromHex(arg);
                    Byte a3 = Runtime_FromHex(Serial_ReadChar());
                    Byte a2 = Runtime_FromHex(Serial_ReadChar());
                    Byte a1 = Runtime_FromHex(Serial_ReadChar());
                    Byte a0 = Runtime_FromHex(Serial_ReadChar());
                    UInt address = (a3 << 0x0C) + (a2 << 0x08) + (a1 << 0x04) + a0;
                    HopperVM_SetBreakpoint(n, address);
                }
                Runtime_WaitForEnter();
                break;
            }
            case 'P':
            {
                Runtime_WaitForEnter();
                Serial_WriteChar(Char(13));
                Runtime_Out4Hex(HopperVM_PC_Get());
                Serial_WriteChar(Char(92));
                break;
            }
            case 'R':
            {
                Runtime_WaitForEnter();
                Serial_WriteChar(Char(13));
                Serial_WriteChar('P');
                Serial_WriteChar('C');
                Serial_WriteChar('=');
                Runtime_Out4Hex(HopperVM_PC_Get());
                Serial_WriteChar(' ');
                Serial_WriteChar('C');
                Serial_WriteChar('S');
                Serial_WriteChar('P');
                Serial_WriteChar('=');
                Runtime_Out2Hex(Byte(HopperVM_CSP_Get()));
                Serial_WriteChar(' ');
                Serial_WriteChar('S');
                Serial_WriteChar('P');
                Serial_WriteChar('=');
                Runtime_Out4Hex(HopperVM_SP_Get() + 0x0600);
                Serial_WriteChar(' ');
                Serial_WriteChar('T');
                Serial_WriteChar('S');
                Serial_WriteChar('P');
                Serial_WriteChar('=');
                Runtime_Out4Hex(HopperVM_SP_Get() + 0x0500);
                Serial_WriteChar(' ');
                Serial_WriteChar('B');
                Serial_WriteChar('P');
                Serial_WriteChar('=');
                Runtime_Out4Hex(HopperVM_BP_Get() + 0x0600);
                Serial_WriteChar(Char(92));
                break;
            }
            case 'L':
            {
                Runtime_WaitForEnter();
                loadedAddress = 0x00;
                UInt codeLength = 0;
                Runtime_loaded = Runtime_SerialLoadIHex_R(loadedAddress, codeLength);
                Serial_WriteChar(Char(13));
                if (Runtime_loaded)
                {
                    HopperVM_Initialize(loadedAddress);
                    HopperVM_Restart();
                    Runtime_Out4Hex(codeLength);
                    Serial_WriteChar(' ');
                    Runtime_Out4Hex(Memory_HeapStart_Get());
                    Serial_WriteChar(' ');
                    Runtime_Out4Hex(Memory_HeapSize_Get());
                    Serial_WriteChar(' ');
                }
                for (;;)
                {
                    ch = Serial_ReadChar();
                    if ((ch == '!') || (ch == '*'))
                    {
                        break;;
                    }
                }
                Serial_WriteChar(Char(13));
                Serial_WriteChar((Runtime_loaded) ? ('*') : ('!'));
                Serial_WriteChar(Char(92));
                if (Runtime_loaded)
                {
                    HopperVM_FlashProgram(loadedAddress, codeLength);
                }
                break;
            }
            default:
            {
                if (Runtime_loaded)
                {
                    switch (ch)
                    {
                    case 'O':
                    {
                        Runtime_WaitForEnter();
                        UInt pc = HopperVM_PC_Get();
                        OpCode opCode = OpCode(Memory_ReadCodeByte(pc));
                        if ((opCode == OpCode::eCALLW) || (opCode == OpCode::eCALLIW))
                        {
                            HopperVM_SetBreakpoint(0x00, pc + 0x03);
                            HopperVM_Execute();
                        }
                        else if (opCode == OpCode::eCALLB)
                        {
                            HopperVM_SetBreakpoint(0x00, pc + 0x02);
                            HopperVM_Execute();
                        }
                        else
                        {
                            HopperVM_ExecuteStepTo();
                        }
                        Serial_WriteChar(Char(92));
                        break;
                    }
                    case 'I':
                    {
                        Runtime_WaitForEnter();
                        HopperVM_ExecuteStepTo();
                        Serial_WriteChar(Char(92));
                        break;
                    }
                    case 'D':
                    {
                        Runtime_WaitForEnter();
                        HopperVM_Execute();
                        Serial_WriteChar(Char(92));
                        break;
                    }
                    case 'X':
                    {
                        Runtime_WaitForEnter();
                        HopperVM_ExecuteWarp();
                        Serial_WriteChar(Char(92));
                        break;
                    }
                    case 'W':
                    {
                        Runtime_WaitForEnter();
                        HopperVM_Restart();
                        break;
                    }
                    } // switch
                }
                break;
            }
            } // switch
        }
    }
    HopperVM_Release();
}

Bool Runtime_LoadAuto_R(UInt & loadedAddress)
{
    Bool success = false;
    loadedAddress = 0;
    UInt address = 0x00;
    UInt path = HopperVM_GetAppName();
    if (HRFile_Exists(path))
    {
        UInt autoHexe = HRFile_Open(path);
        if (HRFile_IsValid(autoHexe))
        {
            while (HRFile_IsValid(autoHexe))
            {
                Byte dataByte = HRFile_Read(autoHexe);
                Memory_WriteCodeByte(0 + address, dataByte);
                
                address++;
            }
            success = true;
            GC_Release(autoHexe);
        }
    }
    GC_Release(path);
    return success;
}

Byte Runtime_FromHex(Char ch)
{
    switch (ch)
    {
    case '0':
    {
        return 0x00;
        break;
    }
    case '1':
    {
        return 0x01;
        break;
    }
    case '2':
    {
        return 0x02;
        break;
    }
    case '3':
    {
        return 0x03;
        break;
    }
    case '4':
    {
        return 0x04;
        break;
    }
    case '5':
    {
        return 0x05;
        break;
    }
    case '6':
    {
        return 0x06;
        break;
    }
    case '7':
    {
        return 0x07;
        break;
    }
    case '8':
    {
        return 0x08;
        break;
    }
    case '9':
    {
        return 0x09;
        break;
    }
    case 'a':
    case 'A':
    {
        return 0x0A;
        break;
    }
    case 'b':
    case 'B':
    {
        return 0x0B;
        break;
    }
    case 'c':
    case 'C':
    {
        return 0x0C;
        break;
    }
    case 'd':
    case 'D':
    {
        return 0x0D;
        break;
    }
    case 'e':
    case 'E':
    {
        return 0x0E;
        break;
    }
    case 'f':
    case 'F':
    {
        return 0x0F;
        break;
    }
    } // switch
    return 0x00;
}

void Runtime_WaitForEnter()
{
    for (;;)
    {
        Char ch = Serial_ReadChar();
        if (ch == Char(13))
        {
            break;;
        }
    }
    Serial_WriteChar(Char(92));
}

void Runtime_DumpPage(Byte iPage, Bool includeAddresses)
{
    UInt rowAddress = (iPage << 0x08);;
    for (Byte row = 0x00; row < 0x10; row++)
    {
        Serial_WriteChar(Char(13));
        if (includeAddresses)
        {
            Runtime_Out4Hex(rowAddress);
            Serial_WriteChar(' ');
            rowAddress = rowAddress + 0x10;
        }
        if (iPage == 0x00)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte data = 0;
                Byte address = col + (row << 0x04);
                switch (address)
                {
                case 0xB1:
                {
                    data = Byte(HopperVM_PC_Get() >> 0x08);
                    break;
                }
                case 0xB0:
                {
                    data = Byte(HopperVM_PC_Get() & 0xFF);
                    break;
                }
                case 0xB3:
                {
                    data = Byte((HopperVM_SP_Get() + 0x0600) >> 0x08);
                    break;
                }
                case 0xB2:
                {
                    data = Byte((HopperVM_SP_Get() + 0x0600) & 0xFF);
                    break;
                }
                case 0xB5:
                {
                    data = Byte(((HopperVM_SP_Get() / 0x02) + 0x0500) >> 0x08);
                    break;
                }
                case 0xB4:
                {
                    data = Byte(((HopperVM_SP_Get() / 0x02) + 0x0500) & 0xFF);
                    break;
                }
                case 0xB7:
                {
                    data = Byte((HopperVM_BP_Get() + 0x0600) >> 0x08);
                    break;
                }
                case 0xB6:
                {
                    data = Byte((HopperVM_BP_Get() + 0x0600) & 0xFF);
                    break;
                }
                case 0xB9:
                {
                    data = Byte((HopperVM_CSP_Get() + 0x0400) >> 0x08);
                    break;
                }
                case 0xB8:
                {
                    data = Byte((HopperVM_CSP_Get() + 0x0400) & 0xFF);
                    break;
                }
                case 0xBB:
                {
                    data = Byte(HopperFlags::eMCUPlatform);
                    if (HopperVM_BreakpointExists_Get())
                    {
                        data = data | Byte(HopperFlags::eBreakpointsSet);
                    }
                    break;
                }
                case 0xE9:
                {
                    data = Byte(Memory_FreeList_Get() >> 0x08);
                    break;
                }
                case 0xE8:
                {
                    data = Byte(Memory_FreeList_Get() & 0xFF);
                    break;
                }
                case 0xEB:
                {
                    data = Byte(Memory_HeapSize_Get() >> 0x08);
                    break;
                }
                case 0xEA:
                {
                    data = Byte(Memory_HeapStart_Get() >> 0x08);
                    break;
                }
                case 0xCA:
                {
                    data = 0x00;
                    break;
                }
                default:
                {
                    if ((address >= 0x50) && (address <= 0x5F))
                    {
                        data = Byte(HopperVM_GetBreakpoint(address - 0x50) & 0xFF);
                    }
                    else if ((address >= 0x60) && (address <= 0x6F))
                    {
                        data = Byte(HopperVM_GetBreakpoint(address - 0x60) >> 0x08);
                    }
                    else
                    {
                        data = 0x00;
                    }
                    break;
                }
                } // switch
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x08)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(data);
            }
        }
        else if (iPage == 0x04)
        {;
            for (Byte col = 0x00; col < 0x08; col++)
            {
                UInt address = col * 0x02 + (row << 0x04);
                UInt stackData = HopperVM_GetCS(address);
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x04)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(Byte(stackData & 0xFF));
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                }
                Runtime_Out2Hex(Byte(stackData >> 0x08));
            }
        }
        else if (iPage == 0x05)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                UInt address = col + (row << 0x04);
                address = address * 0x02;
                Type htype = (Type)0;
                UInt stackData = HopperVM_Get_R(address, htype);
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x08)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(Byte(htype));
            }
        }
        else if (iPage == 0x06)
        {;
            for (Byte col = 0x00; col < 0x08; col++)
            {
                UInt address = col * 0x02 + (row << 0x04);
                Type htype = (Type)0;
                UInt stackData = HopperVM_Get_R(address, htype);
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x04)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(Byte(stackData & 0xFF));
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                }
                Runtime_Out2Hex(Byte(stackData >> 0x08));
            }
        }
        else if (iPage == 0x07)
        {;
            for (Byte col = 0x00; col < 0x08; col++)
            {
                UInt address = col * 0x02 + (row << 0x04);
                address = address + 0x0100;
                Type htype = (Type)0;
                UInt stackData = HopperVM_Get_R(address, htype);
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x04)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(Byte(stackData & 0xFF));
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                }
                Runtime_Out2Hex(Byte(stackData >> 0x08));
            }
        }
        else if (iPage > 0x07)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                UInt address = col + (row << 0x04);
                address = address + (0x0100 * iPage);
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x08)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(Memory_ReadByte(address));
            }
        }
        else
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                if (includeAddresses)
                {
                    Serial_WriteChar(' ');
                    if (col == 0x08)
                    {
                        Serial_WriteChar(' ');
                    }
                }
                Runtime_Out2Hex(0x00);
            }
        }
    }
    Serial_WriteChar(Char(13));
}

void Runtime_Out4Hex(UInt value)
{
    Byte b = Byte(value >> 0x0C);
    Serial_WriteChar(Char_ToHex(b));
    b = Byte((value >> 0x08) & 0x0F);
    Serial_WriteChar(Char_ToHex(b));
    b = Byte((value >> 0x04) & 0x0F);
    Serial_WriteChar(Char_ToHex(b));
    b = Byte(value & 0x0F);
    Serial_WriteChar(Char_ToHex(b));
}

void Runtime_Out2Hex(Byte value)
{
    Byte b = Byte((value >> 0x04) & 0x0F);
    Serial_WriteChar(Char_ToHex(b));
    b = Byte(value & 0x0F);
    Serial_WriteChar(Char_ToHex(b));
}

Bool Runtime_SerialLoadIHex_R(UInt & loadedAddress, UInt & codeLength)
{
    Bool success = true;
    loadedAddress = 0;
    codeLength = 0x00;
    for (;;)
    {
        Char colon = Serial_ReadChar();
        if (colon != ':')
        {
            success = false;
            break;;
        }
        Byte byteCount = 0;
        if (!Runtime_TryReadSerialByte_R(byteCount))
        {
            success = false;
            break;;
        }
        Byte lsb = 0;
        Byte msb = 0;
        if (!Runtime_TryReadSerialByte_R(msb))
        {
            success = false;
            break;;
        }
        if (!Runtime_TryReadSerialByte_R(lsb))
        {
            success = false;
            break;;
        }
        UInt recordAddress = lsb + (msb << 0x08);
        Byte recordType = 0;
        if (!Runtime_TryReadSerialByte_R(recordType))
        {
            success = false;
            break;;
        }
        switch (recordType)
        {
        case 0x00:
        {;
            for (UInt c = 0x00; c < byteCount; c++)
            {
                Byte dataByte = 0;
                if (!Runtime_TryReadSerialByte_R(dataByte))
                {
                    success = false;
                    break;;
                }
                Memory_WriteCodeByte(0 + recordAddress, dataByte);
                
                codeLength++;
                
                recordAddress++;
            }
            Byte checkSum = 0;
            if (!Runtime_TryReadSerialByte_R(checkSum))
            {
                success = false;
                break;;
            }
            Char eol = Serial_ReadChar();
            if ((eol != Char(0x0D)) && (eol != Char(0x0A)))
            {
                success = false;
                break;;
            }
            continue;;
            break;
        }
        case 0x01:
        {
            Byte checkSum = 0;
            if (!Runtime_TryReadSerialByte_R(checkSum))
            {
                success = false;
                break;;
            }
            break;;
            break;
        }
        default:
        {
            success = false;
            break;;
            break;
        }
        } // switch
        break;;
    }
    return success;
}

Bool Runtime_TryReadSerialByte_R(Byte & data)
{
    Char c0 = Serial_ReadChar();
    Char c1 = Serial_ReadChar();
    Byte msn = Runtime_FromHex(c0);
    Byte lsn = Runtime_FromHex(c1);
    data = (msn << 0x04) + lsn;
    return true;
}

void HopperVM_Restart()
{
    HopperVM_DataMemoryReset();
    HopperVM_DiskSetup();
    HopperVM_sp = 0x00;
    HopperVM_bp = 0x00;
    HopperVM_csp = 0x00;
    Minimal_Error_Set(0x00);
    HopperVM_cnp = false;
    HopperVM_pc = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x04);
}

void HopperVM_Initialize(UInt loadedAddress)
{
    HopperVM_binaryAddress = loadedAddress;
    HopperVM_constAddress = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x02);
    HopperVM_methodTable = HopperVM_binaryAddress + 0x06;
}

void HopperVM_ExecuteWarp()
{
    Bool doNext = false;
    UInt watchDog = 0x09C4;
    for (;;)
    {
        HopperVM_opCode = OpCode(Memory_ReadCodeByte(HopperVM_pc));
        
        HopperVM_pc++;
        doNext = External_FunctionCall(HopperVM_jumpTable, Byte(HopperVM_opCode));
        
        watchDog--;
        if (watchDog == 0x00)
        {
            External_WatchDog();
            watchDog = 0x09C4;
            if (IO_IsBreak())
            {
                IO_WriteLn();
                IO_Write('B');
                IO_Write('R');
                IO_Write('E');
                IO_Write('A');
                IO_Write('K');
                IO_WriteLn();
                break;;
            }
        }
        if (doNext)
        {
            continue;;
        }
        if (Minimal_Error_Get() != 0x00)
        {
            break;;
        }
        if (HopperVM_PC_Get() == 0x00)
        {
            HopperVM_Restart();
            break;;
        }
        if (IO_IsBreak())
        {
            IO_WriteLn();
            IO_Write('B');
            IO_Write('R');
            IO_Write('E');
            IO_Write('A');
            IO_Write('K');
            IO_WriteLn();
            break;;
        }
    }
}

void HopperVM_ClearBreakpoints(Bool includingZero)
{;
    for (Byte i = 0x02; i < 0x20; i = i + 0x02)
    {
        Memory_WriteWord(HopperVM_breakpoints + i, 0x00);
    }
    if (includingZero)
    {
        Memory_WriteWord(HopperVM_breakpoints, 0x00);
        HopperVM_breakpointExists = false;
    }
    else
    {
        HopperVM_breakpointExists = Memory_ReadWord(HopperVM_breakpoints) != 0x00;
    }
}

void HopperVM_SetBreakpoint(Byte n, UInt address)
{
    Memory_WriteWord(HopperVM_breakpoints + n * 0x02, address);
    if (address != 0x00)
    {
        HopperVM_breakpointExists = true;
    }
    else
    {
        HopperVM_breakpointExists = false;;
        for (Byte i = 0x00; i < 0x20; i = i + 0x02)
        {
            if (Memory_ReadWord(HopperVM_breakpoints + i) != 0x00)
            {
                HopperVM_breakpointExists = true;
                break;;
            }
        }
    }
}

UInt HopperVM_PC_Get()
{
    return HopperVM_pc;
}

UInt HopperVM_CSP_Get()
{
    return HopperVM_csp;
}

UInt HopperVM_SP_Get()
{
    return HopperVM_sp;
}

UInt HopperVM_BP_Get()
{
    return HopperVM_bp;
}

void HopperVM_FlashProgram(UInt codeLocation, UInt codeLength)
{
    UInt path = HopperVM_GetAppName();
    UInt appFile = HRFile_Create(path);;
    for (UInt i = 0x00; i < codeLength; i++)
    {
        HRFile_Append(appFile, Memory_ReadCodeByte(codeLocation + i));
    }
    HRFile_Flush(appFile);
    GC_Release(path);
}

void HopperVM_Execute()
{
    UInt messagePC = 0;
    for (;;)
    {
        messagePC = HopperVM_pc;
        Bool doNext = HopperVM_ExecuteOpCode();
        if (Minimal_Error_Get() != 0x00)
        {
            break;;
        }
        if (HopperVM_pc == 0x00)
        {
            HopperVM_Restart();
            break;;
        }
        if (IO_IsBreak())
        {
            IO_WriteLn();
            IO_Write('B');
            IO_Write('R');
            IO_Write('E');
            IO_Write('A');
            IO_Write('K');
            IO_WriteLn();
            break;;
        }
        if (HopperVM_BreakpointExists_Get())
        {
            Byte iBreak = 0;
            Bool atBreakpoint = false;;
            for (iBreak = 0x00; iBreak < 0x10; iBreak++)
            {
                if (HopperVM_GetBreakpoint(iBreak) == HopperVM_pc)
                {
                    if (iBreak == 0x00)
                    {
                        HopperVM_SetBreakpoint(0x00, 0x00);
                    }
                    return;
                }
            }
        }
        External_WatchDog();
    }
}

void HopperVM_ExecuteStepTo()
{
    UInt messagePC = HopperVM_pc;
    Bool doNext = HopperVM_ExecuteOpCode();
    if (Minimal_Error_Get() != 0x00)
    {
    }
    else if (HopperVM_pc == 0x00)
    {
        HopperVM_Restart();
    }
}

void HopperVM_Release()
{
    HRArray_Release();
    Memory_Free(HopperVM_breakpoints);
    HopperVM_breakpoints = 0x00;
    if (HopperVM_currentDirectory != 0x00)
    {
        GC_Release(HopperVM_currentDirectory);
        HopperVM_currentDirectory = 0x00;
    }
}

UInt HopperVM_GetAppName()
{
    UInt path = HRString_New();
    HRString_Build_R(path, Char('/'));
    HRString_Build_R(path, Char('B'));
    HRString_Build_R(path, Char('i'));
    HRString_Build_R(path, Char('n'));
    HRString_Build_R(path, Char('/'));
    HRString_Build_R(path, Char('A'));
    HRString_Build_R(path, Char('u'));
    HRString_Build_R(path, Char('t'));
    HRString_Build_R(path, Char('o'));
    HRString_Build_R(path, Char('.'));
    HRString_Build_R(path, Char('h'));
    HRString_Build_R(path, Char('e'));
    HRString_Build_R(path, Char('x'));
    HRString_Build_R(path, Char('e'));
    return path;
}

Bool HopperVM_BreakpointExists_Get()
{
    return HopperVM_breakpointExists;
}

UInt HopperVM_GetBreakpoint(Byte n)
{
    return Memory_ReadWord(HopperVM_breakpoints + n * 0x02);
}

UInt HopperVM_GetCS(UInt address)
{
    return Memory_ReadWord(HopperVM_callStack + address);
}

UInt HopperVM_Get_R(UInt address, Type & htype)
{
    UInt value = Memory_ReadWord(HopperVM_valueStack + address);
    htype = Type(Memory_ReadWord(HopperVM_typeStack + address));
    return value;
}

void HopperVM_DataMemoryReset()
{
    HRArray_Release();
    UInt nextAddress = 0;
    HopperVM_callStack = nextAddress;
    nextAddress = nextAddress + 512;
    HopperVM_valueStack = nextAddress;
    nextAddress = nextAddress + 512;
    HopperVM_typeStack = nextAddress;
    nextAddress = nextAddress + 512;
    HopperVM_jumpTable = nextAddress;
    nextAddress = nextAddress + 512;
    Instructions_PopulateJumpTable(HopperVM_jumpTable);
    HopperVM_dataMemory = nextAddress;
    if (HopperVM_dataMemory < 0x0800)
    {
        HopperVM_dataMemory = 0x0800;
    }
    Memory_Initialize(HopperVM_dataMemory, ((External_GetSegmentSizes() - 0x02) << 0x01) - HopperVM_dataMemory);
    HopperVM_breakpoints = Memory_Allocate(0x20);
    HopperVM_ClearBreakpoints(true);
    HRArray_Initialize();
    HopperVM_currentDirectory = HRString_New();
}

void HopperVM_DiskSetup()
{
    UInt path = HRString_New();
    HRString_Build_R(path, Char('/'));
    HRString_Build_R(path, Char('B'));
    HRString_Build_R(path, Char('i'));
    HRString_Build_R(path, Char('n'));
    if (!HRDirectory_Exists(path))
    {
        HRDirectory_Create(path);
    }
    HRString_Build_R(path);
    HRString_Build_R(path, Char('/'));
    HRString_Build_R(path, Char('T'));
    HRString_Build_R(path, Char('e'));
    HRString_Build_R(path, Char('m'));
    HRString_Build_R(path, Char('p'));
    if (!HRDirectory_Exists(path))
    {
        HRDirectory_Create(path);
    }
    GC_Release(path);
}

Bool HopperVM_ExecuteOpCode()
{
    Bool doNext = false;
    HopperVM_opCode = OpCode(Memory_ReadCodeByte(HopperVM_pc));
    
    HopperVM_pc++;
    doNext = External_FunctionCall(HopperVM_jumpTable, Byte(HopperVM_opCode));
    return doNext;
}

UInt Memory_HeapStart_Get()
{
    return Memory_heapStart;
}

UInt Memory_HeapSize_Get()
{
    return Memory_heapSize;
}

UInt Memory_FreeList_Get()
{
    return Memory_freeList;
}

void Memory_Free(UInt address)
{
    for (;;)
    {
        if (0x00 == address)
        {
            Runtime_ErrorDump(0x01);
            Minimal_Error_Set(0x0B);
            break;;
        }
        UInt blockAddress = address - 0x02;
        UInt size = Memory_ReadWord(blockAddress);
        UInt current = Memory_freeList;
        UInt previous = 0x00;
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            if (current > address)
            {
                break;;
            }
            previous = current;
            UInt currentNext = Memory_ReadWord(current + 0x02);
            current = currentNext;
        }
        UInt currentPrev = previous;
        UInt currentSize = 0x00;
        UInt currentNext = 0x00;
        if (0x00 != current)
        {
            currentSize = Memory_ReadWord(current);
            currentNext = Memory_ReadWord(current + 0x02);
        }
        UInt freeSlot = address - 0x02;
        if (0x00 == currentPrev)
        {
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(freeSlot + 0x04, 0x00);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt gapFront = Memory_freeList - (freeSlot + size);
            if (0x00 == gapFront)
            {
                UInt nextSize = Memory_ReadWord(Memory_freeList);
                UInt nextNext = Memory_ReadWord(Memory_freeList + 0x02);
                Memory_WriteWord(freeSlot, size + nextSize);
                Memory_WriteWord(freeSlot + 0x02, nextNext);
                if (0x00 != nextNext)
                {
                    Memory_WriteWord(nextNext + 0x04, freeSlot);
                }
            }
            Memory_freeList = freeSlot;
        }
        else if (0x00 == current)
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, 0x00);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, 0x00);
            }
        }
        else
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, current);
                Memory_WriteWord(current + 0x04, currentPrev);
                freeSlot = currentPrev;
                size = prevSize + size;
            }
            UInt gapNext = current - (freeSlot + size);
            if (0x00 == gapNext)
            {
                Memory_WriteWord(freeSlot, size + currentSize);
                Memory_WriteWord(freeSlot + 0x02, currentNext);
                if (0x00 != currentNext)
                {
                    Memory_WriteWord(currentNext + 0x04, freeSlot);
                }
            }
        }
        break;;
    }
}

void Memory_Initialize(UInt start, UInt size)
{
    Memory_heapStart = start;
    Memory_heapSize = size;
    Memory_freeList = Memory_heapStart;
    Memory_Set(Memory_freeList, 0x00, Memory_heapSize);
    Memory_WriteWord(Memory_freeList, Memory_heapSize);
    Memory_WriteWord(Memory_freeList + 0x02, 0x00);
    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
}

UInt Memory_Allocate(UInt size)
{
    UInt address = 0;
    for (;;)
    {
        if (0x00 == size)
        {
            Minimal_Error_Set(0x0C);
            break;;
        }
        UInt best = 0;
        UInt bestSize = 0;
        UInt bestNext = 0;
        UInt bestPrev = 0;
        UInt current = Memory_freeList;
        
        size++;
        
        size++;
        if (size < 0x06)
        {
            size = 0x06;
        }
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            UInt currentSize = Memory_ReadWord(current);
            UInt currentNext = Memory_ReadWord(current + 0x02);
            UInt currentPrev = Memory_ReadWord(current + 0x04);
            if ((currentSize >= size) && ((0x00 == bestSize) || (currentSize < bestSize)))
            {
                best = current;
                bestSize = currentSize;
                bestNext = currentNext;
                bestPrev = currentPrev;
            }
            if (bestSize == size)
            {
                break;;
            }
            current = currentNext;
        }
        address = best + 0x02;
        if (bestSize >= size + 0x06)
        {
            Memory_WriteWord(best, size);
            UInt newHole = best + size;
            UInt newHoleSize = bestSize - size;
            Memory_WriteWord(newHole, newHoleSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = newHole;
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, 0x00);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
            else
            {
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, bestPrev);
                Memory_WriteWord(bestPrev + 0x02, newHole);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
        }
        else if (bestSize >= size)
        {
            Memory_WriteWord(best, bestSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = bestNext;
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
                }
            }
            else
            {
                Memory_WriteWord(bestPrev + 0x02, bestNext);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, bestPrev);
                }
            }
        }
        else
        {
            Minimal_Error_Set(0x0C);
            address = 0x00;
        }
        break;;
    }
    return address;
}

void Memory_Set(UInt memory, Byte value, UInt size)
{;
    for (UInt i = 0x00; i < size; i++)
    {
        Memory_WriteByte(memory + i, value);
    }
}

Bool HRFile_Exists(UInt str)
{
    return External_FileExists(str);
}

UInt HRFile_Open(UInt hrpath)
{
    UInt address = HRFile_New();
    if (HRFile_Exists(hrpath))
    {
        Memory_WriteByte(address + 2, 0x01);
        Memory_WriteByte(address + 3, 0x01);
        GC_Release(Memory_ReadWord(address + 5));
        Memory_WriteWord(address + 5, HRString_Clone(hrpath));
        External_FileReadAllBytes(Memory_ReadWord(address + 5), Memory_ReadWord(address + 9));
    }
    return address;
}

Bool HRFile_IsValid(UInt _this)
{
    return (Memory_ReadByte(_this + 2) != 0x00);
}

Byte HRFile_Read(UInt _this)
{
    Byte b = 0;
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt buffer = Memory_ReadWord(_this + 9);
            UInt pos = Memory_ReadWord(_this + 7);
            UInt length = HRList_GetLength(buffer);
            if (pos < length)
            {
                Type itype = (Type)0;
                b = Byte(HRList_GetItem_R(buffer, pos, itype));
                
                pos++;
                Memory_WriteWord(_this + 7, pos);
                break;;
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return b;
}

UInt HRFile_Create(UInt hrpath)
{
    if (HRFile_Exists(hrpath))
    {
        HRFile_Delete(hrpath);
    }
    UInt address = HRFile_New();
    Memory_WriteByte(address + 2, 0x01);
    Memory_WriteByte(address + 4, 0x01);
    GC_Release(Memory_ReadWord(address + 5));
    Memory_WriteWord(address + 5, HRString_Clone(hrpath));
    return address;
}

void HRFile_Append(UInt _this, Byte b)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00))
    {
        UInt buffer = Memory_ReadWord(_this + 9);
        HRList_Append(buffer, b, Type::eByte);
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

void HRFile_Flush(UInt _this)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00))
    {
        External_FileWriteAllBytes(Memory_ReadWord(_this + 5), Memory_ReadWord(_this + 9));
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

UInt HRFile_New()
{
    UInt address = GC_New(0x0B, Type::eFile);
    Memory_WriteByte(address + 2, 0x00);
    Memory_WriteByte(address + 3, 0x00);
    Memory_WriteByte(address + 4, 0x00);
    Memory_WriteWord(address + 5, HRString_New());
    Memory_WriteWord(address + 7, 0x00);
    Memory_WriteWord(address + 9, HRList_New(Type::eByte));
    return address;
}

void HRFile_Delete(UInt path)
{
    External_FileDelete(path);
}

void GC_Release(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    
    referenceCount--;
    Memory_WriteByte(address + 0x01, referenceCount);
    Type htype = Type(Memory_ReadByte(address));
    if (referenceCount == 0x00)
    {
        switch (htype)
        {
        case Type::eArray:
        case Type::eLong:
        case Type::eFloat:
        case Type::eString:
        {
            Memory_Free(address);
            break;
        }
        case Type::eDirectory:
        {
            HRDirectory_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eFile:
        {
            HRFile_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eList:
        {
            HRList_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eDictionary:
        {
            HRDictionary_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::ePair:
        {
            HRPair_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eVariant:
        {
            HRVariant_Clear(address);
            Memory_Free(address);
            break;
        }
        default:
        {
            break;
        }
        } // switch
    }
}

UInt GC_New(UInt size, Type htype)
{
    UInt address = Memory_Allocate(size + 0x02);
    Memory_WriteByte(address, Byte(htype));
    Memory_WriteByte(address + 0x01, 0x01);
    return address;
}

Char Char_ToHex(Byte h)
{
    if (h < 0x0A)
    {
        h = h + 0x30;
    }
    else
    {
        h = h + 0x37;
    }
    return Char(h);
}

void Minimal_Error_Set(Byte value)
{
    Minimal_error = value;
    Diagnostics_SetError(Minimal_error);
}

Byte Minimal_Error_Get()
{
    return Minimal_error;
}

Bool IO_IsBreak()
{
    while (Serial_IsAvailable_Get())
    {
        Char ch = Serial_ReadChar();
        if (ch == Char(0x03))
        {
            return true;
        }
        IO_PushKey(ch);
    }
    return false;
}

void IO_WriteLn()
{
    IO_Write(Char(0x0D));
}

void IO_Write(Char c)
{
    Serial_WriteChar(c);
}

void IO_PushKey(Char c)
{
    Byte k = Byte(c);
    Memory_WriteByte(61440 + IO_keyboardInPointer, k);
    if (IO_keyboardInPointer == 0x0FFF)
    {
        IO_keyboardInPointer = 0x00;
    }
    else
    {
        
        IO_keyboardInPointer++;
    }
}

void HRArray_Release()
{
    if (HRArray_setSlots != 0x00)
    {
        Memory_Free(HRArray_setSlots);
    }
    if (HRArray_clearSlots != 0x00)
    {
        Memory_Free(HRArray_clearSlots);
    }
}

void HRArray_Initialize()
{
    HRArray_setSlots = Memory_Allocate(0x08);
    HRArray_clearSlots = Memory_Allocate(0x08);
    Memory_WriteByte(HRArray_setSlots + 0x00, 0x01);
    Memory_WriteByte(HRArray_setSlots + 0x01, 0x02);
    Memory_WriteByte(HRArray_setSlots + 0x02, 0x04);
    Memory_WriteByte(HRArray_setSlots + 0x03, 0x08);
    Memory_WriteByte(HRArray_setSlots + 0x04, 0x10);
    Memory_WriteByte(HRArray_setSlots + 0x05, 0x20);
    Memory_WriteByte(HRArray_setSlots + 0x06, 0x40);
    Memory_WriteByte(HRArray_setSlots + 0x07, 0x80);
    Memory_WriteByte(HRArray_clearSlots + 0x00, 0xFE);
    Memory_WriteByte(HRArray_clearSlots + 0x01, 0xFD);
    Memory_WriteByte(HRArray_clearSlots + 0x02, 0xFB);
    Memory_WriteByte(HRArray_clearSlots + 0x03, 0xF7);
    Memory_WriteByte(HRArray_clearSlots + 0x04, 0xEF);
    Memory_WriteByte(HRArray_clearSlots + 0x05, 0xDF);
    Memory_WriteByte(HRArray_clearSlots + 0x06, 0xBF);
    Memory_WriteByte(HRArray_clearSlots + 0x07, 0x7F);
}

UInt HRString_New()
{
    UInt address = GC_New(0x02, Type::eString);
    Memory_WriteWord(address + 0x02, 0x00);
    return address;
}

void HRString_Build_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 0x01)
    {
        UInt copy = HRString_clone(_this, 0x01);
        GC_Release(_this);
        _this = copy;
    }
    Memory_WriteByte(_this + 0x04 + length, Byte(ch));
    Memory_WriteWord(_this + 0x02, length + 0x01);
}

void HRString_Build_R(UInt & _this)
{
    Memory_WriteWord(_this + 0x02, 0x00);
}

UInt HRString_Clone(UInt original)
{
    return HRString_clone(original, 0x00);
}

UInt HRString_getCapacity(UInt _this)
{
    return Memory_ReadWord(_this - 0x02) - 0x06;
}

UInt HRString_GetLength(UInt _this)
{
    return Memory_ReadWord(_this + 0x02);
}

UInt HRString_clone(UInt original, UInt extra)
{
    UInt length = Memory_ReadWord(original + 0x02);
    UInt address = GC_New(0x04 + length + extra, Type::eString);
    Memory_WriteWord(address + 0x02, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(address + 0x04 + i, Memory_ReadByte(original + 0x04 + i));
    }
    return address;
}

void Instructions_PopulateJumpTable(UInt jumpTable)
{
    InstructionDelegate instructionDelegate = &Instructions_Add;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADD), instructionDelegate);
    instructionDelegate = &Instructions_Sub;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUB), instructionDelegate);
    instructionDelegate = &Instructions_Div;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDIV), instructionDelegate);
    instructionDelegate = &Instructions_Mul;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMUL), instructionDelegate);
    instructionDelegate = &Instructions_Mod;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMOD), instructionDelegate);
    instructionDelegate = &Instructions_EQ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eEQ), instructionDelegate);
    instructionDelegate = &Instructions_NE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eNE), instructionDelegate);
    instructionDelegate = &Instructions_GT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGT), instructionDelegate);
    instructionDelegate = &Instructions_LT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLT), instructionDelegate);
    instructionDelegate = &Instructions_GE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGE), instructionDelegate);
    instructionDelegate = &Instructions_LE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLE), instructionDelegate);
    instructionDelegate = &Instructions_BoolOr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLOR), instructionDelegate);
    instructionDelegate = &Instructions_BoolAnd;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLAND), instructionDelegate);
    instructionDelegate = &Instructions_BitOr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITOR), instructionDelegate);
    instructionDelegate = &Instructions_BitAnd;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITAND), instructionDelegate);
    instructionDelegate = &Instructions_BitShl;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITSHL), instructionDelegate);
    instructionDelegate = &Instructions_BitShr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITSHR), instructionDelegate);
    instructionDelegate = &Instructions_AddI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADDI), instructionDelegate);
    instructionDelegate = &Instructions_SubI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUBI), instructionDelegate);
    instructionDelegate = &Instructions_DivI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDIVI), instructionDelegate);
    instructionDelegate = &Instructions_MulI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMULI), instructionDelegate);
    instructionDelegate = &Instructions_ModI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMODI), instructionDelegate);
    instructionDelegate = &Instructions_GTI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGTI), instructionDelegate);
    instructionDelegate = &Instructions_LTI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLTI), instructionDelegate);
    instructionDelegate = &Instructions_GEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGEI), instructionDelegate);
    instructionDelegate = &Instructions_LEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLEI), instructionDelegate);
    instructionDelegate = &Instructions_PushIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIB), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PopRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPRELB), instructionDelegate);
    instructionDelegate = &Instructions_PushRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHRELB), instructionDelegate);
    instructionDelegate = &Instructions_PopGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PushGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PushStackAddrB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHSTACKADDRB), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_DecLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_Dup;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDUP), instructionDelegate);
    instructionDelegate = &Instructions_DecSP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECSP), instructionDelegate);
    instructionDelegate = &Instructions_RetB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETB), instructionDelegate);
    instructionDelegate = &Instructions_RetRetB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETRETB), instructionDelegate);
    instructionDelegate = &Instructions_CallB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLB), instructionDelegate);
    instructionDelegate = &Instructions_TestBPB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eTESTBPB), instructionDelegate);
    instructionDelegate = &Instructions_JZB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJZB), instructionDelegate);
    instructionDelegate = &Instructions_JNZB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJNZB), instructionDelegate);
    instructionDelegate = &Instructions_JB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJB), instructionDelegate);
    instructionDelegate = &Instructions_JZW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJZW), instructionDelegate);
    instructionDelegate = &Instructions_JNZW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJNZW), instructionDelegate);
    instructionDelegate = &Instructions_JW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJW), instructionDelegate);
    instructionDelegate = &Instructions_CallW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLW), instructionDelegate);
    instructionDelegate = &Instructions_PushIW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIW), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALBB), instructionDelegate);
    instructionDelegate = &Instructions_PushIWLE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIWLE), instructionDelegate);
    instructionDelegate = &Instructions_BoolNot;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLNOT), instructionDelegate);
    instructionDelegate = &Instructions_BitNot;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITNOT), instructionDelegate);
    instructionDelegate = &Instructions_Swap;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSWAP), instructionDelegate);
    instructionDelegate = &Instructions_PushI0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHI0), instructionDelegate);
    instructionDelegate = &Instructions_PushI1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHI1), instructionDelegate);
    instructionDelegate = &Instructions_PushIM1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIM1), instructionDelegate);
    instructionDelegate = &Instructions_PushGP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGP), instructionDelegate);
    instructionDelegate = &Instructions_Ret0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRET0), instructionDelegate);
    instructionDelegate = &Instructions_CallRel;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLREL), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB02;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB02), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB02;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB02), instructionDelegate);
    instructionDelegate = &Instructions_SysCall0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL0), instructionDelegate);
    instructionDelegate = &Instructions_SysCall1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL1), instructionDelegate);
    instructionDelegate = &Instructions_SysCall;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL), instructionDelegate);
    instructionDelegate = &Instructions_CNP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCOPYNEXTPOP), instructionDelegate);
    instructionDelegate = &Instructions_Enter;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eENTER), instructionDelegate);
    instructionDelegate = &Instructions_Cast;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCAST), instructionDelegate);
    instructionDelegate = &Instructions_PushIWLT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIWLT), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALBB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYRELB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB02;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB02), instructionDelegate);
    instructionDelegate = &Instructions_EnterB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eENTERB), instructionDelegate);
    instructionDelegate = &Instructions_PushIW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHDW), instructionDelegate);
    instructionDelegate = &Instructions_RetFast;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETFAST), instructionDelegate);
    instructionDelegate = &Instructions_PushIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHDB), instructionDelegate);
    instructionDelegate = &Instructions_BitXor;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITXOR), instructionDelegate);
    instructionDelegate = &Instructions_PushIWLEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIWLEI), instructionDelegate);
    instructionDelegate = &Instructions_JIXB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJIXB), instructionDelegate);
    instructionDelegate = &Instructions_JIXW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJIXW), instructionDelegate);
    instructionDelegate = &Instructions_CallIW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLIW), instructionDelegate);
    instructionDelegate = &Instructions_PushIBLE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIBLE), instructionDelegate);
    instructionDelegate = &Instructions_PushIBEQ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIBEQ), instructionDelegate);
    instructionDelegate = &Instructions_AddB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADDB), instructionDelegate);
    instructionDelegate = &Instructions_SubB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUBB), instructionDelegate);
}

Bool Instructions_Add()
{
    HopperVM_Push(HopperVM_Pop() + HopperVM_Pop(), Type::eUInt);
    return true;
}

Bool Instructions_Sub()
{
    UInt top = HopperVM_Pop();
    HopperVM_Push(HopperVM_Pop() - top, Type::eUInt);
    return true;
}

Bool Instructions_Div()
{
    UInt top = HopperVM_Pop();
    HopperVM_Push(HopperVM_Pop() / top, Type::eUInt);
    return true;
}

Bool Instructions_Mul()
{
    HopperVM_Push(HopperVM_Pop() * HopperVM_Pop(), Type::eUInt);
    return true;
}

Bool Instructions_Mod()
{
    UInt top = HopperVM_Pop();
    HopperVM_Push(HopperVM_Pop() % top, Type::eUInt);
    return true;
}

Bool Instructions_EQ()
{
    HopperVM_Push(((HopperVM_Pop() == HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_NE()
{
    HopperVM_Push(((HopperVM_Pop() != HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_GT()
{
    HopperVM_Push(((HopperVM_Pop() < HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_LT()
{
    HopperVM_Push(((HopperVM_Pop() > HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_GE()
{
    HopperVM_Push(((HopperVM_Pop() <= HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_LE()
{
    HopperVM_Push(((HopperVM_Pop() >= HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_BoolOr()
{
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push((((next != 0x00) || (top != 0x00))) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_BoolAnd()
{
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push((((next != 0x00) && (top != 0x00))) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_BitOr()
{
    HopperVM_Push(HopperVM_Pop() | HopperVM_Pop(), Type::eUInt);
    return true;
}

Bool Instructions_BitAnd()
{
    HopperVM_Push(HopperVM_Pop() & HopperVM_Pop(), Type::eUInt);
    return true;
}

Bool Instructions_BitShl()
{
    UInt top = HopperVM_Pop();
    UInt next = HopperVM_Pop();
    UInt result = 0;
    switch (top)
    {
    case 0x01:
    {
        result = (next << 0x01);
        break;
    }
    case 0x02:
    {
        result = (next << 0x02);
        break;
    }
    case 0x03:
    {
        result = (next << 0x03);
        break;
    }
    case 0x04:
    {
        result = (next << 0x04);
        break;
    }
    case 0x05:
    {
        result = (next << 0x05);
        break;
    }
    case 0x06:
    {
        result = (next << 0x06);
        break;
    }
    case 0x07:
    {
        result = (next << 0x07);
        break;
    }
    case 0x08:
    {
        result = (next << 0x08);
        break;
    }
    case 0x09:
    {
        result = (next << 0x09);
        break;
    }
    case 0x0A:
    {
        result = (next << 0x0A);
        break;
    }
    case 0x0B:
    {
        result = (next << 0x0B);
        break;
    }
    case 0x0C:
    {
        result = (next << 0x0C);
        break;
    }
    case 0x0D:
    {
        result = (next << 0x0D);
        break;
    }
    case 0x0E:
    {
        result = (next << 0x0E);
        break;
    }
    case 0x0F:
    {
        result = (next << 0x0F);
        break;
    }
    default:
    {
        Minimal_Error_Set(0x0B);
        Runtime_ErrorDump(0x4F);
        return false;
        break;
    }
    } // switch
    HopperVM_Push(result, Type::eUInt);
    return true;
}

Bool Instructions_BitShr()
{
    UInt top = HopperVM_Pop();
    UInt next = HopperVM_Pop();
    UInt result = 0;
    switch (top)
    {
    case 0x01:
    {
        result = (next >> 0x01);
        break;
    }
    case 0x02:
    {
        result = (next >> 0x02);
        break;
    }
    case 0x03:
    {
        result = (next >> 0x03);
        break;
    }
    case 0x04:
    {
        result = (next >> 0x04);
        break;
    }
    case 0x05:
    {
        result = (next >> 0x05);
        break;
    }
    case 0x06:
    {
        result = (next >> 0x06);
        break;
    }
    case 0x07:
    {
        result = (next >> 0x07);
        break;
    }
    case 0x08:
    {
        result = (next >> 0x08);
        break;
    }
    case 0x09:
    {
        result = (next >> 0x09);
        break;
    }
    case 0x0A:
    {
        result = (next >> 0x0A);
        break;
    }
    case 0x0B:
    {
        result = (next >> 0x0B);
        break;
    }
    case 0x0C:
    {
        result = (next >> 0x0C);
        break;
    }
    case 0x0D:
    {
        result = (next >> 0x0D);
        break;
    }
    case 0x0E:
    {
        result = (next >> 0x0E);
        break;
    }
    case 0x0F:
    {
        result = (next >> 0x0F);
        break;
    }
    default:
    {
        Minimal_Error_Set(0x0B);
        Runtime_ErrorDump(0x50);
        return false;
        break;
    }
    } // switch
    HopperVM_Push(result, Type::eUInt);
    return true;
}

Bool Instructions_AddI()
{
    HopperVM_PushI(HopperVM_PopI() + HopperVM_PopI());
    return true;
}

Bool Instructions_SubI()
{
    Int top = HopperVM_PopI();
    HopperVM_PushI(HopperVM_PopI() - top);
    return true;
}

Bool Instructions_DivI()
{
    Int top = HopperVM_PopI();
    HopperVM_PushI(HopperVM_PopI() / top);
    return true;
}

Bool Instructions_MulI()
{
    HopperVM_PushI(HopperVM_PopI() * HopperVM_PopI());
    return true;
}

Bool Instructions_ModI()
{
    Int top = HopperVM_PopI();
    HopperVM_PushI(HopperVM_PopI() % top);
    return true;
}

Bool Instructions_GTI()
{
    HopperVM_Push(((HopperVM_PopI() < HopperVM_PopI())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_LTI()
{
    HopperVM_Push(((HopperVM_PopI() > HopperVM_PopI())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_GEI()
{
    HopperVM_Push(((HopperVM_PopI() <= HopperVM_PopI())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_LEI()
{
    HopperVM_Push(((HopperVM_PopI() >= HopperVM_PopI())) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_PushIB()
{
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
    return true;
}

Bool Instructions_PopLocalB()
{
    Int offset = 0;
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB();
    }
    else
    {
        Int offset = HopperVM_ReadByteOffsetOperand();
        Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + offset)));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + offset));
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        Memory_WriteWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + offset), value);
        Memory_WriteWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + offset), UInt(htype));
    }
    return true;
}

Bool Instructions_PushLocalB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    UInt value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + offset));
    Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + offset)));
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PopRelB()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyRelB();
    }
    else
    {
        Int offset = HopperVM_ReadByteOffsetOperand();
        UInt referenceAddress = UInt(Int(HopperVM_BP_Get()) + offset);
        Type rtype = (Type)0;
        UInt localAddress = HopperVM_Get_R(referenceAddress, rtype);
        UInt existing = HopperVM_Get_R(localAddress, rtype);
        if (Types_IsReferenceType(rtype))
        {
            GC_Release(existing);
        }
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        HopperVM_Put(localAddress, value, vtype);
    }
    return true;
}

Bool Instructions_PushRelB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    UInt referenceAddress = UInt(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = HopperVM_Get_R(referenceAddress, rtype);
    UInt value = HopperVM_Get_R(localAddress, rtype);
    HopperVM_Push(value, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PopGlobalB()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyGlobalB();
    }
    else
    {
        Byte offset = HopperVM_ReadByteOperand();
        Type htype = Type(Memory_ReadWord(HopperVM_TypeStack_Get() + offset));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = Memory_ReadWord(HopperVM_ValueStack_Get() + offset);
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        Memory_WriteWord(HopperVM_ValueStack_Get() + offset, value);
        Memory_WriteWord(HopperVM_TypeStack_Get() + offset, UInt(htype));
    }
    return true;
}

Bool Instructions_PushGlobalB()
{
    Byte offset = HopperVM_ReadByteOperand();
    UInt value = Memory_ReadWord(HopperVM_ValueStack_Get() + offset);
    Type htype = Type(Memory_ReadWord(HopperVM_TypeStack_Get() + offset));
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PushStackAddrB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    UInt address = UInt(Int(HopperVM_BP_Get()) + offset);
    HopperVM_Push(address, Type::eReference);
    return true;
}

Bool Instructions_IncLocalB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Type itype = (Type)0;
    UInt address = UInt(Int(HopperVM_BP_Get()) + offset);
    UInt value = HopperVM_Get_R(address, itype);
    if (itype == Type::eByte)
    {
        itype = Type::eUInt;
    }
    HopperVM_Put(address, value + 0x01, itype);
    return true;
}

Bool Instructions_DecLocalB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Type itype = (Type)0;
    UInt address = UInt(Int(HopperVM_BP_Get()) + offset);
    UInt value = HopperVM_Get_R(address, itype);
    HopperVM_Put(address, value - 0x01, itype);
    return true;
}

Bool Instructions_Dup()
{
    Byte offset = HopperVM_ReadByteOperand();
    UInt address = HopperVM_SP_Get() - 0x02 - offset;
    UInt value = Memory_ReadWord(HopperVM_ValueStack_Get() + address);
    Type htype = Type(Memory_ReadWord(HopperVM_TypeStack_Get() + address));
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_DecSP()
{
    UInt popBytes = HopperVM_ReadByteOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        popBytes = popBytes - 0x02;
    }
    return true;
}

Bool Instructions_RetB()
{
    UInt popBytes = HopperVM_ReadByteOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        popBytes = popBytes - 0x02;
    }
    HopperVM_BP_Set(HopperVM_PopCS());
    if (HopperVM_CSP_Get() == 0x00)
    {
        HopperVM_PC_Set(0x00);
        return false;
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return true;
}

Bool Instructions_RetRetB()
{
    Type rtype = (Type)0;
    UInt value = HopperVM_Pop_R(rtype);
    UInt popBytes = HopperVM_ReadByteOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        popBytes = popBytes - 0x02;
    }
    HopperVM_Push(value, rtype);
    HopperVM_BP_Set(HopperVM_PopCS());
    if (HopperVM_CSP_Get() == 0x00)
    {
        HopperVM_PC_Set(0x00);
        return false;
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return true;
}

Bool Instructions_CallB()
{
    UInt methodIndex = HopperVM_ReadByteOperand();
    HopperVM_PushCS(HopperVM_PC_Get());
    HopperVM_PC_Set(HopperVM_LookupMethod(methodIndex));
    return true;
}

Bool Instructions_TestBPB()
{
    Byte operand = HopperVM_ReadByteOperand();
    UInt bpExpected = UInt(HopperVM_SP_Get() - operand);
    if (bpExpected != HopperVM_BP_Get())
    {
        Minimal_Error_Set(0x0B);
        return false;
    }
    return true;
}

Bool Instructions_JZB()
{
    if (HopperVM_Pop() == 0x00)
    {
        HopperVM_PC_Set(UInt(HopperVM_ReadByteOffsetOperand() + Int(HopperVM_PC_Get() - 0x02)));
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + 0x01);
    }
    return true;
}

Bool Instructions_JNZB()
{
    if (HopperVM_Pop() != 0x00)
    {
        HopperVM_PC_Set(UInt(HopperVM_ReadByteOffsetOperand() + Int(HopperVM_PC_Get() - 0x02)));
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + 0x01);
    }
    return true;
}

Bool Instructions_JB()
{
    HopperVM_PC_Set(UInt(HopperVM_ReadByteOffsetOperand() + Int(HopperVM_PC_Get() - 0x02)));
    return true;
}

Bool Instructions_JZW()
{
    if (HopperVM_Pop() == 0x00)
    {
        HopperVM_PC_Set(UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_PC_Get() - 0x03)));
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + 0x02);
    }
    return true;
}

Bool Instructions_JNZW()
{
    if (HopperVM_Pop() != 0x00)
    {
        HopperVM_PC_Set(UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_PC_Get() - 0x03)));
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + 0x02);
    }
    return true;
}

Bool Instructions_JW()
{
    HopperVM_PC_Set(UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_PC_Get() - 0x03)));
    return true;
}

Bool Instructions_CallW()
{
    UInt methodIndex = HopperVM_ReadWordOperand();
    HopperVM_PushCS(HopperVM_PC_Get());
    UInt methodAddress = HopperVM_LookupMethod(methodIndex);
    Memory_WriteCodeByte(HopperVM_PC_Get() - 0x03, Byte(OpCode::eCALLIW));
    Memory_WriteCodeWord(HopperVM_PC_Get() - 0x02, methodAddress);
    HopperVM_PC_Set(methodAddress);
    return true;
}

Bool Instructions_PushIW()
{
    HopperVM_Push(HopperVM_ReadWordOperand(), Type::eUInt);
    return true;
}

Bool Instructions_IncLocalBB()
{
    Int offset0 = HopperVM_ReadByteOffsetOperand();
    Int offset1 = HopperVM_ReadByteOffsetOperand();
    UInt address0 = UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + offset0);
    UInt address1 = UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + offset1);
    Memory_WriteWord(address0, Memory_ReadWord(address0) + Memory_ReadWord(address1));
    return true;
}

Bool Instructions_PushIWLE()
{
    UInt top = HopperVM_ReadWordOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_BoolNot()
{
    HopperVM_Push(((HopperVM_Pop() == 0x00)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_BitNot()
{
    HopperVM_Push(~(HopperVM_Pop()), Type::eUInt);
    return true;
}

Bool Instructions_Swap()
{
    UInt topValue = Memory_ReadWord(HopperVM_ValueStack_Get() + HopperVM_SP_Get() - 0x02);
    UInt nextValue = Memory_ReadWord(HopperVM_ValueStack_Get() + HopperVM_SP_Get() - 0x04);
    Memory_WriteWord(HopperVM_ValueStack_Get() + HopperVM_SP_Get() - 0x02, nextValue);
    Memory_WriteWord(HopperVM_ValueStack_Get() + HopperVM_SP_Get() - 0x04, topValue);
    UInt topType = Memory_ReadWord(HopperVM_TypeStack_Get() + HopperVM_SP_Get() - 0x02);
    UInt nextType = Memory_ReadWord(HopperVM_TypeStack_Get() + HopperVM_SP_Get() - 0x04);
    Memory_WriteWord(HopperVM_TypeStack_Get() + HopperVM_SP_Get() - 0x02, nextType);
    Memory_WriteWord(HopperVM_TypeStack_Get() + HopperVM_SP_Get() - 0x04, topType);
    return true;
}

Bool Instructions_PushI0()
{
    HopperVM_Push(0x00, Type::eByte);
    return true;
}

Bool Instructions_PushI1()
{
    HopperVM_Push(0x01, Type::eByte);
    return true;
}

Bool Instructions_PushIM1()
{
    HopperVM_PushI(-0x01);
    return true;
}

Bool Instructions_PushGP()
{
    HopperVM_Push(0x00, Type::eUInt);
    return true;
}

Bool Instructions_Ret0()
{
    HopperVM_BP_Set(HopperVM_PopCS());
    if (HopperVM_CSP_Get() == 0x00)
    {
        HopperVM_PC_Set(0x00);
        return false;
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return true;
}

Bool Instructions_CallRel()
{
    UInt methodIndex = HopperVM_Pop();
    HopperVM_PushCS(HopperVM_PC_Get());
    HopperVM_PC_Set(HopperVM_LookupMethod(methodIndex));
    return true;
}

Bool Instructions_PopLocalB00()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB00();
    }
    else
    {
        Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()))));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get())));
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        Memory_WriteWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get())), value);
        Memory_WriteWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get())), UInt(htype));
    }
    return true;
}

Bool Instructions_PopLocalB02()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB02();
    }
    else
    {
        Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + 0x02)));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + 0x02));
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        Memory_WriteWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + 0x02), value);
        Memory_WriteWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + 0x02), UInt(htype));
    }
    return true;
}

Bool Instructions_PushLocalB00()
{
    UInt value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get())));
    Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()))));
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PushLocalB02()
{
    UInt value = Memory_ReadWord(UInt(Int(HopperVM_ValueStack_Get()) + Int(HopperVM_BP_Get()) + 0x02));
    Type htype = Type(Memory_ReadWord(UInt(Int(HopperVM_TypeStack_Get()) + Int(HopperVM_BP_Get()) + 0x02)));
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_SysCall0()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x00);
}

Bool Instructions_SysCall1()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x01);
}

Bool Instructions_SysCall()
{
    Type htype = (Type)0;
    UInt iOverload = HopperVM_Pop_R(htype);
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, iOverload);
}

Bool Instructions_CNP()
{
    HopperVM_CNP_Set(true);
    return true;
}

Bool Instructions_Enter()
{
    HopperVM_PushCS(HopperVM_BP_Get());
    HopperVM_BP_Set(HopperVM_SP_Get());
    return true;
}

Bool Instructions_Cast()
{
    HopperVM_Put(HopperVM_SP_Get() - 0x02, HopperVM_Get(HopperVM_SP_Get() - 0x02), Type(HopperVM_ReadByteOperand()));
    return true;
}

Bool Instructions_PushIWLT()
{
    UInt top = HopperVM_ReadWordOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(((next < top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_PushLocalBB()
{
    Bool res = Instructions_PushLocalB();
    return Instructions_PushLocalB();
}

Bool Instructions_PopCopyLocalB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Type htype = (Type)0;
    UInt localAddress = UInt(Int(HopperVM_BP_Get()) + offset);
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyRelB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    UInt referenceAddress = UInt(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = HopperVM_Get_R(referenceAddress, rtype);
    UInt oldvalue = HopperVM_Get_R(localAddress, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(rtype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, rtype);
    }
    return true;
}

Bool Instructions_PopCopyGlobalB()
{
    Byte offset = HopperVM_ReadByteOperand();
    Type htype = (Type)0;
    UInt oldvalue = HopperVM_Get_R(offset, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(offset, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyLocalB00()
{
    Type htype = (Type)0;
    UInt localAddress = UInt(Int(HopperVM_BP_Get()));
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyLocalB02()
{
    Type htype = (Type)0;
    UInt localAddress = UInt(Int(HopperVM_BP_Get()) + 0x02);
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_EnterB()
{
    HopperVM_PushCS(HopperVM_BP_Get());
    HopperVM_BP_Set(HopperVM_SP_Get());
    UInt zeros = HopperVM_ReadByteOperand();;
    for (UInt i = 0x00; i < zeros; i++)
    {
        HopperVM_Push(0x00, Type::eByte);
    }
    return true;
}

Bool Instructions_RetFast()
{
    HopperVM_PC_Set(HopperVM_PopCS());
    return true;
}

Bool Instructions_BitXor()
{
    UInt top = HopperVM_Pop();
    UInt next = HopperVM_Pop();
    HopperVM_Push((next | top) & (~(next & top)), Type::eUInt);
    return true;
}

Bool Instructions_PushIWLEI()
{
    HopperVM_Push(HopperVM_ReadWordOperand(), Type::eUInt);
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_JIXB()
{
    UInt switchCase = HopperVM_Pop();
    Byte minRange = HopperVM_ReadByteOperand();
    Byte maxRange = HopperVM_ReadByteOperand();
    Byte lsb = HopperVM_ReadByteOperand();
    Byte msb = HopperVM_ReadByteOperand();
    Int jumpBackOffset = Int(lsb + (msb << 0x08));
    UInt tpc = HopperVM_PC_Get();
    HopperVM_PC_Set(UInt(Int(HopperVM_PC_Get()) - jumpBackOffset - 0x05));
    UInt tableSize = UInt(maxRange) - UInt(minRange) + 0x01;
    UInt offset = 0x00;
    if ((switchCase >= minRange) && (switchCase <= maxRange))
    {
        UInt index = tpc + switchCase - minRange;
        offset = Memory_ReadCodeByte(index);
    }
    if (offset == 0x00)
    {
        HopperVM_PC_Set(tpc + tableSize);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + offset);
    }
    return true;
}

Bool Instructions_JIXW()
{
    UInt switchCase = HopperVM_Pop();
    Byte minRange = HopperVM_ReadByteOperand();
    Byte maxRange = HopperVM_ReadByteOperand();
    Byte lsb = HopperVM_ReadByteOperand();
    Byte msb = HopperVM_ReadByteOperand();
    Int jumpBackOffset = Int(lsb + (msb << 0x08));
    UInt tpc = HopperVM_PC_Get();
    HopperVM_PC_Set(UInt(Int(HopperVM_PC_Get()) - jumpBackOffset - 0x05));
    UInt tableSize = (UInt(maxRange) - UInt(minRange) + 0x01) << 0x01;
    UInt offset = 0x00;
    if ((switchCase >= minRange) && (switchCase <= maxRange))
    {
        UInt index = tpc + (switchCase - minRange) * 0x02;
        offset = Memory_ReadCodeByte(index) + (Memory_ReadCodeByte(index + 0x01) << 0x08);
    }
    if (offset == 0x00)
    {
        HopperVM_PC_Set(tpc + tableSize);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + offset);
    }
    return true;
}

Bool Instructions_CallIW()
{
    UInt methodAddress = HopperVM_ReadWordOperand();
    HopperVM_PushCS(HopperVM_PC_Get());
    HopperVM_PC_Set(methodAddress);
    return true;
}

Bool Instructions_PushIBLE()
{
    UInt top = HopperVM_ReadByteOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_PushIBEQ()
{
    UInt top = HopperVM_ReadByteOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(((next == top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_AddB()
{
    UInt top = HopperVM_ReadByteOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(next + top, Type::eUInt);
    return true;
}

Bool Instructions_SubB()
{
    UInt top = HopperVM_ReadByteOperand();
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_Push(next - top, Type::eUInt);
    return true;
}

Bool HRDirectory_Exists(UInt hrpath)
{
    return External_DirectoryExists(hrpath);
}

void HRDirectory_Create(UInt hrpath)
{
    if (!HRDirectory_Exists(hrpath))
    {
        External_DirectoryCreate(hrpath);
    }
}

void HRDirectory_Clear(UInt _this)
{
    Memory_WriteByte(_this + 2, 0x00);
    GC_Release(Memory_ReadWord(_this + 3));
}

void Runtime_ErrorDump(UInt number)
{
    IO_Write('F');
    IO_Write('U');
    IO_Write('C');
    IO_Write('K');
    IO_Write('!');
    IO_WriteUInt(number);
}

UInt HRList_GetLength(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype)
{
    itype = Type(Memory_ReadByte(_this + 4));
    UInt length = Memory_ReadWord(_this + 2);
    UInt i = 0x00;
    UInt pCurrent = Memory_ReadWord(_this + 5);
    UInt pRecent = Memory_ReadWord(_this + 7);
    if (pRecent != 0x00)
    {
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        if (iRecent <= index)
        {
            i = iRecent;
            pCurrent = pRecent;
        }
    }
    for (;;)
    {
        if (i == index)
        {
            break;;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
        
        i++;
    }
    UInt pData = Memory_ReadWord(pCurrent + 0);
    if (itype == Type::eVariant)
    {
        pData = HRVariant_GetValue_R(pData, itype);
    }
    else if (Types_IsReferenceType(itype))
    {
        itype = Type(Memory_ReadByte(pData));
        pData = GC_Clone(pData);
    }
    Memory_WriteWord(_this + 7, pCurrent);
    Memory_WriteWord(_this + 7 + 0x02, index);
    return pData;
}

void HRList_Append(UInt _this, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pNewItem = HRList_createItem(item, etype, itype);
    UInt pFirstItem = Memory_ReadWord(_this + 5);
    if (pFirstItem == 0x00)
    {
        Memory_WriteWord(_this + 5, pNewItem);
    }
    else
    {
        UInt pCurrentItem = pFirstItem;
        UInt pRecentItem = Memory_ReadWord(_this + 7);
        if (pRecentItem != 0x00)
        {
            pCurrentItem = pRecentItem;
        }
        for (;;)
        {
            UInt pNextItem = Memory_ReadWord(pCurrentItem + 2);
            if (pNextItem == 0x00)
            {
                break;;
            }
            pCurrentItem = pNextItem;
        }
        Memory_WriteWord(pCurrentItem + 2, pNewItem);
    }
    UInt length = Memory_ReadWord(_this + 2) + 0x01;
    Memory_WriteWord(_this + 2, length);
    Memory_WriteWord(_this + 7, pNewItem);
    Memory_WriteWord(_this + 7 + 0x02, length - 0x01);
}

UInt HRList_New(Type htype)
{
    UInt address = GC_New(0x09, Type::eList);
    Memory_WriteWord(address + 2, 0x00);
    Memory_WriteByte(address + 4, Byte(htype));
    Memory_WriteWord(address + 5, 0x00);
    Memory_WriteWord(address + 7, 0x00);
    Memory_WriteWord(address + 7 + 0x02, 0x00);
    return address;
}

void HRList_Clear(UInt _this)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pFirst = Memory_ReadWord(_this + 5);
    if (pFirst != 0x00)
    {
        HRList_clearAllItems(pFirst, etype);
    }
    Memory_WriteWord(_this + 2, 0x00);
    Memory_WriteWord(_this + 5, 0x00);
    Memory_WriteWord(_this + 7, 0x00);
    Memory_WriteWord(_this + 7 + 0x02, 0x00);
}

UInt HRList_createItem(UInt itemData, Type etype, Type itype)
{
    UInt pData = itemData;
    if (Types_IsReferenceType(etype))
    {
        if (Types_IsReferenceType(itype))
        {
            pData = GC_Clone(itemData);
        }
        else
        {
            pData = HRVariant_CreateValueVariant(itemData, itype);
        }
    }
    UInt pitem = Memory_Allocate(0x04);
    Memory_WriteWord(pitem + 0, pData);
    Memory_WriteWord(pitem + 2, 0x00);
    return pitem;
}

void HRList_clearAllItems(UInt pCurrent, Type etype)
{
    UInt pNext = 0;
    for (;;)
    {
        if (pCurrent == 0x00)
        {
            break;;
        }
        pNext = Memory_ReadWord(pCurrent + 2);
        HRList_clearItem(pCurrent, etype);
        pCurrent = pNext;
    }
}

void HRList_clearItem(UInt pCurrent, Type etype)
{
    UInt pData = Memory_ReadWord(pCurrent + 0);
    Memory_Free(pCurrent);
    if (Types_IsReferenceType(etype))
    {
        GC_Release(pData);
    }
}

void HRFile_Clear(UInt _this)
{
    Memory_WriteByte(_this + 2, 0x00);
    Memory_WriteByte(_this + 3, 0x00);
    Memory_WriteByte(_this + 4, 0x00);
    GC_Release(Memory_ReadWord(_this + 5));
    Memory_WriteWord(_this + 5, 0x00);
    Memory_WriteWord(_this + 7, 0x00);
    GC_Release(Memory_ReadWord(_this + 9));
    Memory_WriteWord(_this + 9, 0x00);
}

void HRDictionary_Clear(UInt _this)
{
    UInt iterator = 0;
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    while (HRDictionary_next_R(_this, iterator, ktype, key, vtype, value))
    {
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(value);
        }
    }
    UInt pEntries = Memory_ReadWord(_this + 8);
    if (pEntries != 0x00)
    {
        Memory_Free(pEntries);
    }
    Memory_WriteWord(_this + 4, 0x00);
    Memory_WriteWord(_this + 6, 0x00);
    Memory_WriteWord(_this + 8, 0x00);
}

Bool HRDictionary_next_R(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt & value)
{
    UInt count = Memory_ReadWord(_this + 4);
    if (count == 0x00)
    {
        return false;
    }
    if (iterator == 0xFFFF)
    {
        return false;
    }
    Bool success = false;
    ktype = Type(Memory_ReadByte(_this + 2));
    vtype = Type(Memory_ReadByte(_this + 3));
    Bool isValueTable = (ktype != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    for (;;)
    {
        UInt pEntry = pEntries + 8 * iterator;
        iterator = (iterator + 0x01) % capacity;
        if (isValueTable)
        {
            value = Memory_ReadWord(pEntry + 6);
            Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
            if (!isOccupied)
            {
            }
            else
            {
                key = Memory_ReadWord(pEntry + 0);
                value = Memory_ReadWord(pEntry + 6);
                if (iterator == 0x00)
                {
                    iterator = 0xFFFF;
                }
                success = true;
                break;;
            }
        }
        else
        {
            key = Memory_ReadWord(pEntry + 0);
            if (key == 0x00)
            {
            }
            else
            {
                value = Memory_ReadWord(pEntry + 6);
                if (iterator == 0x00)
                {
                    iterator = 0xFFFF;
                }
                success = true;
                break;;
            }
        }
        if (iterator == 0x00)
        {
            success = false;
            break;;
        }
    }
    if (success && Types_IsReferenceType(vtype))
    {
        vtype = Type(Memory_ReadByte(value));
    }
    return success;
}

void HRPair_Clear(UInt _this)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt key = Memory_ReadWord(_this + 4);
    UInt value = Memory_ReadWord(_this + 6);
    if (Types_IsReferenceType(dkType) && (0x00 != key))
    {
        GC_Release(key);
    }
    if (Types_IsReferenceType(dvType) && (0x00 != value))
    {
        GC_Release(value);
    }
    Memory_WriteWord(_this + 4, 0x00);
    Memory_WriteWord(_this + 6, 0x00);
}

void HRVariant_Clear(UInt _this)
{
    Type vtype = Type(Memory_ReadByte(_this + 2));
    UInt value = Memory_ReadWord(_this + 3);
    if (Types_IsReferenceType(vtype) && (0x00 != value))
    {
        GC_Release(value);
    }
    Memory_WriteWord(_this + 2, 0x00);
    Memory_WriteWord(_this + 3, 0x00);
}

UInt HRVariant_GetValue_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 2));
    UInt value = Memory_ReadWord(_this + 3);
    if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
        vtype = Type(Memory_ReadByte(value));
    }
    return value;
}

UInt HRVariant_CreateValueVariant(UInt value, Type vtype)
{
    UInt address = GC_New(0x03, Type::eVariant);
    Memory_WriteByte(address + 2, Byte(vtype));
    Memory_WriteWord(address + 3, value);
    return address;
}

UInt HopperVM_Pop()
{
    HopperVM_sp = HopperVM_sp - 0x02;
    return Memory_ReadWord(HopperVM_valueStack + HopperVM_sp);
}

void HopperVM_Push(UInt value, Type htype)
{
    Memory_WriteWord(HopperVM_valueStack + HopperVM_sp, value);
    Memory_WriteWord(HopperVM_typeStack + HopperVM_sp, Byte(htype));
    HopperVM_sp = HopperVM_sp + 0x02;
}

UInt HopperVM_Pop_R(Type & htype)
{
    HopperVM_sp = HopperVM_sp - 0x02;
    UInt value = Memory_ReadWord(HopperVM_valueStack + HopperVM_sp);
    htype = Type(Memory_ReadWord(HopperVM_typeStack + HopperVM_sp));
    return value;
}

Int HopperVM_PopI()
{
    HopperVM_sp = HopperVM_sp - 0x02;
    return External_UIntToInt(Memory_ReadWord(HopperVM_valueStack + HopperVM_sp));
}

void HopperVM_PushI(Int ivalue)
{
    UInt value = External_IntToUInt(ivalue);
    Memory_WriteWord(HopperVM_valueStack + HopperVM_sp, value);
    Memory_WriteWord(HopperVM_typeStack + HopperVM_sp, Byte(Type::eInt));
    HopperVM_sp = HopperVM_sp + 0x02;
}

Byte HopperVM_ReadByteOperand()
{
    Byte operand = Memory_ReadCodeByte(HopperVM_pc);
    
    HopperVM_pc++;
    return operand;
}

Bool HopperVM_CNP_Get()
{
    return HopperVM_cnp;
}

void HopperVM_CNP_Set(Bool value)
{
    HopperVM_cnp = value;
}

Int HopperVM_ReadByteOffsetOperand()
{
    Int offset = Int(Memory_ReadCodeByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    return offset;
}

UInt HopperVM_TypeStack_Get()
{
    return HopperVM_typeStack;
}

UInt HopperVM_ValueStack_Get()
{
    return HopperVM_valueStack;
}

void HopperVM_Put(UInt address, UInt value, Type htype)
{
    Memory_WriteWord(HopperVM_valueStack + address, value);
    Memory_WriteWord(HopperVM_typeStack + address, Byte(htype));
}

void HopperVM_BP_Set(UInt value)
{
    HopperVM_bp = value;
}

UInt HopperVM_PopCS()
{
    HopperVM_csp = HopperVM_csp - 0x02;
    return Memory_ReadWord(HopperVM_callStack + HopperVM_csp);
}

void HopperVM_PC_Set(UInt value)
{
    HopperVM_pc = value;
}

void HopperVM_PushCS(UInt value)
{
    Memory_WriteWord(HopperVM_callStack + HopperVM_csp, value);
    HopperVM_csp = HopperVM_csp + 0x02;
}

UInt HopperVM_LookupMethod(UInt methodIndex)
{
    methodIndex = (methodIndex & 0x3FFF);
    UInt address = HopperVM_methodTable;
    for (;;)
    {
        UInt entry = Memory_ReadCodeWord(address);
        if (entry == methodIndex)
        {
            address = Memory_ReadCodeWord(address + 0x02);
            break;;
        }
        address = address + 0x04;
    }
    return address;
}

Int HopperVM_ReadWordOffsetOperand()
{
    Int offset = External_UIntToInt(Memory_ReadCodeWord(HopperVM_pc));
    
    HopperVM_pc++;
    
    HopperVM_pc++;
    return offset;
}

UInt HopperVM_ReadWordOperand()
{
    UInt operand = Memory_ReadCodeWord(HopperVM_pc);
    
    HopperVM_pc++;
    
    HopperVM_pc++;
    return operand;
}

Bool HopperVM_ExecuteSysCall(Byte iSysCall, UInt iOverload)
{
    Bool doNext = true;
    switch (SysCall(iSysCall))
    {
    case SysCall::eSystemCurrentDirectoryGet:
    {
        HopperVM_Push(GC_Clone(HopperVM_currentDirectory), Type::eString);
        break;
    }
    case SysCall::eSystemCurrentDirectorySet:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        GC_Release(HopperVM_currentDirectory);
        HopperVM_currentDirectory = GC_Clone(str);
        GC_Release(str);
        break;
    }
    case SysCall::eFileNew:
    {
        UInt result = HRFile_New();
        HopperVM_Push(result, Type::eFile);
        break;
    }
    case SysCall::eFileExists:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        Bool result = HRFile_Exists(str);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(str);
        break;
    }
    case SysCall::eFileIsValid:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        Bool result = HRFile_IsValid(hrfile);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(hrfile);
        break;
    }
    case SysCall::eFileFlush:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        HRFile_Flush(hrfile);
        GC_Release(hrfile);
        break;
    }
    case SysCall::eFileReadLine:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        UInt str = HRFile_ReadLine(hrfile);
        GC_Release(hrfile);
        HopperVM_Push(str, Type::eString);
        break;
    }
    case SysCall::eFileRead:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            UInt b = HRFile_Read(hrfile);
            GC_Release(hrfile);
            HopperVM_Push(b, Type::eByte);
            break;
        }
        case 0x01:
        {
            Type ltype = (Type)0;
            UInt hrlong = HopperVM_Pop_R(ltype);
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            UInt b = HRFile_Read(hrfile, hrlong);
            GC_Release(hrfile);
            GC_Release(hrlong);
            HopperVM_Push(b, Type::eByte);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eFileAppend:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            UInt top = HopperVM_Pop();
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            HRFile_Append(hrfile, Byte(top));
            GC_Release(hrfile);
            break;
        }
        case 0x01:
        {
            Type stype = (Type)0;
            UInt str = HopperVM_Pop_R(stype);
            Type ftype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            HRFile_Append(hrfile, str);
            GC_Release(str);
            GC_Release(hrfile);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eFileCreate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_Create(str);
        HopperVM_Push(result, Type::eFile);
        GC_Release(str);
        break;
    }
    case SysCall::eFileOpen:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_Open(str);
        HopperVM_Push(result, Type::eFile);
        GC_Release(str);
        break;
    }
    case SysCall::eFileDelete:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRFile_Delete(str);
        GC_Release(str);
        break;
    }
    case SysCall::eFileGetTime:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetTime(str);
        HopperVM_Push(result, Type::eLong);
        GC_Release(str);
        break;
    }
    case SysCall::eFileGetSize:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetSize(str);
        HopperVM_Push(result, Type::eLong);
        GC_Release(str);
        break;
    }
    case SysCall::eDirectoryNew:
    {
        UInt result = HRDirectory_New();
        HopperVM_Push(result, Type::eDirectory);
        break;
    }
    case SysCall::eDirectoryExists:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        Bool result = HRDirectory_Exists(str);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(str);
        break;
    }
    case SysCall::eDirectoryOpen:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_Open(str);
        HopperVM_Push(result, Type::eDirectory);
        GC_Release(str);
        break;
    }
    case SysCall::eDirectoryIsValid:
    {
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        Bool result = HRDirectory_IsValid(hrdir);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(hrdir);
        break;
    }
    case SysCall::eDirectoryGetFileCount:
    {
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetFileCount(hrdir);
        HopperVM_Push(result, Type::eUInt);
        GC_Release(hrdir);
        break;
    }
    case SysCall::eDirectoryGetDirectoryCount:
    {
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetDirectoryCount(hrdir);
        HopperVM_Push(result, Type::eUInt);
        GC_Release(hrdir);
        break;
    }
    case SysCall::eDirectoryGetFile:
    {
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetFile(hrdir, index);
        HopperVM_Push(result, Type::eString);
        GC_Release(hrdir);
        break;
    }
    case SysCall::eDirectoryGetDirectory:
    {
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetDirectory(hrdir, index);
        HopperVM_Push(result, Type::eString);
        GC_Release(hrdir);
        break;
    }
    case SysCall::eDirectoryDelete:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRDirectory_Delete(str);
        GC_Release(str);
        break;
    }
    case SysCall::eDirectoryCreate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRDirectory_Create(str);
        GC_Release(str);
        break;
    }
    case SysCall::eDirectoryGetTime:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetTime(str);
        HopperVM_Push(result, Type::eLong);
        GC_Release(str);
        break;
    }
    case SysCall::eScreenPrintLn:
    {
        IO_WriteLn();
        break;
    }
    case SysCall::eScreenClear:
    {
        IO_Clear();
        break;
    }
    case SysCall::eScreenPrint:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt bc = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt fc = HopperVM_Pop_R(btype);
            Type ctype = (Type)0;
            UInt ch = HopperVM_Pop_R(ctype);
            IO_Write(Char(ch));
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt bc = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt fc = HopperVM_Pop_R(btype);
            Type stype = (Type)0;
            UInt str = HopperVM_Pop_R(stype);
            UInt length = HRString_GetLength(str);;
            for (UInt i = 0x00; i < length; i++)
            {
                Char ch = HRString_GetChar(str, i);
                IO_Write(ch);
            }
            GC_Release(str);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x06);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eSerialIsAvailableGet:
    {
        Bool avail = Serial_IsAvailable_Get();
        HopperVM_Push(UInt(avail), Type::eBool);
        break;
    }
    case SysCall::eSerialReadChar:
    {
        Char ch = Serial_ReadChar();
        HopperVM_Push(UInt(ch), Type::eChar);
        break;
    }
    case SysCall::eSerialWriteChar:
    {
        Type atype = (Type)0;
        UInt ch = HopperVM_Pop_R(atype);
        Serial_WriteChar(Char(ch));
        break;
    }
    case SysCall::eLongNewFromConstant:
    {
        Type atype = (Type)0;
        UInt location = HopperVM_Pop_R(atype);
        UInt address = HRLong_NewFromConstant(HopperVM_constAddress + location);
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCall::eFloatNewFromConstant:
    {
        Type atype = (Type)0;
        UInt location = HopperVM_Pop_R(atype);
        UInt address = HRFloat_NewFromConstant(HopperVM_constAddress + location);
        HopperVM_Push(address, Type::eFloat);
        break;
    }
    case SysCall::eStringNewFromConstant:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type ltype = (Type)0;
            UInt length = HopperVM_Pop_R(ltype);
            Type atype = (Type)0;
            UInt location = HopperVM_Pop_R(atype);
            UInt address = HRString_NewFromConstant0(HopperVM_constAddress + location, length);
            HopperVM_Push(address, Type::eString);
            break;
        }
        case 0x01:
        {
            Type utype = (Type)0;
            UInt doubleChar = HopperVM_Pop_R(utype);
            UInt address = HRString_NewFromConstant1(doubleChar);
            HopperVM_Push(address, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x05);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringNew:
    {
        UInt address = HRString_New();
        HopperVM_Push(address, Type::eString);
        break;
    }
    case SysCall::eStringLengthGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt length = HRString_GetLength(_this);
        GC_Release(_this);
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case SysCall::eStringGetChar:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Char ch = HRString_GetChar(_this, index);
        GC_Release(_this);
        HopperVM_Push(UInt(ch), Type::eChar);
        break;
    }
    case SysCall::eStringInsertChar:
    {
        Type atype = (Type)0;
        UInt ch = HopperVM_Pop_R(atype);
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt result = HRString_InsertChar(_this, index, Char(ch));
        GC_Release(_this);
        HopperVM_Push(result, Type::eString);
        break;
    }
    case SysCall::eStringEndsWith:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = UInt(HRString_EndsWith(_this, Char(with)));
            GC_Release(_this);
            HopperVM_Push(result, Type::eBool);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = UInt(HRString_EndsWith(_this, with));
            GC_Release(_this);
            GC_Release(with);
            HopperVM_Push(result, Type::eBool);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringCompare:
    {
        Type atype = (Type)0;
        UInt right = HopperVM_Pop_R(atype);
        Type btype = (Type)0;
        UInt left = HopperVM_Pop_R(btype);
        Int result = HRString_Compare(left, right);
        GC_Release(right);
        GC_Release(left);
        HopperVM_PushI(result);
        break;
    }
    case SysCall::eStringReplace:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt to = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt from = HopperVM_Pop_R(btype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Replace(_this, from, to);
            GC_Release(_this);
            GC_Release(to);
            GC_Release(from);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt to = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt from = HopperVM_Pop_R(btype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Replace(_this, Char(from), Char(to));
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringAppend:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Append(_this, append);
            GC_Release(_this);
            GC_Release(append);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Append(_this, Char(append));
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringSubstring:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Substring(_this, start);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type ltype = (Type)0;
            UInt limit = HopperVM_Pop_R(ltype);
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Substring(_this, start, limit);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x02:
        {
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type htype = (Type)0;
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_Substring_R(str, start);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x12);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringBuild:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type htype = (Type)0;
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_Build_R(str, append);
            HopperVM_Put(address, str, Type::eString);
            GC_Release(append);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Char ch = Char(HopperVM_Pop_R(htype));
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_Build_R(str, ch);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        case 0x02:
        {
            Type htype = (Type)0;
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_Build_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x2A);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringBuildFront:
    {
        Type htype = (Type)0;
        Char ch = Char(HopperVM_Pop_R(htype));
        UInt address = HopperVM_Pop_R(htype);
        UInt str = HopperVM_Get_R(address, htype);
        HRString_BuildFront_R(str, ch);
        HopperVM_Put(address, str, Type::eString);
        break;
    }
    case SysCall::eStringTrim:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type htype = (Type)0;
            UInt _this = HopperVM_Pop_R(htype);
            UInt result = HRString_Trim(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_TrimRight_R(str);
            HRString_TrimLeft_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x04);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringTrimLeft:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type htype = (Type)0;
            UInt _this = HopperVM_Pop_R(htype);
            UInt result = HRString_TrimLeft(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            UInt address = HopperVM_Pop_R(htype);
            UInt str = HopperVM_Get_R(address, htype);
            HRString_TrimLeft_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x03);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCall::eStringTrimRight:
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        UInt str = HopperVM_Get_R(address, htype);
        HRString_TrimRight_R(str);
        HopperVM_Put(address, str, Type::eString);
        break;
    }
    case SysCall::eArrayNew:
    {
        Type stype = (Type)0;
        Type htype = Type(HopperVM_Pop_R(stype));
        UInt count = HopperVM_Pop_R(stype);
        UInt address = HRArray_New(htype, count);
        HopperVM_Push(address, Type::eArray);
        break;
    }
    case SysCall::eArrayGetItem:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type etype = (Type)0;
        UInt item = HRArray_GetItem_R(_this, index, etype);
        GC_Release(_this);
        HopperVM_Push(item, etype);
        break;
    }
    case SysCall::eArraySetItem:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRArray_SetItem(_this, index, item);
        GC_Release(_this);
        break;
    }
    case SysCall::eArrayCountGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt length = HRArray_GetCount(_this);
        GC_Release(_this);
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case SysCall::eListNew:
    {
        Type stype = (Type)0;
        Type htype = Type(HopperVM_Pop_R(stype));
        UInt address = HRList_New(htype);
        HopperVM_Push(address, Type::eList);
        break;
    }
    case SysCall::eListLengthGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt length = HRList_GetLength(_this);
        GC_Release(_this);
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case SysCall::eListAppend:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Append(_this, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCall::eListSetItem:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_SetItem(_this, index, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCall::eListInsert:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Insert(_this, index, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCall::eListGetItem:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type itype = (Type)0;
        UInt item = HRList_GetItem_R(_this, index, itype);
        GC_Release(_this);
        HopperVM_Push(item, itype);
        break;
    }
    case SysCall::eListClear:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Clear(_this);
        GC_Release(_this);
        break;
    }
    case SysCall::eListRemove:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Remove(_this, index);
        GC_Release(_this);
        break;
    }
    case SysCall::eListContains:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Bool contains = HRList_Contains(_this, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        HopperVM_Push((contains) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case SysCall::ePairNew:
    {
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        UInt address = HRPair_New(ktype, key, vtype, value);
        HopperVM_Push(address, Type::ePair);
        break;
    }
    case SysCall::eVariantBox:
    {
        Type vvtype = (Type)0;
        Type vtype = Type(HopperVM_Pop_R(vvtype));
        Type vttype = (Type)0;
        UInt value = HopperVM_Pop_R(vttype);
        UInt address = HRVariant_New(value, vtype);
        HopperVM_Push(address, Type::eVariant);
        break;
    }
    case SysCall::ePairValue:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type vtype = (Type)0;
        UInt value = HRPair_GetValue_R(_this, vtype);
        GC_Release(_this);
        HopperVM_Push(value, vtype);
        break;
    }
    case SysCall::ePairKey:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type ktype = (Type)0;
        UInt key = HRPair_GetKey_R(_this, ktype);
        GC_Release(_this);
        HopperVM_Push(key, ktype);
        break;
    }
    case SysCall::eTypesTypeOf:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        if (Types_IsReferenceType(ttype))
        {
            GC_Release(_this);
        }
        HopperVM_Push(Byte(ttype), Type::eType);
        break;
    }
    case SysCall::eDictionaryNew:
    {
        Type stype = (Type)0;
        Type vtype = Type(HopperVM_Pop_R(stype));
        Type ktype = Type(HopperVM_Pop_R(stype));
        UInt address = HRDictionary_New(ktype, vtype);
        HopperVM_Push(address, Type::eDictionary);
        break;
    }
    case SysCall::eDictionaryCountGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt count = HRDictionary_GetCount(_this);
        GC_Release(_this);
        HopperVM_Push(count, Type::eUInt);
        break;
    }
    case SysCall::eDictionarySet:
    {
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRDictionary_Set(_this, key, ktype, value, vtype);
        if (Types_IsReferenceType(ktype))
        {
            GC_Release(key);
        }
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(value);
        }
        GC_Release(_this);
        break;
    }
    case SysCall::eDictionaryNext:
    {
        Type htype = (Type)0;
        UInt iterator = HopperVM_Pop_R(htype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt hrpair = 0;
        UInt found = (HRDictionary_Next_R(_this, iterator, hrpair)) ? (0x01) : (0x00);
        GC_Release(_this);
        HopperVM_Push(found, Type::eBool);
        HopperVM_Push(hrpair, Type::ePair);
        HopperVM_Push(iterator, Type::eUInt);
        break;
    }
    case SysCall::eDictionaryContains:
    {
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt found = (HRDictionary_Contains(_this, key)) ? (0x01) : (0x00);
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        GC_Release(_this);
        HopperVM_Push(found, Type::eBool);
        break;
    }
    case SysCall::eDictionaryGet:
    {
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type vtype = (Type)0;
        UInt result = HRDictionary_Get_R(_this, key, vtype);
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        GC_Release(_this);
        HopperVM_Push(result, vtype);
        break;
    }
    case SysCall::eDictionaryClear:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRDictionary_Clear(_this);
        GC_Release(_this);
        break;
    }
    case SysCall::eCharToDigit:
    {
        Type htype = (Type)0;
        UInt b = Byte(HopperVM_Pop_R(htype));
        HopperVM_Push(UInt(b + 0x30), Type::eChar);
        break;
    }
    case SysCall::eUIntToLong:
    {
        Type htype = (Type)0;
        UInt value = HopperVM_Pop_R(htype);
        UInt lng = HRUInt_ToLong(value);
        HopperVM_Push(lng, Type::eLong);
        break;
    }
    case SysCall::eIntToLong:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt lng = HRInt_ToLong(ichunk);
        HopperVM_Push(lng, Type::eLong);
        break;
    }
    case SysCall::eIntToFloat:
    {
        Type htype = (Type)0;
        Int ichunk = HopperVM_PopI_R(htype);
        UInt f = External_IntToFloat(ichunk);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCall::eUIntToFloat:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt f = External_UIntToFloat(ichunk);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCall::eIntToBytes:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt lst = HRInt_ToBytes(ichunk);
        HopperVM_Push(lst, Type::eList);
        break;
    }
    case SysCall::eLongToBytes:
    {
        Type htype = (Type)0;
        UInt l = HopperVM_Pop_R(htype);
        UInt lst = HRLong_ToBytes(l);
        HopperVM_Push(lst, Type::eList);
        break;
    }
    case SysCall::eLongGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt l = HopperVM_Pop_R(htype);
        Byte b = HRLong_GetByte(l, index);
        HopperVM_Push(b, Type::eByte);
        break;
    }
    case SysCall::eLongFromBytes:
    {
        Byte b3 = Byte(HopperVM_Pop());
        Byte b2 = Byte(HopperVM_Pop());
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt l = HRLong_FromBytes(b0, b1, b2, b3);
        HopperVM_Push(l, Type::eLong);
        break;
    }
    case SysCall::eIntGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt i = HopperVM_Pop();
        Byte b = HRInt_GetByte(i, index);
        HopperVM_Push(b, Type::eByte);
        break;
    }
    case SysCall::eIntFromBytes:
    {
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt i = HRInt_FromBytes(b0, b1);
        HopperVM_Push(i, Type::eInt);
        break;
    }
    case SysCall::eFloatToBytes:
    {
        Type htype = (Type)0;
        UInt l = HopperVM_Pop_R(htype);
        UInt lst = HRFloat_ToBytes(l);
        HopperVM_Push(lst, Type::eList);
        break;
    }
    case SysCall::eFloatToString:
    {
        Type htype = (Type)0;
        UInt l = HopperVM_Pop_R(htype);
        UInt str = External_FloatToString(l);
        HopperVM_Push(str, Type::eString);
        break;
    }
    case SysCall::eFloatGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt f = HopperVM_Pop_R(htype);
        Byte b = HRFloat_GetByte(f, index);
        HopperVM_Push(b, Type::eByte);
        break;
    }
    case SysCall::eFloatFromBytes:
    {
        Byte b3 = Byte(HopperVM_Pop());
        Byte b2 = Byte(HopperVM_Pop());
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt f = HRFloat_FromBytes(b0, b1, b2, b3);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCall::eLongToUInt:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt ui = HRLong_ToUInt(_this);
        HopperVM_Push(ui, Type::eUInt);
        GC_Release(_this);
        break;
    }
    case SysCall::eLongToFloat:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt f = External_LongToFloat(_this);
        HopperVM_Push(f, Type::eFloat);
        GC_Release(_this);
        break;
    }
    case SysCall::eLongNegate:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        UInt result = 0;
        Type rtype = Type::eLong;
        result = HRLong_LongNegate(top);
        HopperVM_Push(result, rtype);
        GC_Release(top);
        break;
    }
    case SysCall::eLongAdd:
    case SysCall::eLongSub:
    case SysCall::eLongDiv:
    case SysCall::eLongMul:
    case SysCall::eLongMod:
    case SysCall::eLongEQ:
    case SysCall::eLongLT:
    case SysCall::eLongLE:
    case SysCall::eLongGT:
    case SysCall::eLongGE:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eLong;
        switch (SysCall(iSysCall))
        {
        case SysCall::eLongAdd:
        {
            result = External_LongAdd(next, top);
            break;
        }
        case SysCall::eLongSub:
        {
            result = External_LongSub(next, top);
            break;
        }
        case SysCall::eLongDiv:
        {
            result = External_LongDiv(next, top);
            break;
        }
        case SysCall::eLongMul:
        {
            result = External_LongMul(next, top);
            break;
        }
        case SysCall::eLongMod:
        {
            result = External_LongMod(next, top);
            break;
        }
        case SysCall::eLongEQ:
        {
            result = External_LongEQ(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eLongLT:
        {
            result = External_LongLT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eLongLE:
        {
            result = External_LongLE(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eLongGT:
        {
            result = External_LongGT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eLongGE:
        {
            result = External_LongGE(next, top);
            rtype = Type::eBool;
            break;
        }
        } // switch
        HopperVM_Push(result, rtype);
        GC_Release(top);
        GC_Release(next);
        break;
    }
    case SysCall::eFloatAdd:
    case SysCall::eFloatSub:
    case SysCall::eFloatDiv:
    case SysCall::eFloatMul:
    case SysCall::eFloatEQ:
    case SysCall::eFloatLT:
    case SysCall::eFloatLE:
    case SysCall::eFloatGT:
    case SysCall::eFloatGE:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eFloat;
        switch (SysCall(iSysCall))
        {
        case SysCall::eFloatAdd:
        {
            result = External_FloatAdd(next, top);
            break;
        }
        case SysCall::eFloatSub:
        {
            result = External_FloatSub(next, top);
            break;
        }
        case SysCall::eFloatDiv:
        {
            result = External_FloatDiv(next, top);
            break;
        }
        case SysCall::eFloatMul:
        {
            result = External_FloatMul(next, top);
            break;
        }
        case SysCall::eFloatEQ:
        {
            result = External_FloatEQ(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eFloatLT:
        {
            result = External_FloatLT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eFloatLE:
        {
            result = External_FloatLE(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eFloatGT:
        {
            result = External_FloatGT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCall::eFloatGE:
        {
            result = External_FloatGE(next, top);
            rtype = Type::eBool;
            break;
        }
        } // switch
        HopperVM_Push(result, rtype);
        GC_Release(top);
        GC_Release(next);
        break;
    }
    case SysCall::eLongNew:
    {
        UInt address = HRLong_New();
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCall::eFloatNew:
    {
        UInt address = HRFloat_New();
        HopperVM_Push(address, Type::eFloat);
        break;
    }
    case SysCall::eTimeMillis:
    {
        UInt address = External_GetMillis();
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCall::eTimeDelay:
    {
        External_Delay(HopperVM_Pop());
        doNext = false;
        break;
    }
    case SysCall::eMCUPinMode:
    {
        Byte mode = Byte(HopperVM_Pop());
        Byte pin = Byte(HopperVM_Pop());
        External_PinMode(pin, mode);
        break;
    }
    case SysCall::eMCUDigitalWrite:
    {
        Byte value = Byte(HopperVM_Pop());
        Byte pin = Byte(HopperVM_Pop());
        External_DigitalWrite(pin, value);
        break;
    }
    case SysCall::eMCUDigitalRead:
    {
        Byte pin = Byte(HopperVM_Pop());
        Byte value = External_DigitalRead(pin);
        HopperVM_Push(value, Type::eByte);
        break;
    }
    default:
    {
        Runtime_Out4Hex(HopperVM_PC_Get());
        Serial_WriteChar(':');
        Serial_WriteChar('S');
        Runtime_Out2Hex(iSysCall);
        Serial_WriteChar(' ');
        IO_WriteHex(HopperVM_PC_Get());
        IO_Write(':');
        IO_Write('S');
        IO_WriteHex(iSysCall);
        IO_Write(' ');
        Runtime_ErrorDump(0x02);
        Minimal_Error_Set(0x0A);
        break;
    }
    } // switch
    return doNext && (Minimal_Error_Get() == 0x00);
}

UInt HopperVM_Get(UInt address)
{
    return Memory_ReadWord(HopperVM_valueStack + address);
}

Int HopperVM_PopI_R(Type & htype)
{
    HopperVM_sp = HopperVM_sp - 0x02;
    UInt value = Memory_ReadWord(HopperVM_valueStack + HopperVM_sp);
    htype = Type(Memory_ReadWord(HopperVM_typeStack + HopperVM_sp));
    return External_UIntToInt(value);
}

Bool Types_IsReferenceType(Type htype)
{
    return (Byte(htype) >= 0x0D);
}

void GC_AddReference(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    
    referenceCount++;
    Memory_WriteByte(address + 0x01, referenceCount);
}

UInt GC_Clone(UInt original)
{
    Type htype = Type(Memory_ReadByte(original));
    switch (htype)
    {
    case Type::eLong:
    {
        return HRLong_Clone(original);
        break;
    }
    case Type::eDirectory:
    {
        return HRDirectory_Clone(original);
        break;
    }
    case Type::eFile:
    {
        return HRFile_Clone(original);
        break;
    }
    case Type::eFloat:
    {
        return HRFloat_Clone(original);
        break;
    }
    case Type::eString:
    {
        return HRString_Clone(original);
        break;
    }
    case Type::eList:
    {
        return HRList_Clone(original);
        break;
    }
    case Type::eDictionary:
    {
        return HRDictionary_Clone(original);
        break;
    }
    case Type::ePair:
    {
        return HRPair_Clone(original);
        break;
    }
    case Type::eVariant:
    {
        return HRVariant_Clone(original);
        break;
    }
    default:
    {
        break;
    }
    } // switch
    return 0x00;
}

void IO_WriteUInt(UInt _this)
{
    IO_writeDigit(_this);
}

void IO_Clear()
{
    Serial_WriteChar(Char(0x0C));
}

void IO_WriteHex(UInt u)
{
    Byte msb = Byte(u >> 0x08);
    IO_WriteHex(msb);
    Byte lsb = Byte(u & 0xFF);
    IO_WriteHex(lsb);
}

void IO_WriteHex(Byte b)
{
    Byte msn = ((b >> 0x04) & 0x0F);
    IO_Write(Char_ToHex(msn));
    Byte lsn = b & 0x0F;
    IO_Write(Char_ToHex(lsn));
}

void IO_writeDigit(UInt uthis)
{
    UInt digit = uthis % 0x0A;
    Char c = Char_ToDigit(Byte(digit));
    uthis = uthis / 0x0A;
    if (uthis != 0x00)
    {
        IO_writeDigit(uthis);
    }
    IO_Write(c);
}

UInt HRFile_ReadLine(UInt _this)
{
    UInt str = HRString_New();
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt buffer = Memory_ReadWord(_this + 9);
            UInt pos = Memory_ReadWord(_this + 7);
            UInt length = HRList_GetLength(buffer);
            if (pos < length)
            {
                for (;;)
                {
                    if (pos == length)
                    {
                        if (HRString_GetLength(str) > 0x00)
                        {
                            Memory_WriteByte(_this + 2, 0x01);
                        }
                        else
                        {
                            Memory_WriteByte(_this + 2, 0x00);
                        }
                        break;;
                    }
                    Type itype = (Type)0;
                    Byte b = Byte(HRList_GetItem_R(buffer, pos, itype));
                    
                    pos++;
                    Memory_WriteWord(_this + 7, pos);
                    if (b == 0x0D)
                    {
                        continue;;
                    }
                    if (b == 0x0A)
                    {
                        break;;
                    }
                    HRString_Build_R(str, Char(b));
                }
                break;;
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return str;
}

Byte HRFile_Read(UInt _this, UInt hrseekpos)
{
    Byte b = 0;
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt buffer = Memory_ReadWord(_this + 9);
            UInt seekpos = HRLong_ToUInt(hrseekpos);
            UInt length = HRList_GetLength(buffer);
            if (seekpos < length)
            {
                Type itype = (Type)0;
                b = Byte(HRList_GetItem_R(buffer, seekpos, itype));
                break;;
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return b;
}

void HRFile_Append(UInt _this, UInt hrstr)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00))
    {
        UInt buffer = Memory_ReadWord(_this + 9);
        UInt length = HRString_GetLength(hrstr);;
        for (UInt i = 0x00; i < length; i++)
        {
            HRList_Append(buffer, Byte(HRString_GetChar(hrstr, i)), Type::eByte);
        }
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

UInt HRFile_GetTime(UInt path)
{
    return External_FileGetTime(path);
}

UInt HRFile_GetSize(UInt path)
{
    return External_FileGetSize(path);
}

UInt HRFile_Clone(UInt original)
{
    UInt address = GC_New(0x0B, Type::eFile);
    Memory_WriteByte(address + 2, Memory_ReadByte(original + 2));
    Memory_WriteByte(address + 3, Memory_ReadByte(original + 3));
    Memory_WriteByte(address + 4, Memory_ReadByte(original + 4));
    Memory_WriteWord(address + 5, HRString_Clone(Memory_ReadWord(original + 5)));
    Memory_WriteWord(address + 7, Memory_ReadWord(original + 7));
    Memory_WriteWord(address + 9, HRList_Clone(Memory_ReadWord(original + 9)));
    return address;
}

UInt HRDirectory_New()
{
    UInt address = GC_New(0x03, Type::eDirectory);
    Memory_WriteByte(address + 2, 0x00);
    Memory_WriteWord(address + 3, HRString_New());
    return address;
}

UInt HRDirectory_Open(UInt hrpath)
{
    UInt address = HRDirectory_New();
    Memory_WriteWord(address + 3, HRString_Clone(hrpath));
    if (HRDirectory_Exists(hrpath))
    {
        Memory_WriteByte(address + 2, 0x01);
    }
    return address;
}

Bool HRDirectory_IsValid(UInt _this)
{
    return (Memory_ReadByte(_this + 2) != 0x00);
}

UInt HRDirectory_GetFileCount(UInt hrdir)
{
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetFileCount(Memory_ReadWord(hrdir + 3))) : (0x00);
}

UInt HRDirectory_GetDirectoryCount(UInt hrdir)
{
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetDirectoryCount(Memory_ReadWord(hrdir + 3))) : (0x00);
}

UInt HRDirectory_GetFile(UInt hrdir, UInt index)
{
    if (!HRDirectory_IsValid(hrdir))
    {
        return HRString_New();
    }
    return External_DirectoryGetFile(Memory_ReadWord(hrdir + 3), index);
}

UInt HRDirectory_GetDirectory(UInt hrdir, UInt index)
{
    if (!HRDirectory_IsValid(hrdir))
    {
        return HRString_New();
    }
    return External_DirectoryGetDirectory(Memory_ReadWord(hrdir + 3), index);
}

void HRDirectory_Delete(UInt hrpath)
{
    External_DirectoryDelete(hrpath);
}

UInt HRDirectory_GetTime(UInt hrpath)
{
    return External_DirectoryGetTime(hrpath);
}

UInt HRDirectory_Clone(UInt original)
{
    UInt address = GC_New(0x03, Type::eDirectory);
    Memory_WriteByte(address + 2, Memory_ReadByte(original + 2));
    Memory_WriteWord(address + 3, HRString_Clone(Memory_ReadWord(original + 3)));
    return address;
}

Char HRString_GetChar(UInt _this, UInt index)
{
    UInt length = HRString_GetLength(_this);
    return Char(Memory_ReadByte(_this + 0x04 + index));
}

UInt HRString_NewFromConstant0(UInt location, UInt length)
{
    UInt address = GC_New(0x02 + length, Type::eString);
    Memory_WriteWord(address + 0x02, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(address + 0x04 + i, Memory_ReadCodeByte(location + i));
    }
    return address;
}

UInt HRString_NewFromConstant1(UInt doubleChar)
{
    Byte lsb = Byte(doubleChar & 0xFF);
    Byte msb = Byte(doubleChar >> 0x08);
    UInt address = GC_New(((msb == 0x00)) ? (0x03) : (0x04), Type::eString);
    Memory_WriteWord(address + 2, ((msb == 0x00)) ? (0x01) : (0x02));
    Memory_WriteByte(address + 4, lsb);
    if (msb != 0x00)
    {
        Memory_WriteByte(address + 0x05, msb);
    }
    return address;
}

UInt HRString_InsertChar(UInt _this, UInt index, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = GC_New(0x02 + length + 0x01, Type::eString);
    UInt j = 0x00;;
    for (UInt i = 0x00; i < length; i++)
    {
        if (i == index)
        {
            Memory_WriteByte(result + 0x04 + j, Byte(ch));
            
            j++;
        }
        Memory_WriteByte(result + 0x04 + j, Memory_ReadByte(_this + 0x04 + i));
        
        j++;
    }
    if ((length == 0x00) || (index >= length))
    {
        Memory_WriteByte(result + 0x04 + j, Byte(ch));
    }
    Memory_WriteWord(result + 0x02, length + 0x01);
    return result;
}

Bool HRString_EndsWith(UInt _this, Char with)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0x00)
    {
        return false;
    }
    return (Char(Memory_ReadByte(_this + 0x04 + length - 0x01)) == with);
}

Bool HRString_EndsWith(UInt _this, UInt with)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(with);
    if (length0 < length1)
    {
        return false;
    }
    if (length1 == 0x00)
    {
        return true;
    }
    UInt i = 0x01;
    for (;;)
    {
        Char w = Char(Memory_ReadByte(with + 0x04 + length1 - i));
        Char t = Char(Memory_ReadByte(_this + 0x04 + length0 - i));
        if (w != t)
        {
            return false;
        }
        if (i == length1)
        {
            break;;
        }
        
        i++;
    }
    return true;
}

Int HRString_Compare(UInt left, UInt right)
{
    UInt i = 0;
    Int result = 0;
    UInt ll = HRString_GetLength(left);
    UInt rl = HRString_GetLength(right);
    for (;;)
    {
        if (i >= ll)
        {
            break;;
        }
        if (i >= rl)
        {
            break;;
        }
        if (Memory_ReadByte(left + 4 + i) != Memory_ReadByte(right + 4 + i))
        {
            break;;
        }
        
        i++;
    }
    for (;;)
    {
        if ((ll == 0x00) && (rl == 0x00))
        {
            break;;
        }
        if ((i < ll) && (i < rl))
        {
            if (Int(Memory_ReadByte(left + 4 + i)) > Int(Memory_ReadByte(right + 4 + i)))
            {
                result = 0x01;
            }
            else
            {
                result = -0x01;
            }
            break;;
        }
        if (i >= ll)
        {
            if (i >= rl)
            {
                break;;
            }
            result = -0x01;
            break;;
        }
        result = 0x01;
        break;;
    }
    return result;
}

UInt HRString_Replace(UInt _this, UInt pattern, UInt replace)
{
    UInt result = 0;
    UInt patternLength = HRString_GetLength(pattern);
    if (patternLength == 0x00)
    {
        result = HRString_clone(_this, 0x00);
        return result;
    }
    UInt originalLength = HRString_GetLength(_this);
    UInt replaceLength = HRString_GetLength(replace);
    if (replaceLength <= patternLength)
    {
        result = HRString_clone(_this, 0x00);
    }
    else
    {
        result = HRString_clone(_this, (replaceLength - patternLength) * originalLength);
    }
    UInt i = 0;
    UInt j = 0;
    for (;;)
    {
        if (i == originalLength)
        {
            break;;
        }
        Bool match = false;
        if (i + patternLength <= originalLength)
        {
            match = true;;
            for (UInt n = 0x00; n < patternLength; n++)
            {
                if (Memory_ReadByte(_this + 0x04 + i + n) != Memory_ReadByte(pattern + 0x04 + n))
                {
                    match = false;
                    break;;
                }
            }
        }
        if (match)
        {
            i = i + patternLength;;
            for (UInt n = 0x00; n < replaceLength; n++)
            {
                Memory_WriteByte(result + 0x04 + j, Memory_ReadByte(replace + 0x04 + n));
                
                j++;
            }
        }
        else
        {
            Memory_WriteByte(result + 0x04 + j, Memory_ReadByte(_this + 0x04 + i));
            
            j++;
            
            i++;
        }
    }
    Memory_WriteWord(result + 0x02, j);
    return result;
}

UInt HRString_Replace(UInt _this, Char from, Char to)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 0x00);;
    for (UInt i = 0x00; i < length; i++)
    {
        Char ch = Char(Memory_ReadByte(_this + 0x04 + i));
        if (ch == from)
        {
            Memory_WriteByte(result + 0x04 + i, Byte(to));
        }
    }
    return result;
}

UInt HRString_Append(UInt _this, UInt append)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(append);
    UInt result = GC_New(0x02 + length0 + length1, Type::eString);
    Memory_WriteWord(result + 0x02, length0 + length1);;
    for (UInt i = 0x00; i < length0; i++)
    {
        Memory_WriteByte(result + 0x04 + i, Memory_ReadByte(_this + 0x04 + i));
    };
    for (UInt i = 0x00; i < length1; i++)
    {
        Memory_WriteByte(result + 0x04 + i + length0, Memory_ReadByte(append + 0x04 + i));
    }
    return result;
}

UInt HRString_Append(UInt _this, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 0x01);
    Memory_WriteByte(result + 0x04 + length, Byte(ch));
    Memory_WriteWord(result + 0x02, length + 0x01);
    return result;
}

UInt HRString_Substring(UInt _this, UInt start)
{
    UInt limit = HRString_GetLength(_this);
    return HRString_Substring(_this, start, limit);
}

UInt HRString_Substring(UInt _this, UInt start, UInt limit)
{
    UInt length0 = HRString_GetLength(_this);
    if (start >= length0)
    {
        start = length0;
    }
    UInt length1 = length0 - start;
    UInt result = GC_New(0x02 + length1, Type::eString);
    UInt newLength = 0x00;;
    for (UInt i = 0x00; i < length1; i++)
    {
        if (newLength == limit)
        {
            break;;
        }
        Memory_WriteByte(result + 0x04 + i, Memory_ReadByte(_this + 0x04 + i + start));
        
        newLength++;
    }
    Memory_WriteWord(result + 0x02, newLength);
    return result;
}

void HRString_Substring_R(UInt & _this, UInt start)
{
    UInt length = HRString_GetLength(_this);
    if (start == 0x00)
    {
        return;
    }
    if (start >= length)
    {
        Memory_WriteWord(_this + 0x02, 0x00);
        return;
    }
    Memory_WriteWord(_this + 0x02, length - start);
    UInt i = start;
    UInt j = 0x00;
    for (;;)
    {
        Memory_WriteByte(_this + 0x04 + j, Memory_ReadByte(_this + 0x04 + i));
        
        i++;
        
        j++;
        if (i == length)
        {
            break;;
        }
    }
}

void HRString_Build_R(UInt & _this, UInt append)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(append);
    if (length1 > 0x00)
    {
        if (capacity < length0 + length1)
        {
            UInt copy = HRString_clone(_this, length0 + length1 - capacity);
            GC_Release(_this);
            _this = copy;
        };
        for (UInt i = 0x00; i < length1; i++)
        {
            Memory_WriteByte(_this + 0x04 + length0 + i, Memory_ReadByte(append + 0x04 + i));
        }
        Memory_WriteWord(_this + 0x02, length0 + length1);
    }
}

void HRString_BuildFront_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 0x01)
    {
        UInt copy = HRString_clone(_this, 0x01);
        GC_Release(_this);
        _this = copy;
    }
    UInt i = length;
    for (;;)
    {
        Memory_WriteByte(_this + 0x04 + i, Memory_ReadByte(_this + 0x03 + i));
        
        i--;
        if (i == 0x00)
        {
            break;;
        }
    }
    Memory_WriteByte(_this + 0x04, Byte(ch));
    Memory_WriteWord(_this + 0x02, length + 0x01);
}

UInt HRString_Trim(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimRight_R(copy);
    HRString_TrimLeft_R(copy);
    return copy;
}

void HRString_TrimRight_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0x00)
    {
        return;
    }
    UInt i = length - 0x01;
    for (;;)
    {
        Char ch = Char(Memory_ReadByte(_this + 0x04 + i));
        if (ch != ' ')
        {
            Memory_WriteWord(_this + 0x02, i + 0x01);
            break;;
        }
        if (i == 0x00)
        {
            Memory_WriteWord(_this + 0x02, 0x00);
            break;;
        }
        
        i--;
    }
}

void HRString_TrimLeft_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0x00;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = Char(Memory_ReadByte(_this + 0x04 + i));
        if (ch != ' ')
        {
            break;;
        }
        
        i++;
    }
    HRString_Substring_R(_this, i);
}

UInt HRString_TrimLeft(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimLeft_R(copy);
    return copy;
}

UInt HRLong_NewFromConstant(UInt location)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, Memory_ReadCodeWord(location));
    Memory_WriteWord(address + 0x04, Memory_ReadCodeWord(location + 0x02));
    return address;
}

UInt HRLong_ToBytes(UInt ichunk)
{
    UInt lst = HRList_New(Type::eByte);;
    for (Byte i = 0x00; i < 0x04; i++)
    {
        Byte b = Memory_ReadByte(ichunk + 0x02 + i);
        HRList_Append(lst, b, Type::eByte);
    }
    return lst;
}

Byte HRLong_GetByte(UInt ichunk, UInt i)
{
    return Memory_ReadByte(ichunk + 0x02 + i);
}

UInt HRLong_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteByte(address + 0x02, b0);
    Memory_WriteByte(address + 0x02 + 0x01, b1);
    Memory_WriteByte(address + 0x02 + 0x02, b2);
    Memory_WriteByte(address + 0x02 + 0x03, b3);
    return address;
}

UInt HRLong_ToUInt(UInt _this)
{
    UInt value = Memory_ReadWord(_this + 0x04);
    return Memory_ReadWord(_this + 0x02);
}

UInt HRLong_LongNegate(UInt top)
{
    UInt zero = HRUInt_ToLong(0x00);
    UInt result = External_LongSub(zero, top);
    GC_Release(zero);
    return result;
}

UInt HRLong_New()
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, 0x00);
    Memory_WriteWord(address + 0x04, 0x00);
    return address;
}

UInt HRLong_Clone(UInt original)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, Memory_ReadWord(original + 0x02));
    Memory_WriteWord(address + 0x04, Memory_ReadWord(original + 0x04));
    return address;
}

UInt HRFloat_NewFromConstant(UInt location)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, Memory_ReadCodeWord(location));
    Memory_WriteWord(address + 0x04, Memory_ReadCodeWord(location + 0x02));
    return address;
}

UInt HRFloat_ToBytes(UInt ichunk)
{
    UInt lst = HRList_New(Type::eByte);;
    for (Byte i = 0x00; i < 0x04; i++)
    {
        Byte b = Memory_ReadByte(ichunk + 0x02 + i);
        HRList_Append(lst, b, Type::eByte);
    }
    return lst;
}

Byte HRFloat_GetByte(UInt ichunk, UInt i)
{
    return Memory_ReadByte(ichunk + 0x02 + i);
}

UInt HRFloat_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteByte(address + 0x02, b0);
    Memory_WriteByte(address + 0x03, b1);
    Memory_WriteByte(address + 0x04, b2);
    Memory_WriteByte(address + 0x05, b3);
    return address;
}

UInt HRFloat_New()
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, 0x00);
    Memory_WriteWord(address + 0x04, 0x00);
    return address;
}

UInt HRFloat_Clone(UInt original)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, Memory_ReadWord(original + 0x02));
    Memory_WriteWord(address + 0x04, Memory_ReadWord(original + 0x04));
    return address;
}

UInt HRArray_New(Type htype, UInt count)
{
    UInt elementbytes = 0;
    switch (htype)
    {
    case Type::eBool:
    {
        elementbytes = (count + 0x07) >> 0x03;
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        elementbytes = count;
        break;
    }
    default:
    {
        elementbytes = count * 0x02;
        break;
    }
    } // switch
    UInt _this = GC_New(0x03 + elementbytes, Type::eArray);
    Memory_WriteWord(_this + 2, count);
    Memory_WriteByte(_this + 4, Byte(htype));
    UInt address = _this + 5;;
    for (UInt i = 0x00; i < elementbytes; i++)
    {
        Memory_WriteByte(address, 0x00);
        
        address++;
    }
    return _this;
}

UInt HRArray_GetItem_R(UInt _this, UInt index, Type & etype)
{
    UInt elements = Memory_ReadWord(_this + 2);
    etype = Type(Memory_ReadByte(_this + 4));
    UInt address = _this + 5;
    UInt value = 0;
    switch (etype)
    {
    case Type::eBool:
    {
        UInt offset = address + (index >> 0x03);
        Byte slotIndex = Byte(index & 0x07);
        Byte b = Memory_ReadByte(offset);
        Byte mask = Memory_ReadByte(HRArray_setSlots + slotIndex);
        value = b & mask;
        if (value != 0x00)
        {
            value = 0x01;
        }
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        value = Memory_ReadByte(address + index);
        break;
    }
    default:
    {
        UInt offset = address + (index << 0x01);
        Byte lsb = Memory_ReadByte(offset);
        Byte msb = Memory_ReadByte(offset + 0x01);
        value = (msb << 0x08) | lsb;
        break;
    }
    } // switch
    return value;
}

void HRArray_SetItem(UInt _this, UInt index, UInt value)
{
    UInt elements = Memory_ReadWord(_this + 2);
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt address = _this + 5;
    switch (etype)
    {
    case Type::eBool:
    {
        UInt offset = address + (index >> 0x03);
        Byte slotIndex = Byte(index & 0x07);
        Byte b = Memory_ReadByte(offset);
        if (value == 0x00)
        {
            Byte mask = Memory_ReadByte(HRArray_clearSlots + slotIndex);
            b = b & mask;
        }
        else
        {
            Byte mask = Memory_ReadByte(HRArray_setSlots + slotIndex);
            b = b | mask;
        }
        Memory_WriteByte(offset, b);
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        Memory_WriteByte(address + index, Byte(value & 0xFF));
        break;
    }
    default:
    {
        UInt offset = address + (index << 0x01);
        Memory_WriteByte(offset, Byte(value & 0xFF));
        Memory_WriteByte(offset + 0x01, Byte(value >> 0x08));
        break;
    }
    } // switch
}

UInt HRArray_GetCount(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

void HRList_SetItem(UInt _this, UInt index, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt length = Memory_ReadWord(_this + 2);
    UInt pData = item;
    if (Types_IsReferenceType(etype))
    {
        if (Types_IsReferenceType(itype))
        {
            pData = GC_Clone(item);
        }
        else
        {
            pData = HRVariant_CreateValueVariant(item, itype);
        }
    }
    UInt i = 0x00;
    UInt pCurrent = Memory_ReadWord(_this + 5);
    UInt pRecent = Memory_ReadWord(_this + 7);
    if (pRecent != 0x00)
    {
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        if (iRecent <= index)
        {
            i = iRecent;
            pCurrent = pRecent;
        }
    }
    for (;;)
    {
        if (i == index)
        {
            break;;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
        
        i++;
    }
    UInt oldData = Memory_ReadWord(pCurrent + 0);
    if (Types_IsReferenceType(etype))
    {
        GC_Release(oldData);
    }
    Memory_WriteWord(pCurrent + 0, pData);
    Memory_WriteWord(_this + 7, pCurrent);
    Memory_WriteWord(_this + 7 + 0x02, index);
}

void HRList_Insert(UInt _this, UInt index, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt length = Memory_ReadWord(_this + 2);
    UInt pFirst = Memory_ReadWord(_this + 5);
    if (index >= length)
    {
        HRList_Append(_this, item, itype);
    }
    else if (index == 0x00)
    {
        UInt pItem = HRList_createItem(item, etype, itype);
        Memory_WriteWord(pItem + 2, pFirst);
        Memory_WriteWord(_this + 5, pItem);
        Memory_WriteWord(_this + 2, length + 0x01);
        Memory_WriteWord(_this + 7, 0x00);
        Memory_WriteWord(_this + 7 + 0x02, 0x00);
    }
    else
    {
        UInt pCurrent = pFirst;
        UInt pPrevious = 0x00;
        UInt pRecent = Memory_ReadWord(_this + 7);
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        UInt count = 0x00;
        if ((iRecent != 0x00) && (index > iRecent))
        {
            pCurrent = pRecent;
            count = iRecent;
        }
        UInt pItem = HRList_createItem(item, etype, itype);
        while (0x00 != pCurrent)
        {
            if (index == count)
            {
                Memory_WriteWord(pItem + 2, pCurrent);
                Memory_WriteWord(pPrevious + 2, pItem);
                Memory_WriteWord(_this + 7, pItem);
                Memory_WriteWord(_this + 7 + 0x02, count);
                Memory_WriteWord(_this + 2, length + 0x01);
                break;;
            }
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent + 2);
            
            count++;
        }
    }
}

void HRList_Remove(UInt _this, UInt index)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt length = Memory_ReadWord(_this + 2);
    UInt pCurrent = Memory_ReadWord(_this + 5);
    if (index == 0x00)
    {
        UInt pNext = Memory_ReadWord(pCurrent + 2);
        HRList_clearItem(pCurrent, etype);
        Memory_WriteWord(_this + 5, pNext);
    }
    else
    {
        UInt pPrevious = pCurrent;
        pCurrent = Memory_ReadWord(pCurrent + 2);
        UInt count = 0x01;
        while (count < index)
        {
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent + 2);
            
            count++;
        }
        Memory_WriteWord(pPrevious + 2, Memory_ReadWord(pCurrent + 2));
        HRList_clearItem(pCurrent, etype);
    }
    length = Memory_ReadWord(_this + 2) - 0x01;
    Memory_WriteWord(_this + 2, length);
    Memory_WriteWord(_this + 7, 0x00);
    Memory_WriteWord(_this + 7 + 0x02, 0x00);
}

Bool HRList_Contains(UInt _this, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pCurrent = Memory_ReadWord(_this + 5);
    for (;;)
    {
        if (0x00 == pCurrent)
        {
            break;;
        }
        Type dtype = etype;
        UInt pData = Memory_ReadWord(pCurrent + 0);
        if (Types_IsReferenceType(dtype))
        {
            dtype = Type(Memory_ReadByte(pData));
        }
        if (HRVariant_IsEqual(pData, dtype, item, itype))
        {
            return true;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
    }
    return false;
}

UInt HRList_Clone(UInt original)
{
    Type etype = Type(Memory_ReadByte(original + 4));
    UInt clone = HRList_New(etype);
    UInt pCurrentItem = Memory_ReadWord(original + 5);
    for (;;)
    {
        if (pCurrentItem == 0x00)
        {
            break;;
        }
        UInt itemData = Memory_ReadWord(pCurrentItem + 0);
        Type itype = etype;
        if (Types_IsReferenceType(etype))
        {
            itype = Type(Memory_ReadByte(itemData));
        }
        HRList_Append(clone, itemData, itype);
        pCurrentItem = Memory_ReadWord(pCurrentItem + 2);
    }
    return clone;
}

UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt value)
{
    UInt address = GC_New(0x06, Type::ePair);
    Memory_WriteByte(address + 2, Byte(ktype));
    Memory_WriteByte(address + 3, Byte(vtype));
    if (Types_IsReferenceType(ktype) && (0x00 != key))
    {
        key = GC_Clone(key);
    }
    if (Types_IsReferenceType(vtype) && (0x00 != value))
    {
        value = GC_Clone(value);
    }
    Memory_WriteWord(address + 4, key);
    Memory_WriteWord(address + 6, value);
    return address;
}

UInt HRPair_GetValue_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 3));
    UInt value = Memory_ReadWord(_this + 6);
    if (vtype == Type::eVariant)
    {
        value = HRVariant_GetValue_R(value, vtype);
    }
    else if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
        vtype = Type(Memory_ReadByte(value));
    }
    return value;
}

UInt HRPair_GetKey_R(UInt _this, Type & ktype)
{
    ktype = Type(Memory_ReadByte(_this + 2));
    UInt key = Memory_ReadWord(_this + 4);
    if (Types_IsReferenceType(ktype))
    {
        key = GC_Clone(key);
        ktype = Type(Memory_ReadByte(key));
    }
    return key;
}

UInt HRPair_Clone(UInt original)
{
    Type dkType = Type(Memory_ReadByte(original + 2));
    Type dvType = Type(Memory_ReadByte(original + 3));
    UInt key = Memory_ReadWord(original + 4);
    UInt value = Memory_ReadWord(original + 6);
    return HRPair_New(dkType, key, dvType, value);
}

UInt HRVariant_New(UInt value, Type vtype)
{
    UInt address = GC_New(0x03, Type::eVariant);
    Memory_WriteByte(address + 2, Byte(vtype));
    if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
    }
    Memory_WriteWord(address + 3, value);
    return address;
}

UInt HRVariant_Clone(UInt original)
{
    Type vtype = Type(Memory_ReadByte(original + 2));
    UInt value = Memory_ReadWord(original + 3);
    return HRVariant_New(value, vtype);
}

Bool HRVariant_IsEqual(UInt left, Type ltype, UInt right, Type rtype)
{
    Bool lref = Types_IsReferenceType(ltype);
    Bool rref = Types_IsReferenceType(rtype);
    if (lref == rref)
    {
        if (lref)
        {
            if (ltype == rtype)
            {
                switch (ltype)
                {
                case Type::eString:
                {
                    return 0x00 == HRString_Compare(left, right);
                    break;
                }
                case Type::eLong:
                {
                    return 0x00 != External_LongEQ(left, right);
                    break;
                }
                default:
                {
                    Runtime_ErrorDump(0x4B);
                    Minimal_Error_Set(0x0A);
                    break;
                }
                } // switch
            }
        }
        else
        {
            switch (ltype)
            {
            case Type::eBool:
            {
                if (rtype != Type::eBool)
                {
                    return false;
                }
                return left == right;
                break;
            }
            case Type::eChar:
            {
                if (rtype != Type::eChar)
                {
                    return false;
                }
                return left == right;
                break;
            }
            case Type::eByte:
            case Type::eUInt:
            {
                switch (rtype)
                {
                case Type::eByte:
                case Type::eUInt:
                {
                    return left == right;
                    break;
                }
                case Type::eInt:
                {
                    if (0x00 == (0x8000 & right))
                    {
                        return left == right;
                    }
                    break;
                }
                } // switch
                break;
            }
            case Type::eInt:
            {
                switch (rtype)
                {
                case Type::eByte:
                case Type::eUInt:
                {
                    if (0x00 == (0x8000 & left))
                    {
                        return left == right;
                    }
                    break;
                }
                case Type::eInt:
                {
                    return left == right;
                    break;
                }
                } // switch
                break;
            }
            } // switch
        }
    }
    return false;
}

UInt HRDictionary_New(Type ktype, Type vtype)
{
    UInt address = GC_New(0x08, Type::eDictionary);
    Memory_WriteByte(address + 2, Byte(ktype));
    Memory_WriteByte(address + 3, Byte(vtype));
    Memory_WriteWord(address + 4, 0x00);
    Memory_WriteWord(address + 6, 0x00);
    Memory_WriteWord(address + 8, 0x00);
    return address;
}

UInt HRDictionary_GetCount(UInt _this)
{
    return Memory_ReadWord(_this + 4);
}

void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt value, Type vtype)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt count = Memory_ReadWord(_this + 4);
    UInt capacity = Memory_ReadWord(_this + 6);
    Bool valueKeys = (dkType != Type::eString);
    if (0x04 * (count + 0x01) > capacity * 0x03)
    {
        UInt newCapacity = 0x20;
        if (capacity >= 0x80)
        {
            newCapacity = capacity + 0x20;
        }
        else if (capacity > 0x00)
        {
            newCapacity = capacity * 0x02;
        }
        HRDictionary_adjustCapacity(_this, newCapacity);
    }
    UInt pEntries = Memory_ReadWord(_this + 8);
    capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    UInt ekey = Memory_ReadWord(pEntry + 0);
    UInt evalue = Memory_ReadWord(pEntry + 6);
    Bool isNewKey = (ekey == 0x00);
    Bool existingValue = evalue != 0x00;
    if (valueKeys)
    {
        Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
        isNewKey = !isOccupied;
        existingValue = existingValue && isOccupied;
    }
    if (isNewKey && (evalue == 0x00))
    {
        Memory_WriteWord(_this + 4, Memory_ReadWord(_this + 4) + 0x01);
    }
    if (valueKeys)
    {
        Memory_WriteWord(pEntry + 0, key);
        Memory_WriteWord(pEntry + 2, 0x01);
    }
    else
    {
        if (0x00 != ekey)
        {
            GC_Release(ekey);
        }
        Memory_WriteWord(pEntry + 0, GC_Clone(key));
        Memory_WriteWord(pEntry + 2, hash);
    }
    if (Types_IsReferenceType(dvType))
    {
        if (existingValue)
        {
            GC_Release(evalue);
        }
        Memory_WriteWord(pEntry + 6, GC_Clone(value));
    }
    else
    {
        Memory_WriteWord(pEntry + 6, value);
    }
}

Bool HRDictionary_Next_R(UInt _this, UInt & iterator, UInt & hrpair)
{
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    Bool found = HRDictionary_next_R(_this, iterator, ktype, key, vtype, value);
    if (found && (0x00 != value) && (vtype == Type::eVariant))
    {
        value = HRVariant_UnBox_R(value, vtype);
    }
    hrpair = HRPair_New(ktype, key, vtype, value);
    return found;
}

Bool HRDictionary_Contains(UInt _this, UInt key)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    return HRDictionary_validEntry(pEntry, valueKeys);
}

UInt HRDictionary_Get_R(UInt _this, UInt key, Type & vtype)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    if (!HRDictionary_validEntry(pEntry, valueKeys))
    {
        Runtime_ErrorDump(0x59);
        Minimal_Error_Set(0x03);
        return 0x00;
    }
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt value = Memory_ReadWord(pEntry + 6);
    vtype = dvType;
    if (0x00 != value)
    {
        if (vtype == Type::eVariant)
        {
            value = HRVariant_GetValue_R(value, vtype);
        }
        else if (Types_IsReferenceType(vtype))
        {
            vtype = Type(Memory_ReadByte(value));
            value = GC_Clone(value);
        }
    }
    return value;
}

UInt HRDictionary_Clone(UInt original)
{
    Type dkType = Type(Memory_ReadByte(original + 2));
    Type dvType = Type(Memory_ReadByte(original + 3));
    UInt clone = HRDictionary_New(dkType, dvType);
    UInt iterator = 0;
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    while (HRDictionary_next_R(original, iterator, ktype, key, vtype, value))
    {
        HRDictionary_Set(clone, key, ktype, value, vtype);
    }
    return clone;
}

void HRDictionary_adjustCapacity(UInt _this, UInt newCapacity)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt sizeRequired = 8 * newCapacity;
    UInt pNewEntries = Memory_Allocate(sizeRequired);
    Memory_Set(pNewEntries, 0x00, sizeRequired);
    UInt pOldEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt count = 0x00;;
    for (UInt i = 0x00; i < capacity; i++)
    {
        UInt pOldEntry = pOldEntries + i * 8;
        if (!HRDictionary_validEntry(pOldEntry, valueKeys))
        {
            continue;;
        }
        UInt key = Memory_ReadWord(pOldEntry + 0);
        UInt hash = Memory_ReadWord(pOldEntry + 2);
        UInt pNewEntry = HRDictionary_findEntry(pNewEntries, newCapacity, key, hash, valueKeys);
        Memory_WriteWord(pNewEntry + 0, key);
        Memory_WriteWord(pNewEntry + 2, hash);
        Memory_WriteWord(pNewEntry + 6, Memory_ReadWord(pOldEntry + 6));
        
        count++;
    }
    if (0x00 != pOldEntries)
    {
        Memory_Free(pOldEntries);
    }
    Memory_WriteWord(_this + 4, count);
    Memory_WriteWord(_this + 6, newCapacity);
    Memory_WriteWord(_this + 8, pNewEntries);
}

UInt HRDictionary_hashKey16(UInt key)
{
    UInt length = Memory_ReadWord(key + 0x02);
    UInt hash = 0x9DC5;;
    for (UInt i = 0x00; i < length; i++)
    {
        UInt ch = Memory_ReadByte(key + 0x04 + i);
        hash = hash ^ ch;
        hash = hash * 0x0193;
    }
    return hash;
}

UInt HRDictionary_findEntry(UInt pEntries, UInt capacity, UInt key, UInt hash, Bool valueKeys)
{
    UInt index = 0;
    if (valueKeys)
    {
        index = key % capacity;
    }
    else
    {
        index = hash % capacity;
    }
    UInt tombstone = 0x00;
    for (;;)
    {
        UInt pEntry = pEntries + index * 8;
        UInt ekey = Memory_ReadWord(pEntry + 0);
        if (valueKeys)
        {
            Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
            if (!isOccupied)
            {
                UInt value = Memory_ReadWord(pEntry + 6);
                if (value == 0x00)
                {
                    return (tombstone != 0x00) ? (tombstone) : (pEntry);
                }
                else
                {
                    if (tombstone == 0x00)
                    {
                        tombstone = pEntry;
                    }
                }
            }
            else if (ekey == key)
            {
                return pEntry;
            }
        }
        else
        {
            if (ekey == 0x00)
            {
                UInt value = Memory_ReadWord(pEntry + 6);
                if (value == 0x00)
                {
                    return (tombstone != 0x00) ? (tombstone) : (pEntry);
                }
                else
                {
                    if (tombstone == 0x00)
                    {
                        tombstone = pEntry;
                    }
                }
            }
            else if (HRString_Compare(ekey, key) == 0x00)
            {
                return pEntry;
            }
        }
        index = (index + 0x01) % capacity;
    }
    return 0x00;
}

Bool HRDictionary_validEntry(UInt pEntry, Bool valueKeys)
{
    if (pEntry == 0x00)
    {
        return false;
    }
    else if (valueKeys)
    {
        return Memory_ReadByte(pEntry + 2) != 0x00;
    }
    else
    {
        return Memory_ReadWord(pEntry + 0) != 0x00;
    }
    return false;
}

UInt HRUInt_ToLong(UInt ui)
{
    UInt _this = HRLong_New();
    Memory_WriteWord(_this + 0x02, ui);
    Memory_WriteWord(_this + 0x04, 0x00);
    return _this;
}

UInt HRInt_ToLong(UInt ichunk)
{
    UInt _this = HRLong_New();
    Memory_WriteWord(_this + 0x02, ichunk);
    if ((0x8000 & ichunk) != 0x00)
    {
        Memory_WriteWord(_this + 0x04, 0xFFFF);
    }
    else
    {
        Memory_WriteWord(_this + 0x04, 0x00);
    }
    return _this;
}

UInt HRInt_ToBytes(UInt ichunk)
{
    Byte lsb = Byte(ichunk & 0xFF);
    Byte msb = Byte(ichunk >> 0x08);
    UInt lst = HRList_New(Type::eByte);
    HRList_Append(lst, lsb, Type::eByte);
    HRList_Append(lst, msb, Type::eByte);
    return lst;
}

Byte HRInt_GetByte(UInt ichunk, UInt i)
{
    return ((i == 0x00)) ? (Byte(ichunk & 0xFF)) : (Byte(ichunk >> 0x08));
}

UInt HRInt_FromBytes(Byte b0, Byte b1)
{
    return b0 + (b1 << 0x08);
}

Char Char_ToDigit(Byte d)
{
    d = d + 0x30;
    return Char(d);
}

UInt HRVariant_UnBox_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 2));
    return Memory_ReadWord(_this + 3);
}
