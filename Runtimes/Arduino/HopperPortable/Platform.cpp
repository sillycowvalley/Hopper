#include "Platform.h"
#include "Inlined.h"
#include "HopperScreen.h"

#include <Wire.h>

#ifdef LOLIND1MINI
const size_t segmentPages = 0x30;  // size in words, 12K x2 on the Lolin D1 Mini
#else
const size_t segmentPages = 0xFF; // size in words, 64K x2 on all other devices so far
#endif


// Platform

unsigned char * dataMemoryBlock = nullptr;
unsigned char * codeMemoryBlock = nullptr;

Byte lastError;
bool exited;

struct PinISRStruct
{
    Byte pin;
    Byte status;
    UInt isrDelegate;
};

std::queue<PinISRStruct> isrQueue;

void pinISR(void * param)
{
    uint lparam          = (uint)(param);

    PinISRStruct isrStruct;

    isrStruct.pin         = (UInt)((lparam >> 16) & 0xFF);
    isrStruct.status      = (UInt)((lparam >> 24) & 0xFF);
    isrStruct.isrDelegate = (UInt)(lparam &0xFFFF);
    isrQueue.push(isrStruct);
}

Bool External_AttachToPin(Byte pin, ISRDelegate gpioISRDelegate, Byte status)
{
    if (digitalPinToInterrupt(pin) == -1)
    {
        // pin is not valid for interrupt
        return false;
    }
    pinMode(pin, INPUT_PULLUP);
    uint param = gpioISRDelegate + (pin << 16) + (status << 24);
    attachInterruptParam(digitalPinToInterrupt(pin), pinISR, (PinStatus)status, (void*)param);
    return true;
}

void External_ServiceInterrupts()
{
    for(;;)
    {
        if (isrQueue.empty())
        {
            break; // as fast as possible if empty? what's the overhead of noInterrupts() .. interrupts()?
        }
        noInterrupts();
        if (isrQueue.empty())
        {
            interrupts();
            break;
        }
        PinISRStruct isrStruct = isrQueue.front();
        isrQueue.pop();
        interrupts();

        // construct a delegate call
        uint methodIndex = isrStruct.isrDelegate;
        HopperVM_PushCS(HopperVM_PC_Get());
        uint methodAddress = HopperVM_LookupMethod(methodIndex);
        
        // arguments
        HopperVM_Push(isrStruct.pin,    Type::eByte);
        HopperVM_Push(isrStruct.status, Type::eByte);

        // make the call
        HopperVM_PC_Set(methodAddress);
        break;
    } // for (;;)
}



bool External_LoadAuto_Get()
{
    return loadAuto;
}
UInt External_GetSegmentPages()
{
    return segmentPages;
}


// Platform

void External_WatchDog()
{
#if defined(RP2040PICO) || defined(RP2040PICOW)  
    // Pi Pico appears to not need this
#else
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
    //Serial.println((unsigned int)(segmentPages << 8), HEX);
    codeMemoryBlock = (unsigned char*)malloc((segmentPages << 8)); // calloc does not work on the Lolin D1 Mini
    if (nullptr != codeMemoryBlock)
    {
        memset(codeMemoryBlock, (segmentPages << 8), 0);
    }
    //Serial.println((unsigned int)codeMemoryBlock, HEX);
    dataMemoryBlock = (unsigned char*)malloc((segmentPages << 8));
    if (nullptr != dataMemoryBlock)
    {
        memset(dataMemoryBlock, (segmentPages << 8), 0);
    }
    //Serial.println((unsigned int)dataMemoryBlock, HEX);
    FileSystem_Initialize();

}

void Platform_Release()
{
#ifdef USELITTLEFS  
    LittleFS.end(); // unmount the file system
#endif
#ifdef ESP32LITTLEFS
    LittleFS.end();
#endif

    HRGraphics_End();
    
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
#ifdef CHECKED
    if (address >= (segmentPages << 8))
    {
        Serial.print(address, HEX);
        Serial.print(" out of range in Memory_WriteCodeByte ");
        Error(0x02, HopperVM_PC_Get());
        return;
    }
#endif  
    codeMemoryBlock[address] = value;
}


Byte Memory_ReadByte(UInt address)
{
    return dataMemoryBlock[address];
}

void Memory_WriteByte(UInt address, Byte value)
{
#ifdef CHECKED
    if (address >= (segmentPages << 8))
    {
        Serial.print(address, HEX);
        Serial.print(" out of range in Memory_WriteByte ");
        Error(0x02, HopperVM_PC_Get());
        return;
    }
#endif  
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
#ifdef CHECKED
    if (address+1 >= (segmentPages << 8))
    {
        Serial.print(address, HEX);
        Serial.print(" out of range in Memory_WriteWord ");
        Error(0x02, HopperVM_PC_Get());
        return;
    }
#endif  
    dataMemoryBlock[address] = value & 0xFF;
    dataMemoryBlock[address+1] = value >> 8;
}

void Memory_WriteCodeWord(UInt address, UInt value)
{
#ifdef CHECKED
    if (address+1 >= (segmentPages << 8))
    {
        Serial.print(address, HEX);
        Serial.print(" out of range in Memory_WriteCodeWord ");
        Error(0x02, HopperVM_PC_Get());
        return;
    }
#endif  
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

void External_AnalogWrite(Byte pin, UInt value)
{
    analogWrite(pin, value);
}
void External_AnalogWriteResolution(Byte bits)
{
    analogWriteResolution(bits);
}

Byte External_DigitalRead(Byte pin)
{
    Byte value = digitalRead(pin);
    return value;
}
UInt External_AnalogRead(Byte pin)
{
    UInt value = analogRead(pin);
    return value;
}



// Machine
void Machine_Initialize()
{
    lastError = 0;
    exited    = false;
}
Bool Machine_GetExited()
{
    return exited;
}
void Machine_SetExited(Bool value)
{
    exited = value;
}



void Error(Byte error, UInt pc)
{
    lastError = error;
    if (0 != error)
    {
        char buffer[40];
        sprintf(buffer, "Error: 0x%02X (PC=0x%04X)\n", error, pc);
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
    Error(value, HopperVM_PC_Get());
}

void External_WriteToJumpTable(UInt jumpTable, Byte opCode, InstructionDelegate instructionDelegate)
{
    UInt opOffset = (opCode << 2);
    InstructionDelegate * jumpLocation = (InstructionDelegate * )(&dataMemoryBlock[jumpTable] + opOffset);
    *jumpLocation = instructionDelegate;
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
#ifdef CHECKED    
    if (top == 0)
    {
        Diagnostics_SetError(0x04); // Division by zero.
        return hopperLongFromNativeLong(0);
    }
#endif
    return hopperLongFromNativeLong(next / top);
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
#ifdef CHECKED    
    if (top == 0)
    {
        Diagnostics_SetError(0x04); // Division by zero.
        return hopperLongFromNativeLong(0);
    }
#endif
    return hopperLongFromNativeLong(next % top);
}

UInt External_LongToFloat(UInt hrlong)
{
    long ln = nativeLongFromHopperLong(hrlong);
    return hopperFloatFromNativeFloat(float(ln)); // TODO : range
}

Int  External_LongToInt(UInt hrlong)
{
    long top = nativeLongFromHopperLong(hrlong);
#ifdef CHECKED    
    if ((top < -32768) || (top > 32767))
    {
        Diagnostics_SetError(0x0D); // Numeric type out of range/overflow.
        return 0;
    }
#endif
    return (Int)top;
}

UInt External_FloatToLong(UInt hrfloat)
{
    float fl = nativeFloatFromHopperFloat(hrfloat);
    return hopperLongFromNativeLong(long(fl)); // TODO : range
}
UInt External_FloatToUInt(UInt hrfloat)
{
    float fl = nativeFloatFromHopperFloat(hrfloat);
#ifdef CHECKED    
    if (fl > 65535)
    {
        Diagnostics_SetError(0x0D); // Numeric type out of range/overflow.
        return 0;
    }
#endif
    return UInt(fl);
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
    sprintf(buffer, "%g", fl);
    UInt i = 0;
    while (buffer[i])
    {
        HRString_BuildChar_R(hrstring , (Char)buffer[i]);
        i++;
    }
    return hrstring;
}
UInt External_LongToString(UInt hrlong)
{
    UInt hrstring = HRString_New();
    char buffer[20];
    long l = nativeLongFromHopperLong(hrlong);
    sprintf(buffer, "%d", l);
    UInt i = 0;
    while (buffer[i])
    {
        HRString_BuildChar_R(hrstring , (Char)buffer[i]);
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
#ifdef CHECKED    
    if (top == 0)
    {
        Diagnostics_SetError(0x04); // Division by zero.
        return hopperFloatFromNativeFloat(0);
    }
#endif
    return hopperFloatFromNativeFloat(next / top);
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

void HRWire_Begin()
{
    Wire.begin();
}
void HRWire_BeginTx(Byte address)
{
    Wire.beginTransmission(address);
}
void HRWire_EndTx()
{
    Wire.endTransmission();
}
void HRWire_Write(Byte data)
{
    Wire.write(data);
}

bool External_FunctionCall(UInt jumpTable, Byte opCode)
{
    UInt opOffset = (opCode << 2);
    InstructionDelegate instructionDelegate = *((InstructionDelegate*)(&dataMemoryBlock[jumpTable] + opOffset));
    return instructionDelegate();
}


void HopperVM_InlinedExecuteWarp()
{
    UInt isrCheck = 25;  // check for interrupt events every 25 instructions
    UInt watchDog = 100; // check for <ctrl><C> every 2500 instructions
    for (;;) // loop
    {
#ifdef CHECKED
        HopperVM_messagePC = HopperVM_pc;
#endif
        
        if (--isrCheck == 0)
        {
            isrCheck = 25;
            if (Library_isrExists)
            {
                External_ServiceInterrupts();
            }
            if (--watchDog == 0)
            {
#if defined(RP2040PICO) || defined(RP2040PICOW)  
                // Pi Pico appears to not need this
#else
                External_WatchDog();
#endif
                watchDog = 100;
                if (IO_IsBreak())
                {
                    HopperVM_WriteBREAK();
                    break;
                }
            }
        }
        
        InstructionDelegate instructionDelegate = *((InstructionDelegate*)(&dataMemoryBlock[HopperVM_jumpTable + (codeMemoryBlock[HopperVM_pc++] << 2)]));
        if (instructionDelegate())
        {
            continue;
        }
        if (Minimal_error != 0)
        {
            break;
        }
        if (HopperVM_pc== 0)
        {
            HopperVM_Restart();
            break;
        }
        if (IO_IsBreak())
        {
            HopperVM_WriteBREAK();
            break;
        }
    } // loop
}