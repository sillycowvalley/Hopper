#include "Platform.h"
#include "Inlined.h"
#include "HopperScreen.h"

#include <Wire.h>
#include <SPI.h>

#ifdef RP2040 // use ARDUINO_ARCH_RP2040?
#include <Adafruit_NeoPixel.h>
/*
#define NEOPIN        16 // On Trinket or Gemma, suggest changing this to 1
// How many NeoPixels are attached to the Arduino?
#define NUMPIXELS 1 // Popular NeoPixel ring size
Adafruit_NeoPixel pixels(NUMPIXELS, NEOPIN, NEO_GRB + NEO_KHZ800);
*/
Adafruit_NeoPixel * adafruit_NeoPixel = nullptr;
#endif



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
    //pinMode(pin, INPUT_PULLUP); // should this be here? should it be an option?
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

    WebServer_Restart();
}

void Platform_Release()
{
#ifdef USELITTLEFS  
    LittleFS.end(); // unmount the file system
#endif
#ifdef ESP32LITTLEFS
    LittleFS.end();
#endif

    //HRGraphics_End();
    
    free(dataMemoryBlock);
    dataMemoryBlock = nullptr;
    free(codeMemoryBlock);
    codeMemoryBlock = nullptr;

    WebServer_Release();
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

//#define HWM
//#define LWM

Bool HopperVM_InlinedExecuteWarp(bool logging)
{
    Bool restart = false;
#ifdef HWM  
    UInt hmw = 0;
#endif
#ifdef LWM  
    UInt lwm = 0xFFFF;
#endif
    UInt isrCheck = 25;  // check for interrupt events every 25 instructions
    UInt watchDog = 100; // check for <ctrl><C> every 2500 instructions
    for (;;) // loop
    {
#ifdef CHECKED
        HopperVM_messagePC = HopperVM_pc;
#endif
#ifdef HWM
        uint instructionPC = HopperVM_pc;
#endif
#ifdef LWM
        uint instructionPC = HopperVM_pc;
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
#ifdef DIAGNOSTICS   
        if (logging)
        {
            Serial.print(" PC=0x");
            Serial.print(HopperVM_pc, HEX);
        }
#endif
        
        InstructionDelegate instructionDelegate = *((InstructionDelegate*)(&dataMemoryBlock[HopperVM_jumpTable + (codeMemoryBlock[HopperVM_pc++] << 2)]));
#if defined(HWM) || defined(LWM)
#ifdef HWM
        uint spb = HopperVM_sp;
#endif
        bool goOn = instructionDelegate();
#ifdef HWM
        if (HopperVM_sp > hmw)
        {
            Serial.print(instructionPC, HEX); Serial.print(" "); Serial.print(HopperVM_bp, HEX); Serial.print(":"); Serial.print(spb, HEX); Serial.print("->"), Serial.print(HopperVM_sp, HEX); Serial.println();
            hmw = HopperVM_sp;
            if (hmw >= 512)
            {
                Minimal_error = 0x07; // stack overflow
                break;
            }
        }
#endif
#ifdef LWM        
        UInt available = Memory_Available();
        if (available < lwm)
        {
            UInt maximum = Memory_Maximum();

            Serial.print(instructionPC, HEX); Serial.print(" "); Serial.print(":"); Serial.print(lwm, HEX); Serial.print("->"), Serial.print(available, HEX); Serial.print(" "); Serial.print(maximum, HEX); Serial.println();
            lwm = available;
            if (lwm <= 512)
            {
                Minimal_error = 0x0C; // out of memory
                break;
            }
        }
#endif
        if (goOn)
        {
            continue;
        }
#else
        if (instructionDelegate())
        {
            continue;
        }
#endif
        if (Minimal_error != 0)
        {
            break;
        }
        if (HopperVM_pc== 0)
        {
            restart = true;
            break;
        }
        if (IO_IsBreak())
        {
            HopperVM_WriteBREAK();
            break;
        }
    } // loop
    return restart;
}

bool controller0Configured = false;
bool controller1Configured = false;
Byte sdaPin0 = 0;
Byte sdaPin1 = 0;
Byte sclPin0 = 0;
Byte sclPin1 = 0;

void HRWire_Configure(Byte controller, Byte sdaPin, Byte sclPin)
{
    switch (controller)
    {
        case 0:
            controller0Configured = true;
            sdaPin0 = sdaPin;
            sclPin0 = sclPin;
            break;  
        case 1:
            controller1Configured = true;
            sdaPin1 = sdaPin;
            sclPin1 = sclPin;
            break;  
    }
}
bool HRWire_Begin(Byte controller)
{
    bool success = false;
    for (;;)
    {
        switch (controller)
        {
            case 0:
                if (controller0Configured)
                {
                    if (!Wire.setSDA(sdaPin0))
                    {
                        break;
                    }
                    if (!Wire.setSCL(sclPin0))
                    {
                        break;
                    }
                }
                Wire.begin();
                success = true;
                break;
#ifdef RP2040
            case 1:
                if (controller1Configured)
                {
                    if (!Wire1.setSDA(sdaPin1))
                    {
                        break;
                    }
                    if (!Wire1.setSCL(sclPin1))
                    {
                        break;
                    }
                }
                Wire1.begin();
                success = true;
                break;
#endif
        }
        break;
    }
    return success;
}
void HRWire_BeginTx(Byte controller, Byte address)
{
#ifdef RP2040
    (controller == 0) ? Wire.beginTransmission(address) : Wire1.beginTransmission(address);
#else
    Wire.beginTransmission(address);
#endif
}
Byte HRWire_EndTx(Byte controller)
{
#ifdef RP2040
    Byte result = ((controller == 0) ? Wire.endTransmission(): Wire1.endTransmission());
#else
    Byte result = Wire.endTransmission();
#endif
    
    /*
    0: success
    1: busy timeout upon entering endTransmission()
    2: START bit generation timeout
    3: end of address transmission timeout
    4: data byte transfer timeout
    5: data byte transfer succeeded, busy timeout immediately after
    6: timeout waiting for peripheral to clear stop bit
    */
    return result;
}
void HRWire_Write(Byte controller, Byte data)
{
#ifdef RP2040
    (controller == 0) ? Wire.write(data): Wire1.write(data);
#else
    Wire.write(data);
#endif
}
void HRWire_Write(Byte controller, UInt hrarray, UInt startIndex, UInt length)
{
    Byte * data = &dataMemoryBlock[hrarray + startIndex + 5];
#ifdef RP2040
    (controller == 0) ? Wire.write(data, length): Wire1.write(data, length);
#else
    Wire.write(data, length);
#endif
}

bool External_FunctionCall(UInt jumpTable, Byte opCode)
{
    UInt opOffset = (opCode << 2);
    InstructionDelegate instructionDelegate = *((InstructionDelegate*)(&dataMemoryBlock[jumpTable] + opOffset));
    return instructionDelegate();
}

 // Default speed set to 4MHz, SPI mode set to MODE 0 and Bit order set to MSB first.
uint32_t speedMaximum0 = 4000000;
uint32_t speedMaximum1 = 4000000;
BitOrder dataOrder0 = MSBFIRST;
BitOrder dataOrder1 = MSBFIRST;
SPIMode dataMode0 = SPI_MODE0;
SPIMode dataMode1 = SPI_MODE0;

byte spiCSPin0 = 0;
byte spiCSPin1 = 0;
byte spiClkPin0 = 0;
byte spiClkPin1 = 0;
byte spiTxPin0 = 0;
byte spiTxPin1 = 0;
byte spiRxPin0 = 0;
byte spiRxPin1 = 0;
bool spiCSPin0Set  = false;
bool spiCSPin1Set  = false;
bool spiClkPin0Set = false;
bool spiClkPin1Set = false;
bool spiTxPin0Set  = false;
bool spiTxPin1Set  = false;
bool spiRxPin0Set  = false;
bool spiRxPin1Set  = false;

#ifdef RP2040
#define SPI1EXISTS
SPIClassRP2040* screenSPI0 = nullptr;
SPIClassRP2040* screenSPI1 = nullptr;
SPISettings screenSPISettings0;
SPISettings screenSPISettings1;
#else
SPIClass* screenSPI0 = nullptr;
SPISettings screenSPISettings0;
#endif


// assigns SPI, creates SPISettings0, calls begin() (returns false if pins not set)    
Bool HRSPI_Begin(Byte spiController)
{
    bool success = false;
    switch (spiController)
    {
        case 0:
            if (!spiCSPin0Set || !spiClkPin0Set)
            {
                break;
            }
            if (!spiTxPin0Set && !spiRxPin0Set)
            {
                break;
            }
            pinMode(spiCSPin0, OUTPUT);
            digitalWrite(spiCSPin0, HIGH); // Deselect

            screenSPI0 = &SPI;
            screenSPI0->setSCK(spiClkPin0);
            if (spiTxPin0Set) { screenSPI0->setTX(spiTxPin0); }
            if (spiRxPin0Set) { screenSPI0->setRX(spiRxPin0); }
            
            screenSPISettings0 = SPISettings(speedMaximum0, dataOrder0, dataMode0);
            screenSPI0->begin();
            success = true;
            break;
#ifdef SPI1EXISTS
        case 1:
            if (!spiCSPin1Set || !spiClkPin1Set)
            {
                break;
            }
            if (!spiTxPin1Set && !spiRxPin1Set)
            {
                break;
            }
            pinMode(spiCSPin1, OUTPUT);
            digitalWrite(spiCSPin1, HIGH); // Deselect

            screenSPI1 = &SPI1;
            screenSPI1->setSCK(spiClkPin1);
            if (spiTxPin1Set) { screenSPI1->setTX(spiTxPin1); }
            if (spiRxPin1Set) { screenSPI1->setRX(spiRxPin1); }
            screenSPISettings1 = SPISettings(speedMaximum1, dataOrder1, dataMode1);
            screenSPI1->begin();
            success = true;
            break;
#endif
    }
    return success;
}

void HRSPI_BeginTransaction(Byte spiController)
{
    switch (spiController)
    {
        case 0:
            screenSPI0->beginTransaction(screenSPISettings0);
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->beginTransaction(screenSPISettings1);
            break;
#endif
    }
}

void HRSPI_EndTransaction(Byte spiController)
{
    switch (spiController)
    {
        case 0:
            screenSPI0->endTransaction();
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->endTransaction();
            break;
#endif
    }
}

Byte HRSPI_ReadByte(Byte spiController)
{
    Byte data = 0;
    switch (spiController)
    {
        case 0:
            data = screenSPI0->transfer(data);
            break;
#ifdef SPI1EXISTS
        case 1:
            data = screenSPI1->transfer(data);
            break;
#endif
    }
    return data;
}
void HRSPI_WriteByte(Byte spiController, Byte data)
{
    switch (spiController)
    {
        case 0:
            screenSPI0->transfer(data);
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->transfer(data);
            break;
#endif
    }
}

UInt HRSPI_ReadWord(Byte spiController)
{
    UInt data = 0;
    switch (spiController)
    {
        case 0:
            data = screenSPI0->transfer16(data);
            break;
#ifdef SPI1EXISTS
        case 1:
            data = screenSPI1->transfer16(data);
            break;
#endif
    }
    return data;
}
void HRSPI_WriteWord(Byte spiController, UInt data)
{
switch (spiController)
    {
        case 0:
            screenSPI0->transfer16(data);
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->transfer16(data);
            break;
#endif
    }
}

void HRSPI_WriteBuffer(Byte spiController, UInt hrdata, UInt startIndex, UInt length)
{
    Byte * data = &dataMemoryBlock[hrdata + startIndex + 5];
    switch (spiController)
    {
        case 0:
#ifdef LOLIND1MINI      
            screenSPI0->transferBytes((const uint8_t * )data, nullptr, length);  
#else
            screenSPI0->transfer((const void *)data, nullptr, length);
#endif
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->transfer((const void *)data, nullptr, length);
            break;
#endif
    }
}
void HRSPI_ReadBuffer(Byte spiController, UInt hrdata, UInt startIndex, UInt length)
{
    Byte * data = &dataMemoryBlock[hrdata + startIndex + 5];
    switch (spiController)
    {
        case 0:
#ifdef LOLIND1MINI      
            screenSPI0->transferBytes(nullptr, (uint8_t * )data, length);  
#else
            screenSPI0->transfer(nullptr, (void *)data, length);
#endif
            break;
#ifdef SPI1EXISTS
        case 1:
            screenSPI1->transfer(nullptr, (void *)data, length);
            break;
#endif
    }
}

void HRSPI_Settings(Byte spiController, UInt hrspeedMaximum, DataOrder dataOrder, DataMode dataMode)
{
    long speedMaximum = nativeLongFromHopperLong(hrspeedMaximum);
    switch (spiController)
    {
        case 0:
        {
            speedMaximum0 = speedMaximum;
            dataOrder0 = (BitOrder)dataOrder;
            dataMode0 = (SPIMode)dataMode;
            break;
        }
        case 1:
        {
            speedMaximum1 = speedMaximum;
            dataOrder1 = (BitOrder)dataOrder;
            dataMode1 = (SPIMode)dataMode;
            break;
        }
    }
}

void HRSPI_SetCSPin(Byte spiController, Byte pin)
{
    if (spiController == 0)
    {
        spiCSPin0 = pin;
        spiCSPin0Set = true;
    }
    else if (spiController == 1)
    {
        spiCSPin1 = pin;
        spiCSPin1Set = true;
    }
}

void HRSPI_SetClkPin(Byte spiController, Byte pin)
{
    if (spiController == 0)
    {
        spiClkPin0 = pin;
        spiClkPin0Set = true;
    }
    else if (spiController == 1)
    {
        spiClkPin1 = pin;
        spiClkPin1Set = true;
    }
}

void HRSPI_SetTxPin(Byte spiController, Byte pin)
{
    if (spiController == 0)
    {
        spiTxPin0 = pin;
        spiTxPin0Set = true;
    }
    else if (spiController == 1)
    {
        spiTxPin1 = pin;
        spiTxPin1Set = true;
    }
}

void HRSPI_SetRxPin(Byte spiController, Byte pin)
{
    if (spiController == 0)
    {
        spiRxPin0 = pin;
        spiRxPin0Set = true;
    }
    else if (spiController == 1)
    {
        spiRxPin1 = pin;
        spiRxPin1Set = true;
    }
}

Byte HRSPI_GetCSPin(Byte spiController)
{
    if (spiController == 0)
    {
        return spiCSPin0;
    }
    else if (spiController == 1)
    {
        return spiCSPin1;
    }
    return 0;
}


void HRNeoPixel_Begin(UInt length, Byte pin, UInt pixelType)
{
#ifdef RP2040
    if (nullptr == adafruit_NeoPixel)
    {
        adafruit_NeoPixel = new Adafruit_NeoPixel(length, pin, pixelType);
        adafruit_NeoPixel->begin();
    }
    else
    {
        // On the RP2040, you appear to only be able to call the constructor once during a session.
        // Device needs to be restarted for new settings.
        // I suspect this is because of the way PIO is claimed in the constructor (and never released).
        //adafruit_NeoPixel->setPin(pin);
        //adafruit_NeoPixel->updateLength(length);
        //adafruit_NeoPixel->updateType(pixelType);
    }
#endif
}
void HRNeoPixel_SetBrightness(Byte brightness)
{
#ifdef RP2040
    if (nullptr != adafruit_NeoPixel)
    {
        adafruit_NeoPixel->setBrightness(brightness);
    }
#endif
}
Byte HRNeoPixel_GetBrightness()
{
    Byte brightness = 0;
#ifdef RP2040
    if (nullptr != adafruit_NeoPixel)
    {
        brightness = adafruit_NeoPixel->getBrightness();
    }
#endif
    return brightness;
}
void HRNeoPixel_SetColor(UInt pixel, Byte r, Byte g, Byte b, Byte w)
{
#ifdef RP2040
    if (nullptr != adafruit_NeoPixel)
    {
        adafruit_NeoPixel->setPixelColor(pixel, r, g, b, w);
    }
#endif
}
void HRNeoPixel_Show()
{
#ifdef RP2040
    if (nullptr != adafruit_NeoPixel)
    {
        adafruit_NeoPixel->show();
    }
#endif
}
UInt HRNeoPixel_GetLength()
{
    UInt length = 0;
#ifdef RP2040
    if (nullptr != adafruit_NeoPixel)
    {
        length = adafruit_NeoPixel->numPixels();
    }
#endif
    return length;
}

void HRString_FromString(UInt & hrstr, const String & str)
{
    if (hrstr != 0)
    {
        HRString_BuildClear_R(hrstr);
    }
    else
    {
        hrstr = HRString_New();
    }
    for (int i = 0; i < str.length(); i++)
    {
        HRString_BuildChar_R(hrstr, str.charAt(i));
    }
}

void HRString_ToString(UInt hrstr, String & str)
{
    str = "";
    if (0 != hrstr)
    {
        for (uint i = 0; i < HRString_GetLength(hrstr); i++)
        {
            str += (char)HRString_GetChar(hrstr, i);
        }
    }
}