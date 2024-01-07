#ifndef HOPPERPLATFORM_H
#define HOPPERPLATFORM_H

#define PICO_CYW43_SUPPORTED // TODO : get this from the CMakeLists.txt setting

const bool loadAuto = true; // set this to false if you are booting into a bad flashed Hopper program

#include <stdio.h>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include "pico/stdlib.h"

#ifdef PICO_CYW43_SUPPORTED
#include "pico/cyw43_arch.h"
#endif

#include "hardware/gpio.h"
#include "hardware/pwm.h"
#include "hardware/clocks.h"
#include "hardware/adc.h"
#include "hardware/sync.h"


#include <tusb.h>

#include <queue>
#include <vector>
#include <map>

typedef        uint8_t Byte;
typedef  unsigned char Char;
typedef       uint16_t UInt;
typedef       uint32_t UInt32;
typedef        int16_t Int;
typedef        int32_t Long;
typedef        float   Float;
typedef           bool Bool;

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

inline Bool IsReferenceType(Type htype) { return (Byte(htype) >= 0x0F); }
inline Bool IsReferenceType(Byte htype) { return (htype >= 0x0F); }


enum HopperFlags {
    eTraceOn = 0x0001,
    eWarpSpeed = 0x0002,
    eLongValues = 0x0002,
    eCheckedBuild = 0x0004,
    eStack8Bit = 0x0008,
    eProfileBuild = 0x0010,
    eBreakpointsSet = 0x0020,
    eSingleStep = 0x0040,
    eMCUPlatform = 0x0080,
};

void SetError(Byte error, UInt context);


extern UInt bp;
extern UInt pc;
extern UInt sp;
extern UInt csp;
extern Bool cnp;

extern UInt callStack;
extern UInt valueStack;
extern UInt typeStack;
extern UInt constAddress;

extern Byte * dataMemoryBlock;
extern Byte * codeMemoryBlock;

inline UInt GetBP()  { return bp; }
inline UInt GetPC()  { return pc; }
inline UInt GetSP()  { return sp; }
inline UInt GetCSP() { return csp; }
inline Bool GetCNP() { return cnp; }

inline void SetPC(UInt newpc)   { pc = newpc; }
inline void SetBP(UInt newbp)   { bp = newbp; }
inline void SetCNP(Bool newcnp) { cnp = newcnp; }

inline UInt GetValueStack()      { return valueStack; }
inline UInt GetTypeStack()       { return typeStack;  }
inline UInt GetConstantAddress() { return constAddress; }

inline Byte Memory_ReadCodeByte(UInt address) { return codeMemoryBlock[address]; }
inline UInt Memory_ReadCodeWord(UInt address) { return codeMemoryBlock[address] + (codeMemoryBlock[address+1] << 8); }

void Memory_WriteCodeByte(UInt address, Byte value);

inline Byte Memory_ReadByte(UInt address)              { return dataMemoryBlock[address]; }
inline void Memory_WriteByte(UInt address, Byte value) { dataMemoryBlock[address] = value; }
inline UInt Memory_ReadWord(UInt address)              { return dataMemoryBlock[address] + (dataMemoryBlock[address+1] << 8); }
inline void Memory_WriteWord(UInt address, UInt value) { dataMemoryBlock[address]   = value & 0xFF; dataMemoryBlock[address+1] = value >> 8; }

inline UInt VMReadWordOperand()       { UInt operand = codeMemoryBlock[pc] + (codeMemoryBlock[pc+1] << 8); pc += 2; return operand; }
inline Byte VMReadByteOperand()       { Byte operand = codeMemoryBlock[pc]; pc++; return operand; }
inline Int  VMReadWordOffsetOperand() { Int offset = (Int)(codeMemoryBlock[pc] + (codeMemoryBlock[pc+1] << 8)); pc += 2; return offset; }
inline Int  VMReadByteOffsetOperand() { Int offset = (Int)(codeMemoryBlock[pc]); pc++; if (offset > 0x7F) { offset = offset - 0x0100; } return offset; }

void VMPush(UInt word, Type type);
void VMPush32(UInt32 word, Type type);
UInt VMPop();
Int  VMPopInt();
void VMPushInt(Int i);
UInt VMPop(Type & type);
UInt VMGet(UInt address, Type & type);
UInt32 VMGet32(UInt address, Type & type);
void VMPut  (UInt address, UInt value, Type type);
void VMPut32(UInt address, UInt32 value, Type type);

Long  VMPopLong();
void  VMPushLong(Long l);
Float VMPopFloat();
void  VMPushFloat(Float f);

UInt32 VMPop32(Type & type);
void VMPushCS(UInt word);
UInt VMPopCS();

UInt VMLookupMethod(UInt methodIndex);

bool LibCall();
bool SysCall();
bool SysCall0();
bool SysCall1();

#include "HopperMemory.h"
#include "HopperString.h"
#include "HopperArray.h"
#include "HopperVariant.h"
#include "HopperPair.h"
#include "HopperDictionary.h"
#include "HopperList.h"
#include "OpCodes.h"

#endif // HOPPERPLATFORM_H