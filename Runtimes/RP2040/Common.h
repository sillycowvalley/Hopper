#ifndef HOPPERPLATFORM_H
#define HOPPERPLATFORM_H

#define CHECKED

const bool loadAuto = true; // set this to false if you are booting into a bad flashed Hopper program

#include <stdio.h>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include "pico/stdlib.h"
#include "hardware/gpio.h"
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

Byte Memory_ReadCodeByte(UInt address);
void Memory_WriteCodeByte(UInt address, Byte value);

Byte Memory_ReadByte(UInt address);
void Memory_WriteByte(UInt address, Byte value);
UInt Memory_ReadWord(UInt address);
void Memory_WriteWord(UInt address, UInt value);

UInt GetPC();
void SetPC(UInt newpc);
UInt GetSP();
UInt GetBP();
void SetBP(UInt newbp);
UInt GetCSP();

UInt GetValueStack();
UInt GetTypeStack();

UInt VMReadWordOperand();
Byte VMReadByteOperand();
Int  VMReadWordOffsetOperand();
Int  VMReadByteOffsetOperand();

void VMPush(UInt word, Type type);
void VMPush(UInt32 word, Type type);
UInt VMPop();
UInt VMPop(Type & type);
void VMPushCS(UInt word);
UInt VMPopCS();

UInt VMLookupMethod(UInt methodIndex);

#include "HopperMemory.h"
#include "HopperString.h"
#include "OpCodes.h"

#endif // HOPPERPLATFORM_H