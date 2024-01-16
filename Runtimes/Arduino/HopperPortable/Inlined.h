#ifndef HOPPERINLINED_H
#define HOPPERINLINED_H

#include "Runtime.h"
#include "Platform.h"

#ifdef CHECKED
extern UInt HopperVM_messagePC;
#endif
extern UInt HopperVM_pc;
extern UInt HopperVM_sp;
extern UInt HopperVM_bp;
extern UInt HopperVM_csp;
extern Bool HopperVM_cnp;
extern UInt HopperVM_valueStack;
extern UInt HopperVM_typeStack;
extern UInt HopperVM_callStack;
extern UInt HopperVM_dataMemory;
extern UInt HopperVM_codeMemory;
extern UInt HopperVM_jumpTable;
extern Bool Library_isrExists;
extern Byte Minimal_error;

bool HopperVM_InlinedExecuteWarp(bool logging);

Bool Instructions_InlinedAdd();
Bool Instructions_InlinedSub();
Bool Instructions_InlinedMul();
Bool Instructions_InlinedDiv();
Bool Instructions_InlinedMod();

Bool Instructions_InlinedEQ();
Bool Instructions_InlinedNE();
Bool Instructions_InlinedLT();
Bool Instructions_InlinedGT();
Bool Instructions_InlinedLE();
Bool Instructions_InlinedGE();

Bool Instructions_InlinedAddI();
Bool Instructions_InlinedSubI();
Bool Instructions_InlinedMulI();
Bool Instructions_InlinedDivI();
Bool Instructions_InlinedModI();

Bool Instructions_InlinedGTI();
Bool Instructions_InlinedGEI();
Bool Instructions_InlinedLTI();
Bool Instructions_InlinedLEI();


Bool Instructions_InlinedAddB();
Bool Instructions_InlinedSubB();
Bool Instructions_InlinedPushI0();
Bool Instructions_InlinedPushI1();
Bool Instructions_InlinedPushIB();
Bool Instructions_InlinedPushLocalB();
Bool Instructions_InlinedPushLocalB00();
Bool Instructions_InlinedPushLocalB02();
Bool Instructions_InlinedEnter();
Bool Instructions_InlinedCallI();
Bool Instructions_InlinedRetResB();
Bool Instructions_InlinedJZB();
Bool Instructions_InlinedPushIBLE();


Bool Instructions_InlinedBoolNot();
Bool Instructions_InlinedBoolAnd();
Bool Instructions_InlinedBoolOr();
Bool Instructions_InlinedBitXor();
Bool Instructions_InlinedBitNot();
Bool Instructions_InlinedBitShr();
Bool Instructions_InlinedBitShl();
Bool Instructions_InlinedBitAnd();
Bool Instructions_InlinedBitOr();
Bool Instructions_InlinedCast();

Bool Instructions_InlinedIncLocalB();
Bool Instructions_InlinedDecLocalB();
Bool Instructions_InlinedIncGlobalB();
Bool Instructions_InlinedDecGlobalB();
Bool Instructions_InlinedIncLocalIB();
Bool Instructions_InlinedDecLocalIB();
Bool Instructions_InlinedIncGlobalIB();
Bool Instructions_InlinedDecGlobalIB();

#endif