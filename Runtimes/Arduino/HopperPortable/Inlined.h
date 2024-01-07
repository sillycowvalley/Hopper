#ifndef HOPPERINLINED_H
#define HOPPERINLINED_H

#include "Runtime.h"
#include "Platform.h"

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

Bool Instructions_InlinedAdd();
Bool Instructions_InlinedSub();
Bool Instructions_InlinedMul();
Bool Instructions_InlinedAddB();
Bool Instructions_InlinedSubB();
Bool Instructions_InlinedPushLocalB();
Bool Instructions_InlinedEnter();
Bool Instructions_InlinedCallI();
Bool Instructions_InlinedRetResB();

#endif