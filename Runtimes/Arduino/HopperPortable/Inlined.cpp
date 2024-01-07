#include "Inlined.h"

Bool Instructions_InlinedAdd()
{
    //HopperVM_Push(HopperVM_Pop() + HopperVM_Pop(), Type::eUInt);
    HopperVM_sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next + * top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSub()
{
    //HopperVM_Push(HopperVM_Pop() + HopperVM_Pop(), Type::eUInt);
    HopperVM_sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next - * top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedMul()
{
    //HopperVM_Push(HopperVM_Pop() + HopperVM_Pop(), Type::eUInt);
    HopperVM_sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next * * top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}

Bool Instructions_InlinedAddB()
{
    UInt top = codeMemoryBlock[HopperVM_pc++];
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp + top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSubB()
{
    UInt top = codeMemoryBlock[HopperVM_pc++];
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp - top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}

Bool Instructions_InlinedPushLocalB()
{
    Int offset = Int(codeMemoryBlock[HopperVM_pc++]);
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    UInt * vp  = (UInt*)&dataMemoryBlock[UInt(offset + HopperVM_valueStack + HopperVM_bp)];
    *((UInt*)&dataMemoryBlock[UInt(HopperVM_valueStack + HopperVM_sp)]) = *vp;
    Byte htype = dataMemoryBlock[UInt(offset + HopperVM_typeStack + HopperVM_bp)];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + HopperVM_sp]) = htype;
    HopperVM_sp += 2;
    if (htype >= 0x0D)
    {
        GC_AddReference(*vp);
    }
    return true;
}

Bool Instructions_InlinedEnter()
{
    *((UInt*)&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]) = HopperVM_bp;
    HopperVM_csp += 2;
    HopperVM_bp = HopperVM_sp;
    return true;
}

Bool Instructions_InlinedCallI()
{
    //UInt methodAddress = codeMemoryBlock[HopperVM_pc] + (codeMemoryBlock[HopperVM_pc + 1] << 8); HopperVM_pc += 2;
    UInt methodAddress = codeMemoryBlock[HopperVM_pc++] + (codeMemoryBlock[HopperVM_pc++] << 8);
    *((UInt*)&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]) = HopperVM_pc;
    HopperVM_csp += 2;
    HopperVM_pc = methodAddress;
    return true;
}

Bool Instructions_InlinedRetResB()
{
    HopperVM_sp -= 2;
    UInt * value = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    Byte * rtype = &dataMemoryBlock[HopperVM_typeStack + HopperVM_sp];

    UInt popBytes = codeMemoryBlock[HopperVM_pc++];
    while (popBytes != 0)
    {
        HopperVM_sp -= 2;
        //UInt address = dataMemoryBlock[HopperVM_valueStack + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStack + HopperVM_sp+1] << 8);
        UInt * address = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
        if (dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] >= 0x0D)
        {
            GC_Release(*address);
        }
        popBytes -= 2;
    }
    
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = *value;
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = *rtype;
    HopperVM_sp += 2;

    HopperVM_csp -= 2;
    HopperVM_bp = dataMemoryBlock[HopperVM_callStack + HopperVM_csp] + (dataMemoryBlock[HopperVM_callStack + HopperVM_csp+1] << 8);

    if (HopperVM_csp == 0)
    {
        HopperVM_pc = 0;
        return false;
    }
    else
    {
        HopperVM_csp -= 2;
        HopperVM_pc = dataMemoryBlock[HopperVM_callStack + HopperVM_csp] + (dataMemoryBlock[HopperVM_callStack + HopperVM_csp+1] << 8);
    }
    return true;
}