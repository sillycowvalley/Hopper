#include "Inlined.h"

Bool Instructions_InlinedAdd()
{
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next + *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSub()
{
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next - *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedMul()
{
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next * *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedDiv()
{
#ifdef CHECKED    
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    else
    {
        HopperVM_Push(next / top, Type::eUInt);
    }
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next / *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedMod()
{
#ifdef CHECKED    
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    else
    {
        HopperVM_Push(next % top, Type::eUInt);
    }
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next % *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedAddI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_PushI(next + top);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next + *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedSubI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_PushI(next - top);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next - *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedDivI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    else
    {
        HopperVM_PushI(next / top);
    }
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next / *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedMulI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_PushI(next * top);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next * *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif

    return true;
}

Bool Instructions_InlinedModI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    else
    {
        HopperVM_PushI(next % top);
    }
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next % *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedAddB()
{
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp + codeMemoryBlock[HopperVM_pc++];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSubB()
{
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp - codeMemoryBlock[HopperVM_pc++];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedPushI0()
{
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = 0;
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp += 2;
    return true;
}
Bool Instructions_InlinedPushI1()
{
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = 1;
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp += 2;
    return true;
}
Bool Instructions_InlinedPushIB()
{
#ifdef CHECKED
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
#else
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = codeMemoryBlock[HopperVM_pc++];
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp += 2;
#endif
    return true;
}
Bool Instructions_InlinedPushLocalB00()
{
    UInt * vp  = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_bp];
    *((UInt*)&dataMemoryBlock[UInt(HopperVM_valueStack + HopperVM_sp)]) = *vp;
    Byte htype = dataMemoryBlock[HopperVM_typeStack + HopperVM_bp];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + HopperVM_sp]) = htype;
    HopperVM_sp += 2;
    if (htype >= 0x0D)
    {
        GC_AddReference(*vp);
    }
    return true;
}
Bool Instructions_InlinedPushLocalB02()
{
    UInt * vp  = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_bp + 2];
    *((UInt*)&dataMemoryBlock[UInt(HopperVM_valueStack + HopperVM_sp)]) = *vp;
    Byte htype = dataMemoryBlock[HopperVM_typeStack + HopperVM_bp + 2];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + HopperVM_sp]) = htype;
    HopperVM_sp += 2;
    if (htype >= 0x0D)
    {
        GC_AddReference(*vp);
    }
    return true;
}

Bool Instructions_InlinedPushLocalB()
{
    Int8 offset = (Int8)(codeMemoryBlock[HopperVM_pc++]);
    UInt * vp  = (UInt*)&dataMemoryBlock[offset + HopperVM_valueStack + HopperVM_bp];
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
    HopperVM_bp = HopperVM_sp;
    HopperVM_csp += 2;
    return true;
}

Bool Instructions_InlinedCallI()
{
    UInt methodAddress = codeMemoryBlock[HopperVM_pc++] + (codeMemoryBlock[HopperVM_pc++] << 8);
    *((UInt*)&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]) = HopperVM_pc;
    HopperVM_pc = methodAddress;
    HopperVM_csp += 2;
    return true;
}

Bool Instructions_InlinedRetResB()
{
    UInt vs2 = HopperVM_valueStack-2;
    UInt * value = (UInt*)&dataMemoryBlock[vs2 + HopperVM_sp];
    UInt ts2 = HopperVM_typeStack - 2;
    Byte * rtype = &dataMemoryBlock[ts2 + HopperVM_sp];

    UInt popBytes = codeMemoryBlock[HopperVM_pc++];
    while (popBytes != 0)
    {
        HopperVM_sp -= 2;
        UInt * address = (UInt*)&dataMemoryBlock[vs2 + HopperVM_sp];
        if (dataMemoryBlock[ts2 + HopperVM_sp] >= 0x0D)
        {
            GC_Release(*address);
        }
        popBytes -= 2;
    }
    
    *((UInt*)&dataMemoryBlock[vs2 + HopperVM_sp]) = *value;
    dataMemoryBlock[ts2 + HopperVM_sp] = *rtype;
    
    HopperVM_csp -= 2;
    HopperVM_bp = *((UInt*)(&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]));
    if (HopperVM_csp == 0)
    {
        HopperVM_pc = 0;
        return false;
    }
    else
    {
        HopperVM_csp -= 2;
        HopperVM_pc = *((UInt*)(&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]));
    }
    return true;
}
Bool Instructions_InlinedJZB()
{
    HopperVM_sp = HopperVM_sp - 0x02;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    if (*dp == 0)
    {
        HopperVM_pc = (UInt)(HopperVM_ReadByteOffsetOperand() + HopperVM_pc - 2);
    }
    else
    {
        HopperVM_pc++;
    }
    return true;
}
Bool Instructions_InlinedPushIBLE()
{
    UInt sp2 = HopperVM_valueStack + HopperVM_sp - 2;
    *((UInt*)&dataMemoryBlock[sp2]) = ((*((UInt*)&dataMemoryBlock[sp2]) <= codeMemoryBlock[HopperVM_pc++]) ? 1 : 0);
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp - 2] = 0x06; // Type::eBool
    return true;
}


Bool Instructions_InlinedGTI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_Push(((next > top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next > *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedLTI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_Push(((next < top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next < *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedGEI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_Push(((next >= top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next >= *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedLEI()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_AssertInt(ttype);
    HopperVM_AssertInt(ntype);
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    Int * next = (Int*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next <= *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedEQ()
{
#ifdef CHECKED
    HopperVM_Push(((HopperVM_Pop() == HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next == *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedNE()
{
#ifdef CHECKED
    HopperVM_Push(((HopperVM_Pop() != HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next == *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 0 : 1;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedGT()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(((next > top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next > *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}
Bool Instructions_InlinedLT()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(((next < top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next < *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}
Bool Instructions_InlinedGE()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(((next > top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next >= *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}
Bool Instructions_InlinedLE()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(((next < top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next <= *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}


