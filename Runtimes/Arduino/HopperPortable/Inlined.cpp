#include "Inlined.h"

#ifdef CHECKED
/*
void HopperVM_AssertBool(Type htype, UInt value)
{
    if (htype != Type::eBool)
    {
        HopperVM_AssertUInt(htype, value);
    }
    if (value > 1)
    {
        Runtime_ErrorDump(36);
        Minimal_Error_Set(0x0B);
    }
}
*/
#endif

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
    UInt top = *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    *next = *next / top;
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
    UInt top = *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    *next = *next % top;
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
    Int top = *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    *next = *next / top;
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
    Int top = *((Int*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    *next = *next % top;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedAddB()
{
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp + codeStartAddress[HopperVM_pc++];
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSubB()
{
    UInt sp2 = HopperVM_sp - 2;
    UInt * dp = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *dp = *dp - codeStartAddress[HopperVM_pc++];
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
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = codeStartAddress[HopperVM_pc++];
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp += 2;
#endif
    return true;
}
Bool Instructions_InlinedPushIBB()
{
#ifdef CHECKED
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
#else
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = codeStartAddress[HopperVM_pc++];
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp += 2;
    *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]) = codeStartAddress[HopperVM_pc++];
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
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
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
    UInt methodAddress = codeStartAddress[HopperVM_pc++] + (codeStartAddress[HopperVM_pc++] << 8);
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

    UInt popBytes = codeStartAddress[HopperVM_pc++];
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
    }
    else
    {
        HopperVM_csp -= 2;
        HopperVM_pc = *((UInt*)(&dataMemoryBlock[HopperVM_callStack + HopperVM_csp]));
    }
    return HopperVM_pc != 0;
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
    *((UInt*)&dataMemoryBlock[sp2]) = ((*((UInt*)&dataMemoryBlock[sp2]) <= codeStartAddress[HopperVM_pc++]) ? 1 : 0);
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
    HopperVM_Push(((next >= top)) ? (0x01) : (0x00), Type::eBool);
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
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (*next <= *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp])) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedCast()
{
    dataMemoryBlock[HopperVM_typeStack + HopperVM_sp - 0x02] = codeStartAddress[HopperVM_pc++];
    return true;
}



Bool Instructions_InlinedBoolOr()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push((((next != 0x00) || (top != 0x00))) ? (0x01) : (0x00), Type::eBool);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt *  top = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (((*next != 0) || (*top != 0))) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedBoolAnd()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push((((next != 0x00) && (top != 0x00))) ? (0x01) : (0x00), Type::eBool);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt *  top = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = (((*next != 0) && (*top != 0))) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedBoolNot()
{
#ifdef CHECKED  
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    HopperVM_AssertBool(ttype, top);
    HopperVM_Push(((top == 0x00)) ? (0x01) : (0x00), Type::eBool);
#else
    UInt sp2 = HopperVM_sp - 2;
    UInt * top = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *top = (*top == 0) ? 1 : 0;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedIncLocalB()
{
    UInt address = UInt(HopperVM_bp + (Int8)(codeStartAddress[HopperVM_pc++]));
    UInt * value = (UInt*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*value == 0xFFFF)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*value)++;
    Byte   vtype = dataMemoryBlock[HopperVM_typeStack + address];
    if (vtype == 0x03) // Type::eByte
    {
        vtype = 0x04; // Type::eUInt;
    }
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = vtype; 
    return true;
}

Bool Instructions_InlinedDecLocalB()
{
    UInt * value = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_bp + (Int8)(codeStartAddress[HopperVM_pc++])];
#ifdef CHECKED
    if (*value == 0)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*value)--;
    return true;
}

Bool Instructions_InlinedIncGlobalB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Type itype = (Type)0;
    UInt * value = (UInt*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*value == 0xFFFF)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*value)++;
    Byte   vtype = dataMemoryBlock[HopperVM_typeStack + address];
    if (vtype == 0x03) // Type::eByte
    {
        vtype = 0x04; // Type::eUInt;
    }
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = vtype; 
    return true;
}

Bool Instructions_InlinedDecGlobalB()
{
    UInt * value = (UInt*)&dataMemoryBlock[HopperVM_valueStack + codeStartAddress[HopperVM_pc++] + HopperVM_gp];
#ifdef CHECKED
    if (*value == 0)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*value)--;
    return true;
}

Bool Instructions_InlinedIncLocalIB()
{
    UInt address = UInt(HopperVM_bp + (Int8)(codeStartAddress[HopperVM_pc++]));
    Int * ivalue = (Int*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*ivalue == 32767)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*ivalue)++;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedDecLocalIB()
{
    UInt address = UInt(HopperVM_bp + (Int8)(codeStartAddress[HopperVM_pc++]));
    Int * ivalue = (Int*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*ivalue == -32768)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*ivalue)--;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedIncGlobalIB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Int * ivalue = (Int*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*ivalue == 32767)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*ivalue)++;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedDecGlobalIB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Int * ivalue = (Int*)&dataMemoryBlock[HopperVM_valueStack + address];
#ifdef CHECKED
    if (*ivalue == -32768)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    (*ivalue)--;
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + address]) = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedBitOr()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(next | top, Type::eUInt);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next | *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedBitAnd()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(next & top, Type::eUInt);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next & *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedBitShl()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(next << top, Type::eUInt);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next << *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedBitShr()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push(next >> top, Type::eUInt);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = *next >> *((UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp]);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedBitNot()
{
#ifdef CHECKED  
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_Push(~top, Type::eUInt);
#else
    UInt sp2 = HopperVM_sp - 2;
    UInt * top = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *top = ~(*top);
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

Bool Instructions_InlinedBitXor()
{
#ifdef CHECKED
    Type ttype = (Type)0;
    UInt top = HopperVM_Pop_R(ttype);
    Type ntype = (Type)0;
    UInt next = HopperVM_Pop_R(ntype);
    HopperVM_AssertUInt(ttype, top);
    HopperVM_AssertUInt(ntype, next);
    HopperVM_Push((next | top) & (~(next & top)), Type::eUInt);
#else    
    HopperVM_sp -= 2;
    UInt sp2 = HopperVM_sp - 2;
    UInt *  top = (UInt*)&dataMemoryBlock[HopperVM_valueStack + HopperVM_sp];
    UInt * next = (UInt*)&dataMemoryBlock[HopperVM_valueStack + sp2];
    *next = ((*next | *top) & (~(*next & *top)));
    *((UInt*)&dataMemoryBlock[HopperVM_typeStack + sp2]) = 0x04; // Type::eUInt;
#endif
    return true;
}

