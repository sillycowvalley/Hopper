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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    next = next + (dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSub()
{
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    next = next - (dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedMul()
{
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    next = next * (dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next / top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next % top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    next = next + (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x02; // Type::eInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    next = next - (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x02; // Type::eInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next / top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x02; // Type::eInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    next = next * (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x02; // Type::eInt;
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
    HopperVM_sp -= 1;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    if (top == 0)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next % top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x02; // Type::eInt;
#endif
    return true;
}

Bool Instructions_InlinedAddB()
{
    UInt sp2 = HopperVM_sp - 1;
    UInt dp = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dp = dp + codeStartAddress[HopperVM_pc++];
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(dp & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(dp >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedSubB()
{
    UInt sp2 = HopperVM_sp - 1;
    UInt dp = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dp = dp - codeStartAddress[HopperVM_pc++];
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(dp & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(dp >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
    return true;
}
Bool Instructions_InlinedPushI0()
{
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = 0;  dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp++;
    return true;
}
Bool Instructions_InlinedPushI1()
{
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = 1;  dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp++;
    return true;
}
Bool Instructions_InlinedPushIB()
{
#ifdef CHECKED
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
#else
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = codeStartAddress[HopperVM_pc++];  dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp++;
#endif
    return true;
}
Bool Instructions_InlinedPushIBB()
{
#ifdef CHECKED
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
#else
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = codeStartAddress[HopperVM_pc++];  dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp++;
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = codeStartAddress[HopperVM_pc++];  dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = 0x03; // Type::eByte
    HopperVM_sp++;
#endif
    return true;
}

Bool Instructions_InlinedPushLocalB00()
{
    UInt vp = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_bp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = (Byte)(vp & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = (Byte)(vp >> 8);
    Byte htype = dataMemoryBlock[HopperVM_typeStackPage + HopperVM_bp];
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = htype;
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        GC_AddReference(vp);
    }
    return true;
}
Bool Instructions_InlinedPushLocalB01()
{
    UInt vp = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_bp + 1] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_bp + 1] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = (Byte)(vp & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = (Byte)(vp >> 8);
    Byte htype = dataMemoryBlock[HopperVM_typeStackPage + HopperVM_bp + 1];
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = htype;
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        GC_AddReference(vp);
    }
    return true;
}

Bool Instructions_InlinedPushLocalB()
{
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt vp = dataMemoryBlock[offset + HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[offset + HopperVM_valueStackMSBPage + HopperVM_bp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] = (Byte)(vp & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] = (Byte)(vp >> 8);
    Byte htype = dataMemoryBlock[offset + HopperVM_typeStackPage + HopperVM_bp];
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp] = htype;
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        GC_AddReference(vp);
    }
    return true;
}

Bool Instructions_InlinedEnter()
{
    dataMemoryBlock[HopperVM_callStackLSBPage + HopperVM_csp] = HopperVM_bp; dataMemoryBlock[HopperVM_callStackMSBPage + HopperVM_csp] = 0;
    HopperVM_bp = HopperVM_sp;
    HopperVM_csp++;
    return true;
}

Bool Instructions_InlinedCallI()
{
    UInt methodAddress = codeStartAddress[HopperVM_pc++] + (codeStartAddress[HopperVM_pc++] << 8);
    dataMemoryBlock[HopperVM_callStackLSBPage + HopperVM_csp] = (HopperVM_pc & 0xFF); dataMemoryBlock[HopperVM_callStackMSBPage + HopperVM_csp] = (HopperVM_pc >> 8);
    HopperVM_pc = methodAddress;
    HopperVM_csp++;
    return true;
}

Bool Instructions_InlinedRetResB()
{
    UInt value = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp - 1] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp - 1] << 8);
    Byte rtype = dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp - 1];

    UInt popBytes = codeStartAddress[HopperVM_pc++];
    while (popBytes != 0)
    {
        HopperVM_sp--;
        UInt address = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp - 1] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp - 1] << 8);
        if (dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp - 1] >= 0x0D)
        {
            GC_Release(address);
        }
        popBytes--;
    }
    
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp - 1] = (Byte)(value & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp - 1] = (Byte)(value >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp - 1] = rtype;
    
    HopperVM_csp--;
    HopperVM_bp = dataMemoryBlock[HopperVM_callStackLSBPage + HopperVM_csp];
    if (HopperVM_csp == 0)
    {
        HopperVM_pc = 0;
    }
    else
    {
        HopperVM_csp--;
        HopperVM_pc = dataMemoryBlock[HopperVM_callStackLSBPage + HopperVM_csp] + (dataMemoryBlock[HopperVM_callStackMSBPage + HopperVM_csp] << 8);
    }
    return HopperVM_pc != 0;
}
Bool Instructions_InlinedJZB()
{
    HopperVM_sp = HopperVM_sp - 1;
    UInt dp = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    if (dp == 0)
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
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next <= codeStartAddress[HopperVM_pc++]) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next > top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next < top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] =  0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next >= top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] =  0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    Int next = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8));
    Int top  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next <= top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] =  0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedEQ()
{
#ifdef CHECKED
    HopperVM_Push(((HopperVM_Pop() == HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next == top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedNE()
{
#ifdef CHECKED
    HopperVM_Push(((HopperVM_Pop() != HopperVM_Pop())) ? (0x01) : (0x00), Type::eBool);
#else
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next == top) ? 0 : 1;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next > top) ? 1 : 0; 
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next < top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next >= top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (next <= top) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedCast()
{
    dataMemoryBlock[HopperVM_typeStackPage + HopperVM_sp - 1] = codeStartAddress[HopperVM_pc++];
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (((next != 0) || (top != 0))) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (((next != 0) && (top != 0))) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
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
    UInt sp2 = HopperVM_sp - 1;
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (top == 0) ? 1 : 0;
    dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = 0;
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x06; // Type::eBool;
#endif
    return true;
}

Bool Instructions_InlinedIncLocalB()
{
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt address = UInt(offset + HopperVM_bp);
    UInt value  = dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8);
#ifdef CHECKED
    if (value == 0xFFFF)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    value++;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(value & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(value >> 8);
    Byte   vtype = dataMemoryBlock[HopperVM_typeStackPage + address];
    if (vtype == 0x03) // Type::eByte
    {
        vtype = 0x04; // Type::eUInt;
    }
    dataMemoryBlock[HopperVM_typeStackPage + address] = vtype; 
    return true;
}

Bool Instructions_InlinedIncLocalIBB()
{
    Int8 offset0 = (Int8)(codeStartAddress[HopperVM_pc++]);
    Int8 offset1 = (Int8)(codeStartAddress[HopperVM_pc++]);
    Int value0 = (Int)(dataMemoryBlock[offset0 + HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[offset0 + HopperVM_valueStackMSBPage + HopperVM_bp] << 8));
    Int value1 = (Int)(dataMemoryBlock[offset1 + HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[offset1 + HopperVM_valueStackMSBPage + HopperVM_bp] << 8));
    value0 = value0 + value1;
    dataMemoryBlock[offset0 + HopperVM_valueStackLSBPage + HopperVM_bp] = (Byte)(value0 & 0xFF); dataMemoryBlock[offset0 + HopperVM_valueStackMSBPage + HopperVM_bp] = (Byte)(value0 >> 8);
    return true;
}

Bool Instructions_InlinedIncLocalBB()
{
    Int8 offset0 = (Int8)(codeStartAddress[HopperVM_pc++]);
    Int8 offset1 = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt value0 = (dataMemoryBlock[offset0 + HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[offset0 + HopperVM_valueStackMSBPage + HopperVM_bp] << 8));
    UInt value1 = (dataMemoryBlock[offset1 + HopperVM_valueStackLSBPage + HopperVM_bp] + (dataMemoryBlock[offset1 + HopperVM_valueStackMSBPage + HopperVM_bp] << 8));
    value0 = value0 + value1;
    dataMemoryBlock[offset0 + HopperVM_valueStackLSBPage + HopperVM_bp] = (Byte)(value0 & 0xFF); dataMemoryBlock[offset0 + HopperVM_valueStackMSBPage + HopperVM_bp] = (Byte)(value0 >> 8);
    return true;
}


Bool Instructions_InlinedDecLocalB()
{
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt value  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_bp + offset] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_bp + offset] << 8);
#ifdef CHECKED
    if (value == 0)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    value--;
    dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_bp + offset] = (Byte)(value & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_bp + offset] = (Byte)(value >> 8);
    
    return true;
}

Bool Instructions_InlinedIncGlobalB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Type itype = (Type)0;
    UInt value  = dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8);
#ifdef CHECKED
    if (value == 0xFFFF)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    Byte   vtype = dataMemoryBlock[HopperVM_typeStackPage + address];
    value++;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(value & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(value >> 8);
    if (vtype == 0x03) // Type::eByte
    {
        vtype = 0x04; // Type::eUInt;
    }
    dataMemoryBlock[HopperVM_typeStackPage + address] = vtype; 
    return true;
}

Bool Instructions_InlinedDecGlobalB()
{
    uint address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    UInt value  = dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8);
#ifdef CHECKED
    if (value == 0)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    value--;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(value & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(value >> 8);
    return true;
}

Bool Instructions_InlinedIncLocalIB()
{
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt address = UInt(HopperVM_bp + offset);
    Int ivalue  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8));
#ifdef CHECKED
    if (ivalue == 32767)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    ivalue++;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(ivalue & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(ivalue >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + address] = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedDecLocalIB()
{
    Int8 offset = (Int8)(codeStartAddress[HopperVM_pc++]);
    UInt address = UInt(HopperVM_bp + offset);
    Int ivalue  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8));
#ifdef CHECKED
    if (ivalue == -32768)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    ivalue--;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(ivalue & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(ivalue >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + address] = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedIncGlobalIB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Int ivalue  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8));
#ifdef CHECKED
    if (ivalue == 32767)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    ivalue++;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(ivalue & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(ivalue >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + address] = 0x02; // Type::eInt;
    return true;
}

Bool Instructions_InlinedDecGlobalIB()
{
    UInt address = codeStartAddress[HopperVM_pc++] + HopperVM_gp;
    Int ivalue  = (Int)(dataMemoryBlock[HopperVM_valueStackLSBPage + address] + (dataMemoryBlock[HopperVM_valueStackMSBPage + address] << 8));
#ifdef CHECKED
    if (ivalue == -32768)
    {
        Minimal_Error_Set(0x0D); // numeric type out of range / overflow
        return false;
    }    
#endif
    ivalue--;
    dataMemoryBlock[HopperVM_valueStackLSBPage + address] = (Byte)(ivalue & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + address] = (Byte)(ivalue >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + address] = 0x02; // Type::eInt;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    next = next | top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    next = next & top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    next = next << top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    next = next >> top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    UInt sp2 = HopperVM_sp - 1;
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    top = ~top;
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(top & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(top >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
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
    HopperVM_sp--;
    UInt sp2 = HopperVM_sp - 1;
    UInt top  = dataMemoryBlock[HopperVM_valueStackLSBPage + HopperVM_sp] + (dataMemoryBlock[HopperVM_valueStackMSBPage + HopperVM_sp] << 8);
    UInt next = dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] + (dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] << 8);
    next = ((next | top) & (~(next & top)));
    dataMemoryBlock[HopperVM_valueStackLSBPage + sp2] = (Byte)(next & 0xFF); dataMemoryBlock[HopperVM_valueStackMSBPage + sp2] = (Byte)(next >> 8);
    dataMemoryBlock[HopperVM_typeStackPage + sp2] = 0x04; // Type::eUInt;
#endif
    return true;
}

