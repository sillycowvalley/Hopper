#ifndef HOPPERSTRING_H
#define HOPPERSTRING_H

#include "Common.h"

UInt HRString_New();
void HRString_BuildChar_R(UInt & _this, Char ch);
void HRString_BuildClear_R(UInt & _this);
UInt HRString_GetLength(UInt _this);
UInt HRString_Clone(UInt original);
Char HRString_GetChar(UInt _this, UInt index);
UInt HRString_NewFromConstant0(UInt location, UInt length);
UInt HRString_NewFromConstant1(UInt doubleChar);
UInt HRString_InsertChar(UInt _this, UInt index, Char ch);
Bool HRString_EndsWith(UInt _this, Char with);
Bool HRString_EndsWith(UInt _this, UInt with);
Int HRString_Compare(UInt left, UInt right);
UInt HRString_Replace(UInt _this, UInt pattern, UInt replace);
UInt HRString_Replace(UInt _this, Char from, Char to);
UInt HRString_Append(UInt _this, UInt append);
UInt HRString_Append(UInt _this, Char ch);
UInt HRString_Substring(UInt _this, UInt start);
UInt HRString_Substring(UInt _this, UInt start, UInt limit);
void HRString_Substring_R(UInt & _this, UInt start);
void HRString_BuildString_R(UInt & _this, UInt append);
void HRString_BuildFront_R(UInt & _this, Char ch);
UInt HRString_Trim(UInt _this);
void HRString_TrimRight_R(UInt & _this);
void HRString_TrimLeft_R(UInt & _this);
UInt HRString_TrimLeft(UInt _this);
UInt HRString_ToUpper(UInt _this);
UInt HRString_ToLower(UInt _this);
void HRString_ToUpper_R(UInt & _this);
void HRString_ToLower_R(UInt & _this);
void HRString_Dump(UInt address, UInt indent);

inline Bool HRChar_IsUpper(Char _this)
{
    Byte b;
    b = (Byte)_this;
    return ((b >= 65) && (b <= 90));  // A..Z
}
inline Bool HRChar_IsLower(Char _this)
{
    Byte b;
    b = (Byte)_this;
    return ((b >= 97) && (b <= 122)); // a..z
}
inline Char HRChar_ToUpper(Char _this) 
{
    Byte b;
    if (HRChar_IsLower(_this))
    {
        b = ((Byte)_this) - 97 + 65; // -a + A;
        _this = Char(b);
    }
    return _this;
}
inline Char HRChar_ToDigit(Byte d) // TODO : should be in Byte
{
    d = d + 48; // +0
    return (Char)d;
}
inline Bool HRChar_IsDigit(Char _this)
{
    Byte b;
    b = (Byte)_this;
    return ((b >= 48) && (b <= 57)); // 0..9
}
inline Bool HRChar_IsLetterOrDigit(Char _this)
{
    Byte b;
    b = (Byte)_this;
    return ((b >= 48) && (b <= 57)) || // 0..9
           ((b >= 65) && (b <= 90)) || // A..Z
           ((b >= 97) && (b <= 122));  // a..z
}
inline Char HRChar_ToHex(Byte h) // TODO : should be in Byte
{
    if (h < 10)
    {
        h = h + 48; // +0
    }
    else
    {
        h = h + 55; // +A - 10
    }
    return (Char)h;
}
inline Char HRChar_ToLower(Char _this) 
{
    Byte b;
    if (HRChar_IsUpper(_this))
    {
        b = ((Byte)_this) - 65 + 97; // -A + a;
        _this = Char(b); 
    }
    return _this;
}
inline Bool HRChar_IsHexDigit(Char _this)
{
    Byte b;
    b = (Byte)_this;
    return ((b >= 48) && (b <= 57)) || // 0..9
           ((b >= 65) && (b <= 70)) || // A..F
           ((b >= 97) && (b <= 102));  // a..f
}

#endif // HOPPERSTRING_H
