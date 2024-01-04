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

#endif // HOPPERSTRING_H
