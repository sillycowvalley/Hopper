#ifndef HOPPERPAIR_H
#define HOPPERPAIR_H

#include "Common.h"

UInt   HRPair_New(Type ktype, UInt key, Type vtype, UInt32 value);
UInt   HRPair_Clone(UInt original);
void   HRPair_Clear(UInt _this);
UInt   HRPair_GetKey(UInt _this, Type & ktype);
UInt32 HRPair_GetValue(UInt _this, Type & vtype);
void   HRPair_Dump(UInt address, UInt indent);

#endif // HOPPERPAIR_H