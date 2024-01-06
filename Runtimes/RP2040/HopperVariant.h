#ifndef HOPPERVARIANT_H
#define HOPPERVARIANT_H

#include "Common.h"

UInt   HRVariant_CreateValueVariant(UInt32 value, Type vtype);
UInt   HRVariant_New(UInt32 value, Type vtype);
UInt32 HRVariant_GetValue(UInt _this, Type & vtype);
UInt32 HRVariant_UnBox(UInt _this, Type & vtype);
UInt   HRVariant_Clone(UInt original);
void   HRVariant_Clear(UInt _this);
bool   HRVariant_IsEqual(UInt32 left, Type ltype, UInt32 right, Type rtype);
void   HRVariant_Dump(UInt address, UInt indent);

#endif // HOPPERVARIANT_H