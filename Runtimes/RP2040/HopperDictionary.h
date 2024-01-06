#ifndef HOPPERDICTIONARY_H
#define HOPPERDICTIONARY_H

#include "Common.h"

UInt HRDictionary_New(Type ktype, Type vtype);
void HRDictionary_Clear(UInt _this);
void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt32 value, Type vtype);
bool HRDictionary_Next(UInt _this, UInt & iterator, UInt & hrpair);
Bool HRDictionary_Contains(UInt _this, UInt key);
UInt32 HRDictionary_Get(UInt _this, UInt key, Type & vtype);
UInt HRDictionary_Clone(UInt original);
void HRDictionary_Dump(UInt address, UInt indent);
UInt HRDictionary_GetCount(UInt _this);

#endif // HOPPERDICTIONARY_H