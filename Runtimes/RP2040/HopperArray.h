#ifndef HOPPERARRAY_H
#define HOPPERARRAY_H

#include "Common.h"

UInt HRArray_New(Type htype, UInt count);
UInt HRArray_GetCount(UInt _this);
UInt HRArray_GetItem(UInt _this, UInt index, Type & etype);
void HRArray_SetItem(UInt _this, UInt index, UInt value);

#endif // HOPPERARRAY_H