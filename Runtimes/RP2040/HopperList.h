#ifndef HOPPERLIST_H
#define HOPPERLIST_H

#include "Common.h"

UInt   HRList_New(Type htype);
UInt   HRList_Clone(UInt original);
void   HRList_Clear(UInt _this);
UInt   HRList_GetLength(UInt _this);
UInt32 HRList_GetItem(UInt _this, UInt index, Type & itype);
void   HRList_SetItem(UInt _this, UInt index, UInt32 item, Type itype);
void   HRList_Insert(UInt _this, UInt index, UInt32 item, Type itype);
void   HRList_Append(UInt _this, UInt32 item, Type itype);
bool   HRList_Contains(UInt _this, UInt32 item, Type itype);
void   HRList_Remove(UInt _this, UInt index);
void   HRList_Dump(UInt address, UInt indent);

#endif // HOPPERLIST_H