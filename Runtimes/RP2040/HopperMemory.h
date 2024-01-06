#ifndef HOPPERMEMORY_H
#define HOPPERMEMORY_H

#include "Common.h"

UInt Memory_HeapStart_Get();
UInt Memory_HeapSize_Get();
UInt Memory_FreeList_Get();
void Memory_Free(UInt address);
void Memory_Initialize(UInt start, UInt size);
UInt Memory_Allocate(UInt size);
void Memory_Set(UInt memory, Byte value, UInt size);
void GC_Release(UInt address);
UInt GC_New(UInt size, Type htype);
UInt GC_Clone(UInt original);
void GC_AddReference(UInt address);
Bool IsReferenceType(Type htype);
void GC_Dump(UInt address);
void GC_Dump(UInt address, UInt indent);



#endif // HOPPERMEMORY_H
