#ifndef _MEMORY_MAP_H
#define _MEMORY_MAP_H

////////////////////////////////////////////////////////////////////
// MEMORY LAYOUT
////////////////////////////////////////////////////////////////////

# include "romImage.h"

// 48K MEMORY
#define RAM_START   0x0000
#define RAM_END     (ROM_START-1)
byte    RAM[RAM_END-RAM_START+1];

#endif // _MEMORYMAP_H