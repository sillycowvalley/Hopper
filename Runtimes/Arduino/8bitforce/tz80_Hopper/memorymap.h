#ifndef _MEMORY_MAP_H
#define _MEMORY_MAP_H

////////////////////////////////////////////////////////////////////
// MEMORY LAYOUT
////////////////////////////////////////////////////////////////////

// 32K MEMORY
#define RAM_START   0x0000
#define RAM_END     0x7FFF
byte    RAM[RAM_END-RAM_START+1];

# include "romImage.h"

#define ROM         rom_bin



#endif // _MEMORYMAP_H