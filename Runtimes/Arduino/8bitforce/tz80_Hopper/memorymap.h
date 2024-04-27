#ifndef _MEMORY_MAP_H
#define _MEMORY_MAP_H

////////////////////////////////////////////////////////////////////
// MEMORY LAYOUT
////////////////////////////////////////////////////////////////////

# include "romImage.h"

#define RAM_START   ROM_END + 1
#define RAM_END     0xFFFF
byte    RAM[RAM_END-RAM_START+1];



#define ROM         rom_bin



#endif // _MEMORYMAP_H