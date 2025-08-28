# File Unit: Chained Directory Sectors Implementation Plan

## Project Overview

Transform the File unit from single-sector directory (16 files max) to chained directory sectors (unlimited files) through three incremental stages. Each stage builds robust, reusable infrastructure while maintaining full backward compatibility.

**Current State:** 262 bytes ROM available, 16-file limit  
**Target:** Elegant chained directory system with automatic growth/cleanup

---

## Stage 1: Infrastructure Foundation
**Target:** ~80 bytes, ~180 bytes ROM remaining  
**Goal:** Build sector-aware directory infrastructure

### New Zero Page Variables
```asm
const byte CurrentDirectorySector = ZP.FS15;  // Current directory sector being accessed
```

### New Methods Added

#### `loadDirectory(A=sector)`
- **Size:** ~40 bytes
- **Input:** A = directory sector number to load
- **Output:** DirectoryBuffer contains specified directory sector
- **Purpose:** Replace hardcoded sector 1 with parameterized loading

#### `writeDirectory(A=sector)`  
- **Size:** ~40 bytes
- **Input:** A = directory sector number to write
- **Output:** DirectoryBuffer written to specified sector
- **Purpose:** Replace hardcoded sector 1 with parameterized writing

#### `loadDirectorySector(entryIndex)`
- **Size:** ~40 bytes  
- **Input:** A = global entry index (0-255)
- **Output:** Correct directory sector loaded, `CurrentDirectorySector` updated
- **Logic:** `sector = 1 + (entryIndex / 16)`, load that sector

#### `getCurrentDirectorySector(entryIndex)`
- **Size:** ~15 bytes
- **Input:** A = global entry index  
- **Output:** A = directory sector number for that entry
- **Logic:** `return 1 + (entryIndex / 16)`

### Existing Methods Modified

#### `loadDirectory()` → `loadDirectory(A=1)`
- **Change:** Add `LDA #1` before calling new parameterized version
- **Size:** +5 bytes
- **Maintains:** Exact same external behavior

#### `writeDirectory()` → `writeDirectory(A=1)`  
- **Change:** Add `LDA #1` before calling new parameterized version
- **Size:** +5 bytes
- **Maintains:** Exact same external behavior

### Testing Strategy
- All existing functionality unchanged (still 16-file limit)
- Directory operations now go through sector-aware infrastructure
- Verify DIR, SAVE, LOAD, DELETE all work identically
- Test with full directory (16 files) to ensure no regressions

---

## Stage 2: Growth Capability  
**Target:** ~70 bytes, ~110 bytes ROM remaining  
**Goal:** Enable unlimited file growth through automatic sector allocation

### New Methods Added

#### `allocateDirectorySector()`
- **Size:** ~45 bytes
- **Purpose:** Allocate new directory sector and link it to chain
- **Logic:**
  - Find free sector using `allocateFirstFreeSector()`
  - Clear new sector to all zeros
  - Update FAT to link previous directory sector to new one
  - Mark new sector as end-of-chain (1) in FAT

#### `nextDirectorySector(currentSector)`
- **Size:** ~15 bytes
- **Input:** A = current directory sector
- **Output:** A = next directory sector (or 0 if end of chain)
- **Logic:** `return FATBuffer[currentSector]` (1 = end-of-chain)

### Existing Methods Modified

#### `findFreeDirectoryEntry()`
- **Major Enhancement:** ~25 bytes additional
- **New Logic:**
  1. Start with directory sector 1
  2. Search 16 entries in current sector  
  3. If no free entry found, follow FAT chain to next sector
  4. If at end of chain, call `allocateDirectorySector()`
  5. Continue until free entry found or allocation fails
- **Output:** `CurrentFileEntry` now represents global entry index (0-255)
- **Side Effect:** Sets `CurrentDirectorySector` to sector containing free entry

#### `loadDirectorySector(entryIndex)`
- **Enhancement:** Now handles any entry index, not just 0-15
- **Logic:** Load sector `1 + (entryIndex / 16)`, set `CurrentDirectorySector`

### Testing Strategy  
- Test directory growth past 16 files
- Verify FAT chain linking works correctly
- Test SAVE with 17th, 18th, etc. files
- Ensure DIR command shows all files across sectors
- Test error handling when disk full

---

## Stage 3: Cleanup Optimization
**Target:** ~90 bytes, ~20 bytes ROM remaining  
**Goal:** Complete elegant solution with automatic cleanup

### New Methods Added

#### `isDirectorySectorEmpty(sector)`
- **Size:** ~25 bytes
- **Input:** A = directory sector number
- **Output:** C set if all 16 entries have length = 0
- **Logic:** Load sector, scan all 16 entries for fileLength != 0

#### `unlinkDirectorySector(sector)`
- **Size:** ~40 bytes  
- **Input:** A = directory sector to remove from chain
- **Logic:**
  1. Find previous sector in directory chain
  2. Update previous sector's FAT entry to skip deleted sector
  3. Mark deleted sector as free (0) in FAT
  4. Write updated FAT to EEPROM

#### `findPreviousDirectorySector(targetSector)`
- **Size:** ~20 bytes
- **Input:** A = target directory sector  
- **Output:** A = previous directory sector (or 0 if target is first)
- **Logic:** Walk directory chain from sector 1 until next = target

### Existing Methods Modified

#### `Delete()`
- **Enhancement:** ~25 bytes additional  
- **New Logic:** After clearing directory entry and writing metadata:
  1. Check if current directory sector is now empty
  2. If empty AND not sector 1, unlink and free the sector
  3. Update file system metadata

#### Directory Iteration Methods
All methods that scan directory entries need sector-aware updates:

##### `countFilesAndBytes()`  
- **Change:** Traverse directory chain, scan all sectors
- **Size:** +15 bytes

##### `printAllFileEntries()` / `dumpDirectoryEntries()`
- **Change:** Print files from all directory sectors in chain
- **Size:** +20 bytes each

##### `findFileInDirectory()`
- **Change:** Search across all directory sectors
- **Size:** +15 bytes

### Testing Strategy
- Test file deletion with cleanup
- Verify empty sectors are properly unlinked and freed
- Test edge cases: delete from first sector, middle sector, last sector
- Ensure directory chain integrity maintained after cleanup
- Test fragmented directory scenarios

---

## Implementation Considerations

### Backward Compatibility
- All existing single-sector directories continue working
- No changes to file format or EEPROM layout  
- `Format()` unchanged (still creates sector 1 as directory)

### Error Handling
- Maintain existing error codes and messages
- Graceful degradation when disk full
- Proper cleanup on allocation failures

### Memory Usage
- No additional RAM usage beyond 1 ZP variable
- All operations use existing DirectoryBuffer
- FAT operations use existing FATBuffer

### Performance Impact  
- Single-sector directories: No performance change
- Multi-sector directories: Minimal overhead from sector loading
- Directory traversal becomes O(sectors) instead of O(1)

---

## ROM Space Tracking

| Stage | New Code | Cumulative | ROM Remaining |
|-------|----------|------------|---------------|
| Start | -        | -          | 262 bytes     |
| Stage 1 | ~80 bytes | 80 bytes   | ~180 bytes    |
| Stage 2 | ~70 bytes | 150 bytes  | ~110 bytes    |  
| Stage 3 | ~90 bytes | 240 bytes  | ~20 bytes     |

### Risk Mitigation
- Each stage is fully functional stopping point
- Conservative size estimates (likely smaller in practice)
- Can optimize existing code if space becomes critical
- Bug fix space available until Stage 3

---

## Success Criteria

### Stage 1 Complete
- ✅ All directory operations sector-parameterized  
- ✅ Zero behavior change for existing functionality
- ✅ Infrastructure ready for multi-sector expansion

### Stage 2 Complete  
- ✅ Files beyond 16 can be created and saved
- ✅ DIR command shows all files across sectors
- ✅ Automatic directory sector allocation working
- ✅ FAT chain integrity maintained

### Stage 3 Complete
- ✅ Empty directory sectors automatically cleaned up
- ✅ Optimal disk space utilization  
- ✅ Robust directory chain management
- ✅ Production-ready elegant solution

This incremental approach builds a solid, reusable component suitable for future projects while respecting current ROM constraints.