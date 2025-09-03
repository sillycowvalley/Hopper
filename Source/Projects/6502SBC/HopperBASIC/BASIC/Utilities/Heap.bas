! Heap Manager for HopperBASIC
! Manages dynamic memory allocation in the 2K token buffer
! 
! Allocated blocks: [size:2]
! Free blocks: [size:2][next:2][prev:2] (minimum 6 bytes)
!
! Usage:
!   hpInit() - Initialize heap
!   ptr = hpMalloc(size) - Allocate memory, returns user pointer
!   hpFree(ptr) - Free memory allocated by hpMalloc
!   hpDump() - Debug: show heap layout

! Global heap state
VAR hpBase    ! Heap start address
VAR hpSz      ! Heap total size  
VAR hpHead    ! Free list head pointer

! Initialize heap - creates single free block covering entire buffer
FUNC hpInit()
    hpBase = PEEK(0xAA) * 256
    hpSz = PEEK(0xAB) * 256
    PRINT "Initializing heap at", hpBase, "size", hpSz
    PRINT "Size LSB:", hpSz & 0xFF, "MSB:", hpSz / 256
    hpHead = hpBase
    POKE(hpBase, hpSz & 0xFF)
    POKE(hpBase + 1, hpSz / 256)
    POKE(hpBase + 2, 0)
    POKE(hpBase + 3, 0) 
    POKE(hpBase + 4, 0)
    POKE(hpBase + 5, 0)
    PRINT "Verification: stored size =", hpGetSz(hpBase)
ENDFUNC

! Get block size from any block address
FUNC hpGetSz(addr)
    RETURN PEEK(addr) + PEEK(addr + 1) * 256
ENDFUNC

! Set block size at any block address
FUNC hpSetSz(addr, sz)
    POKE(addr, sz & 0xFF)        ! LSB first
    POKE(addr + 1, sz / 256)     ! MSB second
ENDFUNC

! Get next pointer from free block
FUNC hpGetNext(addr)
    RETURN PEEK(addr + 2) + PEEK(addr + 3) * 256  ! LSB + MSB*256
ENDFUNC

! Set next pointer in free block
FUNC hpSetNext(addr, next)
    POKE(addr + 2, next & 0xFF)      ! LSB first
    POKE(addr + 3, next / 256)       ! MSB second
ENDFUNC

! Get prev pointer from free block
FUNC hpGetPrev(addr)
    RETURN PEEK(addr + 4) + PEEK(addr + 5) * 256  ! LSB + MSB*256
ENDFUNC

! Set prev pointer in free block  
FUNC hpSetPrev(addr, prev)
    POKE(addr + 4, prev & 0xFF)      ! LSB first
    POKE(addr + 5, prev / 256)       ! MSB second
ENDFUNC

! Remove block from free list
FUNC hpUnlink(addr)
    VAR p = hpGetPrev(addr)
    VAR n = hpGetNext(addr)
    IF p <> 0 THEN
        hpSetNext(p, n)
    ELSE
        hpHead = n
    ENDIF
    IF n <> 0 THEN
        hpSetPrev(n, p)
    ENDIF
ENDFUNC

! Add block to head of free list
FUNC hpLink(addr)
    hpSetNext(addr, hpHead)
    hpSetPrev(addr, 0)
    IF hpHead <> 0 THEN
        hpSetPrev(hpHead, addr)
    ENDIF
    hpHead = addr
ENDFUNC

! Check if block is free by walking free list
FUNC hpIsFree(addr)
    VAR cur = hpHead
    WHILE cur <> 0
        IF cur = addr THEN RETURN TRUE ENDIF
        cur = hpGetNext(cur)
    WEND
    RETURN FALSE
ENDFUNC

! Find previous block by walking heap from start
FUNC hpFindPrev(target)
    VAR cur = hpBase
    WHILE cur < target
        VAR next = cur + hpGetSz(cur)
        IF next = target THEN RETURN cur ENDIF
        cur = next
    WEND
    RETURN 0
ENDFUNC

! Allocate memory block - returns user pointer (after 2-byte header)
FUNC hpMalloc(reqSz)
    VAR needSz = reqSz + 2
    VAR best = 0
    VAR bestSz = 0
    VAR cur = hpHead
    
    WHILE cur <> 0
        VAR blkSz = hpGetSz(cur)
        IF blkSz >= needSz THEN
            IF (best = 0) OR (blkSz < bestSz) THEN
                best = cur
                bestSz = blkSz
            ENDIF
        ENDIF
        cur = hpGetNext(cur)
    WEND
    
    IF best = 0 THEN RETURN 0 ENDIF
    
    hpUnlink(best)
    
    IF bestSz >= needSz + 6 THEN
        VAR newFree = best + needSz
        VAR remSz = bestSz - needSz
        hpSetSz(newFree, remSz)
        hpLink(newFree)
        hpSetSz(best, needSz)
    ENDIF
    
    RETURN best + 2
ENDFUNC

! Free memory block - merges with adjacent free blocks
FUNC hpFree(ptr)
    IF ptr = 0 THEN RETURN ENDIF
    
    VAR blk = ptr - 2
    VAR sz = hpGetSz(blk)
    VAR prev = hpFindPrev(blk)
    VAR next = blk + sz
    VAR mergAddr = blk
    VAR mergSz = sz
    
    IF (prev <> 0) AND hpIsFree(prev) THEN
        hpUnlink(prev)
        mergAddr = prev
        mergSz = mergSz + hpGetSz(prev)
    ENDIF
    
    IF (next < hpBase + hpSz) AND hpIsFree(next) THEN
        hpUnlink(next)
        mergSz = mergSz + hpGetSz(next)
    ENDIF
    
    hpSetSz(mergAddr, mergSz)
    hpLink(mergAddr)
ENDFUNC

! Debug: dump heap layout
FUNC hpDump()
    VAR cur = hpBase
    VAR sz
    PRINT "=== HEAP DUMP ==="
    PRINT "Base:", hpBase, "Size:", hpSz  
    PRINT "Free list head:", hpHead
    PRINT
    WHILE cur < hpBase + hpSz
        sz = hpGetSz(cur)
        PRINT "Block at", cur, "size", sz;
        IF hpIsFree(cur) THEN
            PRINT " FREE"
        ELSE
            PRINT " ALLOC" 
        ENDIF
        cur = cur + sz
    WEND
ENDFUNC