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
    hpHead = hpBase
    POKE(hpBase, hpSz & 0xFF)        ! LSB first
    POKE(hpBase + 1, hpSz / 256)     ! MSB second
    POKE(hpBase + 2, 0)              ! Next LSB = 0000
    POKE(hpBase + 3, 0)              ! Next MSB
    POKE(hpBase + 4, 0)              ! Prev LSB = 0000
    POKE(hpBase + 5, 0)              ! Prev MSB
ENDFUNC

! Get block size from any block address
FUNC hpGetSz(addr)
    RETURN PEEK(addr) + PEEK(addr + 1) * 256  ! LSB + MSB*256
ENDFUNC

! Set block size at any block address
FUNC hpSetSz(addr, sz)
    POKE(addr, sz & 0xFF)            ! LSB first
    POKE(addr + 1, sz / 256)         ! MSB second
ENDFUNC

! Get next pointer from free block
FUNC hpGetNext(addr)
    RETURN PEEK(addr + 2) + PEEK(addr + 3) * 256  ! LSB + MSB*256
ENDFUNC

! Set next pointer in free block
FUNC hpSetNext(addr, nxt)
    POKE(addr + 2, nxt & 0xFF)      ! LSB first
    POKE(addr + 3, nxt / 256)       ! MSB second
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
    VAR p, n
    p = hpGetPrev(addr)
    n = hpGetNext(addr)
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
    VAR cur
    cur = hpHead
    WHILE cur <> 0
        IF cur = addr THEN RETURN TRUE ENDIF
        cur = hpGetNext(cur)
    WEND
    RETURN FALSE
ENDFUNC

! Find previous block by walking heap from start
FUNC hpFindPrev(target)
    VAR cur, nxt
    cur = hpBase
    WHILE cur < target
        nxt = cur + hpGetSz(cur)
        IF nxt = target THEN RETURN cur ENDIF
        cur = nxt
    WEND
    RETURN 0
ENDFUNC

! Allocate memory block - returns user pointer (after 2-byte header)
FUNC hpMalloc(reqSz)
    VAR needSz, best, bestSz, cur, blkSz, newFree, remSz
    needSz = reqSz + 2
    best = 0
    bestSz = 0
    cur = hpHead
    
    WHILE cur <> 0
        blkSz = hpGetSz(cur)
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
        newFree = best + needSz
        remSz = bestSz - needSz
        hpSetSz(newFree, remSz)
        hpLink(newFree)
        hpSetSz(best, needSz)
    ENDIF
    
    RETURN best + 2
ENDFUNC

! Free memory block - merges with adjacent free blocks
FUNC hpFree(ptr)
    VAR blk, sz, prev, nxt, mergAddr, mergSz
    IF ptr = 0 THEN
        RETURN
    ENDIF
    
    blk = ptr - 2
    sz = hpGetSz(blk)
    prev = hpFindPrev(blk)
    nxt = blk + sz
    mergAddr = blk
    mergSz = sz
    
    IF (prev <> 0) AND hpIsFree(prev) THEN
        hpUnlink(prev)
        mergAddr = prev
        mergSz = mergSz + hpGetSz(prev)
    ENDIF
    
    IF (nxt < hpBase + hpSz) AND hpIsFree(nxt) THEN
        hpUnlink(nxt)
        mergSz = mergSz + hpGetSz(nxt)
    ENDIF
    
    hpSetSz(mergAddr, mergSz)
    hpLink(mergAddr)
ENDFUNC

! Debug: dump heap layout
FUNC hpDump()
    VAR cur, count, sz
    cur = hpBase
    count = 0
    PRINT "=== HEAP DUMP ==="
    PRINT "Base:", hpBase, "Size:", hpSz
    PRINT "Free list head:", hpHead
    PRINT
    WHILE (cur < hpBase + hpSz) AND (count < 10)
        sz = hpGetSz(cur)
        PRINT "Block at", cur, "size", sz;
        IF hpIsFree(cur) THEN
            PRINT " FREE"
        ELSE
            PRINT " ALLOC"
        ENDIF
        cur = cur + sz
        count = count + 1
    WEND
    IF count >= 10 THEN
        PRINT "... (stopped after 10 blocks)"
    ENDIF
ENDFUNC

! Heap Manager Test Suite
! Tests allocation, freeing, fragmentation, and merging
! Global array for fragmentation test
INT ptrs[10]

FUNC TestBasicAllocation()
    VAR ptr1, ptr2, ptr3
    PRINT "=== BASIC ALLOCATION TEST ==="
    hpDump()
    
    PRINT "Allocating 100 bytes..."
    ptr1 = hpMalloc(100)
    PRINT "Got pointer:", ptr1
    hpDump()
    
    PRINT "Allocating 200 bytes..."
    ptr2 = hpMalloc(200)
    PRINT "Got pointer:", ptr2
    hpDump()
    
    PRINT "Allocating 50 bytes..."
    ptr3 = hpMalloc(50)
    PRINT "Got pointer:", ptr3
    hpDump()
    
    PRINT "Freeing middle block (200 bytes)..."
    hpFree(ptr2)
    hpDump()
    
    PRINT "Freeing first block (100 bytes)..."
    hpFree(ptr1)
    hpDump()
    
    PRINT "Freeing last block (50 bytes)..."
    hpFree(ptr3)
    hpDump()
    PRINT
ENDFUNC

FUNC TestFragmentation()
    VAR i, bigPtr, smallPtr
    PRINT "=== FRAGMENTATION TEST ==="
    
    PRINT "Allocating 10 blocks of 80 bytes each..."
    FOR i = 0 TO 9
        ptrs[i] = hpMalloc(80)
        PRINT "Block", i, "at", ptrs[i]
    NEXT i
    hpDump()
    
    PRINT "Freeing every other block..."
    FOR i = 1 TO 9 STEP 2
        PRINT "Freeing block", i, "at", ptrs[i]
        hpFree(ptrs[i])
        ptrs[i] = 0
    NEXT i
    hpDump()
    
    PRINT "Trying to allocate 200 bytes (should fail - fragmented)..."
    bigPtr = hpMalloc(200)
    PRINT "Result:", bigPtr
    
    PRINT "Allocating 60 bytes (should fit in fragments)..."
    smallPtr = hpMalloc(60)
    PRINT "Got pointer:", smallPtr
    hpDump()
    
    PRINT "Cleaning up remaining blocks..."
    FOR i = 0 TO 9 STEP 2
        IF ptrs[i] <> 0 THEN hpFree(ptrs[i]) ENDIF
    NEXT i
    IF smallPtr <> 0 THEN hpFree(smallPtr) ENDIF
    hpDump()
    PRINT
ENDFUNC

FUNC TestMerging()
    VAR ptr1, ptr2, ptr3
    PRINT "=== MERGING TEST ==="
    
    PRINT "Allocating three adjacent 300-byte blocks..."
    ptr1 = hpMalloc(300)
    ptr2 = hpMalloc(300)
    ptr3 = hpMalloc(300)
    PRINT "Block1:", ptr1, "Block2:", ptr2, "Block3:", ptr3
    hpDump()
    
    PRINT "Freeing middle block..."
    hpFree(ptr2)
    hpDump()
    
    PRINT "Freeing first block (should merge with middle)..."
    hpFree(ptr1)
    hpDump()
    
    PRINT "Freeing last block (should merge all three)..."
    hpFree(ptr3)
    hpDump()
    PRINT
ENDFUNC

FUNC TestOutOfMemory()
    VAR bigPtr, ptr2
    PRINT "=== OUT OF MEMORY TEST ==="
    
    PRINT "Trying to allocate 3000 bytes (more than heap)..."
    bigPtr = hpMalloc(3000)
    PRINT "Result:", bigPtr, "(should be 0)"
    
    PRINT "Allocating entire heap minus overhead..."
    bigPtr = hpMalloc(2040)
    PRINT "Result:", bigPtr
    hpDump()
    
    PRINT "Trying another allocation (should fail)..."
    ptr2 = hpMalloc(10)
    PRINT "Result:", ptr2, "(should be 0)"
    
    PRINT "Freeing large block..."
    hpFree(bigPtr)
    hpDump()
    PRINT
ENDFUNC

FUNC TestEdgeCases()
    VAR ptr1, ptr2
    PRINT "=== EDGE CASES TEST ==="
    
    PRINT "Allocating 0 bytes..."
    ptr1 = hpMalloc(0)
    PRINT "Result:", ptr1
    
    PRINT "Freeing null pointer..."
    hpFree(0)
    PRINT "Should not crash"
    
    PRINT "Allocating minimum size (1 byte)..."
    ptr1 = hpMalloc(1)
    PRINT "Result:", ptr1
    hpDump()
    
    PRINT "Allocating maximum possible size..."
    hpFree(ptr1)
    ptr2 = hpMalloc(2046)
    PRINT "Result:", ptr2
    hpDump()
    
    hpFree(ptr2)
    PRINT
ENDFUNC

FUNC WriteTestPattern(ptr, sz)
    VAR i
    PRINT "Writing test pattern to", ptr, "size", sz
    FOR i = 0 TO sz - 1
        POKE(ptr + i, (i + 1) & 0xFF)
    NEXT i
ENDFUNC

FUNC CheckTestPattern(ptr, sz)
    VAR i, expected, actual
    FOR i = 0 TO sz - 1
        expected = (i + 1) & 0xFF
        actual = PEEK(ptr + i)
        IF actual <> expected THEN
            PRINT "ERROR at offset", i, "expected", expected, "got", actual
            RETURN FALSE
        ENDIF
    NEXT i
    PRINT "Pattern verified OK"
    RETURN TRUE
ENDFUNC

FUNC TestDataIntegrity()
    VAR ptr1, ptr2, ptr3
    PRINT "=== DATA INTEGRITY TEST ==="
    
    ptr1 = hpMalloc(100)
    ptr2 = hpMalloc(150)
    ptr3 = hpMalloc(80)
    
    WriteTestPattern(ptr1, 100)
    WriteTestPattern(ptr2, 150)
    WriteTestPattern(ptr3, 80)
    
    PRINT "Checking patterns after allocation..."
    CheckTestPattern(ptr1, 100)
    CheckTestPattern(ptr2, 150)  
    CheckTestPattern(ptr3, 80)
    
    PRINT "Freeing middle block and checking others..."
    hpFree(ptr2)
    CheckTestPattern(ptr1, 100)
    CheckTestPattern(ptr3, 80)
    
    hpFree(ptr1)
    hpFree(ptr3)
    PRINT
ENDFUNC

BEGIN
    PRINT "HEAP MANAGER TEST SUITE"
    PRINT "======================="
    PRINT
    
    hpInit()
    
    TestBasicAllocation()
    TestFragmentation() 
    TestMerging()
    TestOutOfMemory()
    TestEdgeCases()
    TestDataIntegrity()
    
    PRINT "=== ALL TESTS COMPLETE ==="
    PRINT "Final heap state:"
    hpDump()
END

