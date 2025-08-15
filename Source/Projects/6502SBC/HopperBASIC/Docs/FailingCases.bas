CLS
NEW
TRON

BIT flags[20]

func addthem(a)
    word total
    PRINT "Counting"
    !PRINT a[0]
    for i = 0 TO LEN(a)-1
        PRINT i,
        if a[i] = true THEN
            total = total + 1
        ENDIF
    next i
endfunc

begin
    for i = 0 to len(flags)-1 STEP 2
        PRINT i,
        flags[i] = true
    next i
    PRINT "Initialized"
    print addthem(flags)
end

run
