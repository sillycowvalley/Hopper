func fizzbuzz()
    VAR number
    for number = 1 to 100
        if number mod 15 = 0 then
            print "FizzBuzz"
        else
            if number mod 3 = 0 then
                print "Fizz"
            else
                if number mod 5 = 0 then
                    print "Buzz"
                else
                    print number
                endif
            endif
        endif
    next number
endfunc

begin
    fizzbuzz()
end