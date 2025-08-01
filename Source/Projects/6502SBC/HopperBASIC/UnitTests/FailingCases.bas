REM String Type Tests for HopperBASIC
REM All string-related functionality tests extracted from comparison tests
REM Expected to fail until STRING type is implemented

REM Define required numeric and BIT variables for cross-type tests
int pos0 = 0
int pos1 = 1
int neg1 = -1
word w0 = 0
word w1 = 1
bit true = (1 = 1)
bit false = (1 = 0)

REM Test variables for string operations
string empty = ""
string hello = "hello"
string world = "world"
string hello2 = "hello"
string longer = "this is a longer string"
string space = " "
string number = "123"
string special = "!@#$%"

REM STRING = STRING comparisons (17 tests)
print hello = hello
print hello = hello2
print hello = world
print hello = empty
print hello = longer
print empty = empty
print empty = hello
print world = hello
print world = world
print longer = hello
print longer = longer
print space = space
print space = empty
print number = number
print number = hello
print special = special
print special = hello

REM STRING = literal comparisons (22 tests)
print "hello" = hello
print "hello" = world
print "hello" = hello2
print "world" = world
print "world" = hello
print "" = empty
print "" = hello
print "123" = number
print "123" = hello
print "!@#$%" = special
print "!@#$%" = hello

print hello = "hello"
print world = "hello"
print hello2 = "hello"
print world = "world"
print hello = "world"
print empty = ""
print hello = ""
print number = "123"
print hello = "123"
print special = "!@#$%"
print hello = "!@#$%"

REM STRING <> STRING comparisons (17 tests)
print hello <> hello
print hello <> hello2
print hello <> world
print hello <> empty
print hello <> longer
print empty <> empty
print empty <> hello
print world <> hello
print world <> world
print longer <> hello
print longer <> longer
print space <> space
print space <> empty
print number <> number
print number <> hello
print special <> special
print special <> hello

REM STRING <> literal comparisons (22 tests)
print "hello" <> hello
print "hello" <> world
print "hello" <> hello2
print "world" <> world
print "world" <> hello
print "" <> empty
print "" <> hello
print "123" <> number
print "123" <> hello
print "!@#$%" <> special
print "!@#$%" <> hello

print hello <> "hello"
print world <> "hello"
print hello2 <> "hello"
print world <> "world"
print hello <> "world"
print empty <> ""
print hello <> ""
print number <> "123"
print hello <> "123"
print special <> "!@#$%"
print hello <> "!@#$%"

REM Mixed type (cross-type) = comparisons (16 tests)
print hello = pos0
print hello = pos1
print hello = neg1
print hello = w0
print hello = w1
print hello = true
print hello = false

print empty = pos0
print empty = pos1
print empty = neg1
print empty = w0
print empty = w1
print empty = true
print empty = false

REM Mixed type (cross-type) <> comparisons (16 tests)
print hello <> pos0
print hello <> pos1
print hello <> neg1
print hello <> w0
print hello <> w1
print hello <> true
print hello <> false

print empty <> pos0
print empty <> pos1
print empty <> neg1
print empty <> w0
print empty <> w1
print empty <> true
print empty <> false

print pos0 <> hello
print pos1 <> hello
print neg1 <> hello
print w0 <> hello
print w1 <> hello
print true <> hello
print false <> hello

print pos0 <> empty
print pos1 <> empty
print neg1 <> empty
print w0 <> empty
print w1 <> empty
print true <> empty
print false <> empty

REM TYPE MISMATCH Tests for strings (Should generate errors)

REM STRING ordering operations (illegal - only = and <> allowed) (8 tests)
print hello < world      REM Should error: TYPE MISMATCH
print hello > world      REM Should error: TYPE MISMATCH
print hello <= world     REM Should error: TYPE MISMATCH
print hello >= world     REM Should error: TYPE MISMATCH
print "hello" < "world"  REM Should error: TYPE MISMATCH
print "hello" > "world"  REM Should error: TYPE MISMATCH
print "hello" <= "world" REM Should error: TYPE MISMATCH
print "hello" >= "world" REM Should error: TYPE MISMATCH

REM STRING vs numeric ordering (illegal) (8 tests)
print hello < pos1      REM Should error: TYPE MISMATCH
print hello > pos1      REM Should error: TYPE MISMATCH
print hello <= pos1     REM Should error: TYPE MISMATCH
print hello >= pos1     REM Should error: TYPE MISMATCH
print pos1 < hello      REM Should error: TYPE MISMATCH
print pos1 > hello      REM Should error: TYPE MISMATCH
print pos1 <= hello     REM Should error: TYPE MISMATCH
print pos1 >= hello     REM Should error: TYPE MISMATCH

REM STRING vs WORD ordering (illegal) (8 tests)
print hello < w1        REM Should error: TYPE MISMATCH
print hello > w1        REM Should error: TYPE MISMATCH
print hello <= w1       REM Should error: TYPE MISMATCH
print hello >= w1       REM Should error: TYPE MISMATCH
print w1 < hello        REM Should error: TYPE MISMATCH
print w1 > hello        REM Should error: TYPE MISMATCH
print w1 <= hello       REM Should error: TYPE MISMATCH
print w1 >= hello       REM Should error: TYPE MISMATCH

