REM Comparison Operator Tests for HopperBASIC
REM Testing all comparison operators: = <> < > <= >=
REM Between INT, WORD, BIT, and STRING types

REM Test Count Summary:
REM = tests: 264 tests
REM <> tests: 264 tests  
REM < tests: 56 tests (numeric only)
REM > tests: 56 tests (numeric only)
REM <= tests: 56 tests (numeric only)
REM >= tests: 56 tests (numeric only)
REM BIT comparison tests: 42 tests
REM TYPE MISMATCH tests: 24 tests
REM Total: 818 tests

REM Test variables
int pos0 = 0
int pos1 = 1
int pos127 = 127
int pos128 = 128
int pos255 = 255
int pos256 = 256
int pos1000 = 1000
int pos16383 = 16383
int pos16384 = 16384
int pos32766 = 32766
int pos32767 = 32767

int neg1 = -1
int neg2 = -2
int neg127 = -127
int neg128 = -128
int neg255 = -255
int neg256 = -256
int neg1000 = -1000
int neg16383 = -16383
int neg16384 = -16384
int neg32767 = -32767
int neg32768 = -32768

word w0 = 0
word w1 = 1
word w2 = 2
word w127 = 127
word w128 = 128
word w255 = 255
word w256 = 256
word w1000 = 1000
word w16383 = 16383
word w16384 = 16384
word w32766 = 32766
word w32767 = 32767
word w32768 = 32768
word w32769 = 32769
word w49151 = 49151
word w49152 = 49152
word w65534 = 65534
word w65535 = 65535

bit true = (1 = 1)
bit false = (1 = 0)

string empty = ""
string hello = "hello"
string world = "world"
string hello2 = "hello"
string longer = "this is a longer string"
string space = " "
string number = "123"
string special = "!@#$%"

REM ===== Equality Tests (=) =====

REM INT = WORD comparisons (27 tests)
print pos0 = w0
print pos0 = w1
print pos0 = w32767
print pos0 = w32768
print pos1 = w0
print pos1 = w1
print pos1 = w32767
print pos1 = w32768
print pos127 = w127
print pos127 = w128
print pos128 = w127
print pos128 = w128
print pos255 = w255
print pos255 = w256
print pos256 = w255
print pos256 = w256
print pos1000 = w1000
print pos16383 = w16383
print pos16383 = w16384
print pos16384 = w16383
print pos16384 = w16384
print pos32766 = w32766
print pos32766 = w32767
print pos32766 = w32768
print pos32767 = w32766
print pos32767 = w32767
print pos32767 = w32768

print neg1 = w0
print neg1 = w1
print neg1 = w32767
print neg1 = w32768
print neg1 = w65535
print neg2 = w0
print neg2 = w1
print neg2 = w32767
print neg2 = w32768
print neg127 = w127
print neg127 = w128
print neg128 = w127
print neg128 = w128
print neg255 = w255
print neg255 = w256
print neg256 = w255
print neg256 = w256
print neg1000 = w1000
print neg16383 = w16383
print neg16383 = w16384
print neg16384 = w16383
print neg16384 = w16384
print neg32767 = w32766
print neg32767 = w32767
print neg32767 = w32768
print neg32768 = w0
print neg32768 = w32766
print neg32768 = w32767
print neg32768 = w32768
print neg32768 = w65535

REM WORD = INT comparisons (57 tests)
print w0 = pos0
print w1 = pos0
print w32767 = pos0
print w32768 = pos0
print w0 = pos1
print w1 = pos1
print w32767 = pos1
print w32768 = pos1
print w127 = pos127
print w128 = pos127
print w127 = pos128
print w128 = pos128
print w255 = pos255
print w256 = pos255
print w255 = pos256
print w256 = pos256
print w1000 = pos1000
print w16383 = pos16383
print w16384 = pos16383
print w16383 = pos16384
print w16384 = pos16384
print w32766 = pos32766
print w32767 = pos32766
print w32768 = pos32766
print w32766 = pos32767
print w32767 = pos32767
print w32768 = pos32767

print w0 = neg1
print w1 = neg1
print w32767 = neg1
print w32768 = neg1
print w65535 = neg1
print w0 = neg2
print w1 = neg2
print w32767 = neg2
print w32768 = neg2
print w127 = neg127
print w128 = neg127
print w127 = neg128
print w128 = neg128
print w255 = neg255
print w256 = neg255
print w255 = neg256
print w256 = neg256
print w1000 = neg1000
print w16383 = neg16383
print w16384 = neg16383
print w16383 = neg16384
print w16384 = neg16384
print w32766 = neg32767
print w32767 = neg32767
print w32768 = neg32767
print w0 = neg32768
print w32766 = neg32768
print w32767 = neg32768
print w32768 = neg32768
print w65535 = neg32768

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

REM BIT = BIT comparisons (4 tests)
print true = true
print true = false
print false = true
print false = false

REM BIT = INT comparisons (44 tests)
print true = pos0
print true = pos1
print true = pos127
print true = pos128
print true = pos255
print true = pos256
print true = pos1000
print true = pos16383
print true = pos16384
print true = pos32766
print true = pos32767

print false = pos0
print false = pos1
print false = pos127
print false = pos128
print false = pos255
print false = pos256
print false = pos1000
print false = pos16383
print false = pos16384
print false = pos32766
print false = pos32767

print true = neg1
print true = neg2
print true = neg127
print true = neg128
print true = neg255
print true = neg256
print true = neg1000
print true = neg16383
print true = neg16384
print true = neg32767
print true = neg32768

print false = neg1
print false = neg2
print false = neg127
print false = neg128
print false = neg255
print false = neg256
print false = neg1000
print false = neg16383
print false = neg16384
print false = neg32767
print false = neg32768

REM INT = BIT comparisons (44 tests)
print pos0 = true
print pos1 = true
print pos127 = true
print pos128 = true
print pos255 = true
print pos256 = true
print pos1000 = true
print pos16383 = true
print pos16384 = true
print pos32766 = true
print pos32767 = true

print pos0 = false
print pos1 = false
print pos127 = false
print pos128 = false
print pos255 = false
print pos256 = false
print pos1000 = false
print pos16383 = false
print pos16384 = false
print pos32766 = false
print pos32767 = false

print neg1 = true
print neg2 = true
print neg127 = true
print neg128 = true
print neg255 = true
print neg256 = true
print neg1000 = true
print neg16383 = true
print neg16384 = true
print neg32767 = true
print neg32768 = true

print neg1 = false
print neg2 = false
print neg127 = false
print neg128 = false
print neg255 = false
print neg256 = false
print neg1000 = false
print neg16383 = false
print neg16384 = false
print neg32767 = false
print neg32768 = false

REM BIT = WORD comparisons (38 tests)
print true = w0
print true = w1
print true = w2
print true = w127
print true = w128
print true = w255
print true = w256
print true = w1000
print true = w16383
print true = w16384
print true = w32766
print true = w32767
print true = w32768
print true = w32769
print true = w49151
print true = w49152
print true = w65534
print true = w65535

print false = w0
print false = w1
print false = w2
print false = w127
print false = w128
print false = w255
print false = w256
print false = w1000
print false = w16383
print false = w16384
print false = w32766
print false = w32767
print false = w32768
print false = w32769
print false = w49151
print false = w49152
print false = w65534
print false = w65535

REM WORD = BIT comparisons (38 tests)
print w0 = true
print w1 = true
print w2 = true
print w127 = true
print w128 = true
print w255 = true
print w256 = true
print w1000 = true
print w16383 = true
print w16384 = true
print w32766 = true
print w32767 = true
print w32768 = true
print w32769 = true
print w49151 = true
print w49152 = true
print w65534 = true
print w65535 = true

print w0 = false
print w1 = false
print w2 = false
print w127 = false
print w128 = false
print w255 = false
print w256 = false
print w1000 = false
print w16383 = false
print w16384 = false
print w32766 = false
print w32767 = false
print w32768 = false
print w32769 = false
print w49151 = false
print w49152 = false
print w65534 = false
print w65535 = false

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

print pos0 = hello
print pos1 = hello
print neg1 = hello
print w0 = hello
print w1 = hello
print true = hello
print false = hello

print pos0 = empty
print pos1 = empty
print neg1 = empty
print w0 = empty
print w1 = empty
print true = empty
print false = empty

REM ===== Inequality Tests (<>) =====

REM INT <> WORD comparisons (57 tests)
print pos0 <> w0
print pos0 <> w1
print pos0 <> w32767
print pos0 <> w32768
print pos1 <> w0
print pos1 <> w1
print pos1 <> w32767
print pos1 <> w32768
print pos127 <> w127
print pos127 <> w128
print pos128 <> w127
print pos128 <> w128
print pos255 <> w255
print pos255 <> w256
print pos256 <> w255
print pos256 <> w256
print pos1000 <> w1000
print pos16383 <> w16383
print pos16383 <> w16384
print pos16384 <> w16383
print pos16384 <> w16384
print pos32766 <> w32766
print pos32766 <> w32767
print pos32766 <> w32768
print pos32767 <> w32766
print pos32767 <> w32767
print pos32767 <> w32768

print neg1 <> w0
print neg1 <> w1
print neg1 <> w32767
print neg1 <> w32768
print neg1 <> w65535
print neg2 <> w0
print neg2 <> w1
print neg2 <> w32767
print neg2 <> w32768
print neg127 <> w127
print neg127 <> w128
print neg128 <> w127
print neg128 <> w128
print neg255 <> w255
print neg255 <> w256
print neg256 <> w255
print neg256 <> w256
print neg1000 <> w1000
print neg16383 <> w16383
print neg16383 <> w16384
print neg16384 <> w16383
print neg16384 <> w16384
print neg32767 <> w32766
print neg32767 <> w32767
print neg32767 <> w32768
print neg32768 <> w0
print neg32768 <> w32766
print neg32768 <> w32767
print neg32768 <> w32768
print neg32768 <> w65535

REM WORD <> INT comparisons (57 tests)
print w0 <> pos0
print w1 <> pos0
print w32767 <> pos0
print w32768 <> pos0
print w0 <> pos1
print w1 <> pos1
print w32767 <> pos1
print w32768 <> pos1
print w127 <> pos127
print w128 <> pos127
print w127 <> pos128
print w128 <> pos128
print w255 <> pos255
print w256 <> pos255
print w255 <> pos256
print w256 <> pos256
print w1000 <> pos1000
print w16383 <> pos16383
print w16384 <> pos16383
print w16383 <> pos16384
print w16384 <> pos16384
print w32766 <> pos32766
print w32767 <> pos32766
print w32768 <> pos32766
print w32766 <> pos32767
print w32767 <> pos32767
print w32768 <> pos32767

print w0 <> neg1
print w1 <> neg1
print w32767 <> neg1
print w32768 <> neg1
print w65535 <> neg1
print w0 <> neg2
print w1 <> neg2
print w32767 <> neg2
print w32768 <> neg2
print w127 <> neg127
print w128 <> neg127
print w127 <> neg128
print w128 <> neg128
print w255 <> neg255
print w256 <> neg255
print w255 <> neg256
print w256 <> neg256
print w1000 <> neg1000
print w16383 <> neg16383
print w16384 <> neg16383
print w16383 <> neg16384
print w16384 <> neg16384
print w32766 <> neg32767
print w32767 <> neg32767
print w32768 <> neg32767
print w0 <> neg32768
print w32766 <> neg32768
print w32767 <> neg32768
print w32768 <> neg32768
print w65535 <> neg32768

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

REM BIT <> BIT comparisons (4 tests)
print true <> true
print true <> false
print false <> true
print false <> false

REM BIT <> INT comparisons (44 tests)
print true <> pos0
print true <> pos1
print true <> pos127
print true <> pos128
print true <> pos255
print true <> pos256
print true <> pos1000
print true <> pos16383
print true <> pos16384
print true <> pos32766
print true <> pos32767

print false <> pos0
print false <> pos1
print false <> pos127
print false <> pos128
print false <> pos255
print false <> pos256
print false <> pos1000
print false <> pos16383
print false <> pos16384
print false <> pos32766
print false <> pos32767

print true <> neg1
print true <> neg2
print true <> neg127
print true <> neg128
print true <> neg255
print true <> neg256
print true <> neg1000
print true <> neg16383
print true <> neg16384
print true <> neg32767
print true <> neg32768

print false <> neg1
print false <> neg2
print false <> neg127
print false <> neg128
print false <> neg255
print false <> neg256
print false <> neg1000
print false <> neg16383
print false <> neg16384
print false <> neg32767
print false <> neg32768

REM INT <> BIT comparisons (44 tests)
print pos0 <> true
print pos1 <> true
print pos127 <> true
print pos128 <> true
print pos255 <> true
print pos256 <> true
print pos1000 <> true
print pos16383 <> true
print pos16384 <> true
print pos32766 <> true
print pos32767 <> true

print pos0 <> false
print pos1 <> false
print pos127 <> false
print pos128 <> false
print pos255 <> false
print pos256 <> false
print pos1000 <> false
print pos16383 <> false
print pos16384 <> false
print pos32766 <> false
print pos32767 <> false

print neg1 <> true
print neg2 <> true
print neg127 <> true
print neg128 <> true
print neg255 <> true
print neg256 <> true
print neg1000 <> true
print neg16383 <> true
print neg16384 <> true
print neg32767 <> true
print neg32768 <> true

print neg1 <> false
print neg2 <> false
print neg127 <> false
print neg128 <> false
print neg255 <> false
print neg256 <> false
print neg1000 <> false
print neg16383 <> false
print neg16384 <> false
print neg32767 <> false
print neg32768 <> false

REM BIT <> WORD comparisons (38 tests)
print true <> w0
print true <> w1
print true <> w2
print true <> w127
print true <> w128
print true <> w255
print true <> w256
print true <> w1000
print true <> w16383
print true <> w16384
print true <> w32766
print true <> w32767
print true <> w32768
print true <> w32769
print true <> w49151
print true <> w49152
print true <> w65534
print true <> w65535

print false <> w0
print false <> w1
print false <> w2
print false <> w127
print false <> w128
print false <> w255
print false <> w256
print false <> w1000
print false <> w16383
print false <> w16384
print false <> w32766
print false <> w32767
print false <> w32768
print false <> w32769
print false <> w49151
print false <> w49152
print false <> w65534
print false <> w65535

REM WORD <> BIT comparisons (38 tests)
print w0 <> true
print w1 <> true
print w2 <> true
print w127 <> true
print w128 <> true
print w255 <> true
print w256 <> true
print w1000 <> true
print w16383 <> true
print w16384 <> true
print w32766 <> true
print w32767 <> true
print w32768 <> true
print w32769 <> true
print w49151 <> true
print w49152 <> true
print w65534 <> true
print w65535 <> true

print w0 <> false
print w1 <> false
print w2 <> false
print w127 <> false
print w128 <> false
print w255 <> false
print w256 <> false
print w1000 <> false
print w16383 <> false
print w16384 <> false
print w32766 <> false
print w32767 <> false
print w32768 <> false
print w32769 <> false
print w49151 <> false
print w49152 <> false
print w65534 <> false
print w65535 <> false

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

REM ===== Less Than Tests (<) =====

REM INT < WORD comparisons (57 tests)
print pos0 < w0
print pos0 < w1
print pos0 < w32767
print pos0 < w32768
print pos1 < w0
print pos1 < w1
print pos1 < w32767
print pos1 < w32768
print pos127 < w127
print pos127 < w128
print pos128 < w127
print pos128 < w128
print pos255 < w255
print pos255 < w256
print pos256 < w255
print pos256 < w256
print pos1000 < w1000
print pos16383 < w16383
print pos16383 < w16384
print pos16384 < w16383
print pos16384 < w16384
print pos32766 < w32766
print pos32766 < w32767
print pos32766 < w32768
print pos32767 < w32766
print pos32767 < w32767
print pos32767 < w32768

print neg1 < w0
print neg1 < w1
print neg1 < w32767
print neg1 < w32768
print neg1 < w65535
print neg2 < w0
print neg2 < w1
print neg2 < w32767
print neg2 < w32768
print neg127 < w127
print neg127 < w128
print neg128 < w127
print neg128 < w128
print neg255 < w255
print neg255 < w256
print neg256 < w255
print neg256 < w256
print neg1000 < w1000
print neg16383 < w16383
print neg16383 < w16384
print neg16384 < w16383
print neg16384 < w16384
print neg32767 < w32766
print neg32767 < w32767
print neg32767 < w32768
print neg32768 < w0
print neg32768 < w32766
print neg32768 < w32767
print neg32768 < w32768
print neg32768 < w65535

REM WORD < INT comparisons (57 tests)
print w0 < pos0
print w1 < pos0
print w32767 < pos0
print w32768 < pos0
print w0 < pos1
print w1 < pos1
print w32767 < pos1
print w32768 < pos1
print w127 < pos127
print w128 < pos127
print w127 < pos128
print w128 < pos128
print w255 < pos255
print w256 < pos255
print w255 < pos256
print w256 < pos256
print w1000 < pos1000
print w16383 < pos16383
print w16384 < pos16383
print w16383 < pos16384
print w16384 < pos16384
print w32766 < pos32766
print w32767 < pos32766
print w32768 < pos32766
print w32766 < pos32767
print w32767 < pos32767
print w32768 < pos32767

print w0 < neg1
print w1 < neg1
print w32767 < neg1
print w32768 < neg1
print w65535 < neg1
print w0 < neg2
print w1 < neg2
print w32767 < neg2
print w32768 < neg2
print w127 < neg127
print w128 < neg127
print w127 < neg128
print w128 < neg128
print w255 < neg255
print w256 < neg255
print w255 < neg256
print w256 < neg256
print w1000 < neg1000
print w16383 < neg16383
print w16384 < neg16383
print w16383 < neg16384
print w16384 < neg16384
print w32766 < neg32767
print w32767 < neg32767
print w32768 < neg32767
print w0 < neg32768
print w32766 < neg32768
print w32767 < neg32768
print w32768 < neg32768
print w65535 < neg32768

REM ===== Greater Than Tests (>) =====

REM INT > WORD comparisons (57 tests)
print pos0 > w0
print pos0 > w1
print pos0 > w32767
print pos0 > w32768
print pos1 > w0
print pos1 > w1
print pos1 > w32767
print pos1 > w32768
print pos127 > w127
print pos127 > w128
print pos128 > w127
print pos128 > w128
print pos255 > w255
print pos255 > w256
print pos256 > w255
print pos256 > w256
print pos1000 > w1000
print pos16383 > w16383
print pos16383 > w16384
print pos16384 > w16383
print pos16384 > w16384
print pos32766 > w32766
print pos32766 > w32767
print pos32766 > w32768
print pos32767 > w32766
print pos32767 > w32767
print pos32767 > w32768

print neg1 > w0
print neg1 > w1
print neg1 > w32767
print neg1 > w32768
print neg1 > w65535
print neg2 > w0
print neg2 > w1
print neg2 > w32767
print neg2 > w32768
print neg127 > w127
print neg127 > w128
print neg128 > w127
print neg128 > w128
print neg255 > w255
print neg255 > w256
print neg256 > w255
print neg256 > w256
print neg1000 > w1000
print neg16383 > w16383
print neg16383 > w16384
print neg16384 > w16383
print neg16384 > w16384
print neg32767 > w32766
print neg32767 > w32767
print neg32767 > w32768
print neg32768 > w0
print neg32768 > w32766
print neg32768 > w32767
print neg32768 > w32768
print neg32768 > w65535

REM WORD > INT comparisons (57 tests)
print w0 > pos0
print w1 > pos0
print w32767 > pos0
print w32768 > pos0
print w0 > pos1
print w1 > pos1
print w32767 > pos1
print w32768 > pos1
print w127 > pos127
print w128 > pos127
print w127 > pos128
print w128 > pos128
print w255 > pos255
print w256 > pos255
print w255 > pos256
print w256 > pos256
print w1000 > pos1000
print w16383 > pos16383
print w16384 > pos16383
print w16383 > pos16384
print w16384 > pos16384
print w32766 > pos32766
print w32767 > pos32766
print w32768 > pos32766
print w32766 > pos32767
print w32767 > pos32767
print w32768 > pos32767

print w0 > neg1
print w1 > neg1
print w32767 > neg1
print w32768 > neg1
print w65535 > neg1
print w0 > neg2
print w1 > neg2
print w32767 > neg2
print w32768 > neg2
print w127 > neg127
print w128 > neg127
print w127 > neg128
print w128 > neg128
print w255 > neg255
print w256 > neg255
print w255 > neg256
print w256 > neg256
print w1000 > neg1000
print w16383 > neg16383
print w16384 > neg16383
print w16383 > neg16384
print w16384 > neg16384
print w32766 > neg32767
print w32767 > neg32767
print w32768 > neg32767
print w0 > neg32768
print w32766 > neg32768
print w32767 > neg32768
print w32768 > neg32768
print w65535 > neg32768

REM ===== Less Than or Equal Tests (<=) =====

REM INT <= WORD comparisons (57 tests)
print pos0 <= w0
print pos0 <= w1
print pos0 <= w32767
print pos0 <= w32768
print pos1 <= w0
print pos1 <= w1
print pos1 <= w32767
print pos1 <= w32768
print pos127 <= w127
print pos127 <= w128
print pos128 <= w127
print pos128 <= w128
print pos255 <= w255
print pos255 <= w256
print pos256 <= w255
print pos256 <= w256
print pos1000 <= w1000
print pos16383 <= w16383
print pos16383 <= w16384
print pos16384 <= w16383
print pos16384 <= w16384
print pos32766 <= w32766
print pos32766 <= w32767
print pos32766 <= w32768
print pos32767 <= w32766
print pos32767 <= w32767
print pos32767 <= w32768

print neg1 <= w0
print neg1 <= w1
print neg1 <= w32767
print neg1 <= w32768
print neg1 <= w65535
print neg2 <= w0
print neg2 <= w1
print neg2 <= w32767
print neg2 <= w32768
print neg127 <= w127
print neg127 <= w128
print neg128 <= w127
print neg128 <= w128
print neg255 <= w255
print neg255 <= w256
print neg256 <= w255
print neg256 <= w256
print neg1000 <= w1000
print neg16383 <= w16383
print neg16383 <= w16384
print neg16384 <= w16383
print neg16384 <= w16384
print neg32767 <= w32766
print neg32767 <= w32767
print neg32767 <= w32768
print neg32768 <= w0
print neg32768 <= w32766
print neg32768 <= w32767
print neg32768 <= w32768
print neg32768 <= w65535

REM WORD <= INT comparisons (57 tests)
print w0 <= pos0
print w1 <= pos0
print w32767 <= pos0
print w32768 <= pos0
print w0 <= pos1
print w1 <= pos1
print w32767 <= pos1
print w32768 <= pos1
print w127 <= pos127
print w128 <= pos127
print w127 <= pos128
print w128 <= pos128
print w255 <= pos255
print w256 <= pos255
print w255 <= pos256
print w256 <= pos256
print w1000 <= pos1000
print w16383 <= pos16383
print w16384 <= pos16383
print w16383 <= pos16384
print w16384 <= pos16384
print w32766 <= pos32766
print w32767 <= pos32766
print w32768 <= pos32766
print w32766 <= pos32767
print w32767 <= pos32767
print w32768 <= pos32767

print w0 <= neg1
print w1 <= neg1
print w32767 <= neg1
print w32768 <= neg1
print w65535 <= neg1
print w0 <= neg2
print w1 <= neg2
print w32767 <= neg2
print w32768 <= neg2
print w127 <= neg127
print w128 <= neg127
print w127 <= neg128
print w128 <= neg128
print w255 <= neg255
print w256 <= neg255
print w255 <= neg256
print w256 <= neg256
print w1000 <= neg1000
print w16383 <= neg16383
print w16384 <= neg16383
print w16383 <= neg16384
print w16384 <= neg16384
print w32766 <= neg32767
print w32767 <= neg32767
print w32768 <= neg32767
print w0 <= neg32768
print w32766 <= neg32768
print w32767 <= neg32768
print w32768 <= neg32768
print w65535 <= neg32768

REM ===== Greater Than or Equal Tests (>=) =====

REM INT >= WORD comparisons (57 tests)
print pos0 >= w0
print pos0 >= w1
print pos0 >= w32767
print pos0 >= w32768
print pos1 >= w0
print pos1 >= w1
print pos1 >= w32767
print pos1 >= w32768
print pos127 >= w127
print pos127 >= w128
print pos128 >= w127
print pos128 >= w128
print pos255 >= w255
print pos255 >= w256
print pos256 >= w255
print pos256 >= w256
print pos1000 >= w1000
print pos16383 >= w16383
print pos16383 >= w16384
print pos16384 >= w16383
print pos16384 >= w16384
print pos32766 >= w32766
print pos32766 >= w32767
print pos32766 >= w32768
print pos32767 >= w32766
print pos32767 >= w32767
print pos32767 >= w32768

print neg1 >= w0
print neg1 >= w1
print neg1 >= w32767
print neg1 >= w32768
print neg1 >= w65535
print neg2 >= w0
print neg2 >= w1
print neg2 >= w32767
print neg2 >= w32768
print neg127 >= w127
print neg127 >= w128
print neg128 >= w127
print neg128 >= w128
print neg255 >= w255
print neg255 >= w256
print neg256 >= w255
print neg256 >= w256
print neg1000 >= w1000
print neg16383 >= w16383
print neg16383 >= w16384
print neg16384 >= w16383
print neg16384 >= w16384
print neg32767 >= w32766
print neg32767 >= w32767
print neg32767 >= w32768
print neg32768 >= w0
print neg32768 >= w32766
print neg32768 >= w32767
print neg32768 >= w32768
print neg32768 >= w65535

REM WORD >= INT comparisons (57 tests)
print w0 >= pos0
print w1 >= pos0
print w32767 >= pos0
print w32768 >= pos0
print w0 >= pos1
print w1 >= pos1
print w32767 >= pos1
print w32768 >= pos1
print w127 >= pos127
print w128 >= pos127
print w127 >= pos128
print w128 >= pos128
print w255 >= pos255
print w256 >= pos255
print w255 >= pos256
print w256 >= pos256
print w1000 >= pos1000
print w16383 >= pos16383
print w16384 >= pos16383
print w16383 >= pos16384
print w16384 >= pos16384
print w32766 >= pos32766
print w32767 >= pos32766
print w32768 >= pos32766
print w32766 >= pos32767
print w32767 >= pos32767
print w32768 >= pos32767

print w0 >= neg1
print w1 >= neg1
print w32767 >= neg1
print w32768 >= neg1
print w65535 >= neg1
print w0 >= neg2
print w1 >= neg2
print w32767 >= neg2
print w32768 >= neg2
print w127 >= neg127
print w128 >= neg127
print w127 >= neg128
print w128 >= neg128
print w255 >= neg255
print w256 >= neg255
print w255 >= neg256
print w256 >= neg256
print w1000 >= neg1000
print w16383 >= neg16383
print w16384 >= neg16383
print w16383 >= neg16384
print w16384 >= neg16384
print w32766 >= neg32767
print w32767 >= neg32767
print w32768 >= neg32767
print w0 >= neg32768
print w32766 >= neg32768
print w32767 >= neg32768
print w32768 >= neg32768
print w65535 >= neg32768

REM ===== BIT Comparison Tests =====

bit b0 = false
bit b1 = true

REM BIT = BIT (4 tests)
print b0 = b0
print b0 = b1
print b1 = b0
print b1 = b1

REM BIT <> BIT (4 tests)
print b0 <> b0
print b0 <> b1
print b1 <> b0
print b1 <> b1

REM BIT < BIT (2 tests)
print b0 < b1
print b1 < b0

REM BIT > BIT (2 tests)
print b0 > b1
print b1 > b0

REM BIT <= BIT (2 tests)
print b0 <= b1
print b1 <= b0

REM BIT >= BIT (2 tests)
print b0 >= b1
print b1 >= b0

REM BIT = INT (5 tests)
print b0 = pos0
print b0 = pos1
print b1 = pos0
print b1 = pos1
print b0 = neg1

REM BIT <> INT (5 tests)
print b0 <> pos0
print b0 <> pos1
print b1 <> pos0
print b1 <> pos1
print b0 <> neg1

REM BIT < INT (6 tests)
print b0 < pos0
print b0 < pos1
print b1 < pos0
print b1 < pos1
print b0 < neg1
print b1 < neg1

REM BIT > INT (6 tests)
print b0 > pos0
print b0 > pos1
print b1 > pos0
print b1 > pos1
print b0 > neg1
print b1 > neg1

REM BIT <= INT (4 tests)
print b0 <= pos0
print b0 <= pos1
print b1 <= pos0
print b1 <= pos1

REM BIT >= INT (4 tests)
print b0 >= pos0
print b0 >= pos1
print b1 >= pos0
print b1 >= pos1

REM BIT = WORD (5 tests)
print b0 = w0
print b0 = w1
print b1 = w0
print b1 = w1
print b0 = w32768

REM BIT <> WORD (5 tests)
print b0 <> w0
print b0 <> w1
print b1 <> w0
print b1 <> w1
print b0 <> w32768

REM BIT < WORD (5 tests)
print b0 < w0
print b0 < w1
print b1 < w0
print b1 < w1
print b0 < w32768

REM BIT > WORD (5 tests)
print b0 > w0
print b0 > w1
print b1 > w0
print b1 > w1
print b0 > w32768

REM BIT <= WORD (4 tests)
print b0 <= w0
print b0 <= w1
print b1 <= w0
print b1 <= w1

REM BIT >= WORD (4 tests)
print b0 >= w0
print b0 >= w1
print b1 >= w0
print b1 >= w1

REM INT = BIT (3 tests)
print pos0 = b0
print pos1 = b1
print neg1 = b0

REM INT <> BIT (3 tests)
print pos0 <> b0
print pos1 <> b1
print neg1 <> b0

REM INT < BIT (3 tests)
print pos0 < b0
print pos1 < b1
print neg1 < b0

REM INT > BIT (3 tests)
print pos0 > b0
print pos1 > b1
print neg1 > b0

REM WORD = BIT (3 tests)
print w0 = b0
print w1 = b1
print w32768 = b0

REM WORD <> BIT (3 tests)
print w0 <> b0
print w1 <> b1
print w32768 <> b0

REM WORD < BIT (3 tests)
print w0 < b0
print w1 < b1
print w32768 < b0

REM WORD > BIT (3 tests)
print w0 > b0
print w1 > b1
print w32768 > b0

REM ===== TYPE MISMATCH Tests (Should generate errors) =====

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

