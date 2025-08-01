REM Failing Comparison Tests for HopperBASIC
REM These tests identify actual implementation bugs vs. expected missing features
REM Run these tests to verify fixes and track implementation progress

REM =================================================================
REM ACTUAL IMPLEMENTATION BUGS (Priority: HIGH)
REM =================================================================

REM BIT Expression Assignment Syntax Bug
REM Expected: Should work according to spec
REM Actual: SYNTAX ERROR
bit actualBug1 = (1 = 1)
bit actualBug2 = (1 = 0)
bit actualBug3 = (pos1 = pos1)
bit actualBug4 = (w0 = w0)

REM Test variables needed for expression tests
int pos1 = 1
word w0 = 0

REM =================================================================
REM MISSING FEATURES - PHASE 2 (Priority: MEDIUM - Expected)
REM =================================================================

REM STRING Type Not Implemented (Expected - Phase 2)
string empty = ""
string hello = "hello"
string world = "world"
string hello2 = "hello" 
string longer = "this is a longer string"
string space = " "
string number = "123"
string special = "!@#$%"

REM STRING = STRING comparisons (would work when implemented)
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

REM STRING = literal comparisons (would work when implemented)
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

REM STRING <> STRING comparisons (would work when implemented)
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

REM STRING <> literal comparisons (would work when implemented)
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

REM Mixed type (cross-type) comparisons with STRING (would fail with TYPE MISMATCH when implemented)
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

REM =================================================================
REM NOT ACTUAL FAILURES - CORRECT BEHAVIOR BY DESIGN
REM =================================================================

REM The following tests were included in the original failing analysis
REM but are actually CORRECT behavior according to the updated spec.
REM These should generate TYPE MISMATCH errors by design:

REM BIT vs INT comparisons (TYPE MISMATCH by design - correct behavior)
REM print true = pos0        ' Should error: TYPE MISMATCH ✓
REM print false = pos1       ' Should error: TYPE MISMATCH ✓
REM print pos0 = true        ' Should error: TYPE MISMATCH ✓

REM BIT vs WORD comparisons (TYPE MISMATCH by design - correct behavior)  
REM print true = w0          ' Should error: TYPE MISMATCH ✓
REM print false = w1         ' Should error: TYPE MISMATCH ✓
REM print w0 = true          ' Should error: TYPE MISMATCH ✓

REM BIT ordering operations (TYPE MISMATCH by design - correct behavior)
REM print true < false       ' Should error: TYPE MISMATCH ✓
REM print false > true       ' Should error: TYPE MISMATCH ✓
REM print true <= false      ' Should error: TYPE MISMATCH ✓

REM =================================================================
REM SUMMARY
REM =================================================================

REM Total Failing Tests: ~175
REM - Actual Bugs: 4 tests (BIT expression assignment syntax)
REM - Missing Features: ~171 tests (STRING support - Phase 2)
REM - Correct Rejections: ~300+ tests (TYPE MISMATCH by design)

REM Fix Priority:
REM 1. HIGH: Fix BIT expression assignment syntax (4 tests)
REM 2. MEDIUM: Implement STRING support (171 tests) 
REM 3. VERIFICATION: Ensure TYPE MISMATCH behavior remains correct

REM Success Rate After STRING Implementation: 
REM Expected ~99.6% (1044/1048 tests working correctly)



