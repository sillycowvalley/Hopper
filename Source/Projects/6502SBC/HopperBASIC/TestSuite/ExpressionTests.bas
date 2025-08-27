! Hopper BASIC Expression Grammar Test Suite v2.0
! Tests CORRECTED operator precedence and arithmetic correctness
! Focus on numeric operations, systematic grammar coverage
! UPDATED: Tests now reflect corrected precedence: * / MOD > & > | > + -

CLS
NEW
MEM

! ===== LEVEL 1: Primary Expressions =====
FUNC TestPrimary()
    PRINT "=== Primary Expressions ==="
    PRINT "Literals:"
    PRINT 42; " ! expect 42"
    PRINT -17; " ! expect -17" 
    PRINT 0; " ! expect 0"
    
    PRINT "Parentheses:"
    PRINT (42); " ! expect 42"
    PRINT (-17); " ! expect -17"
    PRINT (5 + 3); " ! expect 8"
    PRINT
ENDFUNC

! ===== LEVEL 2: Unary Expressions =====
FUNC TestUnary()
    PRINT "=== Unary Operators ==="
    PRINT "Negation:"
    PRINT -42; " ! expect -42"
    PRINT -(-17); " ! expect 17"
    PRINT -(5 + 3); " ! expect -8"
    PRINT
ENDFUNC

! ===== LEVEL 2B: Bitwise Complement =====
FUNC TestBitwiseComplement()
    PRINT "=== Bitwise Complement: ~ ==="
    PRINT "Basic complement:"
    PRINT ~0; " ! expect -1 (all bits flipped)"
    PRINT ~(-1); " ! expect 0 (all bits flipped)"
    PRINT ~15; " ! expect -16 (flip 0x0F)"
    PRINT ~255; " ! expect -256 (flip 0xFF)"
    
    PRINT "Precedence (~ is unary, high precedence):"
    PRINT ~5 + 1; " ! expect -5 ((~5) + 1 = -6 + 1)"
    PRINT ~(5 + 1); " ! expect -7 (~6 = -7)"
    PRINT 10 + ~3; " ! expect 6 (10 + (~3) = 10 + (-4))"
    PRINT
ENDFUNC

! ===== LEVEL 3: Multiplicative Expressions =====
FUNC TestMultiplicative()
    PRINT "=== Multiplicative: * / MOD ==="
    
    PRINT "Multiplication:"
    PRINT 6 * 7; " ! expect 42"
    PRINT 10 * (-3); " ! expect -30"
    PRINT (-5) * (-4); " ! expect 20"
    PRINT 0 * 999; " ! expect 0"
    
    PRINT "Division:"
    PRINT 42 / 6; " ! expect 7"
    PRINT 100 / (-10); " ! expect -10"
    PRINT (-50) / (-5); " ! expect 10"
    PRINT 7 / 3; " ! expect 2"
    
    PRINT "Modulo:"
    PRINT 17 MOD 5; " ! expect 2"
    PRINT 100 MOD 7; " ! expect 2"
    PRINT 10 MOD 3; " ! expect 1"
    PRINT 20 MOD 4; " ! expect 0"
    
    PRINT "Left-to-right associativity:"
    PRINT 24 / 4 / 2; " ! expect 3 (not 12)"
    PRINT 2 * 3 * 4; " ! expect 24"
    PRINT 100 / 5 * 2; " ! expect 40 (not 10)"
    PRINT
ENDFUNC

! ===== LEVEL 4: Bitwise AND - CORRECTED LEVEL =====
FUNC TestBitwiseAnd()
    PRINT "=== Bitwise AND: & (Level 4) ==="
    
    PRINT "Basic bitwise AND:"
    PRINT 15 & 7; " ! expect 7 (1111 & 0111)"
    PRINT 12 & 10; " ! expect 8 (1100 & 1010)"
    PRINT 255 & 15; " ! expect 15"
    PRINT 0 & 999; " ! expect 0"
    
    PRINT "CORRECTED precedence (& binds tighter than +):"
    PRINT 2 + 3 & 4; " ! expect 2 (2+(3&4) = 2+0)"
    PRINT 15 & 7 + 1; " ! expect 8 ((15&7)+1 = 7+1)"
    PRINT 4 + 8 & 12; " ! expect 12 (4+(8&12) = 4+8)"
    
    PRINT "& precedence vs multiplication (& after *):"
    PRINT 2 * 3 & 5; " ! expect 4 ((2*3)&5 = 6&5)"
    PRINT 4 * 2 & 7; " ! expect 0 ((4*2)&7 = 8&7)"
    PRINT
ENDFUNC

! ===== LEVEL 5: Bitwise OR - CORRECTED LEVEL =====
FUNC TestBitwiseOr()
    PRINT "=== Bitwise OR: | (Level 5) ==="
    
    PRINT "Basic bitwise OR:"
    PRINT 8 | 4; " ! expect 12 (1000 | 0100)"
    PRINT 15 | 240; " ! expect 255"
    PRINT 0 | 42; " ! expect 42"
    PRINT 7 | 0; " ! expect 7"
    
    PRINT "CORRECTED precedence (| binds tighter than +):"
    PRINT 8 | 1 + 2; " ! expect 11 ((8|1)+2 = 9+2)"
    PRINT 4 + 8 | 1; " ! expect 13 ((4+8)|1 = 12|1)"
    PRINT 1 + 2 | 4; " ! expect 7 ((1+2)|4 = 3|4)"
    
    PRINT "| precedence vs & (| after &):"
    PRINT 8 & 4 | 2; " ! expect 2 ((8&4)|2 = 0|2)"
    PRINT 15 & 7 | 8; " ! expect 15 ((15&7)|8 = 7|8)"
    PRINT
ENDFUNC

! ===== LEVEL 6: Additive Expressions - MOVED TO CORRECT LEVEL =====
FUNC TestAdditive()
    PRINT "=== Additive: + - (Level 6) ==="
    
    PRINT "Addition:"
    PRINT 25 + 17; " ! expect 42"
    PRINT 10 + (-3); " ! expect 7"
    PRINT (-15) + (-5); " ! expect -20"
    PRINT 0 + 42; " ! expect 42"
    
    PRINT "Subtraction:"
    PRINT 50 - 8; " ! expect 42"
    PRINT 10 - (-5); " ! expect 15"
    PRINT (-20) - (-30); " ! expect 10"
    PRINT 0 - 17; " ! expect -17"
    
    PRINT "Left-to-right associativity:"
    PRINT 100 - 30 - 20; " ! expect 50 (not 90)"
    PRINT 10 + 5 - 3; " ! expect 12"
    PRINT 1 + 2 + 3 + 4; " ! expect 10"
    
    PRINT "NOW LOWER precedence than bitwise:"
    PRINT 2 + 3 & 4; " ! expect 2 (2+(3&4))"
    PRINT 8 | 1 + 2; " ! expect 11 ((8|1)+2)"
    PRINT
ENDFUNC

! ===== LEVEL 7: Comparison Operators =====
FUNC TestComparisons()
    PRINT "=== Comparisons (Level 7) ==="
    
    PRINT "Equality:"
    PRINT 42 = 42; " ! expect TRUE"
    PRINT 42 = 43; " ! expect FALSE"
    PRINT (-5) = (-5); " ! expect TRUE"
    
    PRINT "Inequality:"
    PRINT 42 <> 43; " ! expect TRUE"  
    PRINT 42 <> 42; " ! expect FALSE"
    
    PRINT "Ordering:"
    PRINT 10 < 20; " ! expect TRUE"
    PRINT 20 < 10; " ! expect FALSE"
    PRINT 15 > 10; " ! expect TRUE"
    PRINT 10 > 15; " ! expect FALSE"
    PRINT 10 <= 10; " ! expect TRUE"
    PRINT 10 <= 15; " ! expect TRUE"
    PRINT 15 >= 10; " ! expect TRUE"
    PRINT 10 >= 10; " ! expect TRUE"
    
    PRINT "Precedence (comparison after arithmetic and bitwise):"
    PRINT 5 + 5 = 10; " ! expect TRUE"
    PRINT 3 * 4 > 10; " ! expect TRUE"
    PRINT 20 / 4 <= 5; " ! expect TRUE"
    PRINT 8 & 4 = 0; " ! expect TRUE"
    PRINT 8 | 4 > 10; " ! expect TRUE"
    PRINT
ENDFUNC

! ===== COMPREHENSIVE PRECEDENCE VALIDATION =====
FUNC TestCorrectedPrecedence()
    PRINT "=== CORRECTED Precedence Validation ==="
    
    PRINT "Key test cases with CORRECTED precedence:"
    
    PRINT "2 + 3 & 4:"
    PRINT 2 + 3 & 4; " ! expect 2 (2+(3&4) = 2+0)"
    
    PRINT "8 | 1 + 2:"  
    PRINT 8 | 1 + 2; " ! expect 11 ((8|1)+2 = 9+2)"
    
    PRINT "15 & 7 + 1:"
    PRINT 15 & 7 + 1; " ! expect 8 ((15&7)+1 = 7+1)"
    
    PRINT "4 * 2 & 5 + 1:"
    PRINT 4 * 2 & 5 + 1; " ! expect 1 (((4*2)&5)+1 = (8&5)+1 = 0+1)"
    
    PRINT "3 & 4 | 1 + 2:"
    PRINT 3 & 4 | 1 + 2; " ! expect 3 (((3&4)|1)+2 = (0|1)+2 = 1+2)"
    
    PRINT "Full chain: 2 * 3 & 5 | 1 + 4:"
    PRINT 2 * 3 & 5 | 1 + 4; " ! expect 9 ((((2*3)&5)|1)+4 = ((6&5)|1)+4 = (4|1)+4 = 5+4)"
    PRINT
ENDFUNC

! ===== BITWISE OPERATION ISOLATION =====
FUNC TestBitwiseIsolation()
    PRINT "=== Bitwise Operation Isolation ==="
    
    PRINT "Pure bitwise (no arithmetic confusion):"
    PRINT 15 & 7; " ! expect 7"
    PRINT 8 | 4; " ! expect 12"
    PRINT 15 & 12 | 3; " ! expect 15 ((15&12)|3 = 12|3)"
    PRINT 8 | 4 & 2; " ! expect 8 (8|(4&2) = 8|0)"
    
    PRINT "Bitwise with multiplication (precedence * > &,|):"
    PRINT 2 * 4 & 15; " ! expect 8 ((2*4)&15 = 8&15)"
    PRINT 3 * 5 | 7; " ! expect 15 ((3*5)|7 = 15|7)"
    PRINT 4 / 2 & 3; " ! expect 2 ((4/2)&3 = 2&3)"
    
    PRINT "Bitwise complement with arithmetic:"
    PRINT ~5 + 1; " ! expect -5 ((~5)+1 = -6+1)"
    PRINT ~(5 + 1); " ! expect -7 (~6 = -7)"
    PRINT 2 * ~3; " ! expect -8 (2*(~3) = 2*(-4))"
    PRINT
ENDFUNC

! ===== ERROR CASES FOR OLD PRECEDENCE =====
FUNC TestOldPrecedenceErrors()
    PRINT "=== Old Precedence Would Be Wrong ==="
    
    PRINT "If + bound tighter than & (OLD/WRONG way):"
    PRINT "2 + 3 & 4 would give "; (2 + 3) & 4; " (= 4)"
    PRINT "But with CORRECTED precedence it gives "; 2 + (3 & 4); " (= 2)"
    
    PRINT "If + bound tighter than | (OLD/WRONG way):"
    PRINT "8 | 1 + 2 would give "; 8 | (1 + 2); " (= 11)"
    PRINT "But with CORRECTED precedence it gives "; (8 | 1) + 2; " (= 11)"
    PRINT "^^ Note: Same result by coincidence, but different evaluation!"
    
    PRINT "Where difference is clear:"
    PRINT "15 & 7 + 1 OLD way: "; 15 & (7 + 1); " (= 0)"
    PRINT "15 & 7 + 1 NEW way: "; (15 & 7) + 1; " (= 8)"
    PRINT
ENDFUNC

! ===== COMPREHENSIVE PRECEDENCE CHAIN =====
FUNC TestFullPrecedenceChain()
    PRINT "=== Full Precedence Chain ==="
    
    PRINT "Complete chain: () > unary > * / MOD > & > | > + - > compare:"
    
    PRINT "Unary and multiplicative:"
    PRINT -2 * 3; " ! expect -6 ((-2)*3)"
    PRINT -(2 * 3); " ! expect -6 (same result, different parse)"
    PRINT 2 * -3; " ! expect -6 (2*(-3))"
    
    PRINT "Multiplicative and bitwise:"
    PRINT 3 * 4 & 15; " ! expect 12 ((3*4)&15 = 12&15)"
    PRINT 2 * 8 | 1; " ! expect 17 ((2*8)|1 = 16|1)"
    
    PRINT "Bitwise and additive:"
    PRINT 8 & 4 + 2; " ! expect 2 ((8&4)+2 = 0+2)"
    PRINT 5 + 3 & 7; " ! expect 8 (5+(3&7) = 5+3)"
    PRINT 1 | 2 + 4; " ! expect 7 ((1|2)+4 = 3+4)"
    
    PRINT "Full complexity:"
    PRINT 2 + 3 * 4 & 15 | 1; " ! expect 13 ((2+(((3*4)&15)|1)) = 2+((12&15)|1) = 2+(12|1) = 2+13 = 15)"
    PRINT
ENDFUNC

! ===== PRACTICAL HARDWARE PROGRAMMING EXAMPLES =====
FUNC TestHardwareProgramming()
    PRINT "=== Hardware Programming Examples ==="
    
    PRINT "Typical bit manipulation (why & and | need high precedence):"
    VAR reg = 170  ! Binary: 10101010
    VAR mask = 15  ! Binary: 00001111
    
    PRINT "Read lower nibble: reg & mask ="
    PRINT reg & mask; " ! expect 10 (170 & 15)"
    
    PRINT "Set bit 4: reg | 16 ="
    PRINT reg | 16; " ! expect 186 (170 | 16)"
    
    PRINT "Clear bit 1: reg & ~2 ="  
    PRINT reg & ~2; " ! expect 168 (170 & ~2 = 170 & 253)"
    
    PRINT "Complex: set bits then add offset:"
    PRINT reg | 5 + 10; " ! expect 185 ((170|5)+10 = 175+10)"
    PRINT "(reg | 5) + 10 same as above: "; (reg | 5) + 10
    
    PRINT "vs old precedence would be:"
    PRINT "reg | (5 + 10) = "; reg | (5 + 10); " (= 175)"
    PRINT
ENDFUNC

! ===== COMPARISON OPERATORS =====
FUNC TestComparisons()
    PRINT "=== Comparisons (Level 7) ==="
    
    PRINT "Basic comparisons:"
    PRINT 42 = 42; " ! expect TRUE"
    PRINT 42 <> 43; " ! expect TRUE"
    PRINT 10 < 20; " ! expect TRUE"
    PRINT 15 > 10; " ! expect TRUE"
    PRINT 10 <= 10; " ! expect TRUE"
    PRINT 15 >= 10; " ! expect TRUE"
    
    PRINT "Comparisons with expressions:"
    PRINT 5 + 5 = 10; " ! expect TRUE"
    PRINT 3 * 4 > 10; " ! expect TRUE"
    PRINT 8 & 4 = 0; " ! expect TRUE"
    PRINT 8 | 4 > 10; " ! expect TRUE"
    PRINT 2 + 3 * 4 > 13; " ! expect TRUE (14 > 13)"
    PRINT
ENDFUNC

! ===== VALIDATION AGAINST SPECIFICATION =====
FUNC TestSpecCompliance()
    PRINT "=== Specification Compliance ==="
    
    PRINT "Precedence levels from spec:"
    PRINT "1. () [] functions - working"
    PRINT "2. unary -, ~, NOT - working"  
    PRINT "3. *, /, MOD - working"
    PRINT "4. & (bitwise AND) - CORRECTED"
    PRINT "5. | (bitwise OR) - CORRECTED"
    PRINT "6. +, - (additive) - CORRECTED" 
    PRINT "7. comparisons - working"
    PRINT "8. logical AND - (BIT operands)"
    PRINT "9. logical OR - (BIT operands)"
    
    PRINT "Critical fix verification:"
    PRINT "Expression: 2 + 3 & 4 | 1"
    VAR complex = 2 + 3 & 4 | 1
    PRINT "Result: "; complex; " ! expect 3"
    PRINT "Parse: 2 + ((3 & 4) | 1) = 2 + (0 | 1) = 2 + 1 = 3"
    PRINT
ENDFUNC

! ===== MAIN TEST RUNNER =====
BEGIN
    PRINT "Hopper BASIC Expression Test Suite v2.0"
    PRINT "Testing CORRECTED operator precedence"
    PRINT "Precedence: () > unary(-,~) > *,/,MOD > & > | > +,- > comparisons"
    PRINT "KEY FIX: Bitwise operators now bind tighter than arithmetic"
    PRINT "=================================================="
    PRINT
    
    TestPrimary()
    TestUnary()
    TestBitwiseComplement()
    TestMultiplicative()
    TestBitwiseAnd()
    TestBitwiseOr()
    TestAdditive() 
    TestComparisons()
    TestCorrectedPrecedence()
    TestFullPrecedenceChain()
    TestHardwareProgramming()
    TestOldPrecedenceErrors()
    TestSpecCompliance()
    
    PRINT "=================================================="
    PRINT "=== Test Suite Complete ==="
    PRINT "All arithmetic/bitwise precedence should now be correct"
    PRINT "Bitwise operators (&,|) bind tighter than arithmetic (+,-)"
    PRINT "This matches assembly language expectations for hardware programming"
END

RUN

NEW
MEM
