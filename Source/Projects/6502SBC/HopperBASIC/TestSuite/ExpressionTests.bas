! Hopper BASIC Expression Grammar Test Suite
! Tests operator precedence and arithmetic correctness
! Focus on numeric operations, systematic grammar coverage
! CORRECTED: Comments now reflect actual precedence rules

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

! ===== LEVEL 2B: Bitwise Complement (NEW) =====
FUNC TestBitwiseComplement()
    PRINT "=== Bitwise Complement: ~ ==="
    PRINT "Basic complement:"
    PRINT ~0; " ! expect -1 (all bits flipped)"
    PRINT ~(-1); " ! expect 0 (all bits flipped)"
    PRINT ~15; " ! expect -16 (flip 0x0F to 0xFFF0)"
    PRINT ~255; " ! expect -256 (flip 0xFF to 0xFF00)"
    
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

! ===== LEVEL 4: Bitwise AND (CORRECTED LEVEL) =====
FUNC TestBitwiseAnd()
    PRINT "=== Bitwise AND: & ==="
    
    PRINT "Basic bitwise AND:"
    PRINT 15 & 7; " ! expect 7 (1111 & 0111)"
    PRINT 12 & 10; " ! expect 8 (1100 & 1010)"
    PRINT 255 & 15; " ! expect 15"
    PRINT 0 & 999; " ! expect 0"
    
    PRINT "Precedence (& after * but before +):"
    PRINT 4 + 8 & 12; " ! expect 12 (4 + (8&12) = 4+8)"
    PRINT 15 & 7 + 1; " ! expect 8 ((15&7) + 1 = 7+1)"
    PRINT 2 * 3 & 5; " ! expect 6 ((2*3) & 5 = 6&5 = 4)"
    PRINT
ENDFUNC

! ===== LEVEL 5: Bitwise OR (CORRECTED LEVEL) =====
FUNC TestBitwiseOr()
    PRINT "=== Bitwise OR: | ==="
    
    PRINT "Basic bitwise OR:"
    PRINT 8 | 4; " ! expect 12 (1000 | 0100)"
    PRINT 15 | 240; " ! expect 255"
    PRINT 0 | 42; " ! expect 42"
    PRINT 7 | 0; " ! expect 7"
    
    PRINT "Precedence (| after & but before +):"
    PRINT 8 | 4 & 12; " ! expect 12 (8 | (4&12) = 8|4)"
    PRINT 15 & 7 | 8; " ! expect 15 ((15&7) | 8 = 7|8)"
    PRINT 1 + 2 | 4; " ! expect 7 ((1+2) | 4 = 3|4)"
    PRINT
ENDFUNC

! ===== LEVEL 6: Additive Expressions (MOVED DOWN) =====
FUNC TestAdditive()
    PRINT "=== Additive: + - ==="
    
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
    PRINT
ENDFUNC

! ===== LEVEL 7: Arithmetic Precedence (UPDATED) =====
FUNC TestArithmeticPrecedence()
    PRINT "=== Arithmetic Precedence ==="
    
    PRINT "Multiplication before addition:"
    PRINT 2 + 3 * 4; " ! expect 14 (not 20)"
    PRINT 5 * 2 + 1; " ! expect 11 (not 15)"
    PRINT 1 + 2 * 3 + 4; " ! expect 11"
    
    PRINT "Division before subtraction:"
    PRINT 20 - 12 / 3; " ! expect 16 (not 2)"
    PRINT 100 / 5 - 10; " ! expect 10 (not 2)"
    
    PRINT "Mixed operations:"
    PRINT 2 + 3 * 4 - 5; " ! expect 9"
    PRINT 10 - 6 / 2 + 1; " ! expect 8"
    PRINT 3 * 4 + 5 * 2; " ! expect 22"
    
    PRINT "Parentheses override:"
    PRINT (2 + 3) * 4; " ! expect 20 (not 14)"
    PRINT 2 + (3 * 4); " ! expect 14"
    PRINT (10 - 6) / 2; " ! expect 2 (not 7)"
    PRINT
ENDFUNC

! ===== LEVEL 8: Comparison Operators =====
FUNC TestComparisons()
    PRINT "=== Comparisons ==="
    
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
    
    PRINT "Precedence (comparison after arithmetic):"
    PRINT 5 + 5 = 10; " ! expect TRUE"
    PRINT 3 * 4 > 10; " ! expect TRUE"
    PRINT 20 / 4 <= 5; " ! expect TRUE"
    PRINT
ENDFUNC

! ===== LEVEL 9: Complex Precedence (CORRECTED) =====
FUNC TestComplexPrecedence()
    PRINT "=== Complex Precedence ==="
    
    PRINT "Full precedence chain (* > & > | > +):"
    PRINT 2 + 3 * 4 & 15; " ! expect 14 (2+((3*4)&15) = 2+(12&15) = 2+12)"
    PRINT 1 | 2 & 4 + 8; " ! expect 13 (1|(2&(4+8)) = 1|(2&12) = 1|0 = 1)"
    PRINT 8 & 4 + 2; " ! expect 6 ((8&4)+2 = 0+2 = 2)"
    
    PRINT "Nested parentheses:"
    PRINT ((2 + 3) * 4) - 5; " ! expect 15"
    PRINT 2 + (3 * (4 - 1)); " ! expect 11"
    PRINT (10 - (6 / 2)) * 3; " ! expect 21"
    
    PRINT "Complex expressions:"
    PRINT 5 * 3 + 2 * 4; " ! expect 23"
    PRINT (5 + 3) * (2 + 4); " ! expect 48"
    PRINT 100 / (5 * 2) + 10; " ! expect 20"
    PRINT 2 * 3 + 4 * 5 - 6; " ! expect 20"
    PRINT
ENDFUNC

! ===== LEVEL 10: Comprehensive Bitwise Tests (NEW) =====
FUNC TestBitwiseComprehensive()
    PRINT "=== Comprehensive Bitwise Tests ==="
    
    PRINT "Mixed bitwise operations:"
    PRINT 15 & 12 | 3; " ! expect 15 ((15&12)|3 = 12|3)"
    PRINT 8 | 4 & 2; " ! expect 8 (8|(4&2) = 8|0)"
    PRINT ~7 & 15; " ! expect 8 ((~7)&15 = -8&15)"
    
    PRINT "Bitwise with arithmetic:"
    PRINT 10 + 5 & 12; " ! expect 12 ((10+5)&12 = 15&12)"
    PRINT 20 - 4 | 3; " ! expect 19 ((20-4)|3 = 16|3)"
    PRINT 6 * 2 & 8; " ! expect 8 ((6*2)&8 = 12&8)"
    
    PRINT "Complex bitwise chains:"
    PRINT 1 | 2 & 4 | 8; " ! expect 9 (1|(2&4)|8 = 1|0|8)"
    PRINT 15 & 7 | 8 & 12; " ! expect 15 ((15&7)|(8&12) = 7|8)"
    PRINT
ENDFUNC

! ===== LEVEL 11: State Consistency Tests =====
FUNC TestStateConsistency()
    PRINT "=== State Consistency ==="
    
    PRINT "Repeated operations:"
    PRINT 10 * 10; " ! expect 100"
    PRINT 10 * 10; " ! expect 100" 
    PRINT 10 * 10; " ! expect 100"
    
    PRINT "Mixed in same expression:"
    VAR x = 5
    PRINT x * x + x * x; " ! expect 50"
    PRINT x + x * x - x; " ! expect 25"
    
    PRINT "Cross-function consistency:"
    PRINT TestHelper(10); " ! expect 100"
    PRINT 10 * 10; " ! expect 100"
    PRINT
ENDFUNC

FUNC TestHelper(n)
    RETURN n * n
ENDFUNC

! ===== MAIN TEST RUNNER =====
BEGIN
    PRINT "Hopper BASIC Expression Grammar Test Suite v2"
    PRINT "Testing numeric operators with CORRECTED precedence"
    PRINT "Precedence: () > unary(-,~) > *,/,MOD > &,| > +,- > comparisons"
    PRINT
    
    TestPrimary()
    TestUnary()
    TestBitwiseComplement()
    TestMultiplicative()
    TestBitwiseAnd()
    TestBitwiseOr()
    TestAdditive() 
    TestArithmeticPrecedence()
    TestComparisons()
    TestComplexPrecedence()
    TestBitwiseComprehensive()
    TestStateConsistency()
    
    PRINT "=== Test Suite Complete ==="
    PRINT "All outputs should match expected values"
    PRINT "If ~ operator not implemented, those tests will fail"
END

RUN

NEW
MEM
