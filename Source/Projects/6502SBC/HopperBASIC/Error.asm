unit Errors
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    // Error message strings (moved from Messages.asm)
    const string SyntaxErrorMsg = "SYNTAX ERROR";
    const string TypeMismatchMsg = "TYPE MISMATCH";
    const string NotImplementedMsg = "NOT IMPLEMENTED";
    const string InternalErrorMsg = "INTERNAL ERROR";
    const string FunctionExistsMsg = "FUNCTION EXISTS";
    const string ConstantExistsMsg = "CONSTANT EXISTS";
    const string VariableExistsMsg = "VARIABLE EXISTS";
    const string OutOfMemoryMsg = "OUT OF MEMORY";
    const string DivisionByZeroMsg = "DIVISION BY ZERO";
    const string NumericOverflowMsg = "NUMERIC OVERFLOW";
    const string UndefinedIdentifierMsg = "UNDEFINED IDENTIFIER";
    const string IllegalAssignmentMsg = "ILLEGAL ASSIGNMENT";
    const string InvalidOperatorMsg = "INVALID OPERATOR";
    const string BufferOverflowMsg = "BUFFER OVERFLOW";
    const string ExpectedRightParenMsg = ") EXPECTED";
    const string ExpectedLeftParenMsg = "( EXPECTED";
    const string ExpectedQuoteMsg = "QUOTE EXPECTED";
    const string ExpectedExpressionMsg = "EXPRESSION EXPECTED";
    const string InvalidBitValueMsg = "INVALID BIT VALUE";
    const string ConstantExpressionExpectedMsg = "CONSTANT EXPRESSION EXPECTED";
    const string IllegalIdentifierMsg = "ILLEGAL IDENTIFIER";
    
    SyntaxError() { setError(SyntaxErrorMsg); }
    TypeMismatch() { setError(TypeMismatchMsg); }
    NotImplemented() { setError(NotImplementedMsg); }
    InternalError() { setError(InternalErrorMsg); }
    FunctionExists() { setError(FunctionExistsMsg); }
    ConstantExists() { setError(ConstantExistsMsg); }
    VariableExists() { setError(VariableExistsMsg); }
    OutOfMemory() { setError(OutOfMemoryMsg); }
    DivisionByZero() { setError(DivisionByZeroMsg); }
    NumericOverflow() { setError(NumericOverflowMsg); }
    UndefinedIdentifier() { setError(UndefinedIdentifierMsg); }
    IllegalAssignment() { setError(IllegalAssignmentMsg); }
    InvalidOperator() { setError(InvalidOperatorMsg); }
    BufferOverflow() { setError(BufferOverflowMsg); }
    ExpectedRightParen() { setError(ExpectedRightParenMsg); }
    ExpectedLeftParen() { setError(ExpectedLeftParenMsg); }
    ExpectedQuote() { setError(ExpectedQuoteMsg); }
    ExpectedExpression() { setError(ExpectedExpressionMsg); }
    InvalidBitValue() { setError(InvalidBitValueMsg); }
    ConstantExpressionExpected() { setError(ConstantExpressionExpectedMsg); }
    IllegalIdentifier() { setError(IllegalIdentifierMsg); }
    
    // Internal helper
    setError()
    {
        // A contains message address (from calling method setup)
        STA ZP.LastErrorL
        LDA ZP.ACCH
        STA ZP.LastErrorH
        CLC
    }
}