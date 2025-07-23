// This unit is now split into ExpressionParser and StatementCompiler
// Keep this file for backward compatibility, but delegate to the new units

unit BytecodeCompiler
{
    uses "ExpressionParser"
    uses "StatementCompiler"
    
    // Forward all calls to StatementCompiler for backward compatibility
    CompileREPLStatement()
    {
        StatementCompiler.CompileREPLStatement();
    }
}