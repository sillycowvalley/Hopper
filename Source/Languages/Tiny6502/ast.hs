unit AST
{
    enum ExprType
    {
        BINARY,
        UNARY,
        LITERAL,
        VARIABLE
    }

    enum StmtType
    {
        EXPR_STMT,
        PRINT_STMT,
        VAR_STMT,
        RETURN_STMT,
        BLOCK_STMT
    }

    record Expr
    {
        uint Line;
        ExprType Type;
        ExprBinary BinaryExpr;
        ExprUnary UnaryExpr;
        ExprLiteral LiteralExpr;
        ExprVariable VariableExpr;
    }

    record ExprBinary
    {
        Expr Left;
        string Operator;
        Expr Right;
    }

    record ExprUnary
    {
        string Operator;
        Expr Operand;
    }

    record ExprLiteral
    {
        string Value;
    }

    record ExprVariable
    {
        string Name;
    }

    record Stmt
    {
        uint Line;
        StmtType Type;
        StmtExpr ExprStmt;
        StmtPrint PrintStmt;
        StmtVar VarStmt;
        StmtReturn ReturnStmt;
        StmtBlock BlockStmt;
    }

    record StmtExpr
    {
        Expr Expression;
    }

    record StmtPrint
    {
        Expr Expression;
    }

    record StmtVar
    {
        string Name;
        Expr Initializer;
    }

    record StmtReturn
    {
        Expr Expression;
    }

    record StmtBlock
    {
        <Stmt> Statements;
    }

    enum DeclType
    {
        FUNC_DECL,
        VAR_DECL
    }

    record Decl
    {
        uint Line;
        DeclType Type;
        DeclFunc FuncDecl;
        StmtVar VarDecl;
    }

    record DeclFunc
    {
        string Name;
        <string> Params;
        <Stmt> Body;
    }

    record Program
    {
        <Decl> Declarations;
    }

    Expr ExprBinary(uint line, Expr left, string operator, Expr right)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.BINARY;
        ExprBinary binaryExpr;
        binaryExpr.Left = left;
        binaryExpr.Operator = operator;
        binaryExpr.Right = right;
        expr.BinaryExpr = binaryExpr;
        return expr;
    }

    Expr ExprUnary(uint line, string operator, Expr operand)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.UNARY;
        ExprUnary unaryExpr;
        unaryExpr.Operator = operator;
        unaryExpr.Operand = operand;
        expr.UnaryExpr = unaryExpr;
        return expr;
    }

    Expr ExprLiteral(uint line, string value)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.LITERAL;
        ExprLiteral literalExpr;
        literalExpr.Value = value;
        expr.LiteralExpr = literalExpr;
        return expr;
    }

    Expr ExprVariable(uint line, string name)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.VARIABLE;
        ExprVariable variableExpr;
        variableExpr.Name = name;
        expr.VariableExpr = variableExpr;
        return expr;
    }
}

