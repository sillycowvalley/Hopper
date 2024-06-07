unit AST
{
    // Expressions
    record Expr
    {
        uint Line;
        ExprType Type;
        ExprBinary BinaryExpr;
        ExprUnary UnaryExpr;
        ExprLiteral LiteralExpr;
        ExprVariable VariableExpr;
        ExprCall CallExpr;
        ExprAssign AssignExpr;
    }
    enum ExprType
    {
        BINARY,
        UNARY,
        LITERAL,
        VARIABLE,
        CALL,
        ASSIGN
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
    record ExprCall
    {
        Expr Callee;
        <Expr> Arguments;
    }
    record ExprAssign
    {
        Expr Variable;
        Expr Value;
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
    Expr ExprCall(uint line, Expr callee, <Expr> arguments)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.CALL;
        ExprCall callExpr;
        callExpr.Callee = callee;
        callExpr.Arguments = arguments;
        expr.CallExpr = callExpr;
        return expr;
    }
    Expr ExprAssign(uint line, Expr variable, Expr value)
    {
        Expr expr;
        expr.Line = line;
        expr.Type = ExprType.ASSIGN;
        ExprAssign assignExpr;
        assignExpr.Variable = variable;
        assignExpr.Value = value;
        expr.AssignExpr = assignExpr;
        return expr;
    }
    // Statements
    record Stmt
    {
        uint Line;
        StmtType Type;
        StmtExpr ExprStmt;
        StmtReturn ReturnStmt;
        StmtBlock BlockStmt;
        StmtVar VarDecl;
        StmtIf IfStmt;
        StmtWhile WhileStmt;
        StmtFor ForStmt;
        StmtNoOp NoOpStmt;
    }
    enum StmtType
    {
        EXPR_STMT,
        RETURN_STMT,
        BLOCK_STMT,
        VAR_DECL,
        IF_STMT,
        WHILE_STMT,
        FOR_STMT,
        NO_OP_STMT
    }
    record StmtExpr
    {
        Expr Expression;
    }
    record StmtReturn
    {
        Expr Expression;
    }
    record StmtBlock
    {
        <Stmt> Statements;
    }
    record StmtVar
    {
        string Name;
        string Type;
        Expr Initializer;
    }
    record StmtIf
    {
        Expr Condition;
        Stmt ThenBranch;
        Stmt ElseBranch;
    }
    record StmtWhile
    {
        Expr Condition;
        Stmt Body;
    }
    record StmtFor
    {
        Stmt Initializer;
        Expr Condition;
        Expr Increment;
        Stmt Body;
    }
    record StmtNoOp
    {
        // No additional fields required for a no-op statement
    }
    // Declarations
    record Decl
    {
        uint Line;
        DeclType Type;
        DeclFunc FuncDecl;
        StmtVar VarDecl;
    }
    enum DeclType
    {
        FUNC_DECL,
        VAR_DECL
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
}

