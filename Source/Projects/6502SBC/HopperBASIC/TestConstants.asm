unit TestConstants
{
    // Shared test data
    const string testName1 = "VAR1";
    const string testName2 = "COUNT";
    const string testName3 = "FLAG";
    const string testName4 = "TEMP";
    const string testName5 = "CONST1";
    const string testName6 = "INVALID";
    
    // Test token pointers (dummy values)
    const uint testTokens1 = 0x1234;
    const uint testTokens2 = 0x5678;
    const uint testTokens3 = 0x9ABC;
    
    // Table test descriptions
    const string tableDesc1 = "Empty list operations";
    const string tableDesc2 = "Add single node";
    const string tableDesc3 = "Add multiple nodes";
    const string tableDesc4 = "Traverse list";
    const string tableDesc5 = "Delete first node";
    const string tableDesc6 = "Clear entire list";
    
    // Objects test descriptions
    const string objectsDesc1 = "Objects initialize";
    const string objectsDesc2 = "Add symbol";
    const string objectsDesc3 = "Find symbol";
    const string objectsDesc4 = "Get symbol data";
    const string objectsDesc5 = "Set symbol value";
    const string objectsDesc6 = "Symbol type filtering";
    const string objectsDesc7 = "Destroy symbol table";
    
    // Variables test descriptions
    const string variablesDesc1 = "Variables initialize";
    const string variablesDesc2 = "Declare INT variable";
    const string variablesDesc3 = "Declare WORD constant";
    const string variablesDesc4 = "Find variable by name";
    const string variablesDesc5 = "Find constant by name";
    const string variablesDesc6 = "Get variable value";
    const string variablesDesc7 = "Get constant value";
    const string variablesDesc8 = "Set variable value";
    const string variablesDesc9 = "Set constant value (should fail)";
    const string variablesDesc10 = "Get variable type";
    const string variablesDesc11 = "Get variable name";
    const string variablesDesc12 = "Get variable tokens";
    const string variablesDesc13 = "Remove variable";
    const string variablesDesc14 = "Iterate variables only";
    const string variablesDesc15 = "Iterate constants only";
    const string variablesDesc16 = "Iterate all symbols";
    const string variablesDesc17 = "Duplicate declaration (should fail)";
    const string variablesDesc18 = "Type filtering in Find";
    const string variablesDesc19 = "Variables clear all";
}