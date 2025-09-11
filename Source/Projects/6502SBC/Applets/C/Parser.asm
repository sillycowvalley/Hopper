unit Parser
{
    // Parser zero page
    const byte parserSlots = 0x80;
    
    const byte currentToken = parserSlots+0;  // Current token type
    const uint astRoot      = parserSlots+1;  // Root of AST
    const byte astRootL     = parserSlots+1;
    const byte astRootH     = parserSlots+2;
    const uint currentNode  = parserSlots+3;  // Node being built
    const byte currentNodeL = parserSlots+3;
    const byte currentNodeH = parserSlots+4;
    
    // For our first test case, we can use a very simple recursive descent parser
}
