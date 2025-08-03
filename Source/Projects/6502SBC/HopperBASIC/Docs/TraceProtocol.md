## Task: Standardize TRACE Macro Usage in HopperBASIC Codebase
**Document Type: Development Protocol**

### Background
The HopperBASIC project uses conditional compilation with `#ifdef TRACE` to enable method call tracing for debugging. Apply these standards consistently when adding or updating trace instrumentation.

### TRACE Macro Standards

#### 1. String Constant Placement
Place trace string constants immediately before their method, after the method description:
```hopper
// Method description
const string myMethodTrace = "MyMethod";
MyMethod()
{
    // implementation
}
```

** IMPORTANT *** 

The const string is *not* inside an #ifdef block.

#### 2. Trace Call Placement Rules
**Analyze the method structure FIRST** to determine correct trace placement:

##### A. Simple Methods (No Register Preservation)
Place trace calls at method entry and single exit point:
```hopper
// Execute simple command
const string simpleTrace = "Simple";
SimpleMethod()
{
#ifdef TRACE
    LDA #(simpleTrace % 256) STA ZP.TraceMessageL LDA #(simpleTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
    // method implementation
    
#ifdef TRACE
    LDA #(simpleTrace % 256) STA ZP.TraceMessageL LDA #(simpleTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
}
```

##### B. Methods with Register Preservation
Place trace calls INSIDE the preservation block:
```hopper
// Process data with register preservation
const string preservedTrace = "Preserved";
PreservedMethod()
{
    PHA
    PHX
    PHY
#ifdef TRACE
    LDA #(preservedTrace % 256) STA ZP.TraceMessageL LDA #(preservedTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
    // method implementation
    
#ifdef TRACE
    LDA #(preservedTrace % 256) STA ZP.TraceMessageL LDA #(preservedTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
    PLY
    PLX
    PLA
}
```

##### C. Multiple Exit Methods
**DO NOT add trace calls** to methods with multiple exit points (multiple returns). These methods need refactoring to single-exit pattern before adding trace instrumentation:
```hopper
// ❌ DO NOT trace this pattern
BadMethod()
{
    if (condition1) { return; }  // Multiple exits
    // work
    if (condition2) { return; }  // Multiple exits
    // more work
}

// ✅ Refactor to single-exit first, then add tracing
// Execute operation with error handling
const string goodTrace = "Good";
GoodMethod()
{
#ifdef TRACE
    LDA #(goodTrace % 256) STA ZP.TraceMessageL LDA #(goodTrace / 256) STA ZP.TraceMessageH Trace.MethodEntry();
#endif
    
    loop // Single exit pattern
    {
        if (condition1) { break; }
        // work
        if (condition2) { break; }
        // more work
        break;
    } // single exit loop
    
#ifdef TRACE
    LDA #(goodTrace % 256) STA ZP.TraceMessageL LDA #(goodTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
}
```

#### 3. Indentation Rules
- Trace calls use normal code indentation (not left margin alignment)
- `#ifdef TRACE` and `#endif` align with surrounding code
- Trace call itself is indented one level inside the `#ifdef`

#### 4. Pre-Implementation Checklist
Before adding trace calls to any method:

1. **Count return statements** - If > 1, method needs single-exit refactoring first
2. **Check for register preservation** - PHA/PHX/PHY at start means trace goes inside
3. **Identify the single exit point** - Where control flow converges before return
4. **Verify string constant placement** - After method description, immediately before method, no `#ifdef`

#### 5. Method Categories That Should NOT Be Traced
- Debug/utility methods that are called from within trace code
- Very small helper methods (< 5 lines)
- Methods that are called frequently in tight loops
- Interrupt handlers and low-level system code

### Implementation Workflow
1. **Analyze method structure** using checklist above
2. **Add string constant** after method description, immediately before method
3. **Place entry trace** at appropriate location based on method type
4. **Place exit trace** at single exit point
5. **Verify indentation** matches surrounding code
6. **Test compilation** with both `#define TRACE` and without
7. **Report disqualified methods** (see below)

### Required Reporting
**IMPORTANT**: After completing trace instrumentation, provide a report of major methods that were disqualified due to multiple exit points. Include:
- Method name and file location
- Brief description of method's purpose
- Number of exit points found
- Assessment of whether refactoring would be beneficial

Example report format:
```
METHODS DISQUALIFIED FOR TRACE INSTRUMENTATION:

Console.asm:
- cmdRun() - Execute RUN command (2 exits) - Consider refactoring
- processTokens() - Process token stream (4 exits) - High value, should refactor

Statement.asm:
- executeStatement() - Main statement dispatcher (6 exits) - Critical path, needs refactoring
```

### Common Patterns to Avoid
- ❌ Adding trace to methods with multiple returns
- ❌ Placing trace outside register preservation blocks
- ❌ Using `#ifdef` around string constants
- ❌ Left-margin alignment of trace calls
- ❌ Tracing methods that are called from trace code itself
- ❌ Forgetting the final `break;` in single-exit loop patterns
- ❌ Failing to report disqualified methods

### Success Criteria
- Code compiles with and without `#define TRACE`
- No register preservation contracts broken
- Consistent indentation throughout
- Only single-exit methods have trace instrumentation
- All trace strings follow naming convention
- All single-exit loops have proper `break;` at the end
- Complete report of disqualified methods provided

---

This approach ensures trace instrumentation is added correctly from the start while identifying methods that need refactoring to become traceable.