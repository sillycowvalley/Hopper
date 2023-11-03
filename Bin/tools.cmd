cd /source/compiler
PreProcess CODEGEN
Compile CODEGEN -o
Optimize CODEGEN
CODEGEN CODEGEN
DASM CODEGEN

PreProcess DASM
Compile DASM -o
Optimize DASM
CODEGEN DASM
DASM DASM

PreProcess PreProcess
Compile PreProcess -o
Optimize PreProcess
CODEGEN PreProcess
DASM PreProcess

PreProcess Compile
Compile Compile -o
Optimize Compile
CODEGEN Compile
DASM Compile

PreProcess optimize
Compile optimize -o
optimize optimize
CODEGEN optimize
DASM optimize
