cd /source/compiler

PreProcess DASM
Compile DASM -o
Optimize DASM
CODEGEN DASM
DASM DASM

PreProcess Optimize
Compile Optimize -o
Optimize Optimize
CODEGEN Optimize
DASM Optimize

PreProcess CODEGEN
Compile CODEGEN -o 
Optimize CODEGEN
CODEGEN CODEGEN
DASM CODEGEN

PreProcess PreProcess
Compile PreProcess -o
Optimize PreProcess
CODEGEN PreProcess
DASM PreProcess

PreProcess Compile
Compile Compile -o
Optimize Compile
CODEGEN Compile -extended
DASM Compile


