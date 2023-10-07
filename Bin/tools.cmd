cls
cd /source/compiler
PreProcess CODEGEN
Compile /Debug/Obj/CODEGEN -o -t
Optimize /Debug/Obj/CODEGEN
CODEGEN /Debug/Obj/CODEGEN
DASM /Bin/CODEGEN
PreProcess DASM
Compile /Debug/Obj/DASM -o -t
Optimize /Debug/Obj/DASM
CODEGEN /Debug/Obj/DASM
DASM /Bin/DASM
PreProcess PreProcess
Compile /Debug/Obj/PreProcess -o -t
Optimize /Debug/Obj/PreProcess
CODEGEN /Debug/Obj/PreProcess
DASM /Bin/PreProcess
PreProcess Compile
Compile /Debug/Obj/Compile -o -t
Optimize /Debug/Obj/Compile
CODEGEN /Debug/Obj/Compile
DASM /Bin/Compile
PreProcess Optimize
Compile /Debug/Obj/Optimize -o -t
Optimize /Debug/Obj/Optimize
CODEGEN /Debug/Obj/Optimize
DASM /Bin/Optimize

