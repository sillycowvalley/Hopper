cls
cd /source/compiler
PreProcess CODEGEN
Compile /Debug/Obj/CODEGEN
CODEGEN /Debug/Obj/CODEGEN
DASM /Bin/CODEGEN
PreProcess DASM
Compile /Debug/Obj/DASM
CODEGEN /Debug/Obj/DASM
DASM /Bin/DASM
PreProcess PreProcess
Compile /Debug/Obj/PreProcess
CODEGEN /Debug/Obj/PreProcess
DASM /Bin/PreProcess
PreProcess Compile
Compile /Debug/Obj/Compile
CODEGEN /Debug/Obj/Compile
DASM /Bin/Compile

