preprocess /source/compiler/Assemble
preprocess /source/compiler/AsmGen
preprocess /source/compiler/OptAsm
preprocess /source/compiler/65DASM
preprocess /source/compiler/Z80Gen
preprocess /source/compiler/Z80Opt
preprocess /source/compiler/Z80DASM
preprocess /source/debugger/e6502

compile Assemble -o
compile AsmGen -o
compile OptAsm -o
compile 65DASM -o
compile Z80Gen -o
compile Z80Opt -o
compile Z80DASM -o
compile e6502 -o

optimize Assemble
optimize AsmGen
optimize OptAsm
optimize 65DASM
optimize Z80Gen
optimize Z80Opt
optimize Z80DASM
optimize e6502

codegen Assemble
codegen AsmGen
codegen OptAsm
codegen 65DASM
codegen Z80Gen
codegen Z80Opt
codegen Z80DASM
codegen e6502

dasm Assemble
dasm AsmGen
dasm OptAsm
dasm 65DASM
dasm Z80Gen
dasm Z80Opt
dasm Z80DASM
dasm e6502

preprocess /source/runtime/r6502.asm -a
assemble r6502 -a 6502
optasm r6502
asmgen r6502
65dasm r6502

