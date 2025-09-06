preprocess /source/compiler/preprocess
preprocess /source/compiler/65asm
preprocess /source/compiler/65gen
preprocess /source/compiler/65opt
preprocess /source/compiler/65dasm

compile preprocess -o
compile 65asm -o
compile 65gen -o
compile 65opt -o
compile 65dasm -o

optimize preprocess
optimize 65asm
optimize 65gen
optimize 65opt
optimize 65dasm

codegen preprocess
codegen 65asm
codegen 65gen
codegen 65opt
codegen 65dasm

dasm preprocess
dasm 65asm
dasm 65gen
dasm 65opt
dasm 65dasm

