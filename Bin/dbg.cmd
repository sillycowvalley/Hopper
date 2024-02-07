preprocess /source/shell/term
preprocess /source/shell/port
preprocess /source/debugger/hm
preprocess /source/debugger/debug
preprocess /source/debugger/rsod
preprocess /source/editor/edit

compile term -o
compile port -o
compile hm -o
compile debug -o
compile rsod -o
compile edit -o

optimize term
optimize port
optimize hm
optimize debug
optimize rsod
optimize edit

codegen term
codegen port
codegen hm
codegen debug -extended
codegen rsod
codegen edit -extended

dasm term
dasm port
dasm hm
dasm debug
dasm rsod
dasm edit

