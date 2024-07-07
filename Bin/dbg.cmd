preprocess /source/shell/term
preprocess /source/shell/port
preprocess /source/shell/baud
preprocess /source/debugger/hm
preprocess /source/debugger/debug
preprocess /source/debugger/rsod
preprocess /source/editor/edit

compile term -o
compile port -o
compile baud -o
compile hm -o
compile debug -o
compile rsod -o
compile edit -o

optimize term
optimize port
optimize baud
optimize hm
optimize debug
optimize rsod
optimize edit

codegen term
codegen port
codegen baud
codegen hm
codegen debug
codegen rsod
codegen edit

dasm term
dasm port
dasm baud
dasm hm
dasm debug
dasm rsod
dasm edit

