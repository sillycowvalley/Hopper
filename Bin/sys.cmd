preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/cd
preprocess /source/shell/cls
preprocess /source/shell/dir
preprocess /source/shell/del
preprocess /source/shell/term
preprocess /source/shell/port
preprocess /source/debugger/hm
preprocess /source/debugger/debug
preprocess /source/debugger/rsod
preprocess /source/editor/edit

compile show -o
compile shell -o
compile cd -o
compile cls -o
compile dir -o
compile del -o
compile term -o
compile port -o
compile hm -o
compile debug -o
compile rsod -o
compile edit -o

optimize show
optimize shell
optimize cd
optimize cls
optimize dir
optimize del
optimize term
optimize port
optimize hm
optimize debug
optimize rsod
optimize edit

codegen show
codegen shell
codegen cd
codegen cls
codegen dir
codegen del
codegen term
codegen port
codegen hm
codegen debug -extended
codegen rsod
codegen edit

dasm show
dasm shell
dasm cd
dasm cls
dasm dir
dasm del
dasm term
dasm port
dasm hm
dasm debug
dasm rsod
dasm edit

