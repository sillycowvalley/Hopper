preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/cd
preprocess /source/shell/cls
preprocess /source/shell/dir
preprocess /source/shell/del
preprocess /source/shell/help

compile show -o
compile shell -o
compile cd -o
compile cls -o
compile dir -o
compile del -o
compile help -o

optimize show
optimize shell
optimize cd
optimize cls
optimize dir
optimize del
optimize help

codegen show
codegen shell
codegen cd
codegen cls
codegen dir
codegen del
codegen help

dasm show
dasm shell
dasm cd
dasm cls
dasm dir
dasm del
dasm help

