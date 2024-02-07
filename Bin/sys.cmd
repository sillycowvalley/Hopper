preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/cd
preprocess /source/shell/cls
preprocess /source/shell/dir
preprocess /source/shell/del

compile show -o
compile shell -o
compile cd -o
compile cls -o
compile dir -o
compile del -o

optimize show
optimize shell
optimize cd
optimize cls
optimize dir
optimize del

codegen show
codegen shell
codegen cd
codegen cls
codegen dir
codegen del

dasm show
dasm shell
dasm cd
dasm cls
dasm dir
dasm del

