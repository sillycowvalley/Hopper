preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/cd
preprocess /source/shell/cls
preprocess /source/shell/dir
preprocess /source/shell/del
preprocess /source/shell/mkdir
preprocess /source/shell/rmdir
preprocess /source/shell/copy
preprocess /source/shell/del
preprocess /source/shell/help

compile show -o
compile shell -o
compile cd -o
compile cls -o
compile dir -o
compile del -o
compile mkdir -o
compile rmdir -o
compile copy -o
compile help -o

optimize show
optimize shell
optimize cd
optimize cls
optimize dir
optimize del
optimize mkdir
optimize rmdir
optimize copy
optimize help

codegen show
codegen shell
codegen cd
codegen cls
codegen dir
codegen del
codegen mkdir
codegen rmdir
codegen copy
codegen help

dasm show
dasm shell
dasm cd
dasm cls
dasm dir
dasm del
dasm mkdir
dasm rmdir
dasm copy
dasm help

