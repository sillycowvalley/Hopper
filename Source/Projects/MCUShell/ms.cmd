preprocess mcushell
compile mcushell -o
optimize mcushell
codegen mcushell -ihex
dasm mcushell
hm -l mcushell

preprocess /source/shell/help -d SERIAL_CONSOLE
compile help -o
optimize help
codegen help
hm -t /bin/help.hexe /bin

preprocess /source/shell/dir -d SERIAL_CONSOLE
compile dir -o
optimize dir
codegen dir
hm -t /bin/dir.hexe /bin

preprocess /source/shell/cls -d SERIAL_CONSOLE
compile cls -o
optimize cls
codegen cls
hm -t /bin/cls.hexe /bin

preprocess /source/shell/cd -d SERIAL_CONSOLE
compile cd -o
optimize cd
codegen cd
hm -t /bin/cd.hexe /bin

preprocess /source/shell/del -d SERIAL_CONSOLE
compile del -o
optimize del
codegen del
hm -t /bin/del.hexe /bin

preprocess /source/samples/mandelbrot
compile mandelbrot -o
optimize mandelbrot
codegen mandelbrot
hm -t /bin/mandelbrot.hexe /bin

preprocess /source/languages/basic/tiggerbasic
compile tiggerbasic -o
optimize tiggerbasic
codegen tiggerbasic
hm -t /bin/tiggerbasic.hexe /bin

// upload some BASIC samples

hm -t /source/languages/basic/gibl/mandelbench.bas /basic
hm -t /source/languages/basic/gibl/byte.bas /basic
hm -t /source/languages/basic/gibl/nrl.bas /basic

// restore the Windows versions of the shell commands

preprocess /source/shell/help
compile help -o
optimize help
codegen help
dasm help

preprocess /source/shell/dir
compile dir -o
optimize dir
codegen dir
dasm dir

preprocess /source/shell/cls
compile cls -o
optimize cls
codegen cls
dasm cls

preprocess /source/shell/cd
compile cd -o
optimize cd
codegen cd
dasm cd

preprocess /source/shell/del
compile del -o
optimize del
codegen del
dasm del

