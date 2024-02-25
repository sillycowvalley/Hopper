preprocess mcushell
compile mcushell -o
optimize mcushell
codegen mcushell -ihex
dasm mcushell

preprocess /source/shell/help -d SERIAL_CONSOLE
compile help -o
optimize help
codegen help

preprocess /source/shell/dir -d SERIAL_CONSOLE
compile dir -o
optimize dir
codegen dir

preprocess /source/shell/cls -d SERIAL_CONSOLE
compile cls -o
optimize cls
codegen cls

preprocess /source/shell/cd -d SERIAL_CONSOLE
compile cd -o
optimize cd
codegen cd

preprocess /source/shell/del -d SERIAL_CONSOLE
compile del -o
optimize del
codegen del

preprocess /source/shell/mkdir -d SERIAL_CONSOLE
compile mkdir -o
optimize mkdir
codegen mkdir

preprocess /source/shell/rmdir -d SERIAL_CONSOLE
compile rmdir -o
optimize rmdir
codegen rmdir

preprocess /source/shell/copy -d SERIAL_CONSOLE
compile copy -o
optimize copy
codegen copy

preprocess /source/projects/mcushell/show -d SERIAL_CONSOLE
compile show -o
optimize show
codegen show

preprocess /source/projects/mcushell/speed
compile speed -o
optimize speed
codegen speed

preprocess /source/samples/mandelbrot
compile mandelbrot -o
optimize mandelbrot
codegen mandelbrot

preprocess /source/samples/fibouint
compile fibouint -o
optimize fibouint
codegen fibouint

preprocess /source/samples/nrl
compile nrl -o
optimize nrl
codegen nrl

preprocess /source/languages/basic/tiggerbasic
compile tiggerbasic -o
optimize tiggerbasic
codegen tiggerbasic

// upload shell programs

hm -l mcushell
hm -t /bin/help.hexe /bin
hm -t /bin/dir.hexe /bin
hm -t /bin/cls.hexe /bin
hm -t /bin/cd.hexe /bin
hm -t /bin/del.hexe /bin
hm -t /bin/mkdir.hexe /bin
hm -t /bin/rmdir.hexe /bin
hm -t /bin/copy.hexe /bin
hm -t /bin/show.hexe /bin

// upload some programs

hm -t /bin/speed.hexe /bin
hm -t /bin/mandelbrot.hexe /bin
hm -t /bin/fibouint.hexe /bin
hm -t /bin/nrl.hexe /bin
hm -t /bin/tiggerbasic.hexe /bin

// upload some BASIC samples

hm -t /source/languages/basic/gibl/mandelbrot.bas /basic
hm -t /source/languages/basic/gibl/byte.bas /basic
hm -t /source/languages/basic/gibl/nrl.bas /basic

// upload a script sample
hm -t /source/projects/mcushell/fibo.cmd /bin
hm -t /source/projects/mcushell/n.cmd /bin

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

preprocess /source/shell/mkdir
compile mkdir -o
optimize mkdir
codegen mkdir
dasm mkdir

preprocess /source/shell/rmdir
compile rmdir -o
optimize rmdir
codegen rmdir
dasm rmdir

preprocess /source/shell/copy
compile copy -o
optimize copy
codegen copy
dasm copy

preprocess /source/shell/show
compile show -o
optimize show
codegen show
dasm show
