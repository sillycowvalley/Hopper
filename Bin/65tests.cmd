cd /source/testing/minimal
preprocess mrvt
compile -o mrvt
optimize mrvt
codegen mrvt -ihex
dasm mrvt
hm -l mrvt -x
