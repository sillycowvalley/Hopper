preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/dir
preprocess /source/shell/term
preprocess /source/shell/port
preprocess /source/debugger/hm
preprocess /source/debugger/debug
preprocess /source/editor/edit

compile /debug/obj/show -o
compile /debug/obj/shell -o
compile /debug/obj/dir -o
compile /debug/obj/term -o
compile /debug/obj/port -o
compile /debug/obj/hm -o
compile /debug/obj/debug -o
compile /debug/obj/edit -o

optimize /debug/obj/show
optimize /debug/obj/shell
optimize /debug/obj/dir
optimize /debug/obj/term
optimize /debug/obj/port
optimize /debug/obj/hm
optimize /debug/obj/debug
optimize /debug/obj/edit

codegen /debug/obj/show
codegen /debug/obj/shell
codegen /debug/obj/dir
codegen /debug/obj/term
codegen /debug/obj/port
codegen /debug/obj/hm
codegen /debug/obj/debug
codegen /debug/obj/edit

dasm /bin/show
dasm /bin/shell
dasm /bin/dir
dasm /bin/term
dasm /bin/port
dasm /bin/hm
dasm /bin/debug
dasm /bin/edit

