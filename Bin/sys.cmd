preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/dir
preprocess /source/debugger/hm
preprocess /source/debugger/debug
preprocess /source/editor/edit

compile /debug/obj/show -o -t
compile /debug/obj/shell -o -t
compile /debug/obj/dir -o -t
compile /debug/obj/hm -o -t
compile /debug/obj/debug -o -t
compile /debug/obj/edit -o -t

optimize /debug/obj/show
optimize /debug/obj/shell
optimize /debug/obj/dir
optimize /debug/obj/hm
optimize /debug/obj/debug
optimize /debug/obj/edit

codegen /debug/obj/show
codegen /debug/obj/shell
codegen /debug/obj/dir
codegen /debug/obj/hm
codegen /debug/obj/debug
codegen /debug/obj/edit

dasm /bin/show
dasm /bin/shell
dasm /bin/dir
dasm /bin/hm
dasm /bin/debug
dasm /bin/edit

