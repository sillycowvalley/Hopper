preprocess /source/shell/show
preprocess /source/shell/shell
preprocess /source/shell/dir
preprocess /source/editor/edit
preprocess /source/samples/sieve

compile /debug/obj/show
compile /debug/obj/shell
compile /debug/obj/dir
compile /debug/obj/edit
compile /debug/obj/sieve

codegen /debug/obj/show
codegen /debug/obj/shell
codegen /debug/obj/dir
codegen /debug/obj/edit
codegen /debug/obj/sieve

dasm /bin/show
dasm /bin/shell
dasm /bin/dir
dasm /bin/edit
dasm /bin/sieve
