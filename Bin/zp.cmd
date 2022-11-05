cd /source/z80
cls
preprocess zopper
compile /debug/obj/zopper
codegen /debug/obj/zopper
dasm /bin/zopper
zopper firmware/runtime.hs
