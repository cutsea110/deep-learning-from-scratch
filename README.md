$ cabal new-build main --enable-profiling
$ cabal new-exec main -- +RTS -p -N8 -smain.log

