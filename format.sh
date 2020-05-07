find src -name '*.hs' -type f -exec brittany --write-mode=inplace  {} \;
find test -name '*.hs' -type f -exec brittany --write-mode=inplace  {} \;