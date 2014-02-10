#!/bin/sh
# This script is called by OpenBrain/Main/CompileTime.hs on compilation.
# It allows OpenBrain to know how many lines of Haskell it's long.
find Carbon/ -type f -regex .*hs | xargs wc -l | sort -n
