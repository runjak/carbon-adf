#!/bin/sh
# This script is called by OpenBrain/Main/CompileTime.hs on compilation.
# It allows OpenBrain to know how many lines of JavaScript it's long.
find files/js/ -type f -regex .*js | grep -v extern | xargs wc -l | sort -n
