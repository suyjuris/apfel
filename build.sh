#!/bin/bash

set -eu
GXX=g++-8
CXXFLAGS="-fmax-errors=2 -Wall -Wextra -Wno-class-memaccess -Wno-sign-conversion -Wno-unused-variable -Wno-sign-compare -Wno-write-strings -Wno-unused-parameter -Wno-comment -std=c++14 -fno-exceptions -fno-rtti"
LDFLAGS="-lGL -lX11 -lXrandr"
LIBPROFILER=/usr/local/lib/libprofiler.so

NAME=cppsat

if [ "$#" -lt 1 ]; then
    echo "Usage:"
    echo "  $0 [debug|release]"
    exit 1
fi;

if [ "$1" = "debug" ]; then
    "$GXX" $CXXFLAGS -O0 -ggdb -Werror platform_linux.cpp -o "$NAME" $LDFLAGS
elif [ "$1" = "release" ]; then
    "$GXX" $CXXFLAGS -O2 platform_linux.cpp -o "$NAME" $LDFLAGS
elif [ "$1" = "construct" ]; then
    "$GXX" $CXXFLAGS -O0 -ggdb -Werror construct.cpp -o "construct" $LDFLAGS
elif [ "$1" = "construct_release" ]; then
    "$GXX" $CXXFLAGS -O3 -march=native -ggdb -Werror construct.cpp -o "construct" $LDFLAGS
elif [ "$1" = "construct_profile" ]; then
    mkdir -p profdata
    echo LD_PRELOAD="$LIBPROFILER" CPUPROFILE=./profdata/"construct".prof ./"construct"
    echo pprof -http ":" "construct" profdata/"construct".prof
elif [ "$1" = "hashmap_test" ]; then
    "$GXX" $CXXFLAGS -O0 -ggdb -Werror -DHASHMAP_TEST -DHASHMAP_VERBOSE=1 hashmap.cpp -o "hashmap_test"
elif [ "$1" = "hashmap_fuzz" ]; then
    "afl-clang-lto++" $CXXFLAGS -O0 -ggdb -Werror -Wno-unknown-warning-option -DHASHMAP_TEST -DHASHMAP_VERBOSE=0 -stdlib=libstdc++ hashmap.cpp -L/usr/lib/x86_64-linux-gnu -o "hashmap_fuzz"
elif [ "$1" = "hashmap_bench" ]; then
    "$GXX" $CXXFLAGS -march=native -O0 -ggdb -Werror hashmap_bench.cpp -o "hashmap_bench"
else
    echo "Error: first argument must be either debug or release"
fi;

