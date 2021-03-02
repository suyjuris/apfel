#!/bin/bash

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
else
    echo "Error: first argument must be either debug or release"
fi;

