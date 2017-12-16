#!/bin/sh

if [ "$#" -ne 1 ]; then
	echo "Usage: $0 LL_FILE"
	exit 1
fi

LL_FILE=$1

PIETLIB=src/pietlib/pietlib.c

clang-5.0 -g "$LL_FILE" "$PIETLIB"
