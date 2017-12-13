#!/bin/sh

if [ "$#" -ne 1 ]; then
	echo "Usage: $0 LL_FILE"
	exit 1
fi

LL_FILE=$1

PIETLIB=src/pietlib/pietlib.ll

clang-5.0 "$LL_FILE" "$PIETLIB"
