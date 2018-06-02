#!/bin/bash
name=$(basename $1 | sed "s/\.cld//")
if [ ! -f ./toplevel.native ]; then
	echo "The compiler (in toplevel.native) was not found. Please run make. "
	exit 1
fi
./toplevel.native -l $1  | llc > a.s && gcc  -g -lc  -no-pie -lm -lpng -o $name a.s matrix.o image.o