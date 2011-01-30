#!/bin/bash

# arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
arch=$1
if [ "x$arch" != "x86" ]; then 
   if [ "x$arch" != "x64" ]; then 
      echo "One arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)"
      exit 1
   fi
fi
if [ "$arch" == "86" ]; then 
sz=32
fi
if [ "$arch" == "64" ]; then 
sz=64
fi
#echo got sz= $sz and arch= $arch


gcc -m${sz} -O -Iinclude -o bin/aessample${arch} src/aessample.c lib/x${arch}/intel_aes${arch}.a
