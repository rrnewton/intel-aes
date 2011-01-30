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

mkdir -p obj/x${arch}

yasm="../yasm/yasm"

pushd .
asm="iaesx${arch} do_rdtsc"
for i in $asm; do echo do $i.s; $yasm -D__linux__ -g dwarf2 -f elf${sz} asm/x${arch}/$i.s -o obj/x${arch}/$i.o; done
gcc -O3 -g -m${sz} -Iinclude/ -c src/intel_aes.c -o obj/x${arch}/intel_aes.o
ar -r lib/x${arch}/intel_aes${arch}.a obj/x${arch}/*.o
popd


