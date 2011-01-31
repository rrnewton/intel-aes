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

#yasm="../yasm/yasm"
yasm=yasm

pushd .
asm="iaesx${arch} do_rdtsc"
for i in $asm; do 
   # echo do $i.s
   echo "$yasm -D__linux__ -g dwarf2 -f elf${sz} asm/x${arch}/$i.s -o obj/x${arch}/$i.o"
   $yasm -D__linux__ -g dwarf2 -f elf${sz} asm/x${arch}/$i.s -o obj/x${arch}/$i.o
done
# gcc -O3 -g -m${sz} -Iinclude/ -c src/intel_aes.c -o obj/x${arch}/intel_aes.o
# RRN: Building a shared library too:
 echo "Compiling C source"
 gcc -fPIC -O3 -g -Iinclude/ -c src/intel_aes.c -o obj/x64/intel_aes.o

 echo "Linking static and dynamic libraries"
 ar -r lib/x${arch}/libintel_aes${arch}.a obj/x${arch}/*.o
 gcc -shared -dynamic  -o lib/x${arch}/libintel_aes${arch}.so obj/x64/*.o
popd

echo "Copying to cbits parent directory"
cp lib/x${arch}/*.so ../../
cp lib/x${arch}/*.a  ../../
