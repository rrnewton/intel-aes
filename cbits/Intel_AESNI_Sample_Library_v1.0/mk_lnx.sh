#!/bin/bash

# arg required, must be 86 or 64. Compiles all the linux binaries and libraries in subdirs
arch=$1
if [ "x$arch" != "x86" ]; then
   if [ "x$arch" != "x64" ]; then
      echo "One arg in required. Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)"
      exit 1
   fi
fi

cd intel_aes_lib
./mk_lnx_lib$arch.sh
if [ $? -ne 0 ]
then
	echo "Got error "
	exit
fi

./mk_lnx_bin$arch.sh
if [ $? -ne 0 ]
then
	echo "Got error "
	exit
fi
cd ..

cd aes_example
./mk_lnx$arch.sh
if [ $? -ne 0 ]
then
	echo "Got error "
	exit
fi
cd ..

cd aes_gladman_subset
./mk_lnx$arch.sh
if [ $? -ne 0 ]
then
	echo "Got error "
	exit
fi
cd ..


