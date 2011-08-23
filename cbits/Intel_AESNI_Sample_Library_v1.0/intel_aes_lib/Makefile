
UNAME=$(shell uname -m)
OS:=$(shell uname -s)

# Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
ARCH=64
#ARCH=86
# Must be 32 or 64:
SZ=64
#SZ=32
LIBDIR=lib/x$(ARCH)

STATIC=libintel_aes.a
ifeq (Darwin,$(OS))
DYNAMIC=libintel_aes.dylib
else
DYNAMIC=libintel_aes.so
endif

SRC= src/intel_aes.c \
     asm/x$(ARCH)/iaesx$(ARCH).s \
     asm/x$(ARCH)/do_rdtsc.s

OBJ= obj/x$(ARCH)/intel_aes.o \
     obj/x$(ARCH)/iaesx$(ARCH).o \
     obj/x$(ARCH)/do_rdtsc.o

# GCC=gcc -m32
GCC=gcc 
YASM=yasm
ifeq (Darwin,$(OS))
YASMFLAGS= -D__linux__ -g null -f macho$(SZ) --prefix=_
L=libtool
LFLAGS=-dynamic -install_name @rpath/$(DYNAMIC) -macosx_version_min 10.5 -lSystem
else
YASMFLAGS= -D__linux__ -g dwarf2 -f elf$(SZ) 
L=gcc
LFLAGS=-shared -dynamic
endif

all: $(LIBDIR)/$(STATIC) $(LIBDIR)/$(DYNAMIC)
	cp $(LIBDIR)/* ../../

$(LIBDIR)/$(STATIC): $(OBJ)
	@mkdir -p $(LIBDIR)
	ar -r $@ $(OBJ)

$(LIBDIR)/$(DYNAMIC): $(OBJ)
	@mkdir -p $(LIBDIR)
	$(L) $(LFLAGS) -o $(DYNAMIC) $(OBJ)
	mv $(DYNAMIC) $(LIBDIR)

obj/x$(ARCH)/do_rdtsc.o:  asm/x$(ARCH)/do_rdtsc.s
	@mkdir -p obj/x$(ARCH)
	$(YASM) $(YASMFLAGS) $< -o $@

obj/x$(ARCH)/iaesx$(ARCH).o:  asm/x$(ARCH)/iaesx$(ARCH).s
	@mkdir -p obj/x$(ARCH)
	$(YASM) $(YASMFLAGS) $< -o $@

obj/x$(ARCH)/intel_aes.o:  src/intel_aes.c
	@mkdir -p obj/x$(ARCH)
	$(GCC) -fPIC -O3 -g -Iinclude/ -c $< -o $@

clean:
	rm -f $(STATIC) $(DYNAMIC) $(LIBDIR)/$(STATIC) $(LIBDIR)/$(DYNAMIC) $(OBJ)
