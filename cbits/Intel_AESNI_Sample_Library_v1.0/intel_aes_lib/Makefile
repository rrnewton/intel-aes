
UNAME=$(shell uname -m)

# Must be 86 (for 32bit compiler) or 64 (for 64bit compiler)
ARCH=64
# Must be 32 or 64:
SZ=64

STATIC=lib/x$(ARCH)/libintel_aes.a
DYNAMIC=lib/x$(ARCH)/libintel_aes.so

SRC= src/intel_aes.c \
     asm/x$(ARCH)/iaesx$(ARCH).s \
     asm/x$(ARCH)/do_rdtsc.s

OBJ= obj/x$(ARCH)/intel_aes.o \
     obj/x$(ARCH)/iaesx$(ARCH).o \
     obj/x$(ARCH)/do_rdtsc.o

GCC=gcc
YASM=yasm
YASMFLAGS= -D__linux__ -g dwarf2 -f elf$(SZ) 

all: $(STATIC) $(DYNAMIC)

$(STATIC): $(OBJ)
	@mkdir -p lib/x$(ARCH)
	ar -r $@ $(OBJ)

$(DYNAMIC): $(OBJ)
	@mkdir -p lib/x$(ARCH)
	$(GCC) -shared -dynamic  -o $(DYNAMIC) $(OBJ)

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
	rm -f $(STATIC) $(DYNAMIC) $(OBJ)
