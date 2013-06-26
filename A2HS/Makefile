# --- macros
CC=ghc
CFLAGS= -g -O2
#LDFLAGS= -L-Wl,--stack_size,0x1000000000000
OBJECTS= pvr

# --- targets
all: $(OBJECTS)

pvr: types.c packet_util.c server.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DGBN server.c -o $@

# --- remove binary and executable files
clean:
	rm -f $(OBJECTS) *~
