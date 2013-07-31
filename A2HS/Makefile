# --- macros
CC=ghc
CFLAGS= -debug
#LDFLAGS= -L-Wl,--stack_size,0x1000000000000
OBJECTS= pvr loop

# --- targets
all: $(OBJECTS)

pvr: Router.hs RouterIO.hs Main.hs
	$(CC) $(CFLAGS) $(LDFLAGS) --make Main -o $@

loop: loop.sh
	cp loop.sh loop;chmod +x loop;

# --- remove binary and executable files
clean:
	rm -f $(OBJECTS) *~ *.hi *.o table.* start.table.* loop.table.* resolved.table.* edges
