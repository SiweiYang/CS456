# --- macros
CC=gcc
CFLAGS= -g -O2
#LDFLAGS= -L-Wl,--stack_size,0x1000000000000
OBJECTS= gbnReceiver gbnSender srReceiver srSender

# --- targets
all: $(OBJECTS)

gbnReceiver: types.c packet_util.c server.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DGBN server.c -o $@

gbnSender: types.c packet_util.c client.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DGBN client.c -o $@

srReceiver: types.c packet_util.c server.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DSR server.c -o $@

srSender: types.c packet_util.c client.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DSR client.c -o $@


# --- remove binary and executable files
clean:
	rm -f $(OBJECTS) file client server recvInfo channelInfo *~
