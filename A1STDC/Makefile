# --- macros
CC=gcc
CFLAGS= -g -O2
OBJECTS= gbnReceiver gbnSender srReceiver srSender

# --- targets
all: $(OBJECTS)

gbnReceiver: types.c packet_util.c server.c
	$(CC) $(CFLAGS) -DGBN server.c -o $@

gbnSender: types.c packet_util.c client.c
	$(CC) $(CFLAGS) -DGBN client.c -o $@

srReceiver: types.c packet_util.c server.c
	$(CC) $(CFLAGS) -DSR server.c -o $@

srSender: types.c packet_util.c client.c
	$(CC) $(CFLAGS) -DSR client.c -o $@


# --- remove binary and executable files
clean:
	rm -f $(OBJECTS) file client server recvInfo channelInfo *~
