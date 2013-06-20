#include <stdio.h>
#include <stdlib.h>

#ifndef TYPES
#define TYPES

#define DAT                 0
#define ACK                 1
#define EOT                 2

#define PACKET_SIZE_MIN     12
#define PACKET_SIZE_MAX     512

#define WINDOW_SIZE       10
struct Packet {
  unsigned int pt;
  unsigned int sn;
  unsigned int pl;
  char payload[500];
};
typedef struct Packet Packet;

unsigned int EDSwap (unsigned int word) {
  return    ((word>>24) &0xff) | // move byte 3 to byte 0
            ((word<<8)  &0xff0000) | // move byte 1 to byte 2
            ((word>>8)  &0xff00) | // move byte 2 to byte 1
            ((word<<24) &0xff000000); // byte 0 to byte 3
}

void ppacket (Packet p, char* act) {
  char* pt;
  
  if (p.pt == DAT)pt = "DAT";
  if (p.pt == ACK)pt = "ACK";
  if (p.pt == EOT)pt = "EOT";
  printf("PKT %s %s %d %d\n", act, pt, p.sn, p.pl);
}

void failure (const char * buffer) {
  perror(buffer);
}

void exception (const char * buffer) {
  perror(buffer);
  exit(1);
}

Packet readFromBuffer (char* buffer, unsigned int size) {
  if (size > PACKET_SIZE_MAX)failure("buffer size greater than maximum packet");
  if (size < PACKET_SIZE_MIN)failure("buffer size smaller than minimum packet");
  
  Packet p;
  unsigned int offset = 0;
  p.pt = EDSwap(*(unsigned int *)(buffer + offset));
  offset += 4;
  p.sn = EDSwap(*(unsigned int *)(buffer + offset));
  offset += 4;
  p.pl = EDSwap(*(unsigned int *)(buffer + offset));
  offset += 4;
  if (p.pl > PACKET_SIZE_MAX)p.pl = PACKET_SIZE_MAX;
  
  while (offset < size && offset - 12 < p.pl) {
    *(p.payload + offset - 12) = *(buffer + offset);
    offset++;
  }
  
  return p;
  //return *(Packet *)buffer;
}

void writeToBuffer (char* buffer, unsigned int size, Packet p) {
  if (size > PACKET_SIZE_MAX)failure("buffer size greater than maximum packet");
  if (size < PACKET_SIZE_MIN)failure("buffer size smaller than minimum packet");
  
  unsigned int offset = 0;
  *(unsigned int *)(buffer + offset) = EDSwap(p.pt);
  offset += 4;
  *(unsigned int *)(buffer + offset) = EDSwap(p.sn);
  offset += 4;
  *(unsigned int *)(buffer + offset) = EDSwap(p.pl);
  offset += 4;
  
  while (offset < size && offset - 12 < p.pl) {
    *(buffer + offset) = *(p.payload + offset - 12);
    offset++;
  }
  
  /*
  *(Packet *)buffer = p;
  */
}

#ifdef TYPES_TEST
int main(int argc, char *argv[]) {
  Packet p;
  p.pt = 0;
  p.sn = 17;
  p.pl = 12;
  
  char buffer[PACKET_SIZE_MAX+1];
  buffer[PACKET_SIZE_MAX] = 0;
  
  writeToBuffer(buffer, PACKET_SIZE_MAX, p);
  p = readFromBuffer(buffer, PACKET_SIZE_MAX);
  
  ppacket(p, "TEST");
}
#endif

#endif
