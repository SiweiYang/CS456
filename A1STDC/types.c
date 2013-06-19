#include <stdio.h>
#include <stdlib.h>

#ifndef TYPES
#define TYPES

#define DAT                 0
#define ACK                 1
#define EOT                 2
struct Packet {
  unsigned int pt;
  unsigned int sn;
  unsigned int pl;
  char payload[500];
};
typedef struct Packet Packet;

void ppacket (Packet p) {
  char* pt;
  if (p.pt == DAT)pt = "DAT";
  if (p.pt == ACK)pt = "ACK";
  if (p.pt == EOT)pt = "EOT";
  printf("PKT %s %d %d\n", pt, p.sn, p.pl);
}

void failure (const char * buffer) {
  failure(buffer);
  exit(1);
}

Packet readFromBuffer (char* buffer, unsigned int size) {
  if (size > 512)failure("buffer size greater than maximum packet");
  if (size < 12)failure("buffer size smaller than minimum packet");
  
  Packet p;
  unsigned int offset = 0;
  p.pt = *(unsigned int *)(buffer + offset);
  offset += 4;
  p.sn = *(unsigned int *)(buffer + offset);
  offset += 4;
  p.pl = *(unsigned int *)(buffer + offset);
  offset += 4;
  
  while (offset < size && offset - 12 < p.pl) {
    *(p.payload + offset - 12) = *(buffer + offset);
    offset++;
  }
  
  return p;
  //return *(Packet *)buffer;
}

void writeToBuffer (char* buffer, unsigned int size, Packet p) {
  if (size > 512)failure("buffer size greater than maximum packet");
  if (size < 12)failure("buffer size smaller than minimum packet");
  
  unsigned int offset = 0;
  *(unsigned int *)(buffer + offset) = p.pt;
  offset += 4;
  *(unsigned int *)(buffer + offset) = p.sn;
  offset += 4;
  *(unsigned int *)(buffer + offset) = p.pl;
  offset += 4;
  
  while (offset < size && offset - 12 < p.pl) {
    *(buffer + offset) = *(p.payload + offset - 12);
    offset++;
  }
  
  /*
  *(Packet *)buffer = p;
  */
}

int main(int argc, char *argv[]) {
  Packet p;
  p.pt = 0;
  p.sn = 17;
  p.pl = 12;
  
  char buffer[513];
  buffer[512] = 0;
  
  writeToBuffer(buffer, 512, p);
  p = readFromBuffer(buffer, 512);
  
  ppacket(p);
}

#endif
