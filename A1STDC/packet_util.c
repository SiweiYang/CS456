#include "types.c"
#include <limits.h>

#ifndef PACKET_UTIL
#define PACKET_UTIL

#define WINDOW_SIZE       10
struct Stack {
  unsigned int size;
  unsigned int window_low;
  unsigned int window_high;
  Packet * packets;
  unsigned int * timeouts;
};
typedef struct Stack Stack;

Stack createStack (unsigned int size) {
  // calloc auto-set 0
  void * p_ptr = calloc(size, sizeof(Packet));
  //bzero(p_ptr, size * sizeof(Packet));
  void * t_ptr = calloc(size, sizeof(unsigned int));
  //bzero(t_ptr, size * sizeof(unsigned int));
  Stack s;
  s.size = size;
  s.window_low = 0;
  s.window_high = -1 + WINDOW_SIZE;
  s.packets = (Packet *)p_ptr;
  s.timeouts = (unsigned int *)t_ptr;
  
  return s;
}

void destroyStack (Stack stack) {
  free(stack.packets);
  free(stack.timeouts);
}

// changes written to heap memory direcly
Stack modPacket (Stack stack, Packet p, unsigned int t) {
  if (p.sn >= stack.size)failure("Packet don't fit into RTP Stack\n");
  
  Packet * packet = stack.packets + p.sn;
  *packet = p;
  unsigned int * timeout = stack.timeouts + p.sn;
  *timeout = t;
  
  return stack;
}
Stack addPacket (Stack stack, Packet p) {return modPacket(stack, p, 0);}

Stack updateStackWindow(Stack stack) {
  unsigned int adv = 0;
  while (stack.window_low + adv < stack.size) {
    if ((stack.packets + stack.window_low + adv)->pt == ACK)adv++;
  }
  
  stack.window_low += adv;
  stack.window_high += adv;
  
  return stack;
}

unsigned int timeoutStackWindow(Stack stack) {
  unsigned int timeout = UINT_MAX;
  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high) {
    Packet * p_ptr = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    if ((p_ptr->pt == DAT || p_ptr->pt == EOT) && t > 0 && t < timeout)timeout = t;
    cursor++;
  }
  
  if (timeout == UINT_MAX)failure("Requesting next timeout without active transmission");
  return timeout;
}

void onReceive (Stack stack, Packet p) {
  if (p.sn < stack.window_low || p.sn > stack.window_high)failure("Packet SN out of bound");
  
  if (p.pt == DAT) {
    stack = addPacket(stack, p);
#ifdef GBN
    stack.window_low = p.sn + 1;
    stack.window_high = p.sn + WINDOW_SIZE;
#endif

#ifdef SR
    stack = updateStackWindow(stack);
#endif
  }
  
  if (p.pt == ACK) {
    stack = addPacket(stack, p);
#ifdef GBN
    stack.window_low = p.sn + 1;
    stack.window_high = p.sn + WINDOW_SIZE;
#endif

#ifdef SR
    stack = updateStackWindow(stack);
#endif
  }
  
  return stack;
}

Packet * findFreePacket (Stack stack) {
  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high) {
    Packet * p_prt = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    if (p_prt->pt == DAT && t == 0)return p_prt;
    cursor++;
  }
  
  return NULL;
}

Packet * findActivePacket (Stack stack, unsigned int ct) {
  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high) {
    Packet * p_prt = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    if (t <= ct)return p_prt;
    cursor++;
  }
  
  return NULL;
}
#endif
