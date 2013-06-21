#include "types.c"
#include <limits.h>

#ifndef PACKET_UTIL
#define PACKET_UTIL

#define STACK_SIZE_MULTIPLIER     23
struct Stack {
  unsigned int size;
  unsigned int window_low;
  unsigned int window_high;
  Packet * packets;
  unsigned int * timeouts;
};
typedef struct Stack Stack;

Stack createStack (unsigned int size) {
  printf("Creating Stack of size %d\n", size);
  // calloc auto-set 0
  // extra one space for EOT
  void * p_ptr = calloc(size+1, sizeof(Packet));
  //bzero(p_ptr, size * sizeof(Packet));
  void * t_ptr = calloc(size+1, sizeof(unsigned int));
  //bzero(t_ptr, size * sizeof(unsigned int));
  Stack s;
  s.size = size;
  s.window_low = 0;
  s.window_high = -1 + WINDOW_SIZE;
  s.packets = (Packet *)p_ptr;
  s.timeouts = (unsigned int *)t_ptr;
  
  return s;
}

Stack expandStack (Stack stack, unsigned int margin) {
  unsigned int size = stack.size + margin;
  printf("Expanding Stack by size of %d\n", margin);
  Stack s = createStack(size);
  s.window_low = stack.window_low;
  s.window_high = stack.window_high;
  
  memcpy(s.packets, stack.packets, (stack.size+1) * sizeof(Packet));
  memcpy(s.timeouts, stack.timeouts, (stack.size+1) * sizeof(unsigned int));
  
  return s;
}

void destroyStack (Stack stack) {
  free(stack.packets);
  free(stack.timeouts);
}

// changes written to heap memory direcly
Stack modPacket (Stack stack, Packet p, unsigned int t) {
  while (stack.size <= p.sn) {
    stack = expandStack(stack, STACK_SIZE_MULTIPLIER);
  }
  if (t > 0)printf("MOD PKT %d to timeout at %d\n", p.sn, t);
  
  Packet * packet = stack.packets + p.sn;
  *packet = p;
  unsigned int * timeout = stack.timeouts + p.sn;
  *timeout = t;
  
  return stack;
}
Stack addPacket (Stack stack, Packet p) {return modPacket(stack, p, 0);}

unsigned int checkStackWindow(Stack stack) {
  unsigned int adv = 0;
  while (stack.window_low + adv < stack.size) {
    if ((stack.packets + stack.window_low + adv)->pt != ACK)break;
    adv++;
  }
  
  return stack.window_low + adv;
}

unsigned int cleanStackWindow(Stack stack) {
  unsigned int cursor = stack.window_high;
  while (cursor > stack.window_low) {
    Packet * p_ptr = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    
    if (p_ptr->pl > 0 || t > 0)return cursor;
    cursor++;
  }
  
  return cursor;
}

Stack passiveUpdateStackWindow(Stack stack) {
  unsigned int adv = checkStackWindow(stack) - stack.window_low;
  
  stack.window_low += adv;
  stack.window_high += adv;
  
  return stack;
}

unsigned int timeoutStackWindow(Stack stack) {
  //printf("Find Free Packet between %d and %d\n", stack.window_low, stack.window_high);
  unsigned int timeout = UINT_MAX;
  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high && cursor <= stack.size) {
    Packet * p_ptr = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    //printf("Packet %d timeout at %d\n", p_ptr->sn, t);
    if ((p_ptr->pt == DAT || p_ptr->pt == EOT) && t > 0 && t < timeout)timeout = t;
    cursor++;
  }
  printf("Next timeout is %d\n", timeout);
  
  if (timeout == UINT_MAX)failure("Requesting next timeout without active transmission");
  return timeout;
}

Stack updateStackWindow (Stack stack, Packet p) {
  if (p.sn < stack.window_low || p.sn > stack.window_high || p.sn > stack.size)failure("Packet SN out of bound");
  
  if (p.pt == DAT) {
    // receiver can choose to not have a sending window
    /*
#ifdef GBN    
    unsigned int adv = p.sn - stack.window_low + 1;
    stack.window_low += adv;
    stack.window_high += adv;
    
#endif

#ifdef SR
    stack = updateStackWindow(stack);
#endif
    */
  }
  
  if (p.pt == ACK) {
#ifdef GBN
    unsigned int adv = p.sn - stack.window_low + 1;
    stack.window_low += adv;
    stack.window_high += adv;
#endif

#ifdef SR
    stack = passiveUpdateStackWindow(stack);
#endif
  }
  
  return stack;
}

Packet * findFreePacket (Stack stack) {
  //printf("Find Free Packet between %d and %d\n", stack.window_low, stack.window_high);
  
  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high && cursor <= stack.size) {
    Packet * p_ptr = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    // only send fresh DAT packet
    // skip over uninitialized packet though there shouldn't be any    
    if (p_ptr->pt == DAT && p_ptr->pl > 0 && t == 0) {
      //printf("Packet at %d with SN %d\n", cursor, p_ptr->sn);
      return p_ptr;
    }
    cursor++;
  }
  
  //printf("No Free Packet\n");
  return NULL;
}

Packet * findActivePacket (Stack stack, unsigned int ct) {
  //printf("Find Active Packet between %d and %d\n", stack.window_low, stack.window_high);

  unsigned int cursor = stack.window_low;
  while (cursor <= stack.window_high && cursor <= stack.size) {
    Packet * p_ptr = stack.packets+cursor;
    unsigned int t = *(stack.timeouts+cursor);
    // may need to resend EOT so can't filter with only DAT
    if (t > 0 && t <= ct) {
      //printf("Packet at %d with SN %d\n", cursor, p_ptr->sn);
      return p_ptr;
    }
    cursor++;
  }
  
  //printf("No Active Packet\n");
  return NULL;
}
#endif
