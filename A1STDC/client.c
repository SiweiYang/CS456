#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>

#include "types.c"
#include "packet_util.c"

void error(const char *msg)
{
    perror(msg);
    exit(0);
}

Stack iteration (int sockfd, struct sockaddr * dest_addr, char* buffer, Stack stack, struct timeval start, unsigned int timeout) {
  bzero(buffer, PACKET_SIZE_MAX);
  struct timeval timer;
  struct timezone tz;
  gettimeofday(&timer, &tz);
  //printf("Current time is %lu %lu\n", (long)timer.tv_sec, (long)timer.tv_usec);
  unsigned int ct = (unsigned int)((timer.tv_sec - start.tv_sec) * 1000 +  (timer.tv_usec - start.tv_usec) / 1000);
  //printf("Current time is %d\n", ct);
  
  Packet* p_ptr;
  p_ptr = findFreePacket(stack);
  if (p_ptr != NULL) {
    printf("Sending free packet\n");
    ppacket(*p_ptr, "SEND");
    writeToBuffer(buffer, PACKET_SIZE_MAX, *p_ptr);
    int n = sendto(sockfd, buffer, p_ptr->pl, 0, dest_addr, sizeof(*dest_addr));
    if (n < 0)exception("ERROR writing to socket");
    
    stack = modPacket(stack, *p_ptr, ct + timeout);
    return stack;
  }
  
  p_ptr = findActivePacket(stack, ct);
  if (p_ptr != NULL) {
    printf("Sending active packet\n");
    ppacket(*p_ptr, "SEND");
    writeToBuffer(buffer, PACKET_SIZE_MAX, *p_ptr);
    int n = sendto(sockfd, buffer, p_ptr->pl, 0, dest_addr, sizeof(*dest_addr));
    if (n < 0)exception("ERROR writing to socket");
    
    stack = modPacket(stack, *p_ptr, ct + timeout);
    return stack;
  }
  
  unsigned int wait = timeoutStackWindow(stack) - ct;
  fd_set rfds;
  struct timeval tv;
  int retval;

  /* Watch stdin (fd 0) to see when it has input. */
  FD_ZERO(&rfds);
  FD_SET(sockfd, &rfds);

  /* Wait up to five seconds. */
  tv.tv_sec = 0;
  tv.tv_usec = 1000 * wait;
  
  printf("Current time is %d WAIT %dms\n", ct, wait);
  retval = select(sockfd+1, &rfds, NULL, NULL, &tv);
  
  if (retval == -1)exception("ERROR select()");
  if (retval) {
    int n = read(sockfd,buffer,PACKET_SIZE_MAX);
    if (n < 0)exception("ERROR reading from socket");
    //printf("READ %d bytes fom a reply\n", n);
    
    Packet p = readFromBuffer(buffer, n);
    ppacket(p, "RECV");
    if (p.pt == EOT) {
      p_ptr = stack.packets + p.sn;
      if (p_ptr->pt != EOT) {
        failure("Packet SN out of order on EOT");
        return stack;
      }
      
      stack.size = 0;
      return stack;
    }
    if (p.pt != ACK) {
      failure("Packet type other than ACK shouldn't get here");
      return stack;
    }
    
    if (p.sn < stack.window_low || p.sn > stack.window_high) {
      failure("Packet SN out of order");
      return stack;
    }
    
    stack = addPacket(stack, p);
    stack = updateStackWindow(stack, p);
    // dummy window size management for now
    if (stack.window_high > stack.size)stack.window_high = stack.size;
    printf("Update Stack Window to %d and %d\n", stack.window_low, stack.window_high);
    
    // send EOT since all DAT transmissions are done
    if (stack.window_low == stack.size) {
       p.pt = EOT;
       p.sn = stack.size;
       p.pl = PACKET_SIZE_MIN;
       
       ppacket(p, "SEND");
       writeToBuffer(buffer, PACKET_SIZE_MAX, p);
       n = sendto(sockfd, buffer, p.pl, 0, dest_addr, sizeof(*dest_addr));
       if (n < 0)exception("ERROR writing to socket");
       
       stack = modPacket(stack, p, ct + timeout);
    }
  } else failure("TIMEOUT");
  
  return stack;
}

#define CONNECTION_INFO         "channelInfo"
int main(int argc, char *argv[])
{
  // prepare packets for transfer
  if (argc < 2) {
     fprintf(stderr,"usage %s <timeout> <filename>\n", argv[0]);
     exit(0);
  }
#ifdef GBN
    printf("Running Sender based on GBN semantics\n");
#endif
#ifdef SR
    printf("Running Sender based on SR semantics\n");
#endif

  unsigned int timeout = atoi(argv[1]);
  char* filename = argv[2];
  
  char hostname[513];
  int portnumber;
  {
    FILE* fp;
    fp=fopen(CONNECTION_INFO,"r");
    if(fp==NULL)exception("connection info file not found\n");

    fscanf(fp, "%s %d", hostname, &portnumber);
    printf("Listening on %s:%d\n", hostname, portnumber);
  }
  
  // establish socket channel
  int sockfd, n;
  struct hostent *server = gethostbyname("localhost");
  if (server == NULL)exception("ERROR, no such host");
  sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (sockfd < 0)exception("ERROR opening socket");
  
  struct sockaddr_in serv_addr;
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(portnumber);
  bcopy((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr, server->h_length);
  
  // prepare a stack for transfer
  Stack stack;
  {
    FILE* fp;
    fp=fopen(filename,"rb");
    if(fp==NULL)exception("file not found\n");
    
    fseek(fp,0L,SEEK_END);
    unsigned int length = ftell(fp);
    fseek(fp,0L,SEEK_SET);
    
    stack = createStack((length + PACKET_DATA_SIZE_MAX - 1) / PACKET_DATA_SIZE_MAX);
    Packet p;
    unsigned int pos = ftell(fp);
    unsigned int sn = 0;
    while (ftell(fp) < length) {
      Packet* p_ptr = stack.packets + sn;
      p_ptr->pt = DAT;
      p_ptr->sn = sn;
      
      fread(p_ptr->payload, PACKET_SIZE_MAX - PACKET_SIZE_MIN, 1, fp);
      if(ferror(fp))exception("file IO failure\n");
      p_ptr->pl = ftell(fp) - pos + 12;
      //ppacket(*p_ptr, "CREATE");
      
      pos = ftell(fp);
      sn++;
    }  
    fclose(fp);
  }
  
  // start communication
  char buffer[PACKET_SIZE_MAX+1];
  struct timeval start;
  struct timezone tz;
  gettimeofday(&start, &tz);
  while (stack.size > 0)stack = iteration(sockfd, (struct sockaddr *)&serv_addr, buffer, stack, start, timeout);
  
  // clear footprint, and exit
  destroyStack(stack);
  close(sockfd);
  return 0;
}

