#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "types.c"
#include "packet_util.c"

void error(const char *msg)
{
    perror(msg);
    exit(0);
}

void iteration (int sockfd, struct sockaddr * dest_addr, char* buffer, Stack stack, time_t start, unsigned int timeout) {
  bzero(buffer, PACKET_SIZE_MAX);
  time_t timer;
  time(&timer);
  unsigned int ct = (unsigned int)(difftime(timer, start) * 1000);
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
    return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
  }
  
  p_ptr = findActivePacket(stack, ct);
  if (p_ptr != NULL) {
    printf("Sending active packet\n");
    ppacket(*p_ptr, "SEND");
    writeToBuffer(buffer, PACKET_SIZE_MAX, *p_ptr);
    int n = sendto(sockfd, buffer, p_ptr->pl, 0, dest_addr, sizeof(*dest_addr));
    if (n < 0)exception("ERROR writing to socket");
    
    stack = modPacket(stack, *p_ptr, ct + timeout);
    return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
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
  
  printf("WAIT %dms\n", wait);
  retval = select(sockfd+1, &rfds, NULL, NULL, &tv);
  
  if (retval == -1)exception("ERROR select()");
  if (retval) {
    int n = read(sockfd,buffer,PACKET_SIZE_MAX);
    if (n < 0)exception("ERROR reading from socket");
    //printf("READ %d bytes fom a reply\n", n);
    
    Packet p = readFromBuffer(buffer, n);
    ppacket(p, "RECV");
    if (p.pt == EOT) {
      if (p.sn != stack.size) {
        failure("Packet SN out of order on EOT");
        return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
      }
      
      return;
    }
    if (p.pt != ACK) {
      failure("Packet type other than ACK shouldn't get here");
      return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
    }
    
    if (p.sn < stack.window_low || p.sn > stack.window_high) {
      failure("Packet SN out of order");
      return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
    }
    
    stack = addPacket(stack, p);
    stack = updateStackWindow(stack, p);
    
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
  
  return iteration(sockfd, dest_addr, buffer, stack, start, timeout);
}

int main(int argc, char *argv[])
{
  // prepare packets for transfer
  if (argc < 2) {
     fprintf(stderr,"usage %s <timeout> <filename>\n", argv[0]);
     exit(0);
  }
  unsigned int timeout = atoi(argv[1]);
  char* filename = argv[2];
  
  Stack stack;
  {
    FILE* fp;
    fp=fopen(filename,"rb");
    if(fp==NULL)exception("file not found\n");
    
    fseek(fp,0L,SEEK_END);
    unsigned int length = ftell(fp);
    fseek(fp,0L,SEEK_SET);
    
    Stack stack = createStack((length + PACKET_SIZE_MAX - 1) / PACKET_SIZE_MAX);
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
      ppacket(*p_ptr, "CREATE");
      
      pos = ftell(fp);
      sn++;
    }  
    fclose(fp);
  }
  
  // establish socket channel
  int sockfd, portno, n;
  
  char hostname[513];
  {
    FILE* fp;
    //fp=fopen("channelInfo","r");
    fp=fopen("recvInfo","r");
    if(fp==NULL)exception("file not found\n");

    fscanf(fp, "%s %d", hostname, &portno);
    printf("Listening on %s:%d\n", hostname, portno);
  }
  struct hostent *server = gethostbyname("localhost");
  if (server == NULL)exception("ERROR, no such host");
  sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (sockfd < 0)exception("ERROR opening socket");
  
  struct sockaddr_in serv_addr;
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(portno);
  bcopy((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr, server->h_length);
  //if (connect(sockfd,(struct sockaddr *) &serv_addr,sizeof(serv_addr)) < 0)exception("ERROR connecting");
  
  
  // start communication
  char buffer[PACKET_SIZE_MAX+1];
  time_t start;
  time(&start);
  iteration(sockfd, (struct sockaddr *)&serv_addr, buffer, stack, start, timeout);
  
  // clear footprint, and exit
  destroyStack(stack);
  close(sockfd);
  return 0;
}

