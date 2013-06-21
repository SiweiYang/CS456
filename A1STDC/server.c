/* A simple server in the internet domain using TCP
   The port number is passed as an argument */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>

#include "types.c"
#include "packet_util.c"

Stack iteration (int sockfd, char* buffer, Stack stack) {  
  bzero(buffer, PACKET_SIZE_MAX);
  
  struct sockaddr_in serv_addr;
  socklen_t servlen = sizeof(serv_addr);
  
  int n = recvfrom(sockfd, buffer, PACKET_SIZE_MAX, 0, (struct sockaddr *)&serv_addr ,&servlen);  
  if (n < 0)exception("ERROR reading from socket");
  //printf("READ %d bytes fom a reply\n", n);
  
  Packet p = readFromBuffer(buffer, n);
  while (p.sn < stack.window_low)p.sn += 32;
  ppacket(p, "RECV");
  // only possible exit
  if (p.pt == EOT) {
    if (p.sn != checkStackWindow(stack)) {
      failure("Packet SN out of order on EOT");
      
      return stack;
    }
    ppacket(p, "SEND");
    stack = addPacket(stack, p);
    
    p.sn = p.sn % 32;
    writeToBuffer(buffer, PACKET_SIZE_MAX, p);
    int n = sendto(sockfd, buffer, p.pl, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));    
    if (n < 0)exception("ERROR writing to socket");
    
    stack.size = 0;
    return stack;
  }
  
  if (p.pt != DAT) {
    failure("Packet Type other than EOT and DAT aren't supportted");
    
    return stack;
  }
  if (p.sn < stack.window_low || p.sn > stack.window_high) {
    failure("Packet SN out of order");
    
    return stack;
  }
  
  stack = updateStackWindow(stack, p);
  p.pt = ACK;
  stack = addPacket(stack, p);  
  
  p.pl = PACKET_SIZE_MIN;
#ifdef GBN
  unsigned int cursor = checkStackWindow(stack);
  // don't reply if no packet is ready
  if (cursor) {
    p.sn = cursor - 1;
    ppacket(p, "SEND");
    p.sn = p.sn % 32;
    writeToBuffer(buffer, PACKET_SIZE_MAX, p);
    int n = sendto(sockfd, buffer, p.pl, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    if (n < 0)exception("ERROR writing to socket");
  }
#endif
#ifdef SR
  ppacket(p, "SEND");
  {
    p.sn = p.sn % 32;
    writeToBuffer(buffer, PACKET_SIZE_MAX, p);
    int n = sendto(sockfd, buffer, p.pl, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    if (n < 0)exception("ERROR writing to socket");
  }
#endif
  return iteration(sockfd, buffer, stack);
}

#define CONNECTION_INFO         "recvInfo"
int main(int argc, char *argv[])
{
    if (argc < 2) {
     fprintf(stderr,"usage %s <filename>\n", argv[0]);
     exit(0);
    }
#ifdef GBN
    printf("Running Receiver based on GBN semantics\n");
#endif
#ifdef SR
    printf("Running Receiver based on SR semantics\n");
#endif

    int sockfd;
    struct sockaddr_in serv_addr;
    socklen_t srvlen = sizeof(serv_addr);
    sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sockfd < 0)exception("ERROR opening socket");
    bzero(&serv_addr, sizeof(serv_addr));

    char* filename = argv[1];

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(0);
    if (bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0)exception("ERROR on binding");

    // record the address info
    char hostname[513];
    hostname[512] = 0;
    if (gethostname(hostname, 512) != 0)exception("ERROR obtaining hostname");    
    if (getsockname(sockfd, (struct sockaddr *) &serv_addr, &srvlen) != 0)exception("ERROR obtaining sockname");
    unsigned short portnumber = ntohs(serv_addr.sin_port);
    printf("Listening on %s:%u with protocol % d\n", hostname, portnumber, IPPROTO_UDP);
    
    {
      FILE* fp;
      fp=fopen(CONNECTION_INFO,"w");
      if(fp==NULL)exception("file not found\n");
      
      fprintf(fp, "%s %d", hostname, portnumber);
      
      fclose(fp);
    }

    // start receiving transmission
    char buffer[PACKET_SIZE_MAX+1];
    Stack stack = createStack(STACK_SIZE_MULTIPLIER);
    stack.window_high += WINDOW_SIZE;
    while (stack.size > 0)stack = iteration(sockfd, buffer, stack);    
    close(sockfd);
    
    {
      FILE* fp;
      fp=fopen(filename,"wb");
      if(fp==NULL)exception("file not found\n");
      
      unsigned int sn = 0;
      while (1) {
        Packet* p_ptr = stack.packets + sn;
        if (p_ptr->pt == EOT)break;
        
        fwrite(p_ptr->payload, p_ptr->pl - PACKET_SIZE_MIN, 1, fp);
        if(ferror(fp))exception("file IO failure\n");
        sn++;
      }
      
      fclose(fp);
    }
    
    return 0; 
}

