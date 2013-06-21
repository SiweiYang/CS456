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


Stack iteration (int servfd, char* buffer, Stack stack) {  
  bzero(buffer, PACKET_SIZE_MAX);
  
  struct sockaddr_in serv_addr;
  socklen_t servlen = sizeof(serv_addr);
  
  int n = recvfrom(servfd, buffer, PACKET_SIZE_MAX, 0, (struct sockaddr *)&serv_addr ,&servlen);  
  if (n < 0)exception("ERROR reading from socket");
  //printf("READ %d bytes fom a reply\n", n);
  
  int sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  //if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(servlen)) < 0)exception("ERROR connecting");
  
  Packet p = readFromBuffer(buffer, n);
  ppacket(p, "RECV");
  // only possible exit
  if (p.pt == EOT) {
    if (p.sn != checkStackWindow(stack)) {
      failure("Packet SN out of order on EOT");
      
      close(sockfd);
      return iteration(sockfd, buffer, stack);
    }
    ppacket(p, "SEND");
    writeToBuffer(buffer, PACKET_SIZE_MAX, p);
    int n = sendto(sockfd, buffer, p.pl, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));    
    if (n < 0)exception("ERROR writing to socket");
    stack = addPacket(stack, p);
    
    close(sockfd);
    return stack;
  }
  
  if (p.pt != DAT) {
    failure("Packet Type other than EOT and DAT aren't supportted");
    
    close(sockfd);
    return iteration(sockfd, buffer, stack);
  }
  if (p.sn < stack.window_low || p.sn > stack.window_high) {
    failure("Packet SN out of order");
    
    close(sockfd);
    return iteration(servfd, buffer, stack);
  }
  
  
  p.pt = ACK;
  stack = addPacket(stack, p);
  // dummy window size management for now
  stack.window_high = stack.size;
  //stack = updateStackWindow(stack, p);
  
  p.pl = PACKET_SIZE_MIN;
#ifdef GBN
  p.sn = checkStackWindow(stack) - 1;
#endif
#ifdef SR
  // don't need to do anything
#endif
  ppacket(p, "SEND");
  {
    writeToBuffer(buffer, PACKET_SIZE_MAX, p);
    int n = sendto(sockfd, buffer, p.pl, 0, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    if (n < 0)exception("ERROR writing to socket");
  }
  
  close(sockfd);
  return iteration(servfd, buffer, stack);
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
    stack.window_high = stack.size;
    stack = iteration(sockfd, buffer, stack);
    
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
    close(sockfd);
    return 0; 
}

