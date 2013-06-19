/* A simple server in the internet domain using TCP
   The port number is passed as an argument */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>

void error(const char *msg)
{
    perror(msg);
    exit(1);
}

int main(int argc, char *argv[])
{
     int sockfd, newsockfd, portno;
     socklen_t clilen;
     char buffer[513];
     struct sockaddr_in serv_addr, cli_addr;
     int n;
     if (argc < 2) {
         fprintf(stderr,"ERROR, no port provided\n");
         exit(1);
     }
     sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
     if (sockfd < 0) 
        error("ERROR opening socket");
     bzero((char *) &serv_addr, sizeof(serv_addr));
     portno = atoi(argv[1]);
     serv_addr.sin_family = AF_INET;
     serv_addr.sin_addr.s_addr = INADDR_ANY;
     serv_addr.sin_port = htons(portno);
     if (bind(sockfd, (struct sockaddr *) &serv_addr,
              sizeof(serv_addr)) < 0) 
              error("ERROR on binding");
     listen(sockfd,1);
     clilen = sizeof(cli_addr);
     newsockfd = accept(sockfd, 
                 (struct sockaddr *) &cli_addr, 
                 &clilen);
     if (newsockfd < 0) 
          error("ERROR on accept");
     bzero(buffer,512);
     
     fd_set rfds;
     struct timeval tv;
     int retval;

     /* Watch stdin (fd 0) to see when it has input. */
     FD_ZERO(&sockfd);
     FD_SET(0, &sockfd);

     /* Wait up to five seconds. */
     tv.tv_sec = 5;
     tv.tv_usec = 0;

     retval = select(1, &rfds, NULL, NULL, &tv);
     /* Don't rely on the value of tv now! */

     if (retval == -1)perror("select()");
     else if (retval) {
       printf("Data is available now.\n");
       /* FD_ISSET(0, &rfds) will be true. */
       n = read(newsockfd,buffer,512);
       if (n < 0) error("ERROR reading from socket");
       printf("Here is the message: %s\n",buffer);
     } else {
       printf("No data within five seconds.\n");
       n = write(newsockfd,"I got your message",18);
       if (n < 0) error("ERROR writing to socket");
     }
     
     close(newsockfd);
     close(sockfd);
     return 0; 
}

