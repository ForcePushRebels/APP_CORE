/**
 * network_test_main.c
 * Test program for the embedded network library
 */

 #include <stdio.h>
 #include <string.h>
 #include <unistd.h>
 #include <signal.h>
 #include <stdlib.h>
 #include <stdbool.h>
 #include "xNetwork.h"
 
 #define TEST_PORT 8080
 #define BUFFER_SIZE 1024
 
 // Flag to stop the server
 volatile bool running = true;
 
 // Signal handler for clean shutdown
 void sig_handler(int signo) {
     if (signo == SIGINT) {
         printf("\nShutdown requested...\n");
         running = false;
     }
 }
 
 // Server function
 void run_server() {
     printf("Starting server on port %d...\n", TEST_PORT);
     
     // Initialize the network context
     NetworkContext* context = networkInit();
     if (!context) {
         printf("Error: Unable to initialize network context\n");
         return;
     }
     
     // Create a listening socket
     NetworkSocket* serverSocket = networkCreateSocket(context, NETWORK_SOCK_TCP, NETWORK_SOCK_NONBLOCKING);
     if (!serverSocket) {
         printf("Error: Unable to create server socket\n");
         networkCleanup(context);
         return;
     }
     
     // Create the listening address
     NetworkAddress address = networkMakeAddress("0.0.0.0", TEST_PORT);
     
     // Bind the socket to the address
     if (networkBind(serverSocket, &address) != NETWORK_OK) {
         printf("Error: Unable to bind socket to address\n");
         networkCloseSocket(serverSocket);
         networkCleanup(context);
         return;
     }
     
     // Set the socket to listen
     if (networkListen(serverSocket, 5) != NETWORK_OK) {
         printf("Error: Unable to set socket to listen\n");
         networkCloseSocket(serverSocket);
         networkCleanup(context);
         return;
     }
     
     printf("Server listening on port %d\n", TEST_PORT);
     
     // Server main loop
     while (running) {
         // Accept a connection
         NetworkAddress clientAddr;
         NetworkSocket* clientSocket = networkAccept(serverSocket, &clientAddr);
         
         if (clientSocket) {
             printf("New connection from %s:%d\n", clientAddr.t_cAddress, clientAddr.t_usPort);
             
             // Receive data
             char buffer[BUFFER_SIZE];
             int received = networkReceive(clientSocket, buffer, sizeof(buffer));
             
             if (received > 0) {
                 buffer[received] = '\0';
                 printf("Message received: %s\n", buffer);
                 
                 // Reply to client
                 char response[BUFFER_SIZE];
                 snprintf(response, sizeof(response), "Server received: %s", buffer);
                 networkSend(clientSocket, response, strlen(response));
             }
             
             // Close client connection
             networkCloseSocket(clientSocket);
         }
         
         // Small pause to avoid CPU overload
         usleep(10000);  // 10ms
     }
     
     // Cleanup
     printf("Shutting down server...\n");
     networkCloseSocket(serverSocket);
     networkCleanup(context);
     printf("Server stopped\n");
 }
 
 // Client function
 void run_client(const char* message) {
     printf("Starting client...\n");
     
     // Initialize the network context
     NetworkContext* context = networkInit();
     if (!context) {
         printf("Error: Unable to initialize network context\n");
         return;
     }
     
     // Create a client socket
     NetworkSocket* clientSocket = networkCreateSocket(context, NETWORK_SOCK_TCP, NETWORK_SOCK_BLOCKING);
     if (!clientSocket) {
         printf("Error: Unable to create client socket\n");
         networkCleanup(context);
         return;
     }
     
     // Create server address
     NetworkAddress serverAddr = networkMakeAddress("127.0.0.1", TEST_PORT);
     
     // Connect to server
     printf("Connecting to server...\n");
     if (networkConnect(clientSocket, &serverAddr) != NETWORK_OK) {
         printf("Error: Unable to connect to server\n");
         networkCloseSocket(clientSocket);
         networkCleanup(context);
         return;
     }
     
     printf("Connected to server\n");
     
     // Send a message
     printf("Sending message: %s\n", message);
     if (networkSend(clientSocket, message, strlen(message)) < 0) {
         printf("Error: Unable to send message\n");
         networkCloseSocket(clientSocket);
         networkCleanup(context);
         return;
     }
     
     // Receive response
     char buffer[BUFFER_SIZE];
     int received = networkReceive(clientSocket, buffer, sizeof(buffer));
     
     if (received > 0) {
         buffer[received] = '\0';
         printf("Server response: %s\n", buffer);
     } else {
         printf("No response from server or error\n");
     }
     
     // Cleanup
     networkCloseSocket(clientSocket);
     networkCleanup(context);
     printf("Client finished\n");
 }
 
 // Main function
 int main(int argc, char* argv[]) {
     // Configure signal handler
     signal(SIGINT, sig_handler);
     
     if (argc < 2) {
         printf("Usage: %s [server|client <message>]\n", argv[0]);
         return 1;
     }
     
     if (strcmp(argv[1], "server") == 0) {
         run_server();
     } else if (strcmp(argv[1], "client") == 0) {
         if (argc < 3) {
             printf("Error: Client requires a message\n");
             printf("Usage: %s client <message>\n", argv[0]);
             return 1;
         }
         run_client(argv[2]);
     } else {
         printf("Unknown mode: %s\n", argv[1]);
         printf("Usage: %s [server|client <message>]\n", argv[0]);
         return 1;
     }
     
     return 0;
 }
 