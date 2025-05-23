////////////////////////////////////////////////////////////
//  IDCard source file
//  Provides IDCard interface
//
// general disclosure: copy or share the file is forbidden
// Written : 19/05/2025
////////////////////////////////////////////////////////////

#include "idCard.h"
#include "xNetwork.h"
#include "networkEncode.h"
#include "xLog.h"
#include "xTask.h"
#include <ifaddrs.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

static char s_pcRobotName[] = "Robot_zebi_putin_de_merde";
static char s_pcIpAddr[16] = {0};
static RobotType_t s_iRole = 0;
static bool s_bUseLoopback = true; // boolean to enable/disable the use of loopback
static xOsTaskCtx s_xTaskHandle = {0};


///////////////////////////////////////////
/// findIpAddress
///////////////////////////////////////////

int idCardInit(RobotType_t type){
    s_iRole = type;
}


static void findIpAddress(void)
{
    struct ifaddrs *ifaddr, *ifa;
    int family;
    char host[16];
    bool found = false;

    if (getifaddrs(&ifaddr) == -1)
    {
        perror("getifaddrs");
        return;
    }

    // if loopback is used, search first the loopback interface
    if (s_bUseLoopback)
    {
        for (ifa = ifaddr; ifa != NULL && !found; ifa = ifa->ifa_next)
        {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET)
            { // IPv4
                // search specifically the loopback interface
                if (strcmp(ifa->ifa_name, "lo") == 0)
                {
                    void *addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
                    inet_ntop(AF_INET, addr_ptr, host, 16);
                    // copy the IP address to the static variable
                    strncpy(s_pcIpAddr, host, sizeof(s_pcIpAddr) - 1);
                    s_pcIpAddr[sizeof(s_pcIpAddr) - 1] = '\0';
                    found = true;
                    break;
                }
            }
        }
    }

    // if loopback is not used or if no loopback interface is found
    if (!s_bUseLoopback || !found)
    {
        for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)
        {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET)
            { // IPv4
                // ignore the loopback interface
                if (strcmp(ifa->ifa_name, "lo") != 0)
                {
                    void *addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
                    inet_ntop(AF_INET, addr_ptr, host, 16);
                    // copy the IP address to the static variable
                    strncpy(s_pcIpAddr, host, sizeof(s_pcIpAddr) - 1);
                    s_pcIpAddr[sizeof(s_pcIpAddr) - 1] = '\0';
                    found = true;
                    break; // take the first IPv4 non loopback address
                }
            }
        }
    }

    // if no address is found, use 127.0.0.1 by default
    if (!found)
    {
        strcpy(s_pcIpAddr, "127.0.0.1");
    }

    freeifaddrs(ifaddr);

    X_LOG_TRACE("IP address found: %s", s_pcIpAddr);
}

///////////////////////////////////////////
/// createManifest
///////////////////////////////////////////
int createManifest(manifest_t *p_ptManifest)
{
    X_ASSERT(p_ptManifest != NULL);
    memset(p_ptManifest, 0, sizeof(manifest_t));

    findIpAddress();

    strcpy(p_ptManifest->t_pcRobotName, s_pcRobotName);
    strcpy(p_ptManifest->t_pcIpAddr, s_pcIpAddr);
    p_ptManifest->t_iRole = s_iRole;

    return 0;
}

///////////////////////////////////////////
/// idCardNetworkInit
///////////////////////////////////////////
void idCardNetworkInit(void)
{
    int l_iReturn = 0;
    
    // Initialiser la gestion des tâches
    l_iReturn = osTaskInit(&s_xTaskHandle);
    if (l_iReturn != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("ERROR: Failed to initialize task: %s", osTaskGetErrorString(l_iReturn));
        return;
    }
    
    // Configurer le handler comme fonction de tâche
    s_xTaskHandle.t_ptTask = handleIsAnyRobotHere;
    s_xTaskHandle.t_ptTaskArg = NULL;
    
    // Ensure the stop flag is correctly reset before creating the task
    atomic_store(&s_xTaskHandle.a_iStopFlag, OS_TASK_SECURE_FLAG);
    
    // Créer la tâche
    l_iReturn = osTaskCreate(&s_xTaskHandle);
    if (l_iReturn != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("ERROR: Failed to create UDP discovery task: %s", osTaskGetErrorString(l_iReturn));
        return;
    }
    
    X_LOG_TRACE("UDP discovery task started successfully");
}

///////////////////////////////////////////
/// handleIsAnyRobotHere
///////////////////////////////////////////
void* handleIsAnyRobotHere(void* p_pvArg)
{
    (void)p_pvArg; // unused argument avoid warning

    int l_iReturn = 0;
    char l_pcBuffer[16];
    manifest_t l_sManifest = {0};
    uint8_t l_ucSendBuffer[3 + sizeof(manifest_t)];
    
    // ensure the buffer is clean
    memset(l_ucSendBuffer, 0, sizeof(l_ucSendBuffer));

    // create the UDP socket
    NetworkSocket *l_ptSocket = networkCreateSocket(NETWORK_SOCK_UDP);
    X_ASSERT(l_ptSocket != NULL);
    X_LOG_TRACE("UDP socket created successfully");

    // use 0.0.0.0 to listen on all interfaces
    NetworkAddress l_tAddress = networkMakeAddress("127.0.0.1", 13769);
    NetworkAddress l_tSenderAddr;

    l_iReturn = networkBind(l_ptSocket, &l_tAddress);
    if (l_iReturn != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to bind socket: %s", networkGetErrorString(l_iReturn));
        networkCloseSocket(l_ptSocket);
    }

    // prepare the manifest
    l_iReturn = createManifest(&l_sManifest);
    X_ASSERT(l_iReturn == 0);
    
    uint16_t payloadSize = (uint16_t)sizeof(manifest_t);
    
    // build the buffer according to the network_message_t structure with pointer approach
    uint8_t* ptr = l_ucSendBuffer;
    
    // write the payload size (2 bytes)
    *ptr++ = (uint8_t)((payloadSize >> 8) & 0xFF);  // MSB (most significant byte)
    *ptr++ = (uint8_t)(payloadSize & 0xFF);         // LSB (least significant byte)
    
    // write the message type (1 byte)
    *ptr++ = ID_MANIFEST;
    
    // copy the manifest to the buffer
    memcpy(ptr, &l_sManifest, sizeof(manifest_t));

    while (atomic_load(&s_xTaskHandle.a_iStopFlag) == OS_TASK_SECURE_FLAG)
    {
        // wait and receive a datagram
        l_iReturn = networkReceiveFrom(l_ptSocket, l_pcBuffer, sizeof(l_pcBuffer), &l_tSenderAddr);

        if (l_iReturn > 0) // check if data has been received
        {
            if (l_pcBuffer[0] == ID_IS_ANY_ROBOT_HERE)
            {
                X_LOG_TRACE("Received valid robot discovery request");

                size_t totalSize = sizeof(uint16_t) + sizeof(uint8_t) + sizeof(manifest_t);
                
                l_iReturn = networkSendTo(l_ptSocket, l_ucSendBuffer,
                                          totalSize,
                                          &l_tSenderAddr);

                if (l_iReturn < 0)
                {
                    X_LOG_TRACE("Failed to send manifest: %s",
                                networkGetErrorString(l_iReturn));
                }
                else
                {
                    X_LOG_TRACE("Successfully sent manifest response (%d bytes)", l_iReturn);
                }
            }
            else
            {
                // NONE
            }
        }
        else if (l_iReturn == NETWORK_TIMEOUT)
        {
            // normal timeout, continue the loop
            X_LOG_TRACE("Normal timeout waiting for UDP data");
            continue;
        }
        else if (l_iReturn < 0)
        {
            X_LOG_TRACE("Error receiving data: %s", networkGetErrorString(l_iReturn));
            continue;
        }
    }

    // Fermer le socket
    networkCloseSocket(l_ptSocket);
    
    return NULL;
}

RobotType_t getRole()
{
    return s_iRole;
}
