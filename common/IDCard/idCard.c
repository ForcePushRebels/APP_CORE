////////////////////////////////////////////////////////////
//  IDCard source file
//  Provides IDCard interface
//
// general disclosure: copy or share the file is forbidden
// Written : 19/05/2025
////////////////////////////////////////////////////////////

#include "idCard.h"
#include "networkEncode.h"
#include "supervisor.h"
#include "xLog.h"
#include "xNetwork.h"
#include "xServer.h"
#include "xTask.h"
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static char s_pcIpAddr[16] = {0};
#ifdef INTER_BUILD
static RobotType_t s_iRole = IDCARD_ROLE_INTER;
static char s_pcRobotName[] = "Pompier emploie fictif";
#else
static RobotType_t s_iRole = IDCARD_ROLE_EXPLO;
static char s_pcRobotName[] = "Dora l'exploratrice";
#endif
static bool s_bUseLoopback = false; // boolean to enable/disable the use of loopback
static xOsTaskCtx s_xTaskHandle = {0};

///////////////////////////////////////////
/// findIpAddress
///////////////////////////////////////////
static inline struct sockaddr_in* safe_sockaddr_cast(struct sockaddr* addr) 
{
    // This function ensures proper alignment for sockaddr_in cast
    if (addr && addr->sa_family == AF_INET) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
        return (struct sockaddr_in*)addr;
#pragma GCC diagnostic pop
    }
    return NULL;
}

static void findIpAddress(void)
{
    struct ifaddrs *ifaddr, *ifa;
    bool found = false;
    int family;
    char host[NI_MAXHOST];

    if (getifaddrs(&ifaddr) == -1)
    {
        X_LOG_TRACE("Error: Unable to retrieve network interfaces\n");
        // use localhost as default
        strcpy(s_pcIpAddr, "127.0.0.1");
        return;
    }

    // if loopback is used, search specifically for it
    if (s_bUseLoopback)
    {
        for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)
        {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET)
            { // IPv4
                // search specifically the loopback interface
                if (strcmp(ifa->ifa_name, "lo") == 0)
                {
                    struct sockaddr_in* sin = safe_sockaddr_cast(ifa->ifa_addr);
                    if (sin) {
                        void *addr_ptr = &sin->sin_addr;
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
                    struct sockaddr_in* sin = safe_sockaddr_cast(ifa->ifa_addr);
                    if (sin) {
                        void *addr_ptr = &sin->sin_addr;
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
    }

    // if no address is found, use 127.0.0.1 by default
    if (!found)
    {
        strcpy(s_pcIpAddr, "127.0.0.1");
    }
    freeifaddrs(ifaddr);
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
/// handleIsAnyRobotHere
///////////////////////////////////////////
static void isAnyRobotHereHandle(struct clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptMessage; // unused argument avoid warning

    manifest_t l_sManifest = {0};
    int l_iReturn = 0;                                                          // return value

    // create the manifest
    l_iReturn = createManifest(&l_sManifest);
    X_ASSERT(l_iReturn == 0);

    // get client id from the message
    ClientID l_tClientId = xServerGetClientID(p_ptClient);

    l_iReturn = xServerSendMessage(l_tClientId, ID_MANIFEST, &l_sManifest, sizeof(manifest_t));

    if (l_iReturn < 0)
    {
        X_LOG_TRACE("Failed to send manifest: %s", xServerGetErrorString(l_iReturn));
    }
    else
    {
        X_LOG_TRACE("Successfully sent manifest response (%d bytes)", sizeof(manifest_t));
        
        
        supervisor_send_full_map(l_tClientId);
        supervisor_start();
    }
}

///////////////////////////////////////////
/// handleIsAnyRobotHere
///////////////////////////////////////////
void *handleIsAnyRobotHere(void *p_pvArg)
{
    (void)p_pvArg; // unused argument avoid warning

    int l_iReturn = 0;
    uint8_t l_ucBuffer[64]; // Changed to uint8_t and increased size for binary data
    manifest_t l_sManifest = {0};
    
    // Calculate required buffer size and ensure it fits
    size_t l_ulRequiredSize = sizeof(uint16_t) + sizeof(uint8_t) + sizeof(manifest_t);
    uint8_t l_ucSendBuffer[3 + sizeof(manifest_t)];
    
    // Safety check: ensure our buffer is large enough
    if (l_ulRequiredSize > sizeof(l_ucSendBuffer))
    {
        X_LOG_TRACE("ERROR: Send buffer too small (%zu bytes needed, %zu available)", 
                   l_ulRequiredSize, sizeof(l_ucSendBuffer));
        return NULL;
    }

    // ensure the buffer is clean
    memset(l_ucSendBuffer, 0, sizeof(l_ucSendBuffer));

    // create the UDP socket
    NetworkSocket *l_ptSocket = networkCreateSocket(NETWORK_SOCK_UDP);
    X_ASSERT(l_ptSocket != NULL);
    X_LOG_TRACE("UDP socket created successfully");

    // use 0.0.0.0 to listen on all interfaces
    NetworkAddress l_tAddress = networkMakeAddress("0.0.0.0", 13769);
    NetworkAddress l_tSenderAddr;

    l_iReturn = networkBind(l_ptSocket, &l_tAddress);
    if (l_iReturn != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to bind socket: %s", networkGetErrorString(l_iReturn));
        networkCloseSocket(l_ptSocket);
        return NULL;
    }

    // prepare the manifest
    l_iReturn = createManifest(&l_sManifest);
    X_ASSERT(l_iReturn == 0);

    uint16_t payloadSize = (uint16_t)sizeof(manifest_t);

    // build the buffer according to the network_message_t structure with pointer approach
    uint8_t *ptr = l_ucSendBuffer;

    // write the payload size (2 bytes)
    *ptr++ = (uint8_t)((payloadSize >> 8) & 0xFF); // MSB (most significant byte)
    *ptr++ = (uint8_t)(payloadSize & 0xFF);        // LSB (least significant byte)

    // write the message type (1 byte)
    *ptr++ = ID_MANIFEST;

    // copy the manifest to the buffer with bounds checking
    size_t l_ulRemainingSpace = sizeof(l_ucSendBuffer) - (ptr - l_ucSendBuffer);
    if (sizeof(manifest_t) <= l_ulRemainingSpace)
    {
        memcpy(ptr, &l_sManifest, sizeof(manifest_t));
    }
    else
    {
        X_LOG_TRACE("ERROR: Manifest size exceeds remaining buffer space");
        networkCloseSocket(l_ptSocket);
        return NULL;
    }

    while (atomic_load(&s_xTaskHandle.a_iStopFlag) == OS_TASK_SECURE_FLAG)
    {
        // wait and receive a datagram
        l_iReturn = networkReceiveFrom(l_ptSocket, l_ucBuffer, sizeof(l_ucBuffer), &l_tSenderAddr);

        if (l_iReturn > 0) // check if data has been received
        {
            X_LOG_TRACE(
                "Received UDP data: %d bytes from %s:%d", l_iReturn, l_tSenderAddr.t_cAddress, l_tSenderAddr.t_usPort);

            if (l_iReturn == 1 && l_ucBuffer[0] == ID_IS_ANY_ROBOT_HERE)
            {
                X_LOG_TRACE("Received valid robot discovery request (0x%02X)", l_ucBuffer[0]);

                // Use the pre-calculated size for consistency and safety
                l_iReturn = networkSendTo(l_ptSocket, l_ucSendBuffer, l_ulRequiredSize, &l_tSenderAddr);

                if (l_iReturn < 0)
                {
                    X_LOG_TRACE("Failed to send manifest: %s", networkGetErrorString(l_iReturn));
                }
                else
                {
                    X_LOG_TRACE("Successfully sent manifest response (%d bytes)", l_iReturn);
                    supervisor_send_full_map(0);
                    supervisor_start();
                }
            }
            else
            {
                X_LOG_TRACE("Received unrecognized UDP message: %d bytes, first byte=0x%02X", l_iReturn, l_ucBuffer[0]);
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

///////////////////////////////////////////
/// idCardNetworkInit
///////////////////////////////////////////
void idCardNetworkInit(void)
{
    int l_iReturn = 0;

    // Register the message handler for the ID_IS_ANY_ROBOT_HERE message
    registerMessageHandler(ID_IS_ANY_ROBOT_HERE, isAnyRobotHereHandle);

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
/// idCardGetRole
///////////////////////////////////////////
RobotType_t idCardGetRole()
{
    return s_iRole;
}

///////////////////////////////////////////
/// idCardNetworkCleanup
///////////////////////////////////////////
void idCardNetworkCleanup(void)
{
    unregisterMessageHandler(ID_IS_ANY_ROBOT_HERE);

    osTaskStop(&s_xTaskHandle, 3000);
}