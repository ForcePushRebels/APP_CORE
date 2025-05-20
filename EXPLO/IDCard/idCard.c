////////////////////////////////////////////////////////////
//  IDCard source file
//  Provides IDCard interface
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
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
static int s_iRole = 0;
static bool s_bUseLoopback = true; // boolean to enable/disable the use of loopback
static xOsTaskCtx s_xTaskHandle = {0};

///////////////////////////////////////////
/// findIpAddress
///////////////////////////////////////////
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
    l_iReturn = osTaskInit(&s_xTaskHandle);
    X_ASSERT(l_iReturn == OS_TASK_SUCCESS);

    s_xTaskHandle.t_ptTask = handleIsAnyRobotHere;

    l_iReturn = osTaskCreate(&s_xTaskHandle);
    X_ASSERT(l_iReturn == OS_TASK_SUCCESS);
}

///////////////////////////////////////////
/// handleIsAnyRobotHere
///////////////////////////////////////////
void handleIsAnyRobotHere()
{
    int l_iReturn = 0;
    char l_pcBuffer[16];
    manifest_t l_sManifest = {0};
    network_message_t l_tNetworkMessage = {0};

    // La taille de network_message_t a changé (uint16_t au lieu de uint32_t)
    // Calculer la taille correcte du buffer
    uint8_t l_ucSendBuffer[sizeof(network_message_t) + sizeof(manifest_t)];

    // create the UDP socket
    NetworkSocket *l_ptSocket = networkCreateSocket(NETWORK_SOCK_UDP);
    X_ASSERT(l_ptSocket != NULL);

    // create and bind the address
    NetworkAddress l_tAddress = networkMakeAddress(s_pcIpAddr, 4069);
    NetworkAddress l_tSenderAddr;

    l_iReturn = networkBind(l_ptSocket, &l_tAddress);
    X_ASSERT(l_iReturn == NETWORK_OK);

    // define a timeout to avoid blocking
    networkSetTimeout(l_ptSocket, 10000, false); // 10 seconds timeout

    // prepare the manifest
    l_iReturn = createManifest(&l_sManifest);
    X_ASSERT(l_iReturn == 0);

    // prepare the network message
    l_tNetworkMessage.t_iHeader[0] = ID_MANIFEST;

    // Vérifier que la taille du payload ne dépasse pas UINT16_MAX
    if (sizeof(manifest_t) > UINT16_MAX)
    {
        X_LOG_TRACE("Manifest size too large for uint16_t: %zu bytes", sizeof(manifest_t));
        return;
    }
    l_tNetworkMessage.t_iPayloadSize = (uint16_t)sizeof(manifest_t);

    // prepare the send buffer
    memcpy(l_ucSendBuffer, &l_tNetworkMessage, sizeof(network_message_t));
    memcpy(l_ucSendBuffer + sizeof(network_message_t), &l_sManifest, sizeof(manifest_t));

    X_LOG_TRACE("IDCard network initialized on port 4069, waiting for discovery requests");

    while (atomic_load(&s_xTaskHandle.a_iStopFlag) == OS_TASK_SECURE_FLAG)
    {
        // wait and receive a datagram
        l_iReturn = networkReceiveFrom(l_ptSocket, l_pcBuffer, sizeof(l_pcBuffer), &l_tSenderAddr);

        if (l_iReturn > 0) // check if data has been received
        {
            if (l_pcBuffer[0] == ID_IS_ANY_ROBOT_HERE)
            {
                X_LOG_TRACE("Received robot discovery request from %s:%d",
                            l_tSenderAddr.t_cAddress, l_tSenderAddr.t_usPort);

                // send the manifest to the sender
                l_iReturn = networkSendTo(l_ptSocket, l_ucSendBuffer,
                                          sizeof(network_message_t) + sizeof(manifest_t),
                                          &l_tSenderAddr);

                if (l_iReturn < 0)
                {
                    X_LOG_TRACE("Failed to send manifest: %s",
                                networkGetErrorString(l_iReturn));
                }
            }
            else
            {
                X_LOG_TRACE("Ignoring frame with incorrect ID: %d (expected: %d)",
                            l_pcBuffer[0], ID_IS_ANY_ROBOT_HERE);
            }
        }
        else if (l_iReturn == NETWORK_TIMEOUT)
        {
            // normal timeout, continue the loop
        }
        else if (l_iReturn < 0)
        {
            X_LOG_TRACE("Error receiving data: %s", networkGetErrorString(l_iReturn));
        }
    }

    // Fermer le socket
    networkCloseSocket(l_ptSocket);
}
