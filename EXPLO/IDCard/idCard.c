////////////////////////////////////////////////////////////
//  IDCard source file
//  Provides IDCard interface 
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#include "idCard.h"
#include <ifaddrs.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

static char s_pcRobotName [] = "Robot_zebi_putin_de_merde";
static char s_pcIpAddr [16] = {0};
static int s_iRole = 0;
static bool s_bUseLoopback = true; // boolean to enable/disable the use of loopback

///////////////////////////////////////////
/// findIpAddress
///////////////////////////////////////////
static void findIpAddress(void)
{
    struct ifaddrs *ifaddr, *ifa;
    int family;
    char host[16];
    bool found = false;

    if (getifaddrs(&ifaddr) == -1) {
        perror("getifaddrs");
        return;
    }

    // if loopback is used, search first the loopback interface
    if (s_bUseLoopback) {
        for (ifa = ifaddr; ifa != NULL && !found; ifa = ifa->ifa_next) {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET) { // IPv4
                // search specifically the loopback interface
                if (strcmp(ifa->ifa_name, "lo") == 0) {
                    void* addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
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
    if (!s_bUseLoopback || !found) {
        for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET) { // IPv4
                // ignore the loopback interface
                if (strcmp(ifa->ifa_name, "lo") != 0) {
                    void* addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
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
    if (!found) {
        strcpy(s_pcIpAddr, "127.0.0.1");
    }

    freeifaddrs(ifaddr);

    X_LOG_TRACE("IP address found: %s", s_pcIpAddr);
}

///////////////////////////////////////////
/// createManifest
///////////////////////////////////////////
int createManifest(manifest_t* p_ptManifest)
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
static void handleIsAnyRobotHere(clientCtx* p_ptClient, const network_message_t* p_ptMessage)
{
    // log the reception of the message
    X_LOG_TRACE("Received ID_IS_ANY_ROBOT_HERE message");
    
    // create a new manifest to answer
    manifest_t t_sManifest;
    int t_iResult = createManifest(&t_sManifest);
    
    if (t_iResult == 0)
    {
        // get client id
        ClientID l_tClientId = networkServerGetClientID(p_ptClient);
        
        // send manifest to client
        t_iResult = networkServerSendMessage(
                                      l_tClientId, 
                                      ID_MANIFEST, // use 0x31 (ID_MANIFEST)
                                      &t_sManifest, 
                                      sizeof(manifest_t));
        
        if (t_iResult != SERVER_OK)
        {
            X_LOG_TRACE("Failed to send manifest: %s", networkServerGetErrorString(t_iResult));
        }
        else
        {
            X_LOG_TRACE("Sent manifest successfully");
        }
    }
    else
    {
        X_LOG_TRACE("Failed to create manifest");
    }
}

///////////////////////////////////////////
/// idCardNetworkInit
///////////////////////////////////////////
void idCardNetworkInit(void)
{
    registerMessageHandler(ID_IS_ANY_ROBOT_HERE, handleIsAnyRobotHere);
}

///////////////////////////////////////////
/// idCardNetworkCleanup
///////////////////////////////////////////
void idCardNetworkCleanup(void)
{

    unregisterMessageHandler(ID_IS_ANY_ROBOT_HERE);
    
    X_LOG_TRACE("IDCard network handlers unregistered");
} 
