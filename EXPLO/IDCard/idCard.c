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
static bool s_bUseLoopback = true; // Booléen pour activer/désactiver l'utilisation de loopback

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

    // Si on utilise loopback, on cherche d'abord cette interface
    if (s_bUseLoopback) {
        for (ifa = ifaddr; ifa != NULL && !found; ifa = ifa->ifa_next) {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET) { // IPv4
                // Chercher spécifiquement l'interface loopback
                if (strcmp(ifa->ifa_name, "lo") == 0) {
                    void* addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
                    inet_ntop(AF_INET, addr_ptr, host, 16);
                    // Copier l'adresse IP dans la variable statique
                    strncpy(s_pcIpAddr, host, sizeof(s_pcIpAddr) - 1);
                    s_pcIpAddr[sizeof(s_pcIpAddr) - 1] = '\0';
                    found = true;
                    break;
                }
            }
        }
    }

    // Si on n'utilise pas loopback ou si on n'a pas trouvé d'interface loopback
    if (!s_bUseLoopback || !found) {
        for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
            if (ifa->ifa_addr == NULL)
                continue;

            family = ifa->ifa_addr->sa_family;

            if (family == AF_INET) { // IPv4
                // Ignorer l'interface loopback
                if (strcmp(ifa->ifa_name, "lo") != 0) {
                    void* addr_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
                    inet_ntop(AF_INET, addr_ptr, host, 16);
                    // Copier l'adresse IP dans la variable statique
                    strncpy(s_pcIpAddr, host, sizeof(s_pcIpAddr) - 1);
                    s_pcIpAddr[sizeof(s_pcIpAddr) - 1] = '\0';
                    found = true;
                    break; // On prend la première adresse IPv4 non loopback
                }
            }
        }
    }

    // Si aucune adresse n'a été trouvée, on utilise 127.0.0.1 par défaut
    if (!found) {
        strcpy(s_pcIpAddr, "127.0.0.1");
    }

    freeifaddrs(ifaddr);

    X_LOG_TRACE("IP address found: %s", s_pcIpAddr);
}

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


// Définition du gestionnaire pour le message ID_IS_ANY_ROBOT_HERE (0x30)
static void handleIsAnyRobotHere(serverCtx* p_ptServer, clientCtx* p_ptClient, const network_message_t* p_ptMessage)
{
    // Log la réception du message
    X_LOG_TRACE("Received ID_IS_ANY_ROBOT_HERE message");
    
    // Créer un nouveau manifest pour répondre
    manifest_t t_sManifest;
    int t_iResult = createManifest(&t_sManifest);
    
    if (t_iResult == 0)
    {
        // Envoyer le manifest au client
        t_iResult = serverSendMessage(p_ptServer, 
                                      p_ptClient, 
                                      ID_MANIFEST, // Utiliser 0x31 (ID_MANIFEST)
                                      &t_sManifest, 
                                      sizeof(manifest_t));
        
        if (t_iResult != SERVER_OK)
        {
            X_LOG_TRACE("Failed to send manifest: %s", serverGetErrorString(t_iResult));
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

// Initialisation du module de réseau de l'IDCard
void idCardNetworkInit(void)
{
    // Vérifier que le système de handlers est initialisé
    // Note: Cette fonction devrait être appelée après initMessageHandlerSystem()
    
    // Enregistrer le handler pour le message ID_IS_ANY_ROBOT_HERE (0x30)
    registerMessageHandler(ID_IS_ANY_ROBOT_HERE, handleIsAnyRobotHere);
    
    X_LOG_TRACE("IDCard network handlers registered");
}

// Nettoyage du module de réseau de l'IDCard
void idCardNetworkCleanup(void)
{
    // Désenregistrer le handler
    unregisterMessageHandler(ID_IS_ANY_ROBOT_HERE);
    
    X_LOG_TRACE("IDCard network handlers unregistered");
} 
