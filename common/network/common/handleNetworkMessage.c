////////////////////////////////////////////////////////////
//  Network message handler implementation
//  Implements message handling system for network communications
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#include "handleNetworkMessage.h"
#include "networkEncode.h"
#include "networkServer.h"
#include "xLog.h"
#include "xAssert.h"
#include <stdlib.h>

///////////////////////////////////////////
/// Structure interne pour stocker les handlers de messages
///////////////////////////////////////////
typedef struct {
    uint8_t t_ucMessageType;         // Type de message
    message_handler_t t_ptHandler;   // Fonction de traitement
} t_sMessageHandlerEntry;

///////////////////////////////////////////
/// Variables globales pour stocker les handlers
///////////////////////////////////////////
static t_sMessageHandlerEntry* s_ptHandlers = NULL;       // Liste des handlers
static size_t s_ulHandlersCount = 0;                      // Nombre de handlers enregistrés
static size_t s_ulHandlersCapacity = 0;                   // Capacité actuelle du tableau de handlers

///////////////////////////////////////////
/// Handler par défaut pour les messages non reconnus
///
/// @param server Contexte du serveur
/// @param client Contexte du client
/// @param message Message réseau reçu
///////////////////////////////////////////
static void handleUnknownMessage(serverCtx* server, clientCtx* client, const network_message_t* message) 
{
    (void)server; // Éviter l'avertissement de variable non utilisée
    (void)client; // Éviter l'avertissement de variable non utilisée
    X_LOG_TRACE("Unhandled message type: 0x%02X", message->t_iHeader[0]);
}

///////////////////////////////////////////
/// Initialiser le système de handlers
///////////////////////////////////////////
void initMessageHandlerSystem(void) 
{
    s_ulHandlersCapacity = 10; // Taille initiale
    s_ptHandlers = (t_sMessageHandlerEntry*)malloc(s_ulHandlersCapacity * sizeof(t_sMessageHandlerEntry));
    s_ulHandlersCount = 0;
    
    X_LOG_TRACE("Message handler system initialized with capacity %zu", s_ulHandlersCapacity);
}

///////////////////////////////////////////
/// Nettoyer le système de handlers
///////////////////////////////////////////
void cleanupMessageHandlerSystem(void) 
{
    if (s_ptHandlers != NULL) {
        free(s_ptHandlers);
        s_ptHandlers = NULL;
    }
    s_ulHandlersCount = 0;
    s_ulHandlersCapacity = 0;
    
    X_LOG_TRACE("Message handler system cleaned up");
}

///////////////////////////////////////////
/// Enregistrer un handler pour un type de message
///
/// @param messageType Type de message à traiter
/// @param handler Fonction de traitement à appeler
///////////////////////////////////////////
void registerMessageHandler(uint8_t messageType, message_handler_t handler) 
{
    X_ASSERT(s_ptHandlers != NULL); // Vérifier que le système est initialisé
    
    // Vérifier si un handler existe déjà pour ce type
    for (size_t i = 0; i < s_ulHandlersCount; i++) {
        if (s_ptHandlers[i].t_ucMessageType == messageType) {
            s_ptHandlers[i].t_ptHandler = handler;
            X_LOG_TRACE("Updated handler for message type 0x%02X", messageType);
            return;
        }
    }
    
    // Augmenter la capacité si nécessaire
    if (s_ulHandlersCount >= s_ulHandlersCapacity) {
        s_ulHandlersCapacity *= 2;
        s_ptHandlers = (t_sMessageHandlerEntry*)realloc(s_ptHandlers, 
                       s_ulHandlersCapacity * sizeof(t_sMessageHandlerEntry));
        X_ASSERT(s_ptHandlers != NULL);
        
        X_LOG_TRACE("Expanded handler system capacity to %zu", s_ulHandlersCapacity);
    }
    
    // Ajouter le nouveau handler
    s_ptHandlers[s_ulHandlersCount].t_ucMessageType = messageType;
    s_ptHandlers[s_ulHandlersCount].t_ptHandler = handler;
    s_ulHandlersCount++;
    
    X_LOG_TRACE("Registered handler for message type 0x%02X", messageType);
}

///////////////////////////////////////////
/// Désenregistrer un handler
///
/// @param messageType Type de message dont le handler doit être supprimé
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t messageType) 
{
    for (size_t i = 0; i < s_ulHandlersCount; i++) {
        if (s_ptHandlers[i].t_ucMessageType == messageType) {
            // Déplacer le dernier élément à la place de l'élément supprimé
            s_ptHandlers[i] = s_ptHandlers[s_ulHandlersCount - 1];
            s_ulHandlersCount--;
            X_LOG_TRACE("Unregistered handler for message type 0x%02X", messageType);
            return;
        }
    }
    
    X_LOG_TRACE("No handler found for message type 0x%02X", messageType);
}

///////////////////////////////////////////
/// Trouver un handler pour un type de message
///
/// @param messageType Type de message à rechercher
/// @return Fonction de traitement associée ou handler par défaut
///////////////////////////////////////////
static message_handler_t findMessageHandler(uint8_t messageType) 
{
    for (size_t i = 0; i < s_ulHandlersCount; i++) 
    {
        if (s_ptHandlers[i].t_ucMessageType == messageType) 
        {
            return s_ptHandlers[i].t_ptHandler;
        }
    }
    return handleUnknownMessage;
}

///////////////////////////////////////////
/// Gestionnaire de messages réseau principal
///
/// @param server Contexte du serveur
/// @param client Contexte du client
/// @param message Message réseau à traiter
///////////////////////////////////////////
void handleNetworkMessage(serverCtx* p_ptServer, 
                          clientCtx* p_ptClient, 
                          const network_message_t* p_ptMessage) 
{
    X_ASSERT(p_ptServer != NULL);
    X_ASSERT(p_ptClient != NULL);
    X_ASSERT(p_ptMessage != NULL);

    char clientAddress[64];
    serverGetClientAddress(p_ptClient, clientAddress, sizeof(clientAddress));
    
    X_LOG_TRACE("Received message from %s: type=0x%02X, size=%u bytes", 
              clientAddress, p_ptMessage->t_iHeader[0], p_ptMessage->t_iPayloadSize);
    
    uint8_t l_ucMsgType = p_ptMessage->t_iHeader[0];
    
    // Trouver le handler pour ce type de message
    message_handler_t l_ptHandler = findMessageHandler(l_ucMsgType);
    
    // Appeler le handler
    l_ptHandler(p_ptServer, p_ptClient, p_ptMessage);
}
