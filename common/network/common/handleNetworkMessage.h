////////////////////////////////////////////////////////////
//  Network message handler header file
//  Provides message handling interface for network communications
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#ifndef HANDLE_NETWORK_MESSAGE_H_
#define HANDLE_NETWORK_MESSAGE_H_

#include "networkServer.h"
#include "networkEncode.h"

///////////////////////////////////////////
/// @brief Définition du type pour les fonctions de traitement des messages
///////////////////////////////////////////
typedef void (*message_handler_t)(serverCtx* p_ptServer, clientCtx* p_ptClient, const network_message_t* p_ptMessage);

///////////////////////////////////////////
/// @brief Enregistre un handler pour un type de message spécifique
///
/// @param messageType Type de message à traiter
/// @param handler Fonction de traitement à appeler
///////////////////////////////////////////
void registerMessageHandler(uint8_t p_ucMessageType, message_handler_t p_ptHandler);

///////////////////////////////////////////
/// @brief Désenregistre un handler pour un type de message
///
/// @param messageType Type de message dont le handler doit être supprimé
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t p_ucMessageType);

///////////////////////////////////////////
/// @brief Fonction principale de traitement des messages réseau
///
/// @param server Contexte du serveur
/// @param client Contexte du client
/// @param message Message réseau à traiter
///////////////////////////////////////////
void handleNetworkMessage(serverCtx* p_ptServer, 
                          clientCtx* p_ptClient, 
                          const network_message_t* p_ptMessage);

///////////////////////////////////////////
/// @brief Initialise le système de handlers de messages
///////////////////////////////////////////
void initMessageHandlerSystem(void);

///////////////////////////////////////////
/// @brief Nettoie le système de handlers de messages
///////////////////////////////////////////
void cleanupMessageHandlerSystem(void);

#endif // HANDLE_NETWORK_MESSAGE_H_
