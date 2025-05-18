////////////////////////////////////////////////////////////
//  IDCard header file
//  Provides IDCard interface 
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#ifndef IDCARD_H_
#define IDCARD_H_

#include "networkServer.h"
#include "networkEncode.h"
#include "xAssert.h"
#include "xLog.h"
#include "handleNetworkMessage.h"

#include <syscall.h>
#include <ifaddrs.h>
#include <arpa/inet.h>

#pragma pack(push, 1)
typedef struct idCard_t
{
    char t_pcRobotName[32];
    char t_pcIpAddr[16];
    int t_iRole;
} manifest_t;
#pragma pack(pop)

///////////////////////////////////////////
/// @brief Create a manifest for the robot
///
/// @param p_ptManifest Pointer to the manifest structure
/// @return 0 if successful, otherwise an error code
///////////////////////////////////////////
int createManifest(manifest_t* p_ptManifest);

///////////////////////////////////////////
/// @brief Initialise les gestionnaires de messages réseau pour l'IDCard
///
/// Enregistre les handlers pour les messages liés à l'IDCard
/// Cette fonction doit être appelée après initMessageHandlerSystem()
///////////////////////////////////////////
void idCardNetworkInit(void);

///////////////////////////////////////////
/// @brief Nettoie les gestionnaires de messages réseau pour l'IDCard
///
/// Désenregistre les handlers pour les messages liés à l'IDCard
///////////////////////////////////////////
void idCardNetworkCleanup(void);


#endif // IDCARD_H_