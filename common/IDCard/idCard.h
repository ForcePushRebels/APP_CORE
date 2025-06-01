////////////////////////////////////////////////////////////
//  IDCard header file
//  Provides IDCard interface
//
// general disclosure: copy or share the file is forbidden
// Written : 19/05/2025
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
typedef enum
{
    IDCARD_ROLE_EXPLO,
    IDCARD_ROLE_INTER
} RobotType_t;
typedef struct idCard_t
{
    char t_pcRobotName[32];
    char t_pcIpAddr[16];
    RobotType_t t_iRole;
} manifest_t;
#pragma pack(pop)

///////////////////////////////////////////
/// @brief init the IdCard
///////////////////////////////////////////
int idCardInit(RobotType_t type);

///////////////////////////////////////////
/// @brief Create manifest for the robot
///
/// @param p_ptManifest Pointer to the manifest structure
/// @return 0 if successful, otherwise an error code
///////////////////////////////////////////
int createManifest(manifest_t *p_ptManifest);

///////////////////////////////////////////
/// @brief Initialise network message handlers for the IDCard
///
/// Registers handlers for messages related to the IDCard
/// This function must be called after initMessageHandlerSystem()
///////////////////////////////////////////
void idCardNetworkInit(void);

///////////////////////////////////////////
/// @brief Clean up network message handlers for the IDCard
///
/// Unregisters handlers for messages related to the IDCard
///////////////////////////////////////////
void idCardNetworkCleanup(void);

///////////////////////////////////////////
/// @brief Handle the ID_IS_ANY_ROBOT_HERE message
///////////////////////////////////////////
void *handleIsAnyRobotHere(void *p_pvArg);

///////////////////////////////////////////
/// @brief Get the role of the robot
///////////////////////////////////////////
RobotType_t idCardGetRole();

#endif // IDCARD_H_