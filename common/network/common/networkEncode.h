////////////////////////////////////////////////////////////
//  Network encoder header file
//  Implements the network encoder interface with the network
//
// general disclosure: copy or share the file is forbidden
// Written : 13/05/2025
////////////////////////////////////////////////////////////

#ifndef NETWORK_ENCODER_H_
#define NETWORK_ENCODER_H_

#include <stdint.h>
#include "xAssert.h"
#include "xLog.h"

#pragma pack(push, 1)
typedef struct network_message_t
{
    uint8_t t_iHeader[1];    // Message type header
    uint8_t *t_ptucPayload;  // Pointer to payload data
    uint32_t t_iPayloadSize; // Size of payload (not transmitted, used internally)
} network_message_t;
#pragma pack(pop)

typedef enum network_message_type
{
    //====== SEND BY ANDROID ======
    ID_SET_MOVEMENT = 0x01,
    ID_MISSION_CONTROL = 0x02,
    ID_SELECTED_POINTS = 0x05,
    ID_UPLOAD_MAP = 0x07,

    //====== SEND BY BOT ======
    ID_INF_BATTERY = 0x0A,
    ID_INF_STATUS = 0x0B,
    ID_INF_POS = 0x0C,
    ID_INF_TIME = 0x0D,

    ID_MAP_FRAGMENT = 0x20,
    ID_MAP_FULL = 0x21,

    //=============================
    // UDP:
    ID_IS_ANY_ROBOT_HERE = 0x30,
    ID_MANIFEST = 0x31,
} network_message_type_t;

#endif // NETWORK_ENCODER_H_
