////////////////////////////////////////////////////////////
//  Network handler header file
//  Implements the network handler interface with the network
//
// general disclosure: copy or share the file is forbidden
// Written : 13/05/2025
////////////////////////////////////////////////////////////


#ifndef NETWORK_HANDLER_H_
#define NETWORK_HANDLER_H_

#include <stdint.h>

#pragma pack(push, 1)
typedef struct network_message_t
{
    uint8_t t_iHeader[4];
    uint32_t t_iPayloadSize;
    uint8_t* t_ptucPayload;
    uint32_t t_ulCrc;
} network_message_t;
#pragma pack(pop)



#endif // NETWORK_HANDLER_H_
