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
    uint32_t t_iPayloadSize;
    uint8_t t_iHeader[1];
    uint8_t* t_ptucPayload;
} network_message_t;
#pragma pack(pop)

enum network_message_type_t
{
    //====== SEND BY ANDROID ======
    ID_SET_MOVEMENT = 0x00,
    ID_START = 0x01,               
    ID_STOP = 0x02,               
    ID_CHOOSE_MANU_MODE = 0x03,    
    ID_SELECTED_POINTS = 0x04,     
    ID_GET_FINAL_METRICS = 0x05,   
    ID_UPLOAD_MAP = 0x06,          
    ID_ASK_STRATS_LIST = 0x07,     
    ID_SELECT_START = 0x08,        

    //====== SEND BY BOT ======
    ID_INF_BATTERY = 0x10,
    ID_INF_STATUS = 0x11,
    ID_INF_POS = 0x12,
    ID_INF_TIME = 0x13,

    ID_MAP_FRAGMENT = 0x20,
    ID_MAP_FULL = 0x21,
    ID_METRICS_EXPLO = 0x22,
    ID_METRICS_INTER = 0x23,
    ID_STARTS_LIST = 0x24,


    //============================================================
    //UDP:
    ID_IS_ANY_ROBOT_HERE = 0x30,
    ID_MANIFEST = 0x31,
};



#endif // NETWORK_ENCODER_H_



