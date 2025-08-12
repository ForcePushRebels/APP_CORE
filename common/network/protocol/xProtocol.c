////////////////////////////////////////////////////////////
//  Network Protocol Layer Implementation
//  Handles frame creation and protocol management
//  Separates protocol logic from network transport layer
//
// general disclosure: copy or share the file is forbidden
// Written : 22/04/2025
////////////////////////////////////////////////////////////

#include "xProtocol.h"
#include "xNetwork.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>


//////////////////////////////////
/// protocolCreateFrame
//////////////////////////////////
int protocolCreateFrame(network_message_type_t p_eMsgType, 
                       const uint8_t *p_pucPayload, 
                       uint32_t p_ulPayloadSize,
                       uint8_t *p_pucFrameBuffer, 
                       uint32_t p_ulBufferSize,
                       uint32_t *p_pulFrameSize)
{
    // Simply delegate to the in-buffer function
    return protocolCreateFrameInBuffer(p_eMsgType, p_pucPayload, p_ulPayloadSize,
                                      p_pucFrameBuffer, p_ulBufferSize, p_pulFrameSize);
}

//////////////////////////////////
/// protocolCreateFrameInBuffer
//////////////////////////////////
int protocolCreateFrameInBuffer(network_message_type_t p_eMsgType,
                               const uint8_t *p_pucPayload,
                               uint32_t p_ulPayloadSize,
                               uint8_t *p_pucBuffer,
                               uint32_t p_ulBufferSize,
                               uint32_t *p_pulFrameSize)
{
    // Parameter validation
    if (!p_pucBuffer || !p_pulFrameSize)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    // Validate payload size
    if (!protocolIsValidPayloadSize(p_ulPayloadSize))
    {
        return PROTOCOL_FRAME_TOO_LARGE;
    }

    // Check if payload is required but missing
    if (p_ulPayloadSize > 0 && !p_pucPayload)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    // Calculate total frame size
    uint32_t l_ulTotalSize = protocolCalculateFrameSize(p_ulPayloadSize);

    // Check buffer size
    if (p_ulBufferSize < l_ulTotalSize)
    {
        return PROTOCOL_BUFFER_TOO_SMALL;
    }

    // Build frame:
    // 1. Size header (2 bytes) - includes message type (1 byte) + payload size
    uint16_t l_usNetworkPayloadSize = HOST_TO_NET_SHORT((uint16_t)(1 + p_ulPayloadSize));
    memcpy(p_pucBuffer, &l_usNetworkPayloadSize, sizeof(uint16_t));

    // 2. Message type (1 byte)
    p_pucBuffer[sizeof(uint16_t)] = (uint8_t)p_eMsgType;

    // 3. Payload (if any)
    if (p_ulPayloadSize > 0)
    {
        memcpy(p_pucBuffer + PROTOCOL_HEADER_SIZE, p_pucPayload, p_ulPayloadSize);
    }

    *p_pulFrameSize = l_ulTotalSize;
    return PROTOCOL_OK;
}

//////////////////////////////////
/// protocolCreateFrameBuffer
//////////////////////////////////
ProtocolFrameBuffer *protocolCreateFrameBuffer(uint32_t p_ulMaxFrameSize)
{
    if (p_ulMaxFrameSize == 0 || p_ulMaxFrameSize > PROTOCOL_MAX_FRAME_SIZE)
    {
        return NULL;
    }

    ProtocolFrameBuffer *l_ptFrameBuffer = (ProtocolFrameBuffer *)malloc(sizeof(ProtocolFrameBuffer));
    if (!l_ptFrameBuffer)
    {
        return NULL;
    }

    l_ptFrameBuffer->t_pucBuffer = (uint8_t *)malloc(p_ulMaxFrameSize);
    if (!l_ptFrameBuffer->t_pucBuffer)
    {
        free(l_ptFrameBuffer);
        return NULL;
    }

    l_ptFrameBuffer->t_ulBufferSize = p_ulMaxFrameSize;
    l_ptFrameBuffer->t_ulFrameSize = 0;
    l_ptFrameBuffer->t_bOwnsBuffer = true;

    return l_ptFrameBuffer;
}

//////////////////////////////////
/// protocolFreeFrameBuffer - Free frame buffer
//////////////////////////////////
void protocolFreeFrameBuffer(ProtocolFrameBuffer *p_ptFrameBuffer)
{
    if (!p_ptFrameBuffer)
    {
        return;
    }

    if (p_ptFrameBuffer->t_bOwnsBuffer && p_ptFrameBuffer->t_pucBuffer)
    {
        free(p_ptFrameBuffer->t_pucBuffer);
    }

    free(p_ptFrameBuffer);
}

//////////////////////////////////
/// protocolBuildFrame - Build frame in reusable buffer
//////////////////////////////////
int protocolBuildFrame(ProtocolFrameBuffer *p_ptFrameBuffer,
                      network_message_type_t p_eMsgType,
                      const uint8_t *p_pucPayload,
                      uint32_t p_ulPayloadSize)
{
    if (!p_ptFrameBuffer || !p_ptFrameBuffer->t_pucBuffer)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    uint32_t l_ulFrameSize;
    int l_iResult = protocolCreateFrameInBuffer(p_eMsgType, p_pucPayload, p_ulPayloadSize,
                                               p_ptFrameBuffer->t_pucBuffer,
                                               p_ptFrameBuffer->t_ulBufferSize,
                                               &l_ulFrameSize);

    if (l_iResult == PROTOCOL_OK)
    {
        p_ptFrameBuffer->t_ulFrameSize = l_ulFrameSize;
    }

    return l_iResult;
}

//////////////////////////////////
/// Frame Parsing Functions
//////////////////////////////////

//////////////////////////////////
/// protocolParseFrame 
//////////////////////////////////
int protocolParseFrame(const uint8_t *p_pucFrameData,
                      uint32_t p_ulFrameSize,
                      ProtocolFrame *p_ptFrame)
{
    if (!p_pucFrameData || !p_ptFrame || p_ulFrameSize < PROTOCOL_HEADER_SIZE)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    // Validate header and extract info
    uint32_t l_ulExpectedPayloadSize;
    uint8_t l_ucMsgType;
    int l_iResult = protocolValidateHeader(p_pucFrameData, &l_ulExpectedPayloadSize, &l_ucMsgType);
    if (l_iResult != PROTOCOL_OK)
    {
        return l_iResult;
    }

    // Check if frame size matches expected
    uint32_t l_ulExpectedFrameSize = protocolCalculateFrameSize(l_ulExpectedPayloadSize - 1); // -1 for msg type
    if (p_ulFrameSize != l_ulExpectedFrameSize)
    {
        return PROTOCOL_INVALID_FRAME;
    }

    // Fill frame structure (using network_message_t format)
    p_ptFrame->t_iHeader[0] = l_ucMsgType;
    p_ptFrame->t_iPayloadSize = l_ulExpectedPayloadSize - 1; // Exclude message type from payload
    
    if (p_ptFrame->t_iPayloadSize > 0)
    {
        p_ptFrame->t_ptucPayload = (uint8_t*)(p_pucFrameData + PROTOCOL_HEADER_SIZE);
    }
    else
    {
        p_ptFrame->t_ptucPayload = NULL;
    }

    return PROTOCOL_OK;
}

//////////////////////////////////
/// protocolValidateHeader
//////////////////////////////////
int protocolValidateHeader(const uint8_t *p_pucHeader,
                          uint32_t *p_pulExpectedPayloadSize,
                          uint8_t *p_pucMsgType)
{
    if (!p_pucHeader || !p_pulExpectedPayloadSize || !p_pucMsgType)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    // Extract size from header (first 2 bytes)
    uint16_t l_usNetworkSize;
    memcpy(&l_usNetworkSize, p_pucHeader, sizeof(uint16_t));
    uint32_t l_ulPayloadSize = NET_TO_HOST_SHORT(l_usNetworkSize);

    // Validate payload size
    if (l_ulPayloadSize == 0 || l_ulPayloadSize > PROTOCOL_MAX_PAYLOAD_SIZE + 1) // +1 for message type
    {
        return PROTOCOL_INVALID_FRAME;
    }

    // Extract message type (3rd byte)
    *p_pucMsgType = p_pucHeader[2];
    *p_pulExpectedPayloadSize = l_ulPayloadSize;

    return PROTOCOL_OK;
}

//////////////////////////////////
/// protocolExtractPayload
//////////////////////////////////
int protocolExtractPayload(const uint8_t *p_pucFrameData,
                          uint32_t p_ulFrameSize,
                          const uint8_t **p_pptucPayload,
                          uint32_t *p_pulPayloadSize)
{
    if (!p_pucFrameData || !p_pptucPayload || !p_pulPayloadSize)
    {
        return PROTOCOL_INVALID_PARAM;
    }

    if (p_ulFrameSize < PROTOCOL_HEADER_SIZE)
    {
        return PROTOCOL_INVALID_FRAME;
    }

    // Parse frame to get payload info
    ProtocolFrame l_tFrame;
    int l_iResult = protocolParseFrame(p_pucFrameData, p_ulFrameSize, &l_tFrame);
    if (l_iResult != PROTOCOL_OK)
    {
        return l_iResult;
    }

    *p_pptucPayload = l_tFrame.t_ptucPayload;
    *p_pulPayloadSize = l_tFrame.t_iPayloadSize;

    return PROTOCOL_OK;
}

//////////////////////////////////
/// Utility Functions
//////////////////////////////////

//////////////////////////////////
/// protocolGetErrorString - Get error description
//////////////////////////////////
const char *protocolGetErrorString(int p_iError)
{
    switch (p_iError)
    {
        case PROTOCOL_OK:
            return "Success";
        case PROTOCOL_ERROR:
            return "General protocol error";
        case PROTOCOL_INVALID_PARAM:
            return "Invalid parameter";
        case PROTOCOL_BUFFER_TOO_SMALL:
            return "Buffer too small";
        case PROTOCOL_FRAME_TOO_LARGE:
            return "Frame too large";
        case PROTOCOL_INVALID_FRAME:
            return "Invalid frame format";
        case PROTOCOL_MEMORY_ERROR:
            return "Memory allocation error";
        default:
            return "Unknown protocol error";
    }
}


