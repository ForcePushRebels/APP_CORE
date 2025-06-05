////////////////////////////////////////////////////////////
//  Network Protocol Layer Header
//  Handles frame creation and protocol management
//  Separates protocol logic from network transport layer
//
// general disclosure: copy or share the file is forbidden
// Written : 22/04/2025
////////////////////////////////////////////////////////////

#ifndef X_PROTOCOL_H_
#define X_PROTOCOL_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "networkEncode.h"

//////////////////////////////////
/// Protocol Constants
//////////////////////////////////

#define PROTOCOL_MAX_FRAME_SIZE     4096    // Maximum frame size
#define PROTOCOL_HEADER_SIZE        3       // Size header (2 bytes) + message type (1 byte)
#define PROTOCOL_MAX_PAYLOAD_SIZE   (PROTOCOL_MAX_FRAME_SIZE - PROTOCOL_HEADER_SIZE)

//////////////////////////////////
/// Protocol Error Codes
//////////////////////////////////

#define PROTOCOL_OK                 0x300C000
#define PROTOCOL_ERROR              0x300C001
#define PROTOCOL_INVALID_PARAM      0x300C002
#define PROTOCOL_BUFFER_TOO_SMALL   0x300C003
#define PROTOCOL_FRAME_TOO_LARGE    0x300C004
#define PROTOCOL_INVALID_FRAME      0x300C005
#define PROTOCOL_MEMORY_ERROR       0x300C006

//////////////////////////////////
/// Protocol Frame Structure
//////////////////////////////////

// Note: We reuse network_message_t from networkEncode.h for parsed frames
// This ensures compatibility with existing server message handling

// Raw frame structure (as sent over network)
typedef struct
{
    uint16_t t_usSize;      // Total payload size (message type + data) in network byte order
    uint8_t t_ucMsgType;    // Message type
    uint8_t *t_pucPayload;  // Payload data (variable size)
} ProtocolRawFrame;

// Parsed frame is just an alias to existing network_message_t
typedef network_message_t ProtocolFrame;

// Frame buffer for zero-allocation operations
typedef struct
{
    uint8_t *t_pucBuffer;       // Frame buffer
    uint32_t t_ulBufferSize;    // Buffer size
    uint32_t t_ulFrameSize;     // Actual frame size
    bool t_bOwnsBuffer;         // True if buffer should be freed
} ProtocolFrameBuffer;

//////////////////////////////////
/// Frame Creation Functions
//////////////////////////////////

//////////////////////////////////
/// @brief Create a complete protocol frame (header + payload) in provided buffer
/// @param p_eMsgType Message type (from network_message_type_t enum)
/// @param p_pucPayload Payload data (can be NULL if size is 0)
/// @param p_ulPayloadSize Payload size in bytes
/// @param p_pucFrameBuffer Buffer to write frame into (must be large enough)
/// @param p_ulBufferSize Size of the provided buffer
/// @param p_pulFrameSize Output: actual frame size written
/// @return Protocol error code
//////////////////////////////////
int protocolCreateFrame(network_message_type_t p_eMsgType, 
                       const uint8_t *p_pucPayload, 
                       uint32_t p_ulPayloadSize,
                       uint8_t *p_pucFrameBuffer, 
                       uint32_t p_ulBufferSize,
                       uint32_t *p_pulFrameSize);

//////////////////////////////////
/// @brief Create frame in provided buffer (zero allocation)
/// @param p_eMsgType Message type (from network_message_type_t enum)
/// @param p_pucPayload Payload data (can be NULL if size is 0)
/// @param p_ulPayloadSize Payload size in bytes
/// @param p_pucBuffer Target buffer
/// @param p_ulBufferSize Buffer size
/// @param p_pulFrameSize Output: actual frame size
/// @return Protocol error code
//////////////////////////////////
int protocolCreateFrameInBuffer(network_message_type_t p_eMsgType,
                               const uint8_t *p_pucPayload,
                               uint32_t p_ulPayloadSize,
                               uint8_t *p_pucBuffer,
                               uint32_t p_ulBufferSize,
                               uint32_t *p_pulFrameSize);

//////////////////////////////////
/// @brief Create frame buffer object for reuse
/// @param p_ulMaxFrameSize Maximum frame size to support
/// @return ProtocolFrameBuffer* or NULL on error
//////////////////////////////////
ProtocolFrameBuffer *protocolCreateFrameBuffer(uint32_t p_ulMaxFrameSize);

//////////////////////////////////
/// @brief Free frame buffer object
/// @param p_ptFrameBuffer Frame buffer to free
//////////////////////////////////
void protocolFreeFrameBuffer(ProtocolFrameBuffer *p_ptFrameBuffer);

//////////////////////////////////
/// @brief Build frame in reusable buffer
/// @param p_ptFrameBuffer Frame buffer object
/// @param p_eMsgType Message type (from network_message_type_t enum)
/// @param p_pucPayload Payload data
/// @param p_ulPayloadSize Payload size
/// @return Protocol error code
//////////////////////////////////
int protocolBuildFrame(ProtocolFrameBuffer *p_ptFrameBuffer,
                      network_message_type_t p_eMsgType,
                      const uint8_t *p_pucPayload,
                      uint32_t p_ulPayloadSize);

//////////////////////////////////
/// Frame Parsing Functions
//////////////////////////////////

//////////////////////////////////
/// @brief Parse received frame data
/// @param p_pucFrameData Raw frame data
/// @param p_ulFrameSize Frame size
/// @param p_ptFrame Output: parsed frame structure
/// @return Protocol error code
//////////////////////////////////
int protocolParseFrame(const uint8_t *p_pucFrameData,
                      uint32_t p_ulFrameSize,
                      ProtocolFrame *p_ptFrame);

//////////////////////////////////
/// @brief Validate frame header
/// @param p_pucHeader Frame header (first 3 bytes)
/// @param p_pulExpectedPayloadSize Output: expected payload size
/// @param p_pucMsgType Output: message type
/// @return Protocol error code
//////////////////////////////////
int protocolValidateHeader(const uint8_t *p_pucHeader,
                          uint32_t *p_pulExpectedPayloadSize,
                          uint8_t *p_pucMsgType);

//////////////////////////////////
/// @brief Extract payload from complete frame
/// @param p_pucFrameData Complete frame data
/// @param p_ulFrameSize Frame size
/// @param p_ppucPayload Output: payload pointer (points into frame data)
/// @param p_pulPayloadSize Output: payload size
/// @return Protocol error code
//////////////////////////////////
int protocolExtractPayload(const uint8_t *p_pucFrameData,
                          uint32_t p_ulFrameSize,
                          const uint8_t **p_ppucPayload,
                          uint32_t *p_pulPayloadSize);

//////////////////////////////////
/// Utility Functions
//////////////////////////////////

//////////////////////////////////
/// @brief Calculate total frame size for given payload
/// @param p_ulPayloadSize Payload size
/// @return Total frame size (header + payload)
//////////////////////////////////
static inline uint32_t protocolCalculateFrameSize(uint32_t p_ulPayloadSize)
{
    return PROTOCOL_HEADER_SIZE + p_ulPayloadSize;
}

//////////////////////////////////
/// @brief Check if payload size is valid
/// @param p_ulPayloadSize Payload size to check
/// @return true if valid, false if too large
//////////////////////////////////
static inline bool protocolIsValidPayloadSize(uint32_t p_ulPayloadSize)
{
    return (p_ulPayloadSize <= PROTOCOL_MAX_PAYLOAD_SIZE);
}


//////////////////////////////////
/// @brief Macro to declare a maximum-size stack buffer for any frame
/// @param bufferName Name of the buffer variable to declare
/// 
/// Usage example:
///   PROTOCOL_DECLARE_MAX_FRAME_BUFFER(frameBuffer);
///   uint32_t frameSize;
///   int result = protocolCreateFrame(ID_INF_STATUS, payload, payloadLen, frameBuffer, sizeof(frameBuffer), &frameSize);
//////////////////////////////////
#define PROTOCOL_DECLARE_MAX_FRAME_BUFFER(bufferName) \
    uint8_t bufferName[PROTOCOL_MAX_FRAME_SIZE]

//////////////////////////////////
/// @brief Get protocol error string
/// @param p_iError Protocol error code
/// @return Error description string
//////////////////////////////////
const char *protocolGetErrorString(int p_iError);

#endif // X_PROTOCOL_H_