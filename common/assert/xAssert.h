////////////////////////////////////////////////////////////
//  assert header file
//  defines assert types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_ASSERT_H_
#define XOS_ASSERT_H_

#include <stdint.h>

// Assert modes
#define XOS_ASSERT_MODE_CONTINUE    0x00000000
#define XOS_ASSERT_MODE_EXIT        0x00000001
#define XOS_ASSERT_MODE_LOOP        0x00000002


// Assert macros
#define X_ASSERT(expr) \
    ((expr) ? (void)0 : xAssert((uint8_t *)__FILE__, __LINE__, NULL))

#define X_ASSERT_RETURN(expr, ret) \
    do { \
        if (!(expr)) { \
            return xAssertReturn((uint8_t *)__FILE__, __LINE__, NULL, ret); \
        } \
    } while(0)

//////////////////////////////////
/// @brief Assert function
/// @param p_ptkcFile : file name
/// @param p_ulLine : line number
/// @param p_ptMsg : message
/// @return none
//////////////////////////////////
void xAssert(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const void* p_ptMsg);

//////////////////////////////////
/// @brief Assert with return value
/// @param p_ptkcFile : file name
/// @param p_ulLine : line number
/// @param p_ptMsg : message
/// @param p_iRet : return value
/// @return return value
//////////////////////////////////
int xAssertReturn(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const void* p_ptMsg, int p_iRet);

#endif // XOS_ASSERT_H_
