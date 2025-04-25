////////////////////////////////////////////////////////////
//  assert header file
//  defines assert types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Updated : 19/04/2025
////////////////////////////////////////////////////////////

#ifndef XOS_ASSERT_H_
#define XOS_ASSERT_H_

#include <stdint.h>

// Assert modes
#define XOS_ASSERT_MODE_CONTINUE  0
#define XOS_ASSERT_MODE_EXIT      1
#define XOS_ASSERT_MODE_LOOP      2

// Assert macros
#define X_ASSERT(expr) \
    ((expr) ? (void)0 : xAssert((uint8_t *)__FILE__, __LINE__, NULL))

// Nouvelle macro pour assertion avec message personnalisé
#define X_ASSERT_MSG(expr, msg) \
    ((expr) ? (void)0 : xAssert((uint8_t *)__FILE__, __LINE__, (msg)))

#define X_ASSERT_RETURN(expr, ret) \
    do { \
        if (!(expr)) { \
            return xAssertReturn((uint8_t *)__FILE__, __LINE__, NULL, ret); \
        } \
    } while(0)

// Nouvelle macro pour assertion avec retour et message personnalisé
#define X_ASSERT_RETURN_MSG(expr, msg, ret) \
    do { \
        if (!(expr)) { \
            return xAssertReturn((uint8_t *)__FILE__, __LINE__, (msg), ret); \
        } \
    } while(0)

//////////////////////////////////
/// @brief Assert function
/// @param p_ptkcFile : file name
/// @param p_ulLine : line number
/// @param p_ptMsg : message (can be NULL)
/// @return none
//////////////////////////////////
void xAssert(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const char* p_ptMsg);

//////////////////////////////////
/// @brief Assert with return value
/// @param p_ptkcFile : file name
/// @param p_ulLine : line number
/// @param p_ptMsg : message (can be NULL)
/// @param p_iRet : return value
/// @return return value
//////////////////////////////////
int xAssertReturn(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const char* p_ptMsg, int p_iRet);

#endif // XOS_ASSERT_H_
