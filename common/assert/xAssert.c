////////////////////////////////////////////////////////////
//  assert source file
//  implements assert functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xAssert.h"
#include "xLog.h"
#include <stdlib.h>

////////////////////////////////////////////////////////////
/// xAssert
////////////////////////////////////////////////////////////
void xAssert(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const void* p_ptMsg)
{

    const char* filename = (p_ptkcFile != NULL) ? (const char*)p_ptkcFile : "Unknown file";

    if (p_ptMsg != NULL)
    {
        X_LOG_ASSERT("Assertion failed in %s at line %u", filename, p_ulLine);
        X_LOG_ASSERT("%s", (const char*)p_ptMsg);
    }
    else
    {
        X_LOG_ASSERT("Assertion failed in %s at line %u", filename, p_ulLine);
    }

#if defined(XOS_ASSERT_MODE_EXIT)
    exit(1);
#elif defined(XOS_ASSERT_MODE_LOOP)
    while (1);
#endif
}

////////////////////////////////////////////////////////////
/// xAssertReturn
////////////////////////////////////////////////////////////
int xAssertReturn(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const void* p_ptMsg, int p_iRet)
{
    // Utiliser une chaîne par défaut si le nom du fichier est NULL
    const char* filename = (p_ptkcFile != NULL) ? (const char*)p_ptkcFile : "Unknown file";
    
    X_LOG_ASSERT("Assertion failed in %s at line %u with return value %d", filename, p_ulLine, p_iRet);

    if (p_ptMsg != NULL)
    {
        X_LOG_ASSERT("%s", (const char*)p_ptMsg);
    }

    return p_iRet;
}
