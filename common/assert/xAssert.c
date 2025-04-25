////////////////////////////////////////////////////////////
//  assert source file
//  implements assert functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Updated : 19/04/2025
////////////////////////////////////////////////////////////

#include "xAssert.h"
#include "xLog.h"
#include <stdlib.h>

////////////////////////////////////////////////////////////
/// xAssert
////////////////////////////////////////////////////////////
void xAssert(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const char* p_ptMsg)
{
    X_LOG_ASSERT("%s", "Assertion failed");
    
    // Affiche le nom du fichier et le numéro de ligne où l'assertion a échoué
    X_LOG_ASSERT("Location: %s:%lu", p_ptkcFile, p_ulLine);

    // Affiche le message personnalisé si disponible
    if (p_ptMsg != NULL)
    {
        X_LOG_ASSERT("Message: %s", p_ptMsg);
    }

#if defined(XOS_ASSERT_MODE_EXIT)
    exit(1);
#elif defined(XOS_ASSERT_MODE_LOOP)
    while (1);
#elif defined (X_OS_RESTART_APPLICATION)
    X_LOG_ASSERT("Restarting application");

#endif
}

////////////////////////////////////////////////////////////
/// xAssertReturn
////////////////////////////////////////////////////////////
int xAssertReturn(const uint8_t* p_ptkcFile, uint32_t p_ulLine, const char* p_ptMsg, int p_iRet)
{
    X_LOG_ASSERT("Assertion failed with return value %d", p_iRet);
    
    // Affiche le nom du fichier et le numéro de ligne où l'assertion a échoué
    X_LOG_ASSERT("Location: %s:%lu", p_ptkcFile, p_ulLine);

    // Affiche le message personnalisé si disponible
    if (p_ptMsg != NULL)
    {
        X_LOG_ASSERT("Message: %s", p_ptMsg);
    }

    return p_iRet;
}
