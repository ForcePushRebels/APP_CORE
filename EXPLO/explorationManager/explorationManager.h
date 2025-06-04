////////////////////////////////////////////////////////////
//  explorationManager header file
//  defines the explorationManager types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 04/06/2025
////////////////////////////////////////////////////////////

#ifndef EXPLORATION_MANAGER_H_
#define EXPLORATION_MANAGER_H_

#include "handleNetworkMessage.h"
#include <stdint.h>
#include <time.h>

////////////////////////////////////////////////////////////
/// @brief Exploration manager state
////////////////////////////////////////////////////////////
typedef enum explorationManagerState
{
    INIT,             // Initialisation
    PRET,             // Prêt à démarrer
    MISSION_EN_COURS, // Mission en cours
    MODE_MANUEL,      // Contrôle manuel actif
    FIN_DE_MISSION,   // Mission terminée normalement
    ECHEC             // Échec ou interruption anormale
} exploration_manager_state_t;

////////////////////////////////////////////////////////////
/// @brief Initialize the exploration manager
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
void explorationManager_start(void);

////////////////////////////////////////////////////////////
/// @brief Stop the exploration manager
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
void explorationManager_stop(void);

////////////////////////////////////////////////////////////
/// @brief Get the exploration manager state
/// @return The exploration manager state
////////////////////////////////////////////////////////////
exploration_manager_state_t explorationManager_getState(void);

////////////////////////////////////////////////////////////
/// @brief Get the mission start time
/// @return The mission start time
////////////////////////////////////////////////////////////
uint64_t getStartTimeExploration(void);

#endif // EXPLORATION_MANAGER_H_