////////////////////////////////////////////////////////////
//  sensorManager header file
//  Provides sensorManager interface
//
// general disclosure: copy or share the file is forbidden
// Written : 21/05/2025
////////////////////////////////////////////////////////////

#ifndef SENSOR_MANAGER_H
#define SENSOR_MANAGER_H

#include "xLog.h"
#include "xTask.h"
#include "xOsMutex.h"
#include "xAssert.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "stdint.h"

#define SENSOR_MANAGER_SENSORS_COUNT 5

////////////////////////////////////////////////////////////
/// @brief Sensor manager structure
////////////////////////////////////////////////////////////
typedef struct sensorManager_t
{
    xOsTaskCtx t_tTaskHandler;
    xOsMutexCtx t_tMutex;
    xOsTimerCtx t_tTimer;
    uint16_t t_tISensors[SENSOR_MANAGER_SENSORS_COUNT];
} sensorManager_t;



// error codes
#define SENSOR_MANAGER_OK               0x5d8c6010
#define SENSOR_MANAGER_INVALID_ARG      0x5d8c6011


///////////////////////////////////////////
/// @brief Initialize the sensor manager
///
/// @return SENSOR_MANAGER_OK if successful, error code otherwise
///////////////////////////////////////////
int sensorManagerInit(void);


///////////////////////////////////////////
/// @brief Check forward
///
/// @return true if there is something, false otherwise
///////////////////////////////////////////
bool checkForward(void);


///////////////////////////////////////////
/// @brief Start monitoring the environment
///
/// @return SENSOR_MANAGER_OK if successful, error code otherwise
///////////////////////////////////////////
int startMonitoring(void);


///////////////////////////////////////////
/// @brief Stop monitoring the environment
///
/// @return SENSOR_MANAGER_OK if successful, error code otherwise
///////////////////////////////////////////
int stopMonitoring(void);


#endif