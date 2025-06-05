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
#include "xError.h"

#define SENSOR_MANAGER_SENSORS_COUNT 3


#define SENSOR_MAX_RAW_VALUE 254
#define SENSOR_MAX_MM_VALUE 250

#define SENSOR_MIN_RAW_VALUE 12
#define SENSOR_MIN_MM_VALUE 0
////////////////////////////////////////////////////////////
/// @brief Sensor manager structure
////////////////////////////////////////////////////////////
typedef struct sensorManager_t
{
    xOsTaskCtx t_tTaskHandler;
    xOsMutexCtx t_tMutex;
    xOsTimerCtx t_tTimer;
    uint16_t t_tISensors[SENSOR_MANAGER_SENSORS_COUNT];
    uint16_t t_tFloorSensor;
} sensorManager_t;

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
int startMonitoring();

///////////////////////////////////////////
/// @brief Stop monitoring the environment
///
/// @return SENSOR_MANAGER_OK if successful, error code otherwise
///////////////////////////////////////////
int stopMonitoring(void);

///////////////////////////////////////////
/// @brief Get one of the sensor values
/// @param sensor : valeur du capteur (0;1;2)
/// @return one of the sensor value
///////////////////////////////////////////
uint16_t updateVision(int sensor);

///////////////////////////////////////////
/// @brief Convertit une valeur capteur brute (0-255) en millimètres (0-200mm)
/// @param rawValue : valeur brute du capteur (0-255)
/// @return distance en mm (0-200)
///////////////////////////////////////////
uint16_t rawValuesToMm(uint16_t rawValue);

#endif