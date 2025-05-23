/**
 * @file map_engine.h
 * @brief Map Engine
 * @author dbenech
 * @date 23/05/2025
 * @see map_engine.c
 * @copyright Cecill-C (Cf. LICENCE.txt)
 */

#ifndef _MAP_ENGINE_H_
#define _MAP_ENGINE_H_

/* ******************************************************* Includes ****************************************************** */
#include <stdint.h>
#include <stddef.h>

/* ***************************************************** Public macros *************************************************** */

// clang-format off
#define MAP_ENGINE_BASE                     0x56D09A00
#define MAP_ENGINE_OK                       (MAP_ENGINE_BASE + 0)
#define MAP_ENGINE_ERROR_INIT               (MAP_ENGINE_BASE + 1)
#define MAP_ENGINE_ERROR_NO_MAP_AVAILABLE   (MAP_ENGINE_BASE + 2)
#define MAP_ENGINE_ERROR_UPDATE_VISION      (MAP_ENGINE_BASE + 3)
#define MAP_ENGINE_ERROR_UNKNOWN            (MAP_ENGINE_BASE + 4)

// clang-format on

/* ************************************************** Public types definition ******************************************** */

/* *********************************************** Public functions declarations ***************************************** */

/**
 * @brief Initialize the map engine
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_INIT if not
 */
int map_engine_init();

/**
 * @brief Ask the current map to the exploration module
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_NO_MAP_AVAILABLE otherwise
 */
int map_engine_explo_ask_current_map();

/**
 * @brief Get the size of the map
 * @return The size of the map
 */
size_t map_engine_get_map_size();

/**
 * @brief Get the map
 * @param map The map to get
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_NO_MAP_AVAILABLE otherwise
 */
int map_engine_get_map(uint8_t *map);

/**
 * @brief Get the discovery percent
 * @return The discovery percent
 */
int map_engine_get_discovery_percent();

/**
 * @brief Update the vision
 * @param sensor_data The sensor data to update: value in mm.
 * @param sensor_count The number of sensors
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_UPDATE_VISION if not
 */
int map_engine_update_vision(uint16_t *sensor_data, uint8_t sensor_count);

/* ******************************************* Public callback functions declarations ************************************ */

#endif /* _MAP_ENGINE_H_ */