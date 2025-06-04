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
#include <stddef.h>
#include <stdint.h>

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
typedef enum
{
    MAP_CELL_EMPTY = 0,
    MAP_CELL_WALL = 1,
    MAP_CELL_INTEREST_AREA = 2,
    MAP_CELL_UNKNOWN = 3
} map_cell_type_t;

typedef struct __attribute__((packed))
{
    map_cell_type_t type : 8;
    union {
        struct
        {
            uint8_t empty_field; // not used
        } empty;
        struct
        {
            uint8_t wall_intensity; // 0 to 255: 0 is no wall, 255 is a wall
        } wall;
        struct
        {
            uint8_t area_id;
        } interest_area;
        struct
        {
            uint8_t unknown_field; // not used
        } unknown;
    };

} map_cell_t;

typedef struct __attribute__((packed))
{
    int16_t x_grid;
    int16_t y_grid;
    map_cell_t cell;
} map_fragment_t;
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
 * @param x_size The size of the map on the x axis (output)
 * @param y_size The size of the map on the y axis (output)
 * @return The size of the map
 */
size_t map_engine_get_map_size(size_t *x_size, size_t *y_size);

/**
 * @brief Get the map
 * @param map The map to get
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_NO_MAP_AVAILABLE otherwise
 */
int map_engine_get_map(map_cell_t *map);

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

/**
 * @brief Update the floor sensor
 * @param floor_sensor The floor sensor to update: value in mm.
 * @return MAP_ENGINE_OK if successful, MAP_ENGINE_ERROR_UPDATE_FLOOR_SENSOR if not
 */
int map_engine_update_floor_sensor(uint16_t floor_sensor);

/**
 * @brief Get a 32-bit hash of the current map state
 * @return 32-bit hash value representing the current map state
 */
uint32_t map_engine_get_hash();

/**
 * @brief Get the number of updated cells
 * @return The number of updated cells
 */
uint32_t map_engine_get_updated_cells_count();

/**
 * @brief Get the updated cells
 * @param cells The cells to get
 * @param cell_count The number of cells to get
 * @return The number of updated cells
 */
uint32_t map_engine_get_updated_cells(map_fragment_t *cells, size_t cell_count);

/* ******************************************* Public callback functions declarations ************************************ */

#endif /* _MAP_ENGINE_H_ */