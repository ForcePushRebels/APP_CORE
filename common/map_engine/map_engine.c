/**
 * @file map_engine.c
 * @brief Map Engine
 * @author dbenech
 * @date 23/05/2025
 * @see map_engine.h
 * @copyright Cecill-C (Cf. LICENCE.txt)
 */

/* ******************************************************* Includes ****************************************************** */
#include "map_engine.h"
#include <stdint.h>
#include <string.h>
#include "xLog.h"

/* **************************************************** Private macros *************************************************** */
#define MAP_WIDTH 10
#define MAP_HEIGHT 10

/* ************************************************ Private type definition ********************************************** */

typedef enum
{
    MAP_CELL_EMPTY = 0,
    MAP_CELL_WALL = 1,
    MAP_CELL_INTEREST_AREA = 2,
    MAP_CELL_UNKNOWN = 3
} map_cell_type_t;

typedef struct
{
    map_cell_type_t type : 8;
    union
    {
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

typedef struct
{
    map_cell_t map[MAP_WIDTH][MAP_HEIGHT];
    uint8_t discovery_percent;
} map_engine_t;

/* ********************************************* Private functions declarations ****************************************** */

int map_engine_update_map();

int map_engine_update_discovery_percent();

/* ************************************************** Private variables ************************************************** */

map_engine_t map_engine;

/* ********************************************** Private functions definitions ****************************************** */

/* ********************************************** Public functions definitions ******************************************* */

int map_engine_init()
{
    memset(map_engine.map, 0, sizeof(map_engine.map));
    map_engine.discovery_percent = 0;

    return MAP_ENGINE_OK;
}

int map_engine_explo_ask_current_map()
{
    return MAP_ENGINE_OK;
}

size_t map_engine_get_map_size()
{
    return MAP_WIDTH * MAP_HEIGHT;
}

int map_engine_get_map(uint8_t *map)
{
    (void)map;
    return MAP_ENGINE_OK;
}

int map_engine_get_discovery_percent()
{
    return map_engine.discovery_percent;
}

int map_engine_update_vision(uint16_t *sensor_data, uint8_t sensor_count)
{
    (void)sensor_data;
    (void)sensor_count;

    if (sensor_count != 3)
    {
        X_LOG_TRACE("Invalid sensor count: %d", sensor_count);
        return MAP_ENGINE_ERROR_UNKNOWN;
    }

    X_LOG_TRACE("sensor: %d, %d, %d", sensor_data[0], sensor_data[1], sensor_data[2]);

    return MAP_ENGINE_OK;
}

/* ***************************************** Public callback functions definitions *************************************** */