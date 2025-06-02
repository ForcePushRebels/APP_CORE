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
#include "sensorManager.h"
#include <stdio.h>
#include "math.h"
#include "positionControl.h"

/* **************************************************** Private macros *************************************************** */
#define MAP_DEBUG 0

#define MAP_WIDTH_MM 1000
#define MAP_HEIGHT_MM 1000

#define MAP_CELL_SIZE_MM 50


#define MAP_WIDTH (MAP_WIDTH_MM / MAP_CELL_SIZE_MM)
#define MAP_HEIGHT (MAP_HEIGHT_MM / MAP_CELL_SIZE_MM)

/* ************************************************ Private type definition ********************************************** */

typedef struct
{
    map_cell_t map[MAP_WIDTH][MAP_HEIGHT];
    uint8_t discovery_percent;
    int interest_area_count;
} map_engine_t;

typedef struct
{
    int16_t x_mm;
    int16_t y_mm;
} point_t;

/* ********************************************* Private functions declarations ****************************************** */

int map_engine_update_map();

int map_engine_update_discovery_percent();

/* ************************************************** Private variables ************************************************** */

map_engine_t map_engine;

float sensor_angles_rad[3] = {
    [0] = -0.84,
    [1] = 0,
    [2] = 0.84,
};

/* ********************************************** Private functions definitions ****************************************** */

#if MAP_DEBUG
static void print_map()
{
    printf("=== MAP DISPLAY ===\n");

    // Affichage de l'en-tête avec les numéros de colonnes
    printf("   ");
    for (int x = 0; x < MAP_WIDTH; x++)
    {
        printf(" %2d ", x);
    }
    printf("\n");

    // Ligne de séparation supérieure
    printf("   ");
    for (int x = 0; x < MAP_WIDTH; x++)
    {
        printf("----");
    }
    printf("-\n");

    // Affichage de chaque ligne de la grille
    for (int y = 0; y < MAP_HEIGHT; y++)
    {
        printf("%2d |", y); // Numéro de ligne + séparateur

        for (int x = 0; x < MAP_WIDTH; x++)
        {
            uint8_t intensity = map_engine.map[x][y].wall.wall_intensity;
            if (intensity == 0)
            {
                printf("  . "); // Point pour case vide
            }
            else
            {
                printf("%3d ", intensity); // Intensité sur 3 caractères
            }
        }
        printf("|\n"); // Fermeture de ligne

        // Ligne de séparation entre les rangées
        if (y < MAP_HEIGHT - 1)
        {
            printf("   |");
            for (int x = 0; x < MAP_WIDTH; x++)
            {
                printf("----");
            }
            printf("|\n");
        }
    }

    // Ligne de séparation inférieure
    printf("   ");
    for (int x = 0; x < MAP_WIDTH; x++)
    {
        printf("----");
    }
    printf("-\n");

    printf("Legend: '.' = empty, numbers = wall intensity (0-255)\n");
}
#endif

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
    return sizeof(map_engine.map);
}

int map_engine_get_map(map_cell_t *map)
{
    memcpy(map, map_engine.map, sizeof(map_engine.map));
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

    // X_LOG_TRACE("sensor: %d, %d, %d", sensor_data[0], sensor_data[1], sensor_data[2]);

    point_t points[SENSOR_MANAGER_SENSORS_COUNT] = {0};
    uint8_t points_count = 0;

    Position_t robot_pos;
    position_control_get_position(&robot_pos);
    float angle = robot_pos.angle_rad;               // + lidar angle relative
    int16_t offset_x = robot_pos.x_mm + cosf(angle); // * lidar->dist_to_center;
    int16_t offset_y = robot_pos.y_mm + sinf(angle); // * lidar->dist_to_center;

    for (uint8_t i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; i++)
    {
        uint16_t distance = sensor_data[i];

        if (distance >= SENSOR_MAX_MM_VALUE || distance <= SENSOR_MIN_MM_VALUE)
        {
            continue;
        }

        angle = robot_pos.angle_rad + sensor_angles_rad[i];
        // On ajoute le point
        points[i].x_mm = (int16_t)(cosf(angle) * distance) + offset_x;
        points[i].y_mm = (int16_t)(sinf(angle) * distance) + offset_y;
        points_count++;
    }

    for (uint8_t i = 0; i < points_count; i++)
    {
        // X_LOG_TRACE("Point %d: %d, %d", i, points[i].x_mm, points[i].y_mm);
        int16_t grid_x = points[i].x_mm / MAP_CELL_SIZE_MM;
        int16_t grid_y = points[i].y_mm / MAP_CELL_SIZE_MM;
        if (grid_x < 0 || grid_x >= MAP_WIDTH || grid_y < 0 || grid_y >= MAP_HEIGHT)
        {
            continue;
        }
        map_engine.map[grid_x][grid_y].type = MAP_CELL_WALL;
        if (map_engine.map[grid_x][grid_y].wall.wall_intensity < 255)
        {
            map_engine.map[grid_x][grid_y].wall.wall_intensity++;
        }
        // X_LOG_TRACE("Grid: %d, %d, %d", grid_x, grid_y, map_engine.map[grid_x][grid_y].wall.wall_intensity);
    }

#if MAP_DEBUG
    print_map();
#endif

    return MAP_ENGINE_OK;
}

int map_engine_update_floor_sensor(uint16_t floor_sensor)
{

    if (floor_sensor >= 99) //TODO: remove this: select a value for the floor sensor
    {
        return MAP_ENGINE_OK;
    }

    Position_t robot_pos;
    position_control_get_position(&robot_pos);
    int16_t grid_x = robot_pos.x_mm / MAP_CELL_SIZE_MM;
    int16_t grid_y = robot_pos.y_mm / MAP_CELL_SIZE_MM;
    if (grid_x < 0 || grid_x >= MAP_WIDTH || grid_y < 0 || grid_y >= MAP_HEIGHT)
    {
        X_LOG_TRACE("Invalid grid position: %d, %d", grid_x, grid_y);
        return MAP_ENGINE_ERROR_UNKNOWN;
    }

    if (map_engine.map[grid_x][grid_y].type == MAP_CELL_INTEREST_AREA)
    {
        return MAP_ENGINE_OK;
    }

    map_engine.map[grid_x][grid_y].type = MAP_CELL_INTEREST_AREA;
    map_engine.interest_area_count++;

    return MAP_ENGINE_OK;
}

/* ***************************************** Public callback functions definitions *************************************** */