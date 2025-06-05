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
#include "math.h"
#include "positionControl.h"
#include "sensorManager.h"
#include "xLog.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* **************************************************** Private macros *************************************************** */
#define MAP_DEBUG 0

#define MAP_WIDTH_MM 1400
#define MAP_HEIGHT_MM 1400

#define MAP_CELL_SIZE_MM 15

#define MAP_WIDTH (MAP_WIDTH_MM / MAP_CELL_SIZE_MM)
#define MAP_HEIGHT (MAP_HEIGHT_MM / MAP_CELL_SIZE_MM)

/* ************************************************ Private type definition ********************************************** */

typedef struct
{
    map_cell_t map[MAP_WIDTH][MAP_HEIGHT];
    bool updated_cells[MAP_WIDTH][MAP_HEIGHT];
    uint8_t discovery_percent;
    int interest_area_count;
    xOsMutexCtx map_mutex;
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

    // Récupération de la position du robot
    Position_t robot_pos;
    position_control_get_position(&robot_pos);
    int16_t robot_grid_x = robot_pos.x_mm / MAP_CELL_SIZE_MM;
    int16_t robot_grid_y = robot_pos.y_mm / MAP_CELL_SIZE_MM;
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
            // Vérifier si c'est la position du robot
            if (x == robot_grid_x && y == robot_grid_y)
            {
                printf(" R  "); // 'R' pour Robot
            }
            else
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

    printf("Legend: '.' = empty, numbers = wall intensity (0-255), 'R' = robot\n");
}
#endif

/* ********************************************** Public functions definitions ******************************************* */

int map_engine_init()
{
    memset(map_engine.map, 0, sizeof(map_engine.map));
    memset(map_engine.updated_cells, 0, sizeof(map_engine.updated_cells));
    map_engine.discovery_percent = 0;

    if (mutexCreate(&map_engine.map_mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to create map mutex");
        return MAP_ENGINE_ERROR_UNKNOWN;
    }

    return MAP_ENGINE_OK;
}

int map_engine_explo_ask_current_map()
{
    return MAP_ENGINE_OK;
}

size_t map_engine_get_map_size(size_t *x_size, size_t *y_size, size_t *resolution_mm_per_cell)
{
    if (x_size != NULL)
    {
        *x_size = MAP_WIDTH;
    }
    if (y_size != NULL)
    {
        *y_size = MAP_HEIGHT;
    }

    if (resolution_mm_per_cell != NULL)
    {
        *resolution_mm_per_cell = MAP_CELL_SIZE_MM;
    }

    return sizeof(map_engine.map);
}

int map_engine_get_map(map_cell_t *map)
{
    mutexLock(&map_engine.map_mutex);
    memcpy(map, map_engine.map, sizeof(map_engine.map));
    mutexUnlock(&map_engine.map_mutex);
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

    mutexLock(&map_engine.map_mutex);
    for (uint8_t i = 0; i < points_count; i++)
    {
        // X_LOG_TRACE("Point %d: %d, %d", i, points[i].x_mm, points[i].y_mm);
        int16_t grid_x = points[i].x_mm / MAP_CELL_SIZE_MM;
        int16_t grid_y = points[i].y_mm / MAP_CELL_SIZE_MM;
        if (grid_x < 0 || grid_x >= MAP_WIDTH || grid_y < 0 || grid_y >= MAP_HEIGHT)
        {
            continue;
        }
        if (map_engine.map[grid_x][grid_y].type == MAP_CELL_INTEREST_AREA)
        {
            // interest area is priority over wall
            continue;
        }
        map_engine.map[grid_x][grid_y].type = MAP_CELL_WALL;
        if (map_engine.map[grid_x][grid_y].wall.wall_intensity < 255)
        {
            map_engine.map[grid_x][grid_y].wall.wall_intensity++;
        }
        map_engine.updated_cells[grid_x][grid_y] = true;
        // X_LOG_TRACE("Grid: %d, %d, %d", grid_x, grid_y, map_engine.map[grid_x][grid_y].wall.wall_intensity);
    }

    mutexUnlock(&map_engine.map_mutex);

#if MAP_DEBUG
    print_map();
#endif

    return MAP_ENGINE_OK;
}

static bool is_floor_sensor_in_margin(uint16_t floor_sensor, uint16_t color, uint16_t margin)
{
    return floor_sensor >= color - margin && floor_sensor <= color + margin;
}

int map_engine_update_floor_sensor(uint16_t floor_sensor)
{
#define FLOOR_SENSOR_MARGIN 5

    typedef enum
    {
        FLOOR_SENSOR_LIGHT_RED = 293,
        FLOOR_SENSOR_WHITE = 251,
        FLOOR_SENSOR_PURPLE = 401,

    } floor_sensor_color_t;

    floor_sensor_color_t colors[] = {
        FLOOR_SENSOR_LIGHT_RED,
        FLOOR_SENSOR_WHITE,
        FLOOR_SENSOR_PURPLE,
    };

    bool is_interest_area = false;
    for (uint8_t i = 0; i < sizeof(colors) / sizeof(colors[0]); i++)
    {
        if (!is_floor_sensor_in_margin(floor_sensor, colors[i], FLOOR_SENSOR_MARGIN))
        {
            continue;
        }
        is_interest_area = true;
    }

    if (!is_interest_area)
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

    mutexLock(&map_engine.map_mutex);
    if (map_engine.map[grid_x][grid_y].type == MAP_CELL_INTEREST_AREA)
    {
        mutexUnlock(&map_engine.map_mutex);
        return MAP_ENGINE_OK;
    }

    X_LOG_TRACE("Interest area detected at %d, %d", grid_x, grid_y);
    map_engine.map[grid_x][grid_y].type = MAP_CELL_INTEREST_AREA;
    map_engine.interest_area_count++;
    map_engine.updated_cells[grid_x][grid_y] = true;
    mutexUnlock(&map_engine.map_mutex);
    return MAP_ENGINE_OK;
}

uint32_t map_engine_get_hash()
{
    uint32_t hash = 2166136261UL;          // FNV-1a 32-bit offset basis
    const uint32_t fnv_prime = 16777619UL; // FNV-1a 32-bit prime

    // Hash the map data
    mutexLock(&map_engine.map_mutex);
    uint8_t *data = (uint8_t *)map_engine.map;
    size_t size = sizeof(map_engine.map);

    for (size_t i = 0; i < size; i++)
    {
        hash ^= data[i];
        hash *= fnv_prime;
    }

    // Include discovery percent and interest area count in hash
    hash ^= map_engine.discovery_percent;
    hash *= fnv_prime;

    hash ^= map_engine.interest_area_count;
    hash *= fnv_prime;
    mutexUnlock(&map_engine.map_mutex);
    return hash;
}

uint32_t map_engine_get_updated_cells_count()
{
    mutexLock(&map_engine.map_mutex);
    uint32_t count = 0;
    for (int x = 0; x < MAP_WIDTH; x++)
    {
        for (int y = 0; y < MAP_HEIGHT; y++)
        {
            if (map_engine.updated_cells[x][y])
            {
                count++;
            }
        }
    }
    mutexUnlock(&map_engine.map_mutex);
    return count;
}

uint32_t map_engine_get_updated_cells(map_fragment_t *cells, size_t cell_count)
{
    uint32_t count = 0;
    mutexLock(&map_engine.map_mutex);
    for (int x = 0; x < MAP_WIDTH; x++)
    {
        for (int y = 0; y < MAP_HEIGHT; y++)
        {
            if (map_engine.updated_cells[x][y])
            {
                if (count >= cell_count)
                {
                    mutexUnlock(&map_engine.map_mutex);
                    return count;
                }
                cells[count].x_grid = x;
                cells[count].y_grid = y;
                cells[count].cell = map_engine.map[x][y];
                count++;
            }
        }
    }
    mutexUnlock(&map_engine.map_mutex);
    return count;
}

void map_engine_clear_updated_cells(map_fragment_t *cells, size_t cell_count)
{
    mutexLock(&map_engine.map_mutex);
    for (size_t i = 0; i < cell_count; i++)
    {
        if (cells[i].x_grid < 0 || cells[i].x_grid >= MAP_WIDTH || cells[i].y_grid < 0 || cells[i].y_grid >= MAP_HEIGHT)
        {
            continue;
        }
        map_engine.updated_cells[cells[i].x_grid][cells[i].y_grid] = false;
    }
    mutexUnlock(&map_engine.map_mutex);
}

map_fragment_t map_engine_get_robot_fragment()
{
    Position_t robot_pos;
    position_control_get_position(&robot_pos);
    int16_t grid_x = robot_pos.x_mm / MAP_CELL_SIZE_MM;
    int16_t grid_y = robot_pos.y_mm / MAP_CELL_SIZE_MM;

    map_fragment_t cell = {
        .x_grid = grid_x,
        .y_grid = grid_y,
        .cell = {
            .type = MAP_CELL_ROBOT,
            .robot = {
                .robot_id = 1,
            },
        },
    };

    return cell;
}

map_fragment_t map_engine_get_fragment(int16_t x_mm, int16_t y_mm)
{
    int16_t grid_x = x_mm / MAP_CELL_SIZE_MM;
    int16_t grid_y = y_mm / MAP_CELL_SIZE_MM;

    map_fragment_t cell = {
        .x_grid = grid_x,
        .y_grid = grid_y,
        .cell = map_engine.map[grid_x][grid_y],
    };

    return cell;
}

/* ***************************************** Public callback functions definitions *************************************** */