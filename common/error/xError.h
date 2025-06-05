#ifndef XERROR_H
#define XERROR_H

#include <stdint.h>
#include <stdbool.h>

// Error codes are defined as int32_t values
// Limits: -2,147,483,648 (0x80000000) to 2,147,483,647 (0x7FFFFFFF)
// Format: 0xCCCC0000 where CCCC is the category code

// Base codes for different categories (4 bytes format: 0xCCCC0000)
#define BASE_SYSTEM          0x10000000  // System level errors
#define BASE_NETWORK         0x20000000  // Network related errors
#define BASE_POSITION        0x30000000  // Position control errors
#define BASE_MAP            0x40000000  // Map and navigation errors
#define BASE_PILOT          0x50000000  // Pilot control errors
#define BASE_SENSOR         0x60000000  // Sensor related errors
#define BASE_STRATEGY       0x70000000  // Strategy management errors
#define BASE_SUPERVISOR     0x10000000  // Supervisor errors (moved to system base)
#define BASE_OS             0x20000000  // OS related errors (moved to network base)

// System level errors (0x10000000 - 0x1FFFFFFF)
OK
#define POSITION_OK                   0x10000000
#define POSITION_NOT_INITIALIZED      0x10000001
#define POSITION_INVALID_PARAM        0x10000002
#define POSITION_ERROR                0x10000003
#define POSITION_MUTEX_ERROR          0x10000004
#define POSITION_TASK_ERROR           0x10000005
#define POSITION_MOTOR_ERROR          0x10000006

// Supervisor errors (moved to system base)
OK
#define SUPERVISOR_OK                 0x10000010
#define SUPERVISOR_ERROR_INVALID_PARAM 0x10000011
#define SUPERVISOR_ERROR_MEMORY_ALLOCATION 0x10000012

// Network errors (0x20000000 - 0x2FFFFFFF)
OK
#define SERVER_OK                     0x20000000
#define SERVER_ERROR                  0x20000001
#define SERVER_MAX_CLIENTS_REACHED    0x20000002
#define SERVER_INVALID_STATE          0x20000003
#define SERVER_THREAD_ERROR           0x20000004
#define SERVER_CLIENT_DISCONNECTED    0x20000005
#define SERVER_SOCKET_ERROR           0x20000006
#define SERVER_MEMORY_ERROR           0x20000007
#define SERVER_TIMEOUT                0x20000008
#define SERVER_INVALID_PARAM          0x20000009
#define SERVER_NOT_RUNNING            0x2000000A
#define SERVER_CLIENT_NOT_FOUND       0x2000000B

OK
#define NETWORK_OK                    0x21000000
#define NETWORK_ERROR                 0x21000001
#define NETWORK_TIMEOUT               0x21000002
#define NETWORK_INVALID_PARAM         0x21000003
#define NETWORK_ZERO_SIZE             0x21000004

// OS related errors (moved to network base)
OK
#define XOS_TIMER_OK                  0x22000000
#define XOS_TIMER_ERROR               0x22000001
#define XOS_TIMER_INVALID             0x22000002
#define XOS_TIMER_TIMEOUT             0x22000003
#define XOS_TIMER_NOT_INIT            0x22000004

OK
#define MUTEX_OK                      0x22100000
#define MUTEX_ERROR                   0x22100001
#define MUTEX_TIMEOUT                 0x22100002
#define MUTEX_INVALID                 0x22100003
#define MUTEX_ALREADY_INIT            0x22100004
#define MUTEX_NOT_INIT                0x22100005

OK
#define OS_TASK_SUCCESS               0x22200000
#define OS_TASK_ERROR_NULL_POINTER    0x22200001
#define OS_TASK_ERROR_INVALID_PARAM   0x22200002
#define OS_TASK_ERROR_INIT_FAILED     0x22200003
#define OS_TASK_ERROR_CREATE_FAILED   0x22200004
#define OS_TASK_ERROR_ALREADY_RUNNING 0x22200005
#define OS_TASK_ERROR_NOT_RUNNING     0x22200006
#define OS_TASK_ERROR_TERMINATE_FAILED 0x22200007
#define OS_TASK_ERROR_JOIN_FAILED     0x22200008
#define OS_TASK_ERROR_TIMEOUT         0x22200009
#define OS_TASK_ERROR_PRIORITY        0x2220000A
#define OS_TASK_ERROR_STACK_SIZE      0x2220000B
#define OS_TASK_ERROR_POLICY          0x2220000C

OK
#define XOS_LOG_OK                    0x22300000
#define XOS_LOG_ERROR                 0x22300001
#define XOS_LOG_INVALID               0x22300002
#define XOS_LOG_NOT_INIT              0x22300003
#define XOS_LOG_MUTEX_ERROR           0x22300004
#define XOS_LOG_SECURITY_ERROR        0x22300005

OK
#define WATCHDOG_SUCCESS              0x22400000
#define WATCHDOG_ERROR_INIT_FAILED    0x22400001
#define WATCHDOG_ERROR_ALREADY_INIT   0x22400002
#define WATCHDOG_ERROR_NOT_INIT       0x22400003
#define WATCHDOG_ERROR_TIMER_CREATE   0x22400004
#define WATCHDOG_ERROR_TIMER_SET      0x22400005
#define WATCHDOG_ERROR_THREAD_CREATE  0x22400006
#define WATCHDOG_ERROR_THREAD_FAILED  0x22400007
#define WATCHDOG_ERROR_INVALID_PARAM  0x22400008

OK
#define XOS_HORODATEUR_OK             0x22500000
#define XOS_HORODATEUR_ERROR          0x22500001
#define XOS_HORODATEUR_INVALID        0x22500002

OK
#define XOS_SECURE_OK                 0x22600000
#define XOS_SECURE_ERROR_NULL_POINTER 0x22600001
#define XOS_SECURE_ERROR_INVALID_SIZE 0x22600002
#define XOS_SECURE_ERROR_SIZE_OVERFLOW 0x22600003

OK
#define OS_CRITICAL_SUCCESS           0x22700000
#define OS_CRITICAL_ERROR             0x22700001

OK
#define OS_SEM_SUCCESS                0x22800000
#define OS_SEM_ERROR                  0x22800001
#define OS_SEM_TIMEOUT                0x22800002
#define OS_SEM_NOT_AVAILABLE          0x22800003

// Map and navigation errors (0x40000000 - 0x4FFFFFFF)
#define MAP_ENGINE_OK                 0x40000000
#define MAP_ENGINE_ERROR_INIT         0x40000001
#define MAP_ENGINE_ERROR_NO_MAP_AVAILABLE 0x40000002
#define MAP_ENGINE_ERROR_UPDATE_VISION 0x40000003
#define MAP_ENGINE_ERROR_UNKNOWN      0x40000004

OK
#define IHM_OK                        0x41000000
#define IHM_ERROR_INIT                0x41000001
#define IHM_ERROR_NO_MAP_AVAILABLE    0x41000002
#define IHM_ERROR_UPDATE_VISION       0x41000003
#define IHM_ERROR_UNKNOWN             0x41000004

// Pilot control errors (0x50000000 - 0x5FFFFFFF)
OK
#define PILOT_OK                      0x50000000
#define PILOT_ERROR_INVALID_ARGUMENT  0x50000001
#define PILOT_ERROR_QUEUE_FULL        0x50000002
#define PILOT_ERROR_QUEUE_EMPTY       0x50000003
#define PILOT_ERROR_INIT_FAILED       0x50000004
#define PILOT_ERROR_NOT_INITIALIZED   0x50000005

// Motor control errors (0x51000000 - 0x51FFFFFF)
OK
#define MOTOR_OK                      0x51000000
#define MOTOR_ERROR_NOT_INITIALIZED   0x51000001
#define MOTOR_ERROR_INVALID_MOTOR_ID  0x51000002
#define MOTOR_ERROR_INVALID_SPEED     0x51000003
#define MOTOR_ERROR_MUTEX_CREATE      0x51000004
#define MOTOR_ERROR_TASK_CREATE       0x51000005
#define MOTOR_ERROR_TASK_STOP         0x51000006
#define MOTOR_ERROR_ENCODER_READ      0x51000007
#define MOTOR_ERROR_SPEED_SET         0x51000008

// Sensor errors (0x60000000 - 0x6FFFFFFF)
OK
#define SENSOR_MANAGER_OK             0x60000000
#define SENSOR_MANAGER_INVALID_ARG    0x60000001

// Strategy management errors (0x70000000 - 0x7FFFFFFF)
OK
#define STRATEGY_MANAGER_OK           0x70000000
#define STRATEGY_MANAGER_ERR_INIT     0x70000001
#define STRATEGY_MANAGER_ERR_NOT_IMPL 0x70000002

OK
#define ASTAR_WRAPPER_OK              0x71000000
#define ASTAR_WRAPPER_ERR_INIT        0x71000001
#define ASTAR_WRAPPER_ERR_NOT_IMPL    0x71000002

OK
#define INTERVENTION_MANAGER_OK       0x72000000
#define INTERVENTION_MANAGER_ERR_INIT 0x72000001
#define INTERVENTION_MANAGER_ERR_NOT_IMPL 0x72000002

#endif // XERROR_H