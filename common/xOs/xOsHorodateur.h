////////////////////////////////////////////////////////////
//  horodateur header file
//  defines timestamp types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_HORODATEUR_H_
#define XOS_HORODATEUR_H_

#include <stdint.h>

// Horodateur error codes
#define XOS_HORODATEUR_OK            0
#define XOS_HORODATEUR_ERROR        -1
#define XOS_HORODATEUR_INVALID      -2

//////////////////////////////////
/// @brief Get current formatted timestamp
/// @return Pointer to static timestamp string
//////////////////////////////////
const char* xHorodateurGetString(void);

//////////////////////////////////
/// @brief Get current timestamp in seconds
/// @return Current Unix timestamp
//////////////////////////////////
uint32_t xHorodateurGet(void);

#endif // XOS_HORODATEUR_H_
