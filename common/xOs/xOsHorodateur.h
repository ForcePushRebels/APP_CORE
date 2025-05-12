////////////////////////////////////////////////////////////
//  horodateur header file
//  defines horodateur functions for timestamp
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_HORODATEUR_H_
#define XOS_HORODATEUR_H_

#include <stdint.h>

// Horodateur error codes
#define XOS_HORODATEUR_OK            0xB8E73D90
#define XOS_HORODATEUR_ERROR         0xB8E73D91
#define XOS_HORODATEUR_INVALID       0xB8E73D92

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
