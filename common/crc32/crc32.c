////////////////////////////////////////////////////////////
//  Standard CRC32 Checksum Algorithm (zlib compatible)
//  Polynomial: 0x04C11DB7
//  Used in Ethernet, gzip, zip, PNG, etc.
//  Compatible with java.util.zip.CRC32 and zlib crc32
////////////////////////////////////////////////////////////

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <zlib.h>
#include "crc32.h"

////////////////////////////////////////////////////////////
/// @brief Calculate the CRC32 checksum of a buffer (zlib compatible)
/// @param p_ptcData The data buffer to calculate the checksum for
/// @param p_ulLength The length of the data buffer
/// @return The calculated CRC32 checksum
////////////////////////////////////////////////////////////
uint32_t compute_crc32(const uint8_t *p_ptcData, unsigned long p_ulLength)
{
    if (p_ptcData == NULL && p_ulLength > 0) {
        // Protection contre les pointeurs NULL avec une longueur non nulle
        return 0;
    }
    
    // Initialisation du CRC avec la valeur de départ zlib (0)
    uLong crc = crc32(0L, Z_NULL, 0);
    
    // Calcul du CRC en utilisant la fonction zlib
    crc = crc32(crc, p_ptcData, (unsigned int)p_ulLength);
    
    return (uint32_t)crc;
}

