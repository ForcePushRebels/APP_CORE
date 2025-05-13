////////////////////////////////////////////////////////////
//  Network core header file for embedded systems
//  Defines the basic network types and constants
//  IPv4-focused, simplified API with thread-safety
//
// general disclosure: copy or share the file is forbidden
// Written : 13/05/2025
////////////////////////////////////////////////////////////

/*
 *  Standard CRC32 Checksum Algorithm (zlib compatible)
 *
 *  Polynomial: 0x04C11DB7
 *
 *  Used in Ethernet, gzip, zip, PNG, etc.
 *  Compatible with java.util.zip.CRC32 and zlib crc32
 */

#ifndef CRC32_H_
#define CRC32_H_

#include <stdint.h>
#include <stddef.h>

////////////////////////////////////////////////////////////
/// @brief Calculate the CRC32 checksum of a buffer (zlib compatible)
/// @param data The data buffer to calculate the checksum for
/// @param length The length of the data buffer
/// @return The calculated CRC32 checksum
////////////////////////////////////////////////////////////
uint32_t compute_crc32(const uint8_t *data, unsigned long length);

#endif // CRC32_H_