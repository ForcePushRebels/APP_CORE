////////////////////////////////////////////////////////////
//  Network core header file for embedded systems
//  Defines the basic network types and constants
//  IPv4-focused, simplified API with thread-safety
//
// general disclosure: copy or share the file is forbidden
// Written : 13/05/2025
////////////////////////////////////////////////////////////

/*
 *  Castagnoli CRC32C Checksum Algorithm
 *
 *  Polynomial: 0x11EDC6F41
 *
 *  Castagnoli93: Guy Castagnoli and Stefan Braeuer and Martin Herrman
 *               "Optimization of Cyclic Redundancy-Check Codes with 24
 *                 and 32 Parity Bits",IEEE Transactions on Communication,
 *                Volume 41, Number 6, June 1993
 *
 *  Copyright (c) 2013 Red Hat, Inc.,
 *
 *  Authors:
 *   Jeff Cody <jcody@redhat.com>
 *
 *  Based on the Linux kernel cryptographic crc32c module,
 *
 *  Copyright (c) 2004 Cisco Systems, Inc.
 *  Copyright (c) 2008 Herbert Xu <herbert@gondor.apana.org.au>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 */

#ifndef CRC32_H_
#define CRC32_H_

#include <stdint.h>
#include <stddef.h>


////////////////////////////////////////////////////////////
/// @brief CRC32C polynomial
////////////////////////////////////////////////////////////
const uint32_t k_ulCrc32cPolynomial = 0x11EDC6F41;

////////////////////////////////////////////////////////////
/// @brief CRC32C initial value
////////////////////////////////////////////////////////////
const uint32_t k_ulCrc32cInitialValue = 0xffffffff;

////////////////////////////////////////////////////////////
/// @brief Calculate the CRC32C checksum of a buffer
/// @param data The data buffer to calculate the checksum for
/// @param length The length of the data buffer
/// @return The calculated CRC32C checksum
////////////////////////////////////////////////////////////
uint32_t crc32c(const uint8_t *data, unsigned int length);


#endif // CRC32_H_