// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file main.c
 * @brief Source file for the main module.
 *
 * @author
 * ForcePushRebels – PATO Project (collective contributor)  
 * Uriel Fodong <uriel.fodong@reseau.eseo.fr> (individual contributor)
 * *
 * @copyright
 * © 2025 ESEO – All rights reserved.
 *
 * @par License
 * PATO ESEO License (see LICENSE.md)
 */

#include "../include/main.h"

int main(int argc, char *argv[]) {
	
	(void)argc;
	(void)argv;
	
	InterventionManager *interventionManager = intervention_manager__create();

	intervention_manager__startInter(interventionManager);
	
	/*
		Logic to implement here...
	*/

	intervention_manager__stopInter(interventionManager);

	return EXIT_FAILURE;
}