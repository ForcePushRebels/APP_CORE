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
	
	StrategyManager *strategyManager = strategy_manager__create();

	strategy_manager__endMove(strategyManager);
	
	/*
		Logic to implement here...
	*/
	
	strategy_manager__delete(strategyManager);
	
	return EXIT_FAILURE;
}