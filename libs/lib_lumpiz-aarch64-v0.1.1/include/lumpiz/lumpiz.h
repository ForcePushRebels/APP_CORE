/*
 * @file lumpiz.h
 *
 * API de programmation du capteur de luminosité d'une PiZ.
 *
 * @version 0.1
 * @author Matthias Brun (matthias.brun@eseo.fr)
 *
 */

#ifndef LUMPIZ_H_
#define LUMPIZ_H_

#ifndef DOXYGEN
#include <stddef.h>
#include "error.h"
#endif

/**
 * @defgroup lumpiz API de programmation du système de capteur de luminosité pour PiZ.
 *
 * @brief
 * Fonctions et macros utiles pour la programmation du système de capteur de luminosité pour PiZ.
 *
 * Voir les modules de documentation pour plus de détails.
 */

// defgroup lumpiz_luminosity
/**
 * @defgroup lumpiz_luminosity Manipulation du capteur de luminosité du robot.
 * @ingroup lumpiz
 *
 * @brief
 * Fonctions et macros utiles pour piloter le capteur de luminosité du robot..
 *
 * @{
 */

/**
 * @fn int lumpiz_luminosity_init()
 *
 * @brief Initialise le capteur de luminosité.
 *
 * @return 0 en cas de succès, -1 en cas d'erreur (cf. error.h)
 */
int lumpiz_luminosity_init();


/**
 * @enum lumpiz_luminosity_state_t
 *
 * @brief valeurs des états d'allumage du capteur de luminosité.
 */
typedef enum
{
	LUMPIZ_LUMINOSITY_OFF 		= 0,	/**< éteint */
	LUMPIZ_LUMINOSITY_ON 		= 1,	/**< allumé */
} lumpiz_luminosity_state_t;


/**
 * @fn int lumpiz_luminosity_light_set(lumpiz_luminosity_state_t state)
 *
 * @brief Fixe l'allumage du capteur de luminosité.
 *
 * @param state état d'allumage du capteur de luminosité.
 *
 * @return 0 en cas de succès, -1 en cas d'erreur (cf. error.h)
 */
int lumpiz_luminosity_light_set(lumpiz_luminosity_state_t state);


/**
 * @fn lumpiz_luminosity_state_t lumpiz_luminosity_light_get()
 *
 * @brief Donne l'état d'allumage du capteur de luminosité.
 *
 * @return l'état d'allumage, -1 en cas d'erreur (cf. error.h)
 */
lumpiz_luminosity_state_t lumpiz_luminosity_light_get();


/**
 * @fn int lumpiz_luminosity_get()
 *
 * @brief Donne la valeur du capteur de luminosité.
 *
 * La valeur d'un capteur de luminosité se situe dans l'interval [0, 1300].
 *
 * @return la valeur du capteur, -1 en cas d'erreur (cf. error.h)
 */
int lumpiz_luminosity_get();

/** @} */
// end of lumpiz_luminosity group


#endif /* LUMPIZ_H_ */
