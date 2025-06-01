/**
 * @file mrpiz.h
 *
 * API de programmation du MRPiZ pour le projet PATO.
 *
 * @version 0.1
 * @author Matthias Brun (matthias.brun@eseo.fr)
 */

#ifndef MRPIZ_H_
#define MRPIZ_H_

#ifndef DOXYGEN
#include "error.h"
#endif

/**
 * @defgroup mrpiz API de programmation d'un robot MRPiZ.
 *
 * @brief
 * Fonctions et macros utiles pour la programmation d'un MRPiZ.
 *
 * Voir les modules de documentation pour plus de détails.
 */

// defgroup mrpiz_init_close
/**
 * @defgroup mrpiz_init_close Initialisation et fermeture de l'interface de communication avec le robot.
 * @ingroup mrpiz
 *
 * @brief
 * Fonctions pour initialiser et fermer l'interface de communication avec un MRPiZ.
 *
 * @{
 */
#ifndef DOXYGEN
	#ifdef INTOX
		int mrpiz_init_intox(const char * address, const int port);
		#if defined INTOX_ADDRESS && defined INTOX_PORT
			#define xstr(s) str(s)
			#define str(s) #s
			#define mrpiz_init() mrpiz_init_intox(xstr(INTOX_ADDRESS), INTOX_PORT)
		#else
			#define mrpiz_init() mrpiz_init_intox(intox_address, intox_port)
		#endif
	#else
		int mrpiz_init();
	#endif
#else
/**
 * @fn int mrpiz_init();
 *
 * @brief Fonction d'initialisation du robot MRPiZ.
 *
 * Cette fonction doit être appelée en début de programme (avant toute utilisation de l'API du robot).
 *
 * @return 0 en cas de success, -1 sinon (cf. error.h)
 */
int mrpiz_init();
#endif


/**
 * @fn void mrpiz_close()
 *
 * @brief Fonction de terminaison d'utilisation du robot MRPiZ.
 *
 * Cette fonction doit être appelée en fin de programme.
 */
void mrpiz_close();

/** @} */
// end of mrpiz_init_close group



// defgroup mrpiz_motor
/**
 * @defgroup mrpiz_motor Manipulation des moteurs du robot.
 * @ingroup mrpiz
 *
 * @brief
 * Fonctions et macros utiles pour piloter les moteurs gauche et droit du MRPiZ.
 *
 * @{
 */

/**
 * @enum mrpiz_motor_id
 *
 * @brief Identifiants des moteurs.
 */
typedef enum
{
	MRPIZ_MOTOR_LEFT 	= 0,	/**< identifiant du moteur gauche */
	MRPIZ_MOTOR_RIGHT 	= 1,	/**< identifiant du moteur droit */
	MRPIZ_MOTOR_BOTH 	= 2		/**< identifiant des deux moteurs */
} mrpiz_motor_id;

/**
 * @def MRPIZ_ENCODE_PER_TURN
 *
 * @brief Nombre de pas d'encodage par tour pour les codeurs des moteurs.
 */
#define MRPIZ_ENCODE_PER_TURN (390)


/**
 * @fn int mrpiz_motor_set(mrpiz_motor_id id, int cmd)
 *
 * @brief Envoi une consigne à un moteur pour qu'il tourne à une certaine vitesse dans un certains sens.
 *
 * Fixe la consigne de rotation d'un moteur en pourcentage :
 * <ul>
 * <li> une consigne négative fait tourner le moteur vers l'arrière ;</li>
 * <li> une consigne positive fait tourner le moteur vers l'avant ;</li>
 * <li> une consigne nulle arrête le moteur.</li>
 * </ul>
 *
 * Remarque : L'identifiant #MRPIZ_MOTOR_BOTH permet de commander les deux moteurs.
 *
 * @param id l'identifiant du moteur
 * @param cmd la consigne en pourcentage à appliquer au moteur
 *
 * @return 0 en cas de success, -1 en cas d'erreur (cf. error.h)
 */
int mrpiz_motor_set(mrpiz_motor_id id, int cmd);

/**
 * @fn int mrpiz_motor_encoder_get(mrpiz_motor_id id)
 *
 * @brief Donne la valeur d'un codeur de moteur.
 *
 * La valeur d'un codeur se situe dans l'intervalle [-2147483648, 2147483647].
 * La gestion d'erreur se fait avec errno (cf. error.h).
 *
 * @param id l'identifiant du moteur concerné
 *
 * Remarque : L'identifiant #MRPIZ_MOTOR_BOTH n'est pas considéré comme un identifiant valide pour cette fonction (errno = #MRPIZ_E_MOTOR_ID).
 *
 * @return la valeur du codeur, errno est renseigné en cas d'erreur (cf. error.h)
 */
int mrpiz_motor_encoder_get(mrpiz_motor_id id);

/**
 * @fn int mrpiz_motor_encoder_reset(mrpiz_motor_id id)
 *
 * @brief Remise à zéro de la valeur d'un codeur d'un moteur
 *
 * @param id l'identifiant du moteur concerné
 *
 * Remarque : L'identifiant #MRPIZ_MOTOR_BOTH permet de remettre à zéro les codeurs des deux moteurs.
 *
 * @return 0 en cas de success, -1 en cas d'erreur (cf. error.h)
 *
 */
int mrpiz_motor_encoder_reset(mrpiz_motor_id id);

/** @} */
// end of mrpiz_motor group



// defgroup mrpiz_proxy_sensor
/**
 * @defgroup mrpiz_proxy_sensor Manipulation des capteurs de proximité du robot.
 * @ingroup mrpiz
 *
 * @brief
 * Fonctions et macros utiles pour piloter les capteurs de proximité du MRPiZ.
 *
 * @{
 */

/**
 * @enum mrpiz_proxy_sensor_id
 *
 * @brief Identifiants des capteurs de proximité.
 */
typedef enum
{
	MRPIZ_PROXY_SENSOR_FRONT_LEFT 			= 1,	/**< capteur de proximité avant gauche */
	MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT 	= 2,	/**< capteur de proximité avant centre gauche */
	MRPIZ_PROXY_SENSOR_FRONT_CENTER 		= 3,	/**< capteur de proximité avant centre */
	MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT 	= 4,	/**< capteur de proximité avant centre droit */
	MRPIZ_PROXY_SENSOR_FRONT_RIGHT 			= 5		/**< capteur de proximité avant droit */
} mrpiz_proxy_sensor_id;


/**
 * @fn int mrpiz_proxy_sensor_get(mrpiz_proxy_sensor_id id)
 *
 * @brief Donne la valeur d'un capteur de proximité.
 *
 * La valeur d'un capteur de proximité se situe dans l'interval [0, 255].
 *
 * @param id l'identifiant du capteur de proximité concerné
 *
 * @return la valeur du capteur, -1 en cas d'erreur (cf. error.h)
 */
int mrpiz_proxy_sensor_get(mrpiz_proxy_sensor_id id);

/** @} */
// end of mrpiz_proxy_sensor group


// defgroup mrpiz_led_rgb
/**
 * @defgroup mrpiz_led_rgb Manipulation de la led RGB du robot.
 * @ingroup mrpiz
 *
 * @brief
 * Fonctions et macros utiles pour piloter la led RGB du MRPiZ.
 *
 * @{
 */

/**
 * @enum mrpiz_led_rgb_color_t
 *
 * @brief valeurs des couleurs de la led RGB
 */
typedef enum
{
	MRPIZ_LED_OFF 		= 0,	/**< extinction de la led */
	MRPIZ_LED_RED 		= 1,	/**< couleur rouge */
	MRPIZ_LED_GREEN 	= 2,	/**< couleur verte */
	MRPIZ_LED_BLUE	 	= 3		/**< couleur bleue */
} mrpiz_led_rgb_color_t;


/**
 * @fn int mrpiz_led_rgb_set(mrpiz_led_rgb_color_t color)
 *
 * @brief Fixe la valeur de la led RGB.
 *
 * @param color la couleur (rouge, verte ou bleue) a donner à la led.
 *
 * @return 0 en cas de succès, -1 en cas d'erreur (cf. error.h)
 */
int mrpiz_led_rgb_set(mrpiz_led_rgb_color_t color);

/** @} */
// end of mrpiz_led_rgb group


// defgroup mrpiz_battery
/**
 * @defgroup mrpiz_battery Informations sur la batterie du robot.
 * @ingroup mrpiz
 *
 * @brief
 * Fonctions et macros utiles pour s'informer sur l'état de la batterie du MRPiZ.
 *
 * @{
 */

/**
 * @fn mrpiz_battery_voltage(void)
 *
 * @brief Donne la tension de la batterie.
 *
 * @return la tension en volt, -1 en cas d'erreur (cf. error.h)
 */
float mrpiz_battery_voltage(void);

/**
 * @fn int mrpiz_battery_level(void)
 *
 * @brief Donne le niveau de charge de la batterie (en pourcentage).
 *
 * @return le pourcentage de charge de la batterie, -1 en cas d'erreur (cf. error.h)
 */
int mrpiz_battery_level(void);

/** @} */
// end of mrpiz_battery group




#endif /* MRPIZ_H_ */
