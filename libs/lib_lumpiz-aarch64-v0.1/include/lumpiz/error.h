/*
 * @file error.h
 *
 * Gestion d'erreur de l'API de programmation du capteur de luminosité d'une PiZ.
 *
 * @version 0.1
 * @author Matthias Brun (matthias.brun@eseo.fr)
 */

#ifndef LUMPIZ_ERROR_H_
#define LUMPIZ_ERROR_H_

/**
 * @defgroup lumpiz_error Gestion d'erreur pour la programmation du capteur de luminosité d'une PiZ.
 *
 * @brief
 * Fonctions et macros utiles à la gestion d'erreur pour la programmation du capteur de luminosité d'une PiZ.
 *
 * @{
 */

/**
 * @enum lumpiz_error_t
 *
 * @brief Liste des erreurs LumPiz
 *
 * Code d'erreur utilisés pour renseigné errno lors de l'exécution des fonctions de la bibliothèque.
 *
 * Remarque : L'énumératin commence à 1 (errno n'est jamais mis à zero lorsqu'une erreur survient (man errno)).
 *
 */
typedef enum
{
	// Erreurs spécifiques à Intox :

	LUMPIZ_INTOX_E_SYSTEM				= 1,		/**< Problème interne d'utilisation d'un appel système (voir logs précédents issus de perror). */

	LUMPIZ_INTOX_E_SOCKET,						/**< Erreur durant la création de la socket de connexion au simulateur. */
	LUMPIZ_INTOX_E_CONNECT,						/**< Erreur durant la connexion au simulateur Intox (erreur par défaut). */
	LUMPIZ_INTOX_E_CONNECT_REFUSED,				/**< Erreur durant la connexion au simulateur Intox : connexion refusée. */
	LUMPIZ_INTOX_E_CONNECT_NET,					/**< Erreur durant la connexion au simulateur Intox : réseau inaccessible. */
	LUMPIZ_INTOX_E_CONNECT_HOST,					/**< Erreur durant la connexion au simulateur Intox : serveur inaccessible. */
	LUMPIZ_INTOX_E_CONNECT_TIMEOUT,				/**< Erreur durant la connexion au simulateur Intox : échéance de connexion. */

	LUMPIZ_INTOX_E_ACCESS, 						/**< Erreur d'accès au simulateur Intox. */
	LUMPIZ_INTOX_E_LOST, 						/**< Perte d'accès au simulateur Intox. */
	LUMPIZ_INTOX_E_CMD,	 						/**< Commande invalide au simulateur Intox. */

	// Erreurs LUMPIZ :

	LUMPIZ_E_LUM_INIT,						/**< Erreur durant l'initialisation du capteur de luminosité. */
	LUMPIZ_E_LUM_SENSOR_ID,					/**< Identifiant de capteur de luminosité invalide. */
	LUMPIZ_E_LUM_SENSOR_STATE,				/**< État d'allumage du capteur de luminosité invalide. */

	LUMPIZ_E_SYSTEM							/**< Problème interne d'utilisation d'un appel système (voir logs précédents issus de perror). */

} lumpiz_error_t;


/**
 * @fn char const * lumpiz_error_msg()
 *
 * @brief Donne le message d'erreur (associé à la valeur de errno de la lib lumpiz).
 *
 * @return le message d'erreur
 */
char const * lumpiz_error_msg();

/**
 * @fn void lumpiz_error_print(char * msg)
 *
 * @brief Affiche le message d'erreur (associé à la valeur errno de la lib lumpiz).
 *
 *
 * @param msg le préfixe du message à afficher (NULL si pas de préfixe)
 */
void lumpiz_error_print(char * msg);

/** @} */
// end of lumpiz_error group

#endif /* LUMPIZ_ERROR_H_ */
