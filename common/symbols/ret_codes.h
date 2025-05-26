// SPDX-License-Identifier: LicenseRef-PATO-ESEO
// SPDX-Licence-CopyrightText: PATO ESEO License (see LICENSE.md)

#ifndef __RET_CODES_H__
#define __RET_CODES_H__

#include <stddef.h>
#include <stdbool.h>

/* --- Codes de retour génériques --- */
#define RET_OK                0   // Succès
#define RET_ERR_GENERIC       1   // Erreur générique (échec)

/* --- Codes d'erreur détaillés --- */
#define RET_ERR_INVALID_ARG   2   // Argument invalide
#define RET_ERR_TIMEOUT       3   // Timeout
#define RET_ERR_IO            4   // Erreur I/O
#define RET_ERR_NOT_FOUND     5   // Ressource non trouvée
#define RET_ERR_UNSUPPORTED   6   // Fonctionnalité non supportée
#define RET_ERR_BUSY          7   // Ressource occupée
#define RET_ERR_NOMEM         8   // Mémoire insuffisante
#define RET_ERR_PERM          9   // Permission refusée
#define RET_ERR_STATE        10   // État invalide
#define RET_ERR_RANGE        11   // Valeur hors plage
#define RET_ERR_NULL         12   // Pointeur NULL

/* --- Codes pour valeurs "non implémenté" --- */
#define RET_NOT_IMPL_INT     (-1)
#define RET_NOT_IMPL_BOOL    false
#define RET_NOT_IMPL_PTR     NULL
#define RET_NOT_IMPL_STR     "unused"

#endif /* __RET_CODES_H__ */
