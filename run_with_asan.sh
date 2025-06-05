#!/bin/bash

# Script pour exécuter le programme avec AddressSanitizer sur ARM64
# Résout le problème "ASan runtime does not come first"

ASAN_LIB="/usr/lib/aarch64-linux-gnu/libasan.so.8"

# Vérifier que la bibliothèque ASan existe
if [ ! -f "$ASAN_LIB" ]; then
    echo "Erreur: Bibliothèque AddressSanitizer non trouvée: $ASAN_LIB"
    echo "Recherche d'autres versions..."
    find /usr/lib -name "*libasan*" 2>/dev/null
    exit 1
fi

# Changer vers le répertoire de build
cd build/aarch64-3d-debug || {
    echo "Erreur: Impossible d'accéder au répertoire build/aarch64-3d-debug"
    exit 1
}

# Exécuter avec LD_PRELOAD
echo "Lancement avec LD_PRELOAD=$ASAN_LIB"
LD_PRELOAD="$ASAN_LIB" ./bin/explo_intox_3D "$@" 