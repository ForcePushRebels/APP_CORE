#!/usr/bin/env bash
set -euo pipefail

# Config défaut (modifiable via variables d'env ou options)
PI_USER="${PI_USER:-pato}"
PI_PASS="${PI_PASS:-pato}"
PI_HOST="${PI_HOST:-10.42.0.1}"
REMOTE_BASE="${REMOTE_BASE:-/home/${PI_USER}/app_core}"
# Dossier de build local (choisir raspi-debug ou raspi-release)
LOCAL_BUILD_DIR="${LOCAL_BUILD_DIR:-/home/chris/PATO/APP_CORE/build/raspi-debug}"

# Déductions
LOCAL_BIN_DIR="${LOCAL_BUILD_DIR}/bin"
LOCAL_PKI_DIR="${LOCAL_BUILD_DIR}/pki"

usage() {
  echo "Usage: PI_USER=pato PI_PASS=pato PI_HOST=10.42.0.1 LOCAL_BUILD_DIR=/path/to/build ./deploy_to_pi.sh"
  echo "Par défaut: PI_USER=pato, PI_PASS=pato, PI_HOST=10.42.0.1, LOCAL_BUILD_DIR=${LOCAL_BUILD_DIR}"
}

# Dépendances
need() { command -v "$1" >/dev/null 2>&1 || { echo "ERROR: '$1' est requis."; exit 1; }; }
need sshpass
need rsync
need ssh

# Checks locaux
[ -d "${LOCAL_BIN_DIR}" ] || { echo "ERROR: '${LOCAL_BIN_DIR}' introuvable."; usage; exit 1; }
[ -x "${LOCAL_BIN_DIR}/explo_raspi_debug" ] || [ -x "${LOCAL_BIN_DIR}/explo_raspi_release" ] || {
  echo "WARN: exécutable explo introuvable dans ${LOCAL_BIN_DIR} (debug/release)."; }
[ -x "${LOCAL_BIN_DIR}/inter_raspi_debug" ] || [ -x "${LOCAL_BIN_DIR}/inter_raspi_release" ] || {
  echo "WARN: exécutable inter introuvable dans ${LOCAL_BIN_DIR} (debug/release)."; }
[ -d "${LOCAL_PKI_DIR}" ] || { echo "ERROR: PKI manquante: '${LOCAL_PKI_DIR}'"; exit 1; }

# Vérification structure PKI locale (identique à x86_64)
echo "[0/5] Vérification structure PKI locale..."
REQUIRED_PKI_FILES=(
  "${LOCAL_PKI_DIR}/root/ca.pem"
  "${LOCAL_PKI_DIR}/root/ca.key"
  "${LOCAL_PKI_DIR}/server/server.pem"
  "${LOCAL_PKI_DIR}/server/server.key"
  "${LOCAL_PKI_DIR}/server/fullchain.pem"
  "${LOCAL_PKI_DIR}/client/client.pem"
  "${LOCAL_PKI_DIR}/client/client.key"
  "${LOCAL_PKI_DIR}/client/fullchain.pem"
  "${LOCAL_PKI_DIR}/sub/sub.pem"
  "${LOCAL_PKI_DIR}/sub/sub.key"
)

for file in "${REQUIRED_PKI_FILES[@]}"; do
  [ -f "$file" ] || { echo "ERROR: Fichier PKI manquant: $file"; exit 1; }
done
echo "✓ Structure PKI locale valide (10 fichiers trouvés)"

echo "[1/5] Création des dossiers distants: ${REMOTE_BASE}/{bin,pki}"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" \
  "mkdir -p '${REMOTE_BASE}/bin' '${REMOTE_BASE}/pki'"

echo "[2/5] Envoi des binaires -> ${REMOTE_BASE}/bin"
# rsync avec chmod explicite : exécutables doivent être rwxr-xr-x (755)
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rwx,Fg=rx,Fo=rx \
  --delete "${LOCAL_BIN_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/bin/"

echo "[3/5] Envoi de la PKI -> ${REMOTE_BASE}/pki"
# PKI: certificats 644, clés privées 600 (sécurité)
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
  --delete "${LOCAL_PKI_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/"

echo "[4/5] Sécurisation des clés privées (chmod 600)"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  chmod 600 '${REMOTE_BASE}/pki/root/ca.key' 2>/dev/null || true; \
  chmod 600 '${REMOTE_BASE}/pki/server/server.key' 2>/dev/null || true; \
  chmod 600 '${REMOTE_BASE}/pki/client/client.key' 2>/dev/null || true; \
  chmod 600 '${REMOTE_BASE}/pki/sub/sub.key' 2>/dev/null || true; \
"

echo "[5/5] Vérification finale côté distant"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  set -e; \
  echo '=== BINAIRES ==='; \
  ls -lh '${REMOTE_BASE}/bin' | grep -E '(explo|inter)'; \
  \
  echo ''; \
  echo '=== PKI ROOT ==='; \
  ls -lh '${REMOTE_BASE}/pki/root'; \
  \
  echo ''; \
  echo '=== PKI SERVER ==='; \
  ls -lh '${REMOTE_BASE}/pki/server'; \
  \
  echo ''; \
  echo '=== PKI CLIENT ==='; \
  ls -lh '${REMOTE_BASE}/pki/client'; \
  \
  echo ''; \
  echo '=== PKI SUB ==='; \
  ls -lh '${REMOTE_BASE}/pki/sub'; \
  \
  echo ''; \
  echo '=== VÉRIFICATION FICHIERS PKI OBLIGATOIRES ==='; \
  test -r '${REMOTE_BASE}/pki/root/ca.pem' && echo '✓ ca.pem (ROOT)' || { echo '✗ MISSING: ca.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/server/server.pem' && echo '✓ server.pem (SERVER)' || { echo '✗ MISSING: server.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/server/fullchain.pem' && echo '✓ fullchain.pem (SERVER)' || { echo '✗ MISSING: server fullchain.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/client/client.pem' && echo '✓ client.pem (CLIENT)' || { echo '✗ MISSING: client.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/client/fullchain.pem' && echo '✓ fullchain.pem (CLIENT)' || { echo '✗ MISSING: client fullchain.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/sub/sub.pem' && echo '✓ sub.pem (INTERMEDIATE)' || { echo '✗ MISSING: sub.pem'; exit 1; }; \
  \
  echo ''; \
  echo '=== TEST EXÉCUTION BINAIRES ==='; \
  test -x '${REMOTE_BASE}/bin/explo_raspi_debug' && echo '✓ explo_raspi_debug est exécutable' || { echo '✗ explo_raspi_debug NON exécutable'; exit 1; }; \
  test -x '${REMOTE_BASE}/bin/inter_raspi_debug' && echo '✓ inter_raspi_debug est exécutable' || { echo '✗ inter_raspi_debug NON exécutable'; exit 1; }; \
  \
  echo ''; \
  echo '=== STRUCTURE PKI COMPLÈTE ==='; \
  tree -L 2 '${REMOTE_BASE}/pki' 2>/dev/null || find '${REMOTE_BASE}/pki' -type f; \
"

echo ""
echo "=========================================="
echo "✓ Déploiement réussi: ${PI_USER}@${PI_HOST}:${REMOTE_BASE}"
echo "=========================================="
echo ""
echo "Structure PKI identique à x86_64-debug confirmée"
echo "Chemins de certificats pour le code embarqué:"
echo "  - Root CA:      ${REMOTE_BASE}/pki/root/ca.pem"
echo "  - Server cert:  ${REMOTE_BASE}/pki/server/server.pem"
echo "  - Server chain: ${REMOTE_BASE}/pki/server/fullchain.pem"
echo "  - Client cert:  ${REMOTE_BASE}/pki/client/client.pem"
echo "  - Client chain: ${REMOTE_BASE}/pki/client/fullchain.pem"
echo ""
echo "Pour exécuter:"
echo "  ssh ${PI_USER}@${PI_HOST} 'cd ${REMOTE_BASE}/bin && ./explo_raspi_debug'"
