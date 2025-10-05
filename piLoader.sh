#!/usr/bin/env bash
set -euo pipefail

# Config défaut (modifiable via variables d'env ou options)
PI_USER="${PI_USER:-pato}"
PI_PASS="${PI_PASS:-pato}"
PI_HOST="${PI_HOST:-10.42.0.1}"
REMOTE_BASE="${REMOTE_BASE:-/home/${PI_USER}/app_core}"
LOCAL_BUILD_DIR="${LOCAL_BUILD_DIR:-/home/chris/PATO/APP_CORE/build/raspi-debug}"

# Déductions
LOCAL_BIN_DIR="${LOCAL_BUILD_DIR}/bin"
LOCAL_PKI_DIR="${LOCAL_BUILD_DIR}/pki"
LOCAL_PKI_EXPLORER_DIR="${LOCAL_PKI_DIR}/pato-explo"
LOCAL_PKI_ROOT_DIR="${LOCAL_PKI_DIR}/root-ca"

usage() {
  echo "Usage: PI_USER=pato PI_PASS=pato PI_HOST=10.42.0.1 LOCAL_BUILD_DIR=/path/to/build ./piLoader.sh"
  echo "Par défaut: PI_USER=pato, PI_PASS=pato, PI_HOST=10.42.0.1, LOCAL_BUILD_DIR=${LOCAL_BUILD_DIR}"
  echo "Cette version déploie uniquement l'explorateur (explo) et la PKI minimale: 'root-ca' + 'pato-explo'."
}

need() { command -v "$1" >/dev/null 2>&1 || { echo "ERROR: '$1' est requis."; exit 1; }; }
need sshpass
need rsync
need ssh

[ -d "${LOCAL_BIN_DIR}" ] || { echo "ERROR: '${LOCAL_BIN_DIR}' introuvable."; usage; exit 1; }
[ -x "${LOCAL_BIN_DIR}/explo_raspi_debug" ] || [ -x "${LOCAL_BIN_DIR}/explo_raspi_release" ] || {
  echo "WARN: exécutable explo introuvable dans ${LOCAL_BIN_DIR} (debug/release)."; }
[ -d "${LOCAL_PKI_EXPLORER_DIR}" ] || { echo "ERROR: PKI explorateur manquante: '${LOCAL_PKI_EXPLORER_DIR}'"; exit 1; }
[ -d "${LOCAL_PKI_ROOT_DIR}" ] || { echo "ERROR: PKI ROOT manquante: '${LOCAL_PKI_ROOT_DIR}'"; exit 1; }

echo "[0/9] Vérification structure PKI locale (explorateur, minimale)..."
REQUIRED_PKI_EXPLORER=(
  "${LOCAL_PKI_EXPLORER_DIR}/server-chain.pem"
  "${LOCAL_PKI_EXPLORER_DIR}/server.key"
  "${LOCAL_PKI_EXPLORER_DIR}/client.pem"
  "${LOCAL_PKI_EXPLORER_DIR}/client.key"
  "${LOCAL_PKI_ROOT_DIR}/root-ca.pem"
)

for file in "${REQUIRED_PKI_EXPLORER[@]}"; do
  [ -f "$file" ] || { echo "ERROR: Fichier PKI manquant: $file"; exit 1; }
done
echo "✓ Structure PKI explorateur valide (5 fichiers trouvés)"

echo "[1/9] Synchronisation de la date sur le robot"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" \
  "sudo date -s '$(date -u '+%Y-%m-%d %H:%M:%S') UTC'"

echo "[2/9] Suppression propre du dossier distant ${REMOTE_BASE}"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  set -e; \
  echo 'Suppression en cours de ${REMOTE_BASE}'; \
  sudo rm -rf '${REMOTE_BASE}'; \
  mkdir -p '${REMOTE_BASE}'; \
  echo 'Recréation du dossier ${REMOTE_BASE}'; \
"

echo "[3/9] Création des sous-dossiers distants"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" \
  "mkdir -p '${REMOTE_BASE}/bin' '${REMOTE_BASE}/pki/root-ca' '${REMOTE_BASE}/pki/pato-explo'"

echo "[4/9] Envoi des binaires"
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rwx,Fg=rx,Fo=rx \
  --delete "${LOCAL_BIN_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/bin/"

echo "[5/9] Envoi PKI explorateur (serveur + client)"
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
  --delete --include='server-chain.pem' --include='server.key' --include='client.pem' --include='client.key' --exclude='*' \
  "${LOCAL_PKI_EXPLORER_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/pato-explo/"

echo "[6/9] Envoi Root CA"
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
  "${LOCAL_PKI_ROOT_DIR}/root-ca.pem" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/root-ca/"

echo "[7/9] Sécurisation des clés privées (chmod 600)"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  chmod 600 '${REMOTE_BASE}/pki/pato-explo/server.key' 2>/dev/null || true; \
  chmod 600 '${REMOTE_BASE}/pki/pato-explo/client.key' 2>/dev/null || true;"

echo "[8/9] Configuration hostname robot (pato-explo)"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  echo '${PI_PASS}' | sudo -S hostnamectl set-hostname pato-explo && \
  echo '${PI_PASS}' | sudo -S bash -c 'grep -q \"pato-explo\" /etc/hosts || echo \"127.0.1.1 pato-explo\" >> /etc/hosts' && \
  echo 'Hostname configuré:' && hostname && grep -m1 pato-explo /etc/hosts || true"

echo "[9/9] Vérification finale côté distant"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  set -e; \
  echo '=== BINAIRES ==='; \
  ls -lh '${REMOTE_BASE}/bin' | grep -E '(explo|inter)'; \
  echo ''; \
  echo '=== PKI ROOT-CA ==='; \
  ls -lh '${REMOTE_BASE}/pki/root-ca'; \
  echo ''; \
  echo '=== PKI PATO-EXPLO ==='; \
  ls -lh '${REMOTE_BASE}/pki/pato-explo'; \
  echo ''; \
  echo '=== VÉRIFICATION FICHIERS PKI OBLIGATOIRES ==='; \
  test -r '${REMOTE_BASE}/pki/root-ca/root-ca.pem' && echo '✓ root-ca.pem (ROOT)' || { echo '✗ MISSING: root-ca.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/server-chain.pem' && echo '✓ server-chain.pem (SERVER)' || { echo '✗ MISSING: server-chain.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/server.key' && echo '✓ server.key (SERVER)' || { echo '✗ MISSING: server.key'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/client.pem' && echo '✓ client.pem (CLIENT)' || { echo '✗ MISSING: client.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/client.key' && echo '✓ client.key (CLIENT)' || { echo '✗ MISSING: client.key'; exit 1; }; \
  echo ''; \
  echo '=== TEST EXÉCUTION BINAIRES ==='; \
  test -x '${REMOTE_BASE}/bin/explo_raspi_debug' && echo '✓ explo_raspi_debug est exécutable' || { echo '✗ explo_raspi_debug NON exécutable'; exit 1; }"

echo ""
echo "=========================================="
echo "✓ Déploiement réussi: ${PI_USER}@${PI_HOST}:${REMOTE_BASE}"
echo "=========================================="
echo ""
echo "PKI explorateur déployée"
echo "Chemins de certificats pour le code embarqué:"
echo "  - CA root:       ${REMOTE_BASE}/pki/root-ca/root-ca.pem"
echo "  - Server chain:  ${REMOTE_BASE}/pki/pato-explo/server-chain.pem"
echo "  - Server key:    ${REMOTE_BASE}/pki/pato-explo/server.key"
echo "  - Client chain:  ${REMOTE_BASE}/pki/pato-explo/client.pem"
echo "  - Client key:    ${REMOTE_BASE}/pki/pato-explo/client.key"
echo ""
echo "Pour exécuter:"
echo "  ssh ${PI_USER}@${PI_HOST} 'cd ${REMOTE_BASE}/bin && ./explo_raspi_debug'"
