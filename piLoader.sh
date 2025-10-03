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
LOCAL_PKI_EXPLORER_DIR="${LOCAL_PKI_DIR}/pato-explo"
LOCAL_PKI_ROOT_DIR="${LOCAL_PKI_DIR}/root-ca"

usage() {
  echo "Usage: PI_USER=pato PI_PASS=pato PI_HOST=10.42.0.1 LOCAL_BUILD_DIR=/path/to/build ./piLoader.sh"
  echo "Par défaut: PI_USER=pato, PI_PASS=pato, PI_HOST=10.42.0.1, LOCAL_BUILD_DIR=${LOCAL_BUILD_DIR}"
  echo "Cette version déploie uniquement l'explorateur (explo) et la PKI minimale: 'root-ca' + 'pato-explo'."
}

# Dépendences
need() { command -v "$1" >/dev/null 2>&1 || { echo "ERROR: '$1' est requis."; exit 1; }; }
need sshpass
need rsync
need ssh

# Checks locaux
[ -d "${LOCAL_BIN_DIR}" ] || { echo "ERROR: '${LOCAL_BIN_DIR}' introuvable."; usage; exit 1; }
[ -x "${LOCAL_BIN_DIR}/explo_raspi_debug" ] || [ -x "${LOCAL_BIN_DIR}/explo_raspi_release" ] || {
  echo "WARN: exécutable explo introuvable dans ${LOCAL_BIN_DIR} (debug/release)."; }
[ -d "${LOCAL_PKI_EXPLORER_DIR}" ] || { echo "ERROR: PKI explorateur manquante: '${LOCAL_PKI_EXPLORER_DIR}'"; exit 1; }
[ -d "${LOCAL_PKI_ROOT_DIR}" ] || { echo "ERROR: PKI ROOT manquante: '${LOCAL_PKI_ROOT_DIR}'"; exit 1; }

# Vérification structure PKI minimale pour EXPLO
echo "[0/7] Vérification structure PKI locale (explorateur, minimale)..."
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

echo "[1/7] Création des dossiers distants: ${REMOTE_BASE}/{bin,pki/root-ca,pki/pato-explo}"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" \
  "mkdir -p '${REMOTE_BASE}/bin' '${REMOTE_BASE}/pki/root-ca' '${REMOTE_BASE}/pki/pato-explo'"

echo "[2/7] Envoi des binaires -> ${REMOTE_BASE}/bin"
# rsync avec chmod explicite : exécutables doivent être rwxr-xr-x (755)
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rwx,Fg=rx,Fo=rx \
  --delete "${LOCAL_BIN_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/bin/"

echo "[3/7] Envoi PKI explorateur (serveur + client) -> ${REMOTE_BASE}/pki/pato-explo"
# PKI: certificats 644, clés privées 600 (sécurité) pour pato-explo
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
  --delete --include='server-chain.pem' --include='server.key' --include='client.pem' --include='client.key' --exclude='*' \
  "${LOCAL_PKI_EXPLORER_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/pato-explo/"

echo "[4/7] Envoi Root CA -> ${REMOTE_BASE}/pki/root-ca"
sshpass -p "${PI_PASS}" rsync -avz \
  --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
  "${LOCAL_PKI_ROOT_DIR}/root-ca.pem" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/root-ca/"

echo "[5/7] Sécurisation des clés privées (chmod 600)"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  chmod 600 '${REMOTE_BASE}/pki/pato-explo/server.key' 2>/dev/null || true; \
  chmod 600 '${REMOTE_BASE}/pki/pato-explo/client.key' 2>/dev/null || true; \
"

# Optionnel : Envoi de la PKI locale associée au binaire si elle existe
if [ -d "${LOCAL_PKI_DIR}" ]; then
  echo "[6/7] Envoi PKI locale associée au binaire -> ${REMOTE_BASE}/pki/local-pki"
  sshpass -p "${PI_PASS}" rsync -avz \
    --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r \
    --delete "${LOCAL_PKI_DIR}/" "${PI_USER}@${PI_HOST}:${REMOTE_BASE}/pki/local-pki/"
fi

echo "[7/7] Configuration hostname robot (pato-explo)"
# Nécessite sudo -S avec mot de passe PI_PASS
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  echo '${PI_PASS}' | sudo -S hostnamectl set-hostname pato-explo && \
  echo '${PI_PASS}' | sudo -S bash -c 'grep -q "pato-explo" /etc/hosts || echo "127.0.1.1 pato-explo" >> /etc/hosts' && \
  echo 'Hostname configuré:' && hostname && grep -m1 pato-explo /etc/hosts || true \
"

echo "[8/8] Mise à jour de l'heure système"
# Nécessite sudo -S avec mot de passe PI_PASS
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  echo '${PI_PASS}' | sudo -S timedatectl set-timezone Europe/Paris && \
  echo '${PI_PASS}' | sudo -S timedatectl set-ntp no && \
  echo '${PI_PASS}' | sudo -S timedatectl set-ntp yes && \
  echo '${PI_PASS}' | sudo -S timedatectl status && \
  echo '${PI_PASS}' | sudo -S timedatectl show"


echo "[Check] Vérification finale côté distant"
sshpass -p "${PI_PASS}" ssh -o StrictHostKeyChecking=no "${PI_USER}@${PI_HOST}" "\
  set -e; \
  echo '=== BINAIRES ==='; \
  ls -lh '${REMOTE_BASE}/bin' | grep -E '(explo|inter)'; \
  \
  echo ''; \
  echo '=== PKI ROOT-CA ==='; \
  ls -lh '${REMOTE_BASE}/pki/root-ca'; \
  \
  echo ''; \
  echo '=== PKI PATO-EXPLO ==='; \
  ls -lh '${REMOTE_BASE}/pki/pato-explo'; \
  \
  echo ''; \
  echo '=== PKI LOCALE ASSOCIÉE AU BINAIRE ==='; \
  if [ -d '${REMOTE_BASE}/pki/local-pki' ]; then ls -lh '${REMOTE_BASE}/pki/local-pki'; else echo 'Absent'; fi; \
  \
  echo ''; \
  echo '=== VÉRIFICATION FICHIERS PKI OBLIGATOIRES ==='; \
  test -r '${REMOTE_BASE}/pki/root-ca/root-ca.pem' && echo '✓ root-ca.pem (ROOT)' || { echo '✗ MISSING: root-ca.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/server-chain.pem' && echo '✓ server-chain.pem (SERVER)' || { echo '✗ MISSING: server-chain.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/server.key' && echo '✓ server.key (SERVER)' || { echo '✗ MISSING: server.key'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/client.pem' && echo '✓ client.pem (CLIENT)' || { echo '✗ MISSING: client.pem'; exit 1; }; \
  test -r '${REMOTE_BASE}/pki/pato-explo/client.key' && echo '✓ client.key (CLIENT)' || { echo '✗ MISSING: client.key'; exit 1; }; \
  \
  echo ''; \
  echo '=== TEST EXÉCUTION BINAIRES ==='; \
  test -x '${REMOTE_BASE}/bin/explo_raspi_debug' && echo '✓ explo_raspi_debug est exécutable' || { echo '✗ explo_raspi_debug NON exécutable'; exit 1; }; \
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
echo "PKI explorateur déployée"
echo "Chemins de certificats pour le code embarqué:"
echo "  - CA chain:     ${REMOTE_BASE}/pki/root-ca/root-ca.pem"
echo "  - Server chain: ${REMOTE_BASE}/pki/pato-explo/server-chain.pem"
echo "  - Server key:   ${REMOTE_BASE}/pki/pato-explo/server.key"
echo "  - Client chain: ${REMOTE_BASE}/pki/pato-explo/client.pem"
echo "  - Client key:   ${REMOTE_BASE}/pki/pato-explo/client.key"
echo ""
echo "Pour exécuter:"
echo "  ssh ${PI_USER}@${PI_HOST} 'cd ${REMOTE_BASE}/bin && ./explo_raspi_debug'"
