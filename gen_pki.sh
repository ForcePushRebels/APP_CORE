#!/usr/bin/env bash
# --------------------------------------------------------------------
# gen_pki_ecc.sh  –  PKI ECC 3 niveaux (racine, intermédiaire, EE)
# Racine : secp384r1 (P-384)          Intermédiaire : prime256v1 (P-256)
# EE     : prime256v1 (P-256)         TLS 1.3 / navigateurs compatibles
# --------------------------------------------------------------------
set -euo pipefail
umask 077                                # protège les clés privées

# ----------------------- paramètres généraux -------------------------
ROOT_DAYS=7300       # ~20 ans
ICA_DAYS=3650        # 10 ans
EE_DAYS=825         # 27 mois
DIR="pki"

CURVE_ROOT=secp384r1       # P-384  ↔ criticité forte[1]
CURVE_ICA=prime256v1       # P-256  ↔ criticité moyenne[1]
CURVE_EE=prime256v1        # P-256  ↔ feuilles[5]

mkdir -p "$DIR"/{root,ica,server,client}

# ----------------------- 1. Racine auto-signée -----------------------
openssl ecparam -name "$CURVE_ROOT" -genkey -noout -out "$DIR/root/ca.key"
openssl req -new -x509 -days "$ROOT_DAYS" \
        -subj "/C=FR/O=Demo Org/CN=Demo Root CA ECC P384" \
        -key  "$DIR/root/ca.key" -out "$DIR/root/ca.pem"

# ----------------------- 2. Intermédiaire ---------------------------
openssl ecparam -name "$CURVE_ICA" -genkey -noout -out "$DIR/ica/ica.key"
openssl req -new -key "$DIR/ica/ica.key" \
        -subj "/C=FR/O=Demo Org/CN=Demo ICA ECC P256" \
        -out  "$DIR/ica/ica.csr"

openssl x509 -req -days "$ICA_DAYS" \
        -in  "$DIR/ica/ica.csr" \
        -CA  "$DIR/root/ca.pem" -CAkey "$DIR/root/ca.key" -set_serial 1000 \
        -extfile <(printf "basicConstraints=critical,CA:true,pathlen:0\nkeyUsage=critical,keyCertSign,cRLSign") \
        -out "$DIR/ica/ica.pem"

# ----------------------- 3. Certificat serveur -----------------------
openssl ecparam -name "$CURVE_EE" -genkey -noout -out "$DIR/server/server.key"
openssl req -new -key "$DIR/server/server.key" \
        -subj "/C=FR/O=Demo Org/CN=server.demo.local" \
        -out  "$DIR/server/server.csr"

openssl x509 -req -days "$EE_DAYS" \
        -in  "$DIR/server/server.csr" \
        -CA  "$DIR/ica/ica.pem" -CAkey "$DIR/ica/ica.key" -set_serial 2000 \
        -extfile <(printf "extendedKeyUsage=serverAuth") \
        -out "$DIR/server/server.pem"

cat "$DIR/server/server.pem" "$DIR/ica/ica.pem" > "$DIR/server/fullchain.pem"

# ----------------------- 4. Certificat client -----------------------
openssl ecparam -name "$CURVE_EE" -genkey -noout -out "$DIR/client/client.key"
openssl req -new -key "$DIR/client/client.key" \
        -subj "/C=FR/O=Demo Org/CN=tls-client" \
        -out  "$DIR/client/client.csr"

openssl x509 -req -days "$EE_DAYS" \
        -in  "$DIR/client/client.csr" \
        -CA  "$DIR/ica/ica.pem" -CAkey "$DIR/ica/ica.key" -set_serial 3000 \
        -extfile <(printf "extendedKeyUsage=clientAuth") \
        -out "$DIR/client/client.pem"

cat "$DIR/client/client.pem" "$DIR/ica/ica.pem" > "$DIR/client/client.full.pem"

# ----------------------- récapitulatif ------------------------------
cat <<EOF
-----------------------------------------------------------------
PKI ECC générée dans  : $DIR
 Racine  (P-384)       : root/ca.pem      (clé root/ca.key)
 Intermédiaire (P-256) : ica/ica.pem      (clé ica/ica.key)
 Serveur   (P-256)     : server/fullchain.pem  + server/server.key
 Client    (P-256)     : client/client.full.pem + client/client.key
Copiez root/ca.pem dans le magasin de confiance de chaque pair TLS.
-----------------------------------------------------------------
EOF
