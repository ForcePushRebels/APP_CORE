#!/bin/bash

# Script PKI à 3+ niveaux avec ECC (Root CA → Intermédiaires par usage → Certificats finaux)
# Respecte la séparation ANSSI :
#   - intermediate-robots : pour robots
#   - intermediate-command : pour la tablette
#   - intermediate-development : pour localhost/tests
#
# Les SAN sont pris en compte exactement comme dans le script initial
# Les fichiers ca-chain.pem servent UNIQUEMENT à openssl verify, PAS à TLS/mTLS

set -e

# Chemin d'installation
if [ $# -gt 0 ]; then
    EXECUTABLE_PATH="$1"
    BIN_DIR=$(dirname "$EXECUTABLE_PATH")
    BUILD_DIR=$(dirname "$BIN_DIR")
    PKI_DIR="$BUILD_DIR/pki"
else
    PKI_DIR="./pki"
fi

DAYS_ROOT_CA=7300
DAYS_INT_CA=3650
DAYS_DEV_CA=1825
DAYS_CERT=730

CURVE_ROOT="secp384r1"
CURVE_INT="secp384r1"
CURVE_FINAL="prime256v1"

# === Détection automatique du hostname local
LOCAL_HOSTNAME=$(hostname)
LOCAL_HOSTNAME_SHORT=$(hostname -s 2>/dev/null || hostname)

# === Définir les intermédiaires ===
declare -A INTERMEDIATES=(
    [robots]="intermediate-robots"
    [command]="intermediate-command"
    [development]="intermediate-development"
)

# Dictionnaire acteur -> intermédiaire
declare -A ACTOR_INTER=(
    [command]="command:${INTERMEDIATES[command]}"
    [pato-explo]="pato-explo:${INTERMEDIATES[robots]}"
    [pato-inter]="pato-inter:${INTERMEDIATES[robots]}"
    [local]="local:${INTERMEDIATES[development]}"
)

declare -A NEED_SERVER=(
    [command]="no"
    [pato-explo]="yes"
    [pato-inter]="no"
    [local]="yes"
)

# Création dossiers
mkdir -p "$PKI_DIR/root-ca"
for inter in "${INTERMEDIATES[@]}"; do
    mkdir -p "$PKI_DIR/${inter}/csr"
done
for actor in "${!ACTOR_INTER[@]}"; do
    actor_name=${actor}
    mkdir -p "$PKI_DIR/$actor_name"
done

chmod -R 755 "$PKI_DIR"

# === Génération du Root CA ===
if [ ! -f "$PKI_DIR/root-ca/root-ca.key" ]; then
    openssl ecparam -name $CURVE_ROOT -genkey -noout -out "$PKI_DIR/root-ca/root-ca.key"
    cat > "$PKI_DIR/root-ca/root-ca.cnf" <<EOF
[req]
distinguished_name = req_distinguished_name
x509_extensions = v3_ca
prompt = no
[req_distinguished_name]
C = FR
ST = Pays-de-la-Loire
L = Angers
O = Robots-PKI
OU = Cybersecurite-Embarquee
CN = Root-CA-Robots-ECC
[v3_ca]
basicConstraints = critical,CA:TRUE
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
keyUsage = critical,keyCertSign,cRLSign
EOF
    openssl req -new -x509 -days $DAYS_ROOT_CA -config "$PKI_DIR/root-ca/root-ca.cnf" \
        -key "$PKI_DIR/root-ca/root-ca.key" -sha384 -out "$PKI_DIR/root-ca/root-ca.pem"
    chmod 400 "$PKI_DIR/root-ca/root-ca.key"
fi

# === Génération des Intermediate CA ===
for usage in "${!INTERMEDIATES[@]}"; do
    inter_dir="$PKI_DIR/${INTERMEDIATES[$usage]}"
    if [ ! -f "$inter_dir/${INTERMEDIATES[$usage]}.key" ]; then
        openssl ecparam -name $CURVE_INT -genkey -noout -out "$inter_dir/${INTERMEDIATES[$usage]}.key"
        chmod 400 "$inter_dir/${INTERMEDIATES[$usage]}.key"
        CN="Intermediate-CA-${usage^}"
        cat > "$inter_dir/${INTERMEDIATES[$usage]}.cnf" <<EOF
[req]
distinguished_name = req_distinguished_name
prompt = no
[req_distinguished_name]
C = FR
ST = Pays-de-la-Loire
L = Angers
O = Robots-PKI
OU = Cybersecurite-Embarquee
CN = $CN
[v3_intermediate_ca]
basicConstraints = critical,CA:TRUE,pathlen:0
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
keyUsage = critical,keyCertSign,cRLSign
EOF
        openssl req -new -sha384 -config "$inter_dir/${INTERMEDIATES[$usage]}.cnf" \
            -key "$inter_dir/${INTERMEDIATES[$usage]}.key" \
            -out "$inter_dir/csr/${INTERMEDIATES[$usage]}.csr"
        openssl x509 -req -in "$inter_dir/csr/${INTERMEDIATES[$usage]}.csr" \
            -CA "$PKI_DIR/root-ca/root-ca.pem" \
            -CAkey "$PKI_DIR/root-ca/root-ca.key" \
            -CAcreateserial \
            -out "$inter_dir/${INTERMEDIATES[$usage]}.pem" \
            -days $DAYS_INT_CA -sha384 \
            -extfile "$inter_dir/${INTERMEDIATES[$usage]}.cnf" -extensions v3_intermediate_ca
        # ca-chain pour tests uniquement
        cat "$inter_dir/${INTERMEDIATES[$usage]}.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$inter_dir/ca-chain.pem"
    fi
done

# === Génération des certificats finaux ===
generate_actor_certificates() {
    local actor=$1
    IFS=: read actor_name inter_dir_name <<< "$2"
    local need_server=$3
    local actor_dir="$PKI_DIR/$actor_name"
    local inter_dir="$PKI_DIR/$inter_dir_name"

    cat > "$actor_dir/actor.cnf" <<EOF
[req]
distinguished_name = req_distinguished_name
prompt = no
[req_distinguished_name]
C = FR
ST = Pays-de-la-Loire
L = Angers
O = Robots-PKI
OU = $actor_name
CN = $actor_name
EOF

    # Extensions avec SAN personnalisés
    if [ "$actor_name" == "local" ]; then
        cat > "$actor_dir/extensions.cnf" <<EOF
authorityKeyIdentifier = keyid,issuer
basicConstraints = CA:FALSE
keyUsage = digitalSignature, keyAgreement
extendedKeyUsage = serverAuth, clientAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
DNS.2 = localhost.localdomain
DNS.3 = $LOCAL_HOSTNAME_SHORT
DNS.4 = $LOCAL_HOSTNAME_SHORT.local
IP.1 = 127.0.0.1
IP.2 = ::1
EOF
    else
        cat > "$actor_dir/extensions.cnf" <<EOF
authorityKeyIdentifier = keyid,issuer
basicConstraints = CA:FALSE
keyUsage = digitalSignature, keyAgreement
extendedKeyUsage = serverAuth, clientAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = $actor_name.local
DNS.2 = $actor_name
EOF
    fi

    # Certificat serveur
    if [ "$need_server" == "yes" ]; then
        openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$actor_dir/server.key"
        chmod 400 "$actor_dir/server.key"
        openssl req -new -sha256 -key "$actor_dir/server.key" -out "$actor_dir/server.csr" -config "$actor_dir/actor.cnf"
        openssl x509 -req -in "$actor_dir/server.csr" -CA "$inter_dir/${inter_dir_name}.pem" \
            -CAkey "$inter_dir/${inter_dir_name}.key" -CAcreateserial -out "$actor_dir/server.pem" \
            -days $DAYS_CERT -sha256 -extfile "$actor_dir/extensions.cnf"
        # Chaîne identité (pas de root !)
        cat "$actor_dir/server.pem" "$inter_dir/${inter_dir_name}.pem" > "$actor_dir/server-chain.pem"
        rm "$actor_dir/server.csr"
    fi

    # Certificat client
    openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$actor_dir/client.key"
    chmod 400 "$actor_dir/client.key"
    openssl req -new -sha256 -key "$actor_dir/client.key" -out "$actor_dir/client.csr" -config "$actor_dir/actor.cnf"
    openssl x509 -req -in "$actor_dir/client.csr" -CA "$inter_dir/${inter_dir_name}.pem" \
        -CAkey "$inter_dir/${inter_dir_name}.key" -CAcreateserial -out "$actor_dir/client.pem" \
        -days $DAYS_CERT -sha256 -extfile "$actor_dir/extensions.cnf"
    cat "$actor_dir/client.pem" "$inter_dir/${inter_dir_name}.pem" > "$actor_dir/client-chain.pem"
    rm "$actor_dir/client.csr"

    # Copie du root CA pour le trust store TLS - uniquement ça pour mTLS !
    cp "$PKI_DIR/root-ca/root-ca.pem" "$actor_dir/"
}

# Génération pour tous les acteurs
for actor in "${!ACTOR_INTER[@]}"; do
    generate_actor_certificates "$actor" "${ACTOR_INTER[$actor]}" "${NEED_SERVER[$actor]}"
done

echo "PKI à architecture séparée (robots, command, development) générée avec succès !"
echo "NB : Les fichiers ca-chain.pem dans les dossiers intermediate-* sont uniquement pour les tests avec openssl verify, jamais pour la configuration TLS/mTLS."

exit 0
