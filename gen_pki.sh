#!/bin/bash

# Script de génération PKI à 3 niveaux avec ECC
# Root CA (P-384) -> Intermediate CA (P-384) -> Certificats finaux (P-256)
# Architecture mTLS pour robots et tablette Android
# Le hostname de la machine locale est détecté automatiquement

set -e

# Configuration générale
# Si un argument est fourni (chemin de l'exécutable), utiliser le répertoire build (parent de bin)
if [ $# -gt 0 ]; then
    EXECUTABLE_PATH="$1"
    BIN_DIR=$(dirname "$EXECUTABLE_PATH")
    BUILD_DIR=$(dirname "$BIN_DIR")
    PKI_DIR="$BUILD_DIR/pki"
else
    PKI_DIR="./pki"
fi
DAYS_ROOT_CA=7300    # 20 ans pour le Root CA
DAYS_INT_CA=3650     # 10 ans pour l'Intermediate CA
DAYS_CERT=730        # 2 ans pour les certificats finaux

# Courbes elliptiques
CURVE_ROOT="secp384r1"      # P-384 pour Root CA
CURVE_INT="secp384r1"       # P-384 pour Intermediate CA
CURVE_FINAL="prime256v1"    # P-256 pour certificats finaux

# ==========================================
# DÉTECTION AUTOMATIQUE DU HOSTNAME LOCAL
# ==========================================

LOCAL_HOSTNAME=$(hostname)
LOCAL_HOSTNAME_SHORT=$(hostname -s 2>/dev/null || hostname)

echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   PKI à 3 niveaux avec ECC - Robots & Tablette            ║${NC}"
echo -e "${BLUE}║   Root CA (P-384) → Int CA (P-384) → Certs (P-256)        ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"

echo -e "\n${YELLOW}[INFO]${NC} Hostname local détecté: ${GREEN}$LOCAL_HOSTNAME${NC}"
echo -e "${YELLOW}[INFO]${NC} PKI sera générée dans: ${GREEN}$PKI_DIR${NC}"

# Acteurs du système avec leurs hostnames (sans le hostname local)
declare -A ACTORS=(
    ["command"]="command"
    ["pato-explo"]="pato_explo"
    ["pato-inter"]="pato_inter"
)

# Définir qui a besoin de certificats serveur
declare -A NEED_SERVER=(
    ["command"]="no"
    ["pato-explo"]="yes"
    ["pato-inter"]="no"
)

# Couleurs pour affichage
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Création de la structure de dossiers
echo -e "\n${YELLOW}[INFO]${NC} Création de la structure de dossiers..."
mkdir -p "$PKI_DIR"/{root-ca,intermediate-ca,command,local,pato-explo,pato-inter}
mkdir -p "$PKI_DIR/intermediate-ca"/{certs,csr}

# S'assurer que les permissions sont correctes
chmod -R 755 "$PKI_DIR"

echo -e "${BLUE}[INFO]${NC} Certificats pour tests locaux (hostname: ${GREEN}$LOCAL_HOSTNAME${NC}): ${GREEN}$PKI_DIR/local/${NC}"

# ==========================================
# ÉTAPE 1 : GÉNÉRATION DU ROOT CA (P-384)
# ==========================================

if [ ! -f "$PKI_DIR/root-ca/root-ca.key" ]; then
    echo -e "\n${GREEN}[1/6] Génération du Root CA avec courbe P-384...${NC}"
    
    # Génération de la clé privée Root CA avec P-384
    openssl ecparam -name $CURVE_ROOT -genkey -noout -out "$PKI_DIR/root-ca/root-ca.key" 2>/dev/null
    
    # Fichier de configuration pour Root CA
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

    # Génération du certificat Root CA (auto-signé)
    openssl req -new -x509 -days $DAYS_ROOT_CA \
        -config "$PKI_DIR/root-ca/root-ca.cnf" \
        -key "$PKI_DIR/root-ca/root-ca.key" \
        -sha384 \
        -out "$PKI_DIR/root-ca/root-ca.pem" 2>/dev/null
    
    # Protection maximale de la clé Root CA
    chmod 400 "$PKI_DIR/root-ca/root-ca.key"
    
    echo -e "  ${GREEN}✓${NC} Root CA créé avec P-384 (secp384r1)"
    echo -e "  ${YELLOW}⚠${NC}  IMPORTANT: Protégez le fichier root-ca.key !"
else
    echo -e "\n${GREEN}[1/6] Root CA existant détecté, réutilisation${NC}"
fi

# ==========================================
# ÉTAPE 2 : GÉNÉRATION DE L'INTERMEDIATE CA (P-384)
# ==========================================

if [ ! -f "$PKI_DIR/intermediate-ca/intermediate-ca.key" ]; then
    echo -e "\n${GREEN}[2/6] Génération de l'Intermediate CA avec courbe P-384...${NC}"
    
    # Génération de la clé privée Intermediate CA avec P-384
    openssl ecparam -name $CURVE_INT -genkey -noout -out "$PKI_DIR/intermediate-ca/intermediate-ca.key" 2>/dev/null
    chmod 400 "$PKI_DIR/intermediate-ca/intermediate-ca.key"
    
    # Fichier de configuration pour Intermediate CA
    cat > "$PKI_DIR/intermediate-ca/intermediate-ca.cnf" <<EOF
[req]
distinguished_name = req_distinguished_name
prompt = no

[req_distinguished_name]
C = FR
ST = Pays-de-la-Loire
L = Angers
O = Robots-PKI
OU = Cybersecurite-Embarquee
CN = Intermediate-CA-Robots-ECC

[v3_intermediate_ca]
basicConstraints = critical,CA:TRUE,pathlen:0
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid:always,issuer
keyUsage = critical,keyCertSign,cRLSign
EOF

    # Création du CSR pour l'Intermediate CA
    openssl req -new -sha384 \
        -config "$PKI_DIR/intermediate-ca/intermediate-ca.cnf" \
        -key "$PKI_DIR/intermediate-ca/intermediate-ca.key" \
        -out "$PKI_DIR/intermediate-ca/csr/intermediate-ca.csr" 2>/dev/null
    
    # Signature du certificat Intermediate par le Root CA
    openssl x509 -req \
        -in "$PKI_DIR/intermediate-ca/csr/intermediate-ca.csr" \
        -CA "$PKI_DIR/root-ca/root-ca.pem" \
        -CAkey "$PKI_DIR/root-ca/root-ca.key" \
        -CAcreateserial \
        -out "$PKI_DIR/intermediate-ca/intermediate-ca.pem" \
        -days $DAYS_INT_CA \
        -sha384 \
        -extfile "$PKI_DIR/intermediate-ca/intermediate-ca.cnf" \
        -extensions v3_intermediate_ca 2>/dev/null
    
    # Création de la chaîne CA complète (Intermediate + Root)
    cat "$PKI_DIR/intermediate-ca/intermediate-ca.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$PKI_DIR/intermediate-ca/ca-chain.pem"
    
    echo -e "  ${GREEN}✓${NC} Intermediate CA créé avec P-384 (secp384r1)"
    
    # Vérification de la chaîne
    if openssl verify -CAfile "$PKI_DIR/root-ca/root-ca.pem" "$PKI_DIR/intermediate-ca/intermediate-ca.pem" > /dev/null 2>&1; then
        echo -e "  ${GREEN}✓${NC} Chaîne de confiance valide"
    else
        echo -e "  ${RED}✗${NC} Erreur dans la chaîne de confiance"
        exit 1
    fi
else
    echo -e "\n${GREEN}[2/6] Intermediate CA existant détecté, réutilisation${NC}"
fi

# ==========================================
# ÉTAPE 3 : FONCTION DE GÉNÉRATION DES CERTIFICATS FINAUX (P-256)
# ==========================================

generate_actor_certificates() {
    local actor_name=$1
    local hostname=$2
    local need_server=$3
    local actor_dir="$PKI_DIR/$actor_name"
    
    echo -e "\n${GREEN}[3/6] Génération des certificats pour: ${BLUE}$actor_name${NC} (${YELLOW}$hostname.local${NC})"
    
    # Configuration de base pour l'acteur
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
CN = $hostname
EOF

    # Fichier d'extensions avec SAN (CRITIQUE pour validation TLS)
    cat > "$actor_dir/extensions.cnf" <<EOF
authorityKeyIdentifier = keyid,issuer
basicConstraints = CA:FALSE
keyUsage = digitalSignature, keyAgreement
extendedKeyUsage = serverAuth, clientAuth
subjectAltName = @alt_names

[alt_names]
DNS.1 = $hostname.local
DNS.2 = $hostname
EOF

    # ==========================================
    # GÉNÉRATION CERTIFICAT SERVEUR (si nécessaire)
    # ==========================================
    
    if [ "$need_server" == "yes" ]; then
        echo -e "  ${YELLOW}→${NC} Génération certificat serveur (P-256)..."
        
        # Clé privée serveur avec P-256
        openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$actor_dir/server.key" 2>/dev/null
        chmod 400 "$actor_dir/server.key"
        
        # CSR serveur
        openssl req -new -sha256 \
            -key "$actor_dir/server.key" \
            -out "$actor_dir/server.csr" \
            -config "$actor_dir/actor.cnf" 2>/dev/null
        
        # Signature du certificat serveur par Intermediate CA
        openssl x509 -req \
            -in "$actor_dir/server.csr" \
            -CA "$PKI_DIR/intermediate-ca/intermediate-ca.pem" \
            -CAkey "$PKI_DIR/intermediate-ca/intermediate-ca.key" \
            -CAcreateserial \
            -out "$actor_dir/server.pem" \
            -days $DAYS_CERT \
            -sha256 \
            -extfile "$actor_dir/extensions.cnf" 2>/dev/null
        
        # Création de la chaîne complète serveur (server + intermediate + root)
        cat "$actor_dir/server.pem" "$PKI_DIR/intermediate-ca/intermediate-ca.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$actor_dir/server-chain.pem"
        
        echo -e "  ${GREEN}✓${NC} Certificat serveur créé"
    else
        echo -e "  ${BLUE}ℹ${NC}  Pas de certificat serveur pour cet acteur"
    fi
    
    # ==========================================
    # GÉNÉRATION CERTIFICAT CLIENT (pour mTLS - TOUJOURS)
    # ==========================================
    
    echo -e "  ${YELLOW}→${NC} Génération certificat client (P-256)..."
    
    # Clé privée client avec P-256
    openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$actor_dir/client.key" 2>/dev/null
    chmod 400 "$actor_dir/client.key"
    
    # CSR client
    openssl req -new -sha256 \
        -key "$actor_dir/client.key" \
        -out "$actor_dir/client.csr" \
        -config "$actor_dir/actor.cnf" 2>/dev/null
    
    # Signature du certificat client par Intermediate CA
    openssl x509 -req \
        -in "$actor_dir/client.csr" \
        -CA "$PKI_DIR/intermediate-ca/intermediate-ca.pem" \
        -CAkey "$PKI_DIR/intermediate-ca/intermediate-ca.key" \
        -CAcreateserial \
        -out "$actor_dir/client.pem" \
        -days $DAYS_CERT \
        -sha256 \
        -extfile "$actor_dir/extensions.cnf" 2>/dev/null
    
    # Création de la chaîne complète client (client + intermediate + root)
    cat "$actor_dir/client.pem" "$PKI_DIR/intermediate-ca/intermediate-ca.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$actor_dir/client-chain.pem"
    
    echo -e "  ${GREEN}✓${NC} Certificat client créé"
    
    # Copie de la chaîne CA pour vérification
    cp "$PKI_DIR/intermediate-ca/ca-chain.pem" "$actor_dir/"
    cp "$PKI_DIR/root-ca/root-ca.pem" "$actor_dir/"
    
    # Nettoyage des CSR
    rm -f "$actor_dir"/*.csr
    
    echo -e "  ${GREEN}✓${NC} Certificats complétés pour $actor_name"
}

# ==========================================
# ÉTAPE 3b : GÉNÉRATION DES CERTIFICATS LOCALHOST POUR TESTS
# ==========================================

generate_local_certificates() {
    local local_dir="$PKI_DIR/local"
    
    echo -e "\n${GREEN}[4/6] Génération des certificats LOCALHOST pour tests (hostname: $LOCAL_HOSTNAME)...${NC}"
    
    # Configuration de base pour localhost
    cat > "$local_dir/actor.cnf" <<EOF
[req]
distinguished_name = req_distinguished_name
prompt = no

[req_distinguished_name]
C = FR
ST = Pays-de-la-Loire
L = Angers
O = Robots-PKI
OU = localhost-tests
CN = localhost
EOF

    # Fichier d'extensions avec SAN pour localhost (incluant IPv4 et IPv6)
    cat > "$local_dir/extensions.cnf" <<EOF
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

    # ==========================================
    # GÉNÉRATION CERTIFICAT SERVEUR LOCALHOST
    # ==========================================
    
    echo -e "  ${YELLOW}→${NC} Génération certificat serveur localhost (P-256)..."
    
    # Clé privée serveur avec P-256
    openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$local_dir/server.key" 2>/dev/null
    chmod 400 "$local_dir/server.key"
    
    # CSR serveur
    openssl req -new -sha256 \
        -key "$local_dir/server.key" \
        -out "$local_dir/server.csr" \
        -config "$local_dir/actor.cnf" 2>/dev/null
    
    # Signature du certificat serveur par Intermediate CA
    openssl x509 -req \
        -in "$local_dir/server.csr" \
        -CA "$PKI_DIR/intermediate-ca/intermediate-ca.pem" \
        -CAkey "$PKI_DIR/intermediate-ca/intermediate-ca.key" \
        -CAcreateserial \
        -out "$local_dir/server.pem" \
        -days $DAYS_CERT \
        -sha256 \
        -extfile "$local_dir/extensions.cnf" 2>/dev/null
    
    # Création de la chaîne complète serveur
    cat "$local_dir/server.pem" "$PKI_DIR/intermediate-ca/intermediate-ca.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$local_dir/server-chain.pem"
    
    echo -e "  ${GREEN}✓${NC} Certificat serveur localhost créé"
    
    # ==========================================
    # GÉNÉRATION CERTIFICAT CLIENT LOCALHOST
    # ==========================================
    
    echo -e "  ${YELLOW}→${NC} Génération certificat client localhost (P-256)..."
    
    # Clé privée client avec P-256
    openssl ecparam -name $CURVE_FINAL -genkey -noout -out "$local_dir/client.key" 2>/dev/null
    chmod 400 "$local_dir/client.key"
    
    # CSR client
    openssl req -new -sha256 \
        -key "$local_dir/client.key" \
        -out "$local_dir/client.csr" \
        -config "$local_dir/actor.cnf" 2>/dev/null
    
    # Signature du certificat client par Intermediate CA
    openssl x509 -req \
        -in "$local_dir/client.csr" \
        -CA "$PKI_DIR/intermediate-ca/intermediate-ca.pem" \
        -CAkey "$PKI_DIR/intermediate-ca/intermediate-ca.key" \
        -CAcreateserial \
        -out "$local_dir/client.pem" \
        -days $DAYS_CERT \
        -sha256 \
        -extfile "$local_dir/extensions.cnf" 2>/dev/null
    
    # Création de la chaîne complète client
    cat "$local_dir/client.pem" "$PKI_DIR/intermediate-ca/intermediate-ca.pem" "$PKI_DIR/root-ca/root-ca.pem" > "$local_dir/client-chain.pem"
    
    echo -e "  ${GREEN}✓${NC} Certificat client localhost créé"
    
    # Copie de la chaîne CA pour vérification
    cp "$PKI_DIR/intermediate-ca/ca-chain.pem" "$local_dir/"
    cp "$PKI_DIR/root-ca/root-ca.pem" "$local_dir/"
    
    # Nettoyage des CSR
    rm -f "$local_dir"/*.csr
    
    # Affichage des SAN pour info
    echo -e "  ${BLUE}ℹ${NC}  SAN configurés pour localhost:"
    openssl x509 -in "$local_dir/server.pem" -noout -text | grep -A 6 "Subject Alternative Name" | tail -n 6 | sed 's/^/     /'
    
    echo -e "  ${GREEN}✓${NC} Certificats localhost complétés"
}

# ==========================================
# ÉTAPE 4 : GÉNÉRATION DES CERTIFICATS POUR TOUS LES ACTEURS
# ==========================================

echo -e "\n${GREEN}[5/6] Génération des certificats pour tous les acteurs...${NC}"

for actor in "${!ACTORS[@]}"; do
    generate_actor_certificates "$actor" "${ACTORS[$actor]}" "${NEED_SERVER[$actor]}"
done

# Génération des certificats localhost
generate_local_certificates

# ==========================================
# ÉTAPE 5 : VÉRIFICATION DES CERTIFICATS
# ==========================================

echo -e "\n${GREEN}[6/6] Vérification de l'intégrité des certificats...${NC}"

VERIFICATION_SUCCESS=true

for actor in "${!ACTORS[@]}"; do
    echo -e "\n${YELLOW}Vérification:${NC} $actor"
    
    # Vérification certificat serveur (si existe)
    if [ -f "$PKI_DIR/$actor/server.pem" ]; then
        if openssl verify -CAfile "$PKI_DIR/intermediate-ca/ca-chain.pem" "$PKI_DIR/$actor/server.pem" > /dev/null 2>&1; then
            echo -e "  ${GREEN}✓${NC} Certificat serveur valide"
        else
            echo -e "  ${RED}✗${NC} Certificat serveur invalide"
            VERIFICATION_SUCCESS=false
        fi
        
        # Vérification de la courbe
        CURVE=$(openssl ec -in "$PKI_DIR/$actor/server.key" -text -noout 2>/dev/null | grep "NIST CURVE" | awk '{print $3}')
        echo -e "  ${BLUE}ℹ${NC}  Courbe serveur: $CURVE"
    fi
    
    # Vérification certificat client
    if openssl verify -CAfile "$PKI_DIR/intermediate-ca/ca-chain.pem" "$PKI_DIR/$actor/client.pem" > /dev/null 2>&1; then
        echo -e "  ${GREEN}✓${NC} Certificat client valide"
    else
        echo -e "  ${RED}✗${NC} Certificat client invalide"
        VERIFICATION_SUCCESS=false
    fi
    
    # Vérification de la courbe
    CURVE=$(openssl ec -in "$PKI_DIR/$actor/client.key" -text -noout 2>/dev/null | grep "NIST CURVE" | awk '{print $3}')
    echo -e "  ${BLUE}ℹ${NC}  Courbe client: $CURVE"
    
    # Affichage des SAN
    echo -e "  ${BLUE}ℹ${NC}  SAN configurés:"
    openssl x509 -in "$PKI_DIR/$actor/client.pem" -noout -text | grep -A 1 "Subject Alternative Name" | tail -n 1 | sed 's/^/     /'
done

# Vérification des certificats localhost
echo -e "\n${YELLOW}Vérification:${NC} localhost (tests locaux)"

if openssl verify -CAfile "$PKI_DIR/intermediate-ca/ca-chain.pem" "$PKI_DIR/local/server.pem" > /dev/null 2>&1; then
    echo -e "  ${GREEN}✓${NC} Certificat serveur localhost valide"
else
    echo -e "  ${RED}✗${NC} Certificat serveur localhost invalide"
    VERIFICATION_SUCCESS=false
fi

if openssl verify -CAfile "$PKI_DIR/intermediate-ca/ca-chain.pem" "$PKI_DIR/local/client.pem" > /dev/null 2>&1; then
    echo -e "  ${GREEN}✓${NC} Certificat client localhost valide"
else
    echo -e "  ${RED}✗${NC} Certificat client localhost invalide"
    VERIFICATION_SUCCESS=false
fi

# ==========================================
# RÉSUMÉ FINAL
# ==========================================

echo -e "\n${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║          PKI À 3 NIVEAUX GÉNÉRÉE AVEC SUCCÈS ! 🎉        ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"

echo -e "\n${GREEN}✓ Root CA créé${NC} (P-384, 20 ans)"
echo -e "${GREEN}✓ Intermediate CA créé${NC} (P-384, 10 ans)"
echo -e "${GREEN}✓ 3 acteurs configurés:${NC}"
echo -e "  • command (tablette) → ${YELLOW}command.local${NC} ${BLUE}[CLIENT ONLY]${NC}"
echo -e "  • pato-explo → ${YELLOW}pato_explo.local${NC} ${GREEN}[SERVER + CLIENT]${NC}"
echo -e "  • pato-inter → ${YELLOW}pato_inter.local${NC} ${BLUE}[CLIENT ONLY]${NC}"
echo -e "${GREEN}✓ Certificats localhost pour tests locaux créés${NC} (hostname: ${YELLOW}$LOCAL_HOSTNAME${NC})"

if [ "$VERIFICATION_SUCCESS" = true ]; then
    echo -e "\n${GREEN}✓ Tous les certificats sont valides${NC}"
else
    echo -e "\n${RED}✗ Certains certificats ont des erreurs${NC}"
fi
echo -e "\n${BLUE}📂 Emplacement:${NC} $PKI_DIR/"
echo -e "${BLUE}📂 Certificats pour tests localhost:${NC} $PKI_DIR/local/"

echo -e "\n${YELLOW}⚠  ARCHITECTURE:${NC}"
echo -e "  • Root CA (P-384) signe Intermediate CA (P-384)"
echo -e "  • Intermediate CA signe les certificats finaux (P-256)"
echo -e "  • Chaînes complètes: cert + intermediate + root"

echo -e "\n${YELLOW}⚠  TESTS LOCAUX:${NC}"
echo -e "  • Utilisez ${GREEN}$PKI_DIR/local/${NC} pour tester en local"
echo -e "  • Serveur: connectez-vous à ${YELLOW}localhost${NC} ou ${YELLOW}127.0.0.1${NC}"
echo -e "  • Les certificats incluent IPv4 (127.0.0.1) et IPv6 (::1)"
echo -e "  • Les certificats incluent aussi le hostname: ${YELLOW}$LOCAL_HOSTNAME${NC}"

echo -e "\n${YELLOW}⚠  IMPORTANT:${NC}"
echo -e "  1. Hostname local détecté: ${GREEN}$LOCAL_HOSTNAME${NC}"
echo -e "  2. Configurer les hostnames sur les autres appareils"
echo -e "  3. Installer/activer Avahi sur les appareils Linux"
echo -e "  4. Protéger ${RED}root-ca.key${NC} et ${RED}intermediate-ca.key${NC}"
echo -e "  5. Utiliser ${GREEN}ca-chain.pem${NC} pour la vérification"
echo -e "  6. wolfSSL doit être compilé avec ${GREEN}--enable-ecc${NC}"

echo -e "\n${GREEN}Prochaines étapes:${NC}"
echo -e "  1. ${BLUE}Tests locaux:${NC}"
echo -e "     openssl s_server -cert $PKI_DIR/local/server-chain.pem -key $PKI_DIR/local/server.key -accept 4433"
echo -e "     openssl s_client -connect localhost:4433 -CAfile $PKI_DIR/local/ca-chain.pem"
echo -e "  2. Vérifier: ${BLUE}openssl verify -CAfile pki/intermediate-ca/ca-chain.pem pki/local/server.pem${NC}"
echo -e "  3. Tester mDNS: ${BLUE}ping pato_explo.local${NC}"
echo -e "  4. Transférer les certificats vers les autres machines"

echo ""

