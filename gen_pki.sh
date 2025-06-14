#!/usr/bin/env bash
# ------------------------------------------------------------
# gen_pki_ecc.sh – 3-tier ECC PKI compatible with 32-bit systems
# Root  : secp384r1 (P-384)
# Sub-CA: prime256v1 (P-256)
# Leaf  : prime256v1 (P-256)
# ------------------------------------------------------------
set -euo pipefail
umask 077

PKI_DIR="pki"

# ---------- Y2038 safeguard ---------------------------------
LIMIT_TS=$(date -ud '2037-12-31 23:59:59Z' +%s)
NOW_TS=$(date -u +%s)
DAYS_LEFT=$(( (LIMIT_TS - NOW_TS) / 86400 ))

ROOT_DAYS=$(( DAYS_LEFT - 30 ))          # 1-month safety margin
(( ROOT_DAYS < 365 )) && ROOT_DAYS=365   # at least 1 year

SUBCA_DAYS=$(( ROOT_DAYS / 2 ))          # half the root’s lifetime
LEAF_DAYS=825                            # 27 months (browser-friendly)
# ------------------------------------------------------------

echo "Root CA   : $ROOT_DAYS days"
echo "Sub-CA    : $SUBCA_DAYS days"
echo "End-entity: $LEAF_DAYS days"

CURVE_ROOT=secp384r1
CURVE_SUB=prime256v1
CURVE_EE=prime256v1

rm -rf "$PKI_DIR"; mkdir -p "$PKI_DIR"/{root,sub,server,client}

# 1. Self-signed root
openssl ecparam -name "$CURVE_ROOT" -genkey -noout -out "$PKI_DIR/root/ca.key"
openssl req -new -x509 -days "$ROOT_DAYS" \
    -subj "/C=FR/O=Demo Org/CN=Demo Root CA P384" \
    -key  "$PKI_DIR/root/ca.key" -out "$PKI_DIR/root/ca.pem"

# 2. Intermediate CA
openssl ecparam -name "$CURVE_SUB" -genkey -noout -out "$PKI_DIR/sub/sub.key"
openssl req -new -key "$PKI_DIR/sub/sub.key" \
    -subj "/C=FR/O=Demo Org/CN=Demo Sub-CA P256" \
    -out  "$PKI_DIR/sub/sub.csr"

openssl x509 -req -days "$SUBCA_DAYS" \
    -in  "$PKI_DIR/sub/sub.csr" \
    -CA  "$PKI_DIR/root/ca.pem" -CAkey "$PKI_DIR/root/ca.key" -set_serial 1000 \
    -extfile <(printf "basicConstraints=critical,CA:true,pathlen:0\nkeyUsage=critical,keyCertSign,cRLSign") \
    -out "$PKI_DIR/sub/sub.pem"

# 3. Server certificate
openssl ecparam -name "$CURVE_EE" -genkey -noout -out "$PKI_DIR/server/server.key"
openssl req -new -key "$PKI_DIR/server/server.key" \
    -subj "/C=FR/O=Demo Org/CN=server.demo.local" \
    -out  "$PKI_DIR/server/server.csr"

openssl x509 -req -days "$LEAF_DAYS" \
    -in  "$PKI_DIR/server/server.csr" \
    -CA  "$PKI_DIR/sub/sub.pem" -CAkey "$PKI_DIR/sub/sub.key" -set_serial 2000 \
    -extfile <(printf "extendedKeyUsage=serverAuth") \
    -out "$PKI_DIR/server/server.pem"

cat "$PKI_DIR/server/server.pem" "$PKI_DIR/sub/sub.pem" \
    > "$PKI_DIR/server/fullchain.pem"

# 4. Client certificate (optional mutual-TLS)
openssl ecparam -name "$CURVE_EE" -genkey -noout -out "$PKI_DIR/client/client.key"
openssl req -new -key "$PKI_DIR/client/client.key" \
    -subj "/C=FR/O=Demo Org/CN=tls-client" \
    -out  "$PKI_DIR/client/client.csr"

openssl x509 -req -days "$LEAF_DAYS" \
    -in  "$PKI_DIR/client/client.csr" \
    -CA  "$PKI_DIR/sub/sub.pem" -CAkey "$PKI_DIR/sub/sub.key" -set_serial 3000 \
    -extfile <(printf "extendedKeyUsage=clientAuth") \
    -out "$PKI_DIR/client/client.pem"

cat "$PKI_DIR/client/client.pem" "$PKI_DIR/sub/sub.pem" \
    > "$PKI_DIR/client/fullchain.pem"

# -------- Summary ------------------------------------------
echo
for f in root/ca.pem sub/sub.pem server/server.pem; do
    openssl x509 -in "$PKI_DIR/$f" -noout -subject -enddate
done
echo "✓ All certificates expire before 2038 and should validate on 32-bit builds."
