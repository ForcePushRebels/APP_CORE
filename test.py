#!/usr/bin/env python3
# --------------------------------------------------------------------
# tls_test_client.py – client de test TLS-1.3 pour le serveur WolfSSL
#
# Usage :
#   python3 tls_test_client.py --host 127.0.0.1 --port 4433           \
#       --cafile pki/root/ca.pem                                      \
#       [--cert  pki/client/client.full.pem --key pki/client/client.key]
# --------------------------------------------------------------------
import argparse
import socket
import sys
import wolfssl                            # pip install wolfssl 3.14+ [3]

# --------------------------------------------------------------------
# Analyse des arguments
# --------------------------------------------------------------------
parser = argparse.ArgumentParser(description="Client de test TLS 1.3 WolfSSL")
parser.add_argument("--host",   required=True, help="Adresse du serveur")
parser.add_argument("--port",   type=int, default=4433, help="Port TCP")
parser.add_argument("--cafile", required=True, help="CA racine à faire confiance")
parser.add_argument("--cert",   help="Certificat client (PEM) pour mTLS")
parser.add_argument("--key",    help="Clé privée client (PEM)")
args = parser.parse_args()

# --------------------------------------------------------------------
# Création du socket TCP
# --------------------------------------------------------------------
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

# --------------------------------------------------------------------
# Contexte TLS 1.3
# --------------------------------------------------------------------
try:
    ctx = wolfssl.SSLContext(wolfssl.PROTOCOL_TLSv1_3)               # méthode TLS 1.3 [3]
except AttributeError:
    print("Votre binding wolfssl n’a pas été compilé avec TLS 1.3")
    sys.exit(1)

ctx.verify_mode = wolfssl.CERT_REQUIRED
ctx.load_verify_locations(args.cafile)                               # CA racine pin-née

# Authentification mutuelle (facultatif)
if args.cert and args.key:
    ctx.load_cert_chain(args.cert, args.key)

# Restreindre les suites aux mêmes qu’au serveur
ctx.set_ciphers(
    "TLS13-AES256-GCM-SHA384:TLS13-CHACHA20-POLY1305-SHA256:"
    "TLS13-AES128-GCM-SHA256"
)

# --------------------------------------------------------------------
# Connection + handshake
# --------------------------------------------------------------------
print(f"Connexion à {args.host}:{args.port} …")
tls_sock = ctx.wrap_socket(sock, server_hostname=args.host)          # SNI
tls_sock.connect((args.host, args.port))                             # handshake auto [5]

print("Handshake TLS 1.3 réussi.")
print("Suite négociée : ", tls_sock.cipher())

# --------------------------------------------------------------------
# Échange simple
# --------------------------------------------------------------------
MESSAGE = b"Hello from Python / TLS 1.3 !\n"
tls_sock.write(MESSAGE)
print(">>  envoyé :", MESSAGE.decode().strip())

data = tls_sock.read(4096)
print("<<  reçu   :", data.decode(errors="replace").strip())

tls_sock.close()
