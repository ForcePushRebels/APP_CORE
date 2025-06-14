#!/usr/bin/env python3
# client_tls_test.py
# ----------------------------------------------------------
# Vérifie qu'un serveur expose correctement le certificat
# « server/fullchain.pem » signé par votre CA racine.
# ----------------------------------------------------------

import socket
import ssl
import sys
from pprint import pprint

HOST = "server.demo.local"   # Utiliser le nom du certificat
PORT = 8080                  # port où tourne votre serveur TLS

# --- 1. Contexte TLS côté client ----------------------------------------
ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)          # contexte « client »[4]
ctx.load_verify_locations("pki/root/ca.pem")           # CA racine pour valider
ctx.minimum_version = ssl.TLSVersion.TLSv1_3           # (ou TLSv1_3 si supporté)
ctx.set_ciphers("ECDHE+AESGCM:ECDHE+CHACHA20")         # ciphers modernes, optionnel

# --- Mutual TLS (facultatif) --------------------------------------------
# Décommentez si le serveur exige un certificat client :
ctx.load_cert_chain(certfile="pki/client/fullchain.pem",
                     keyfile="pki/client/client.key")

# --- 2. Connexion socket + handshake ------------------------------------
try:
    # Résoudre server.demo.local vers 127.0.0.1
    with socket.create_connection(("127.0.0.1", PORT), timeout=5) as raw_sock:  # création socket[4]
        with ctx.wrap_socket(raw_sock, server_hostname="server.demo.local") as tls_sock: # handshake + SNI[4]
            print("Version TLS négociée :", tls_sock.version())           # ex. TLSv1.3
            print("Suite chiffrée     :", tls_sock.cipher())
            print("Certificat serveur :")
            pprint(tls_sock.getpeercert())                                # dict x509

            # Petit test d'écho facultatif
            tls_sock.sendall(b"PING\n")
            data = tls_sock.recv(1024)
            print("Réponse du serveur :", data.decode(errors="ignore"))

except ssl.SSLError as e:
    print("Échec TLS :", e)
    sys.exit(1)
except Exception as e:
    print("Erreur réseau :", e)
    sys.exit(2)
