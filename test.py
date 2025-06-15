#!/usr/bin/env python3
# client_tls_test.py
# ----------------------------------------------------------
# Vérifie qu'un serveur expose correctement le certificat
# « server/fullchain.pem » signé par votre CA racine.
# ----------------------------------------------------------

import socket
import ssl
import sys
import struct
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

            # Construction du message 0x30 (ID_IS_ANY_ROBOT_HERE) selon le format de trame
            # Format: [payload_size (2 bytes, big-endian)] + [message_type (1 byte)] + [payload]
            # Pour ID_IS_ANY_ROBOT_HERE, pas de payload, donc payload_size = 1 (juste le type de message)
            
            message_type = 0x30  # ID_IS_ANY_ROBOT_HERE
            payload_size = 1     # Juste le type de message, pas de payload supplémentaire
            
            # Construction de la trame complète
            frame = struct.pack('>H', payload_size) + struct.pack('B', message_type)
            
            print(f"Envoi du message 0x{message_type:02X} avec taille payload {payload_size}")
            print(f"Trame envoyée: {frame.hex()}")
            
            tls_sock.sendall(frame)
            
            # Attendre une réponse
            data = tls_sock.recv(4096)
            print(f"Réponse du serveur ({len(data)} bytes): {data.hex()}")
            
            # Tenter de décoder la réponse
            if len(data) >= 3:
                response_payload_size = struct.unpack('>H', data[0:2])[0]
                response_msg_type = data[2]
                print(f"Réponse - Type: 0x{response_msg_type:02X}, Taille payload: {response_payload_size}")
                
                if len(data) >= 3 + response_payload_size - 1:  # -1 car le type est inclus dans payload_size
                    payload_data = data[3:3 + response_payload_size - 1]
                    print(f"Payload reçu ({len(payload_data)} bytes): {payload_data.hex()}")

except ssl.SSLError as e:
    print("Échec TLS :", e)
    sys.exit(1)
except Exception as e:
    print("Erreur réseau :", e)
    sys.exit(2)
