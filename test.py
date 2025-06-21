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
import threading
import time
from pprint import pprint

HOST = "server.demo.local"   # Utiliser le nom du certificat
PORT = 8080                  # port où tourne votre serveur TLS

# Variable globale pour contrôler l'arrêt
should_stop = False

def input_thread():
    """Thread pour surveiller l'entrée utilisateur"""
    global should_stop
    while not should_stop:
        try:
            user_input = input().strip().lower()
            if user_input == 'q':
                print("\nArrêt demandé par l'utilisateur...")
                should_stop = True
                break
        except (EOFError, KeyboardInterrupt):
            should_stop = True
            break

def main_client_loop():
    """Boucle principale du client"""
    global should_stop
    
    # --- 1. Contexte TLS côté client ----------------------------------------
    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)          # contexte « client »
    ctx.load_verify_locations("build/x86_64-debug/pki/root/ca.pem")           # CA racine pour valider
    ctx.minimum_version = ssl.TLSVersion.TLSv1_3           # (ou TLSv1_3 si supporté)
    ctx.set_ciphers("ECDHE+CHACHA20")         # ciphers modernes, optionnel

    # --- Mutual TLS (facultatif) --------------------------------------------
    # Décommentez si le serveur exige un certificat client :
    ctx.load_cert_chain(certfile="build/x86_64-debug/pki/client/fullchain.pem",
                         keyfile="build/x86_64-debug/pki/client/client.key")

    # --- 2. Connexion socket + handshake ------------------------------------
    try:
        # Résoudre server.demo.local vers 127.0.0.1
        with socket.create_connection(("127.0.0.1", PORT)) as raw_sock:
            with ctx.wrap_socket(raw_sock, server_hostname="server.demo.local") as tls_sock:
                print("Version TLS négociée :", tls_sock.version())
                print("Suite chiffrée     :", tls_sock.cipher())
                print("Certificat serveur :")
                pprint(tls_sock.getpeercert())
                
                # Configuration du timeout pour les opérations réseau
                #tls_sock.settimeout(1.0)  # 1 seconde de timeout
                
                print("\n=== Client TLS en fonctionnement ===")
                print("Appuyez sur 'q' puis Entrée pour quitter")
                print("Envoi périodique de messages 0x30...")
                
                message_count = 0
                
                # Boucle principale jusqu'à ce que l'utilisateur tape 'q'
                while not should_stop:
                    try:
                        # Construction du message 0x30 (ID_IS_ANY_ROBOT_HERE)
                        message_type = 0x30  # ID_IS_ANY_ROBOT_HERE
                        payload_size = 1     # Juste le type de message
                        
                        # Construction de la trame complète
                        frame = struct.pack('>H', payload_size) + struct.pack('B', message_type)
                        
                        message_count += 1
                        print(f"\n[Message #{message_count}] Envoi 0x{message_type:02X} - {frame.hex()}")
                        
                        tls_sock.sendall(frame)
                        
                        # Attendre une réponse
                        try:
                            data = tls_sock.recv(4096)
                            if data:
                                print(f"Réponse ({len(data)} bytes): {data.hex()}")
                                
                                # Décoder la réponse
                                if len(data) >= 3:
                                    response_payload_size = struct.unpack('>H', data[0:2])[0]
                                    response_msg_type = data[2]
                                    print(f"Type: 0x{response_msg_type:02X}, Taille: {response_payload_size}")
                                    
                                    if len(data) >= 3 + response_payload_size - 1:
                                        payload_data = data[3:3 + response_payload_size - 1]
                                        
                                        # Décoder le manifest si c'est 0x31
                                        if response_msg_type == 0x31 and len(payload_data) >= 48:
                                            robot_name = payload_data[0:32].rstrip(b'\x00').decode('utf-8', errors='ignore')
                                            ip_addr = payload_data[32:48].rstrip(b'\x00').decode('utf-8', errors='ignore')
                                            print(f"Robot: '{robot_name}', IP: '{ip_addr}'")
                            else:
                                print("Connexion fermée par le serveur")
                                break
                                
                        except socket.timeout:
                            print("Timeout - pas de réponse")
                        except Exception as e:
                            print(f"Erreur réception: {e}")
                            
                        # Pause entre les messages (vérifier should_stop régulièrement)
                        for _ in range(50):  # 5 secondes au total (50 * 0.1s)
                            if should_stop:
                                break
                            time.sleep(0.1)
                            
                    except Exception as e:
                        print(f"Erreur lors de l'envoi: {e}")
                        if should_stop:
                            break
                        time.sleep(1)
                        
                print("\nFermeture de la connexion...")

    except ssl.SSLError as e:
        print("Échec TLS :", e)
        return 1
    except Exception as e:
        print("Erreur réseau :", e)
        return 2
    
    return 0

if __name__ == "__main__":
    # Démarrer le thread de surveillance des entrées utilisateur
    input_thread_handle = threading.Thread(target=input_thread, daemon=True)
    input_thread_handle.start()
    
    try:
        # Lancer la boucle principale
        exit_code = main_client_loop()
        sys.exit(exit_code)
    except KeyboardInterrupt:
        print("\nInterruption par Ctrl+C")
        should_stop = True
        sys.exit(0)
