#!/usr/bin/env python3
"""
Client TLS minimal pour tester la connexion à la Raspberry Pi (pato-explo)
avec debug verbose pour diagnostiquer les problèmes PKI.

Ce client utilise :
- Certificats du PC de développement (local)
- Connexion vers serveur pato-explo sur Raspberry Pi
- Debug détaillé pour comprendre les erreurs TLS
"""

import socket
import ssl
import sys
from pathlib import Path

def find_pki_directory():
    """Trouve automatiquement le répertoire PKI."""
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    # Chercher dans les builds disponibles
    possible_builds = ["raspi-debug"]

    for build in possible_builds:
        pki_dir = project_root / "build" / build / "pki"
        if pki_dir.exists():
            print(f"📁 PKI trouvée: {pki_dir}")
            return pki_dir

    return None

def load_certificates_for_actor(pki_dir, actor_name):
    """Charge les certificats pour un acteur spécifique."""
    actor_dir = pki_dir / actor_name

    cert_chain = actor_dir / "client-chain.pem"
    cert_key = actor_dir / "client.key"
    root_ca = pki_dir / "root-ca" / "root-ca.pem"

    missing = []
    if not cert_chain.exists():
        missing.append(f"client-chain.pem ({cert_chain})")
    if not cert_key.exists():
        missing.append(f"client.key ({cert_key})")
    if not root_ca.exists():
        missing.append(f"root-ca.pem ({root_ca})")

    if missing:
        print(f"❌ Certificats manquants pour {actor_name}:")
        for item in missing:
            print(f"   - {item}")
        return None

    print(f"✅ Certificats {actor_name} chargés:")
    print(f"   - Chaîne client: {cert_chain}")
    print(f"   - Clé privée: {cert_key}")
    print(f"   - CA racine: {root_ca}")

    return {
        "chain": cert_chain,
        "key": cert_key,
        "ca": root_ca
    }

def find_intermediate_cas(pki_dir):
    """Trouve toutes les intermédiaires CA disponibles."""
    intermediates = []
    possible_inters = ["intermediate-robots", "intermediate-command", "intermediate-development", "intermediate-system"]

    for inter_name in possible_inters:
        inter_file = pki_dir / inter_name / f"{inter_name}.pem"
        if inter_file.exists():
            intermediates.append(inter_file)
            print(f"   - {inter_name}: {inter_file}")

    return intermediates

def test_tls_connection(host, port, client_certs, intermediate_cas):
    """Test de connexion TLS avec debug détaillé."""
    print(f"\n🔌 Tentative de connexion TLS vers {host}:{port}")
    print("=" * 60)

    try:
        # 1. Création du contexte SSL
        print("🔧 Création du contexte SSL...")
        ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
        ctx.minimum_version = ssl.TLSVersion.TLSv1_3
        ctx.maximum_version = ssl.TLSVersion.TLSv1_3
        print("✅ Contexte SSL créé (TLS 1.3 uniquement)")

        # 2. Configuration de la vérification
        print("\n🔍 Configuration de la vérification des certificats...")

        # Charger la CA racine
        print(f"📜 Chargement CA racine: {client_certs['ca']}")
        ctx.load_verify_locations(str(client_certs['ca']))
        ctx.check_hostname = False  # Désactiver pour debug
        ctx.verify_mode = ssl.CERT_REQUIRED
        print("✅ CA racine chargée, vérification activée (hostname désactivé pour debug)")

        # Charger les intermédiaires CA pour compatibilité
        print("
🔗 Chargement intermédiaires CA:"        loaded_inters = 0
        for inter_ca in intermediate_cas:
            try:
                ctx.load_verify_locations(str(inter_ca))
                print(f"   ✅ {inter_ca.name}")
                loaded_inters += 1
            except Exception as e:
                print(f"   ⚠️ Échec {inter_ca.name}: {e}")

        if loaded_inters == 0:
            print("   ⚠️ Aucune intermédiaire CA chargée")

        # 3. Configuration du certificat client
        print("
🔑 Configuration certificat client:"        print(f"   Chaîne: {client_certs['chain']}")
        print(f"   Clé: {client_certs['key']}")
        ctx.load_cert_chain(
            certfile=str(client_certs['chain']),
            keyfile=str(client_certs['key'])
        )
        print("✅ Certificat client chargé")

        # 4. Configuration des options SSL
        print("
⚙️ Configuration SSL:"        # Activer les cipher suites modernes
        try:
            ctx.set_ciphers("ECDHE+AESGCM:ECDHE+CHACHA20")
            print("✅ Cipher suites configurées: ECDHE+AESGCM, ECDHE+CHACHA20")
        except Exception as e:
            print(f"⚠️ Erreur configuration cipher: {e}")

        # 5. Connexion TCP
        print(f"\n🌐 Connexion TCP vers {host}:{port}...")
        sock = socket.create_connection((host, port), timeout=10)
        print("✅ Connexion TCP établie")

        # 6. Wrap SSL
        print("🔒 Établissement connexion TLS...")
        print("   (Ceci peut échouer si les certificats ne sont pas compatibles)"
        tls_sock = ctx.wrap_socket(sock, server_hostname=host)
        print("✅ Handshake TLS réussi!")

        # 7. Informations sur la connexion
        print("
📊 Informations connexion TLS:"        print(f"   Version: {tls_sock.version()}")
        print(f"   Cipher: {tls_sock.cipher()[0]}")

        # Vérifier le certificat du serveur
        server_cert = tls_sock.getpeercert()
        if server_cert:
            subject = server_cert.get('subject', [])
            issuer = server_cert.get('issuer', [])
            subject_str = ', '.join([f"{name[0][1]}" for name in subject if name])
            issuer_str = ', '.join([f"{name[0][1]}" for name in issuer if name])
            print(f"   Certificat serveur: {subject_str}")
            print(f"   Émis par: {issuer_str}")
        else:
            print("   ⚠️ Aucun certificat serveur reçu")

        # 8. Test d'envoi/réception
        print("
📨 Test communication:"        # Envoyer un ping simple (message ID 0x30 = Is Any Robot Here?)
        test_message = b'\x00\x01\x30'  # size=1, msg_type=0x30, no payload
        tls_sock.send(test_message)
        print(f"   → Envoyé: {test_message.hex()}")

        # Attendre une réponse (timeout court)
        tls_sock.settimeout(2.0)
        try:
            response = tls_sock.recv(1024)
            print(f"   ← Reçu: {response.hex()}")
            print("   ✅ Communication bidirectionnelle fonctionnelle")
        except socket.timeout:
            print("   ⏰ Timeout réception (normal si serveur ne répond pas)")
        except Exception as e:
            print(f"   ⚠️ Erreur réception: {e}")

        # 9. Fermeture propre
        print("
🔌 Fermeture connexion..."        tls_sock.close()
        print("✅ Connexion fermée proprement")

        return True

    except ssl.SSLCertVerificationError as e:
        print(f"\n❌ Erreur vérification certificat: {e}")
        print("Causes possibles:")
        print("  - Le certificat serveur n'est pas signé par une CA de confiance")
        print("  - L'intermédiaire CA du serveur n'est pas disponible côté client")
        print("  - Problème de chaîne de certification")

        # Plus de détails sur l'erreur
        error_code = getattr(e, 'errno', None)
        if error_code:
            print(f"Code d'erreur SSL: {error_code}")

    except ssl.SSLZeroReturnError:
        print("\n❌ Connexion fermée proprement par le serveur")

    except ssl.SSLError as e:
        error_code = getattr(e, 'errno', None)
        print(f"\n❌ Erreur SSL (code: {error_code}): {e}")

        if error_code == -308:
            print("🔍 Code -308 = ASN_NO_SIGNER_E")
            print("   Le certificat présenté ne peut pas être validé")
            print("   Vérifiez que le client a accès à l'intermédiaire CA du serveur")

    except socket.timeout:
        print("\n❌ Timeout de connexion")
        print("  - Le serveur n'est pas accessible")
        print("  - Port incorrect ou firewall")

    except ConnectionRefusedError:
        print("\n❌ Connexion refusée")
        print("  - Le serveur n'écoute pas sur ce port")
        print("  - Problème réseau")

    except Exception as e:
        print(f"\n❌ Erreur inattendue: {type(e).__name__}: {e}")
        import traceback
        traceback.print_exc()

    return False

def main():
    print("🚀 Client TLS de test - PC de développement vers Raspberry Pi (pato-explo)")
    print("=" * 80)

    # Configuration
    CLIENT_ACTOR = "local"      # PC de développement
    SERVER_ACTOR = "pato-explo" # Serveur sur Raspberry Pi
    DEFAULT_HOST = "127.0.0.1"  # Changer selon l'IP de la Raspberry Pi
    DEFAULT_PORT = 8080

    # Arguments en ligne de commande
    if len(sys.argv) >= 2:
        host = sys.argv[1]
    else:
        host = DEFAULT_HOST

    if len(sys.argv) >= 3:
        port = int(sys.argv[2])
    else:
        port = DEFAULT_PORT

    print(f"Configuration:")
    print(f"  Client: {CLIENT_ACTOR}")
    print(f"  Serveur: {SERVER_ACTOR}")
    print(f"  Hôte: {host}")
    print(f"  Port: {port}")
    print()

    # 1. Trouver la PKI
    pki_dir = find_pki_directory()
    if not pki_dir:
        print("❌ Aucune PKI trouvée dans les répertoires build/")
        print("   Exécutez d'abord: ./gen_pki.sh")
        sys.exit(1)

    # 2. Charger les certificats client
    client_certs = load_certificates_for_actor(pki_dir, CLIENT_ACTOR)
    if not client_certs:
        sys.exit(1)

    # 3. Trouver les intermédiaires CA
    print("
🔗 Intermédiaires CA disponibles:"    intermediate_cas = find_intermediate_cas(pki_dir)

    # 4. Test de connexion
    success = test_tls_connection(host, port, client_certs, intermediate_cas)

    print("\n" + "=" * 80)
    if success:
        print("🎉 Test réussi ! La connexion TLS fonctionne.")
    else:
        print("❌ Test échoué. Consultez les messages d'erreur ci-dessus.")
        print("\n🔧 Solutions possibles :")
        print("  1. Vérifiez que le serveur pato-explo fonctionne")
        print("  2. Vérifiez l'adresse IP et le port")
        print("  3. Régénérez la PKI si nécessaire: ./gen_pki.sh")
        print("  4. Assurez-vous que les intermédiaires CA sont compatibles")

    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
