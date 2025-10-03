## PKI - Guide de chargement par cible

### Commun

- Certificats finaux: P-256, CA: P-384 (Root/Intermediate)
- Chaînes: `*-chain.pem = [end-entity + intermediate + root]`
- mTLS: chaque pair vérifie l’autre avec `pki/intermediate-ca/ca-chain.pem`
- TLS 1.3 requis, ECC activé

### PC Développement (Local)

- Dossier PKI: `build/x86_64-*/pki/`
- Serveur (localhost):
  - Cert: `pki/local/server-chain.pem`
  - Key: `pki/local/server.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`
  - Host/SNI attendu: `localhost`
- Client:
  - Cert: `pki/local/client-chain.pem`
  - Key: `pki/local/client.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`
  - Cible/SNI: `localhost`

### Raspberry Pi – PATO EXPLO (SERVER + CLIENT)

- Dossier PKI: `build/raspi-*/pki/`
- Serveur:
  - Cert: `pki/pato-explo/server-chain.pem`
  - Key: `pki/pato-explo/server.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`
  - Host/SNI attendu: `pato_explo.local`
- Client (si utilisé):
  - Cert: `pki/pato-explo/client-chain.pem`
  - Key: `pki/pato-explo/client.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`

### Raspberry Pi – PATO INTER (CLIENT only)

- Dossier PKI: `build/raspi-*/pki/`
- Client:
  - Cert: `pki/pato-inter/client-chain.pem`
  - Key: `pki/pato-inter/client.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`
  - Cible/SNI: `pato_explo.local`

### Tablette Android – COMMAND (CLIENT only)

- Dossier PKI: `build/*/pki/`
- Client:
  - Cert: `pki/command/client-chain.pem`
  - Key: `pki/command/client.key`
  - CA (verify): `pki/intermediate-ca/ca-chain.pem`
  - Cible/SNI: `pato_explo.local`

### Rappels essentiels

- Toujours vérifier avec `pki/intermediate-ca/ca-chain.pem` (contient Intermediate + Root)
- Le serveur charge obligatoirement `server-chain.pem` + `server.key`
- Le client charge `client-chain.pem` + `client.key`
- SNI/Host:
  - Local: `localhost`
  - Robots: `pato_explo.local`, `pato_inter.local`
  - Command: `command.local`

### Commandes OpenSSL (tests rapides)

- Serveur local:

```
openssl s_server -accept 4433 \
  -cert pki/local/server-chain.pem -key pki/local/server.key \
  -CAfile pki/intermediate-ca/ca-chain.pem -verify 1
```

- Client local:

```
openssl s_client -connect localhost:4433 \
  -CAfile pki/intermediate-ca/ca-chain.pem \
  -cert pki/local/client-chain.pem -key pki/local/client.key \
  -servername localhost
```

- Serveur explo (RPi):

```
openssl s_server -accept 4433 \
  -cert pki/pato-explo/server-chain.pem -key pki/pato-explo/server.key \
  -CAfile pki/intermediate-ca/ca-chain.pem -verify 1
```

- Client command → explo:

```
openssl s_client -connect pato_explo.local:4433 \
  -CAfile pki/intermediate-ca/ca-chain.pem \
  -cert pki/command/client-chain.pem -key pki/command/client.key \
  -servername pato_explo.local
```
