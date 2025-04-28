# Robot MRPiZ - APP_CORE

Ce projet est une interface Rust pour le robot MRPiZ basée sur la bibliothèque C existante.

## Structure du projet

Le projet est organisé comme un workspace Rust contenant plusieurs crates :

```
APP_CORE/RUST/
├── Cargo.toml              # Fichier Cargo.toml du workspace
├── common/                 # Crate bibliothèque commune
│   ├── Cargo.toml
│   └── src/                # Code source de la bibliothèque commune
├── explo/                  # Crate exécutable
│   ├── Cargo.toml
│   └── src/                # Code source de l'application
├── mrpiz_bindings/         # Crate pour les bindings FFI
│   ├── Cargo.toml
│   ├── build.rs            # Script de build pour la génération des bindings
│   └── src/                # Code source des bindings
└── lib_mrpiz/              # Bibliothèque C du robot MRPiZ
    ├── include/            # Fichiers d'en-tête
    │   └── mrpiz/          # Headers du robot
    │       ├── mrpiz.h
    │       └── error.h
    └── lib/                # Bibliothèques statiques
        ├── libintox.a
        ├── libintoxmrpiz.a
        └── libmrpiz.a
```

## Configuration requise

- Rust et Cargo (version récente)
- Les bibliothèques statiques du robot MRPiZ doivent être présentes dans le dossier `lib_mrpiz/lib/`
- Les fichiers d'en-tête doivent être présents dans le dossier `lib_mrpiz/include/mrpiz/`

## Installation

1. Clonez ce dépôt
2. Assurez-vous que le répertoire `lib_mrpiz` est correctement structuré avec les sous-répertoires :
   - `include/mrpiz/` contenant les fichiers .h
   - `lib/` contenant les bibliothèques statiques (.a)
3. Compilez le projet avec `cargo build`

## Compilation

```bash
cargo build
```

Pour compiler en mode release :

```bash
cargo build --release
```

## Exécution

```bash
cargo run -p explo
```

## Résolution des problèmes courants

### Erreur de lien avec les bibliothèques statiques

Si vous obtenez une erreur du type "could not find native static library", vérifiez que :
1. Les bibliothèques statiques (.a) sont bien présentes dans le dossier `lib_mrpiz/lib/`
2. Les noms des bibliothèques correspondent à ceux attendus dans le script de build 