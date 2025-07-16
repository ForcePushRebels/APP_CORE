# TLS Engine Build System - Support de compilation conditionnelle C/Rust
#
# Utilisation:
#   make                    # Compile uniquement la version Rust 
#   make c_compatible       # Compile pour remplacement transparent du C
#   make install_headers    # Installe les headers compatibles C/Rust
#   make clean             # Nettoie tous les artefacts
#
# Variables d'environnement:
#   TARGET=<arch>          # Architecture cible (x86_64, aarch64, etc.)
#   PROFILE=release        # Profile de compilation (debug par défaut)

.PHONY: all c_compatible rust_library install_headers clean help test example

# Configuration par défaut
CARGO_TARGET_DIR ?= target
PROFILE ?= debug
TARGET ?= 

# Détection automatique de l'architecture si non spécifiée
ifeq ($(TARGET),)
    TARGET := $(shell rustc --version --verbose | grep host | cut -d' ' -f2)
endif

# Répertoires
BUILD_DIR = build
INSTALL_DIR = $(BUILD_DIR)/install
INCLUDE_DIR = $(INSTALL_DIR)/include
LIB_DIR = $(INSTALL_DIR)/lib

# Noms des fichiers de sortie
RUST_LIB_NAME = libtls_engine_rust.a
RUST_LIB_PATH = $(CARGO_TARGET_DIR)/$(TARGET)/$(PROFILE)/$(RUST_LIB_NAME)
INSTALLED_LIB = $(LIB_DIR)/$(RUST_LIB_NAME)
INSTALLED_HEADER = $(INCLUDE_DIR)/tls_engine.h

# Options de compilation
CARGO_FLAGS = 
ifeq ($(PROFILE),release)
    CARGO_FLAGS += --release
endif
ifneq ($(TARGET),)
    CARGO_FLAGS += --target $(TARGET)
endif

# Cible par défaut : compilation Rust standard
all: rust_library install_headers
	@echo "✓ TLS Engine Rust compilé avec succès"
	@echo "  Bibliothèque: $(INSTALLED_LIB)"
	@echo "  Header: $(INSTALLED_HEADER)"
	@echo "  Architecture: $(TARGET)"
	@echo "  Profile: $(PROFILE)"

# Compilation pour remplacement transparent du C
c_compatible: rust_library install_headers
	@echo ""
	@echo "════════════════════════════════════════════════════════════════"
	@echo "✓ TLS Engine Rust - PRÊT POUR REMPLACEMENT TRANSPARENT DU C"
	@echo "════════════════════════════════════════════════════════════════"
	@echo ""
	@echo "Intégration dans votre projet:"
	@echo ""
	@echo "1. CMake (recommandé):"
	@echo "   # Dans votre CMakeLists.txt"
	@echo "   option(USE_RUST_TLS_ENGINE \"Use Rust TLS implementation\" OFF)"
	@echo "   "
	@echo "   if(USE_RUST_TLS_ENGINE)"
	@echo "       add_definitions(-DUSE_RUST_TLS_ENGINE)"
	@echo "       target_link_libraries(your_target $(INSTALLED_LIB))"
	@echo "       target_include_directories(your_target PRIVATE $(INCLUDE_DIR))"
	@echo "   else()"
	@echo "       # Votre compilation C existante"
	@echo "   endif()"
	@echo ""
	@echo "2. Makefile:"
	@echo "   # Pour activer Rust:"
	@echo "   CFLAGS += -DUSE_RUST_TLS_ENGINE -I$(INCLUDE_DIR)"
	@echo "   LDFLAGS += $(INSTALLED_LIB) -lpthread -ldl"
	@echo ""
	@echo "3. Compilation directe:"
	@echo "   gcc -DUSE_RUST_TLS_ENGINE -I$(INCLUDE_DIR) your_code.c $(INSTALLED_LIB) -lwolfssl -lpthread -ldl"
	@echo ""
	@echo "AUCUNE MODIFICATION du code source nécessaire !"
	@echo "Les mêmes noms de fonctions: tlsEngineCreate, tlsEngineDestroy, etc."
	@echo ""

# Compilation de la bibliothèque Rust
rust_library:
	@echo "Compilation de la bibliothèque TLS Engine Rust..."
	@echo "Target: $(TARGET), Profile: $(PROFILE)"
	cargo build $(CARGO_FLAGS)
	@echo "✓ Compilation Rust terminée"

# Installation des headers et bibliothèques
install_headers: rust_library
	@echo "Installation des headers et bibliothèques..."
	@mkdir -p $(INCLUDE_DIR) $(LIB_DIR)
	
	# Copie de la bibliothèque Rust
	cp $(RUST_LIB_PATH) $(INSTALLED_LIB)
	
	# Copie du header unifié
	cp include/rust_tls_engine.h $(INSTALLED_HEADER)
	
	@echo "✓ Installation terminée"
	@echo "  Header: $(INSTALLED_HEADER)"
	@echo "  Bibliothèque: $(INSTALLED_LIB)"

# Génération du header (si besoin de régénérer)
generate_header:
	@echo "Génération du header depuis le code Rust..."
	cargo build
	cbindgen --config cbindgen.toml --crate rust-tls-engine --output include/rust_tls_engine.h.tmp
	@echo "Header généré dans include/rust_tls_engine.h.tmp"

# Tests
test:
	@echo "Exécution des tests Rust..."
	cargo test $(CARGO_FLAGS)

# Exemple d'utilisation
example: c_compatible
	@echo "Compilation de l'exemple d'intégration..."
	@mkdir -p $(BUILD_DIR)/examples
	
	# Crée un exemple simple
	@echo '#include <stdio.h>' > $(BUILD_DIR)/examples/test_integration.c
	@echo '#include "tls_engine.h"' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '' >> $(BUILD_DIR)/examples/test_integration.c
	@echo 'int main() {' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    printf("TLS Engine: %s\\n", tlsEngineGetImplementation());' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    printf("Version: %s\\n", tlsEngineGetVersion());' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    ' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    // Test initialisation certificats' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    int result = xCertificateInit();' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    if (result == CERT_OK) {' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '        printf("✓ Système de certificats initialisé\\n");' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '        xCertificateCleanup();' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    } else {' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '        printf("✗ Erreur initialisation: 0x%x\\n", result);' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    }' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '    return 0;' >> $(BUILD_DIR)/examples/test_integration.c
	@echo '}' >> $(BUILD_DIR)/examples/test_integration.c
	
	# Compile l'exemple avec l'implémentation Rust
	gcc -DUSE_RUST_TLS_ENGINE \
		-I$(INCLUDE_DIR) \
		$(BUILD_DIR)/examples/test_integration.c \
		$(INSTALLED_LIB) \
		-lwolfssl -lpthread -ldl \
		-o $(BUILD_DIR)/examples/test_integration_rust
	
	@echo "✓ Exemple compilé: $(BUILD_DIR)/examples/test_integration_rust"
	@echo "Exécution de l'exemple:"
	$(BUILD_DIR)/examples/test_integration_rust

# Nettoyage
clean:
	@echo "Nettoyage des artefacts..."
	cargo clean
	rm -rf $(BUILD_DIR)
	rm -f include/rust_tls_engine.h.tmp
	@echo "✓ Nettoyage terminé"

# Documentation et aide
help:
	@echo "TLS Engine Build System - Compilation C/Rust"
	@echo ""
	@echo "Cibles disponibles:"
	@echo "  all              - Compile la bibliothèque Rust (défaut)"
	@echo "  c_compatible     - Prépare le remplacement transparent du C"
	@echo "  rust_library     - Compile uniquement la bibliothèque Rust"
	@echo "  install_headers  - Installe headers et bibliothèques"
	@echo "  generate_header  - Régénère le header depuis Rust" 
	@echo "  test            - Exécute les tests"
	@echo "  example         - Compile et exécute un exemple"
	@echo "  clean           - Nettoie tous les artefacts"
	@echo "  help            - Affiche cette aide"
	@echo ""
	@echo "Variables:"
	@echo "  TARGET=<arch>    - Architecture cible (auto-détectée par défaut)"
	@echo "  PROFILE=release  - Profile de compilation (debug par défaut)"
	@echo ""
	@echo "Exemples:"
	@echo "  make c_compatible                    # Version optimisée pour remplacement C"
	@echo "  make TARGET=aarch64-unknown-linux-gnu # Cross-compilation ARM64"
	@echo "  make PROFILE=release c_compatible    # Version optimisée release"

# Validation de l'intégration
validate: c_compatible
	@echo "Validation de l'intégration..."
	@echo "Vérification des fichiers..."
	@test -f $(INSTALLED_LIB) || (echo "✗ Bibliothèque manquante: $(INSTALLED_LIB)" && exit 1)
	@test -f $(INSTALLED_HEADER) || (echo "✗ Header manquant: $(INSTALLED_HEADER)" && exit 1)
	
	@echo "Vérification des symboles exportés..."
	@nm $(INSTALLED_LIB) | grep -q "tlsEngineCreate" || (echo "✗ Symbole tlsEngineCreate non trouvé" && exit 1)
	@nm $(INSTALLED_LIB) | grep -q "xCertificateInit" || (echo "✗ Symbole xCertificateInit non trouvé" && exit 1)
	
	@echo "✓ Validation réussie - Prêt pour l'intégration"

# Informations sur l'environnement
info:
	@echo "Informations d'environnement:"
	@echo "  Rust version: $(shell rustc --version 2>/dev/null || echo 'Non installé')"
	@echo "  Cargo version: $(shell cargo --version 2>/dev/null || echo 'Non installé')"
	@echo "  Target: $(TARGET)"
	@echo "  Profile: $(PROFILE)"
	@echo "  WolfSSL: $(shell pkg-config --modversion wolfssl 2>/dev/null || echo 'À vérifier')"
	@echo "  Build dir: $(BUILD_DIR)"
	@echo "  Library: $(INSTALLED_LIB)"
	@echo "  Header: $(INSTALLED_HEADER)" 