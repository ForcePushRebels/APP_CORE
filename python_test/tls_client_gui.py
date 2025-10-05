#!/usr/bin/env python3
"""
PyQt6 GUI TLS client that allows creating multiple simultaneous secure
connections to a server, sending arbitrary hexadecimal messages and
viewing responses in real-time.

Usage:
  pip install PyQt6
  python tls_client_gui.py

Requirements:
  ‑ Python ≥ 3.8
  ‑ PyQt6 ≥ 6.4
  ‑ OpenSSL ≥ 1.1.1 (for TLS 1.3)

Notes:
  ‑ The GUI lets you create several connection tabs, each running in its
    own QThread so they operate independently.
  ‑ Certificate paths are hard-coded to match the existing test setup but
    can be edited directly in the UI if needed.
"""

import queue
import socket
import ssl
import struct
import sys
import threading
from pathlib import Path
from typing import Optional

from PyQt6.QtCore import QThread, pyqtSignal, Qt, QTimer
from PyQt6.QtGui import QFont, QTextCursor, QPalette, QColor
from PyQt6.QtWidgets import (
    QApplication,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QSpinBox,
    QComboBox,
    QTabWidget,
    QTextEdit,
    QVBoxLayout,
    QWidget,
    QDialog,
    QFormLayout,
    QCheckBox,
    QDoubleSpinBox,
    QButtonGroup,
    QRadioButton,
    QGroupBox,
    QScrollArea,
)

# ---------------------------------------------------------------------------
# Pre-defined server targets with their certificates and settings
# ---------------------------------------------------------------------------

SERVER_TARGETS = [
    {
        "name": "🔧 Local Development",
        "host": "127.0.0.1",
        "port": 8080,
        "description": "Serveur local de développement",
        "environment": "local",
        "sni": "localhost"
    },
    {
        "name": "📱 Command Tablet",
        "host": "command.robot.local",
        "port": 8080,
        "description": "Serveur tablette de commande",
        "environment": "command",
        "sni": "command.robot.local"
    },
    {
        "name": "🤖 Robot Pato-Explo",
        "host": "pato-explo.robot.local",
        "port": 8080,
        "description": "Robot d'exploration PATO",
        "environment": "pato-explo",
        "sni": "pato-explo.robot.local"
    },
    {
        "name": "🔧 Robot Pato-Inter",
        "host": "pato-inter.robot.local",
        "port": 8080,
        "description": "Robot d'intervention PATO",
        "environment": "pato-inter",
        "sni": "pato-inter.robot.local"
    },
    {
        "name": "🌐 Production Server",
        "host": "prod.robot.local",
        "port": 8080,
        "description": "Serveur de production",
        "environment": "production",
        "sni": "prod.robot.local"
    },
    {
        "name": "🔧 Custom Target",
        "host": "",
        "port": 8080,
        "description": "Configuration personnalisée",
        "environment": "manual",
        "sni": ""
    }
]

# ---------------------------------------------------------------------------
# Pre-defined message templates (based on network_encode.h)
# ---------------------------------------------------------------------------

PREDEFINED_MESSAGES = [
    # Messages envoyables par le client (Android/PC)
    {"label": "🔍 UDP • Is Any Robot Here? (0x30)", "id": 0x30, "payload": "", "category": "sendable", "fields": []},
    {"label": "🎮 CMD • Set Movement (0x01)", "id": 0x01, "payload": "", "category": "sendable", 
     "fields": [
         {"name": "direction", "type": "choice", "choices": ["FORWARD", "LEFT", "RIGHT"], "values": [0, 1, 2]},
         {"name": "distance_mm", "type": "uint32", "default": 1000},
         {"name": "speed_mm_s", "type": "uint32", "default": 200}
     ]},
    {"label": "🎯 CMD • Mission Control (0x02)", "id": 0x02, "payload": "", "category": "sendable", "fields": []},
    {"label": "📍 CMD • Selected Points (0x05)", "id": 0x05, "payload": "", "category": "sendable", 
     "fields": [
         {"name": "x_mm", "type": "int32", "default": 0},
         {"name": "y_mm", "type": "int32", "default": 0},
         {"name": "speed_mm_s", "type": "uint32", "default": 200}
     ]},
    {"label": "🗺️ CMD • Upload Map (0x07)", "id": 0x07, "payload": "", "category": "sendable", "fields": []},
    
    # Messages renvoyés par le robot (informatif seulement)
    {"label": "🔋 INF • Battery (0x0A) [ROBOT ONLY]", "id": 0x0A, "payload": "", "category": "robot_only", "fields": []},
    {"label": "📊 INF • Status (0x0B) [ROBOT ONLY]", "id": 0x0B, "payload": "", "category": "robot_only", "fields": []},
    {"label": "📍 INF • Position (0x0C) [ROBOT ONLY]", "id": 0x0C, "payload": "", "category": "robot_only", 
     "fields": [
         {"name": "x_mm", "type": "int32", "default": 0},
         {"name": "y_mm", "type": "int32", "default": 0},
         {"name": "angle_rad", "type": "double", "default": 0.0}
     ]},
    {"label": "⏰ INF • Time (0x0D) [ROBOT ONLY]", "id": 0x0D, "payload": "", "category": "robot_only", "fields": []},
    {"label": "🗺️ MAP • Fragment (0x20) [ROBOT ONLY]", "id": 0x20, "payload": "", "category": "robot_only", "fields": []},
    {"label": "🗺️ MAP • Full (0x21) [ROBOT ONLY]", "id": 0x21, "payload": "", "category": "robot_only", "fields": []},
    {"label": "🤖 UDP • Manifest (0x31) [ROBOT ONLY]", "id": 0x31, "payload": "", "category": "robot_only", "fields": []},
]

# ---------------------------------------------------------------------------
# Theme management
# ---------------------------------------------------------------------------

class ThemeManager:
    def __init__(self):
        self.current_theme = "dark"
        
    def get_liquid_glass_dark(self):
        return {
            "name": "Professional Dark Glass",
            "primary_bg": "#2a2d3a",
            "secondary_bg": "#363946",
            "tertiary_bg": "#404554",
            "accent": "#4fc3f7",
            "accent_secondary": "#ab47bc",
            "success": "#66bb6a",
            "warning": "#ffa726",
            "danger": "#ef5350",
            "text": "#ffffff",
            "text_secondary": "#b0bec5",
            "border": "#4a5568",
            "border_focus": "#4fc3f7",
            "shadow": "rgba(0, 0, 0, 0.5)",
            "glass_effect": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #363946,
                    stop:1 #2a2d3a);
                border: 1px solid #4a5568;
                border-radius: 12px;
            """,
            "glass_blur": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #2a2d3a,
                    stop:1 #1e2127);
                border: 1px solid #4fc3f7;
                border-radius: 16px;
            """,
            "card_effect": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #404554,
                    stop:1 #363946);
                border: 1px solid #4a5568;
                border-radius: 10px;
            """
        }
    
    def get_liquid_glass_light(self):
        return {
            "name": "Professional Light Glass",
            "primary_bg": "#f5f7fa",
            "secondary_bg": "#e8ecf1", 
            "tertiary_bg": "#dae1e7",
            "accent": "#1976d2",
            "accent_secondary": "#7b1fa2", 
            "success": "#388e3c",
            "warning": "#f57c00",
            "danger": "#d32f2f",
            "text": "#263238",
            "text_secondary": "#546e7a",
            "border": "#b0bec5",
            "border_focus": "#1976d2",
            "shadow": "rgba(15, 23, 42, 0.1)",
            "glass_effect": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #e8ecf1,
                    stop:1 #f5f7fa);
                border: 1px solid #b0bec5;
                border-radius: 12px;
            """,
            "glass_blur": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #f5f7fa,
                    stop:1 #eceff1);
                border: 1px solid #1976d2;
                border-radius: 16px;
            """,
            "card_effect": """
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 #dae1e7,
                    stop:1 #e8ecf1);
                border: 1px solid #b0bec5;
                border-radius: 10px;
            """
        }
    
    def get_current_theme(self):
        if self.current_theme == "dark":
            return self.get_liquid_glass_dark()
        else:
            return self.get_liquid_glass_light()
    
    def toggle_theme(self):
        self.current_theme = "light" if self.current_theme == "dark" else "dark"
        return self.get_current_theme()

# Global theme manager
theme_manager = ThemeManager()

# ---------------------------------------------------------------------------
# Custom Frame Builder Dialog
# ---------------------------------------------------------------------------

class FrameBuilderDialog(QDialog):
    def __init__(self, message_template, parent=None):
        super().__init__(parent)
        self.template = message_template
        self.field_widgets = {}
        self.init_ui()
        
    def init_ui(self):
        theme = theme_manager.get_current_theme()
        self.setWindowTitle(f"🔧 Constructeur de Frame - {self.template['label']}")
        self.setModal(True)
        self.resize(500, 600)
        
        # Apply liquid glass theme
        self.setStyleSheet(f"""
            QDialog {{
                {theme['glass_effect']}
                color: {theme['text']};
            }}
            QLabel {{
                color: {theme['text']};
                font-weight: bold;
                padding: 4px;
            }}
            QLineEdit, QSpinBox, QDoubleSpinBox {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                border: 2px solid {theme['border']};
                border-radius: 8px;
                padding: 8px;
                font-family: 'Courier New', monospace;
            }}
            QComboBox {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                border: 2px solid {theme['border']};
                border-radius: 8px;
                padding: 8px;
            }}
            QPushButton {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1, 
                    stop:0 {theme['accent']}, stop:1 {theme['accent_secondary']});
                color: white;
                border: none;
                border-radius: 8px;
                padding: 10px 20px;
                font-weight: bold;
            }}
            QPushButton:hover {{
                background: {theme['accent']};
            }}
            QGroupBox {{
                {theme['glass_effect']}
                color: {theme['text']};
                font-weight: bold;
                padding: 10px;
                margin-top: 15px;
            }}
            QGroupBox::title {{
                color: {theme['accent']};
                subcontrol-origin: margin;
                left: 10px;
                padding: 0 5px 0 5px;
            }}
        """)
        
        layout = QVBoxLayout(self)
        
        # Header
        header = QLabel(f"🎯 Message ID: 0x{self.template['id']:02X}")
        header.setStyleSheet(f"color: {theme['accent']}; font-size: 16px; font-weight: bold;")
        layout.addWidget(header)
        
        # Fields
        if self.template.get('fields'):
            scroll = QScrollArea()
            scroll.setWidgetResizable(True)
            scroll_widget = QWidget()
            scroll_layout = QVBoxLayout(scroll_widget)
            
            for field in self.template['fields']:
                group = QGroupBox(f"📝 {field['name'].replace('_', ' ').title()}")
                form_layout = QFormLayout(group)
                
                widget = self.create_field_widget(field)
                self.field_widgets[field['name']] = widget
                form_layout.addRow(f"{field['type'].upper()}:", widget)
                
                scroll_layout.addWidget(group)
            
            scroll.setWidget(scroll_widget)
            layout.addWidget(scroll)
        else:
            info = QLabel("ℹ️ Ce message ne contient pas de payload personnalisable.")
            info.setStyleSheet(f"color: {theme['text_secondary']}; font-style: italic;")
            layout.addWidget(info)
        
        # Preview
        preview_group = QGroupBox("🔍 Aperçu Hexadécimal")
        preview_layout = QVBoxLayout(preview_group)
        self.preview_label = QLabel("Payload: (vide)")
        self.preview_label.setStyleSheet(f"""
            background: {theme['primary_bg']};
            color: {theme['accent']};
            border: 1px solid {theme['border']};
            border-radius: 6px;
            padding: 8px;
            font-family: 'Courier New', monospace;
            font-size: 12px;
        """)
        preview_layout.addWidget(self.preview_label)
        layout.addWidget(preview_group)
        
        # Buttons
        button_layout = QHBoxLayout()
        self.update_btn = QPushButton("🔄 Mettre à jour")
        self.update_btn.clicked.connect(self.update_preview)
        self.ok_btn = QPushButton("✅ Utiliser")
        self.ok_btn.clicked.connect(self.accept)
        self.cancel_btn = QPushButton("❌ Annuler")
        self.cancel_btn.clicked.connect(self.reject)
        
        button_layout.addWidget(self.update_btn)
        button_layout.addStretch()
        button_layout.addWidget(self.ok_btn)
        button_layout.addWidget(self.cancel_btn)
        layout.addLayout(button_layout)
        
        # Connect all widgets to update preview
        for widget in self.field_widgets.values():
            if hasattr(widget, 'valueChanged'):
                widget.valueChanged.connect(self.update_preview)
            elif hasattr(widget, 'textChanged'):
                widget.textChanged.connect(self.update_preview)
            elif hasattr(widget, 'currentTextChanged'):
                widget.currentTextChanged.connect(self.update_preview)
        
        self.update_preview()
    
    def create_field_widget(self, field):
        field_type = field['type']
        
        if field_type == 'choice':
            widget = QComboBox()
            for choice in field['choices']:
                widget.addItem(choice)
            return widget
        elif field_type == 'uint32':
            widget = QSpinBox()
            widget.setRange(0, 4294967295)
            widget.setValue(field.get('default', 0))
            return widget
        elif field_type == 'int32':
            widget = QSpinBox()
            widget.setRange(-2147483648, 2147483647)
            widget.setValue(field.get('default', 0))
            return widget
        elif field_type == 'double':
            widget = QDoubleSpinBox()
            widget.setRange(-1000000.0, 1000000.0)
            widget.setDecimals(6)
            widget.setValue(field.get('default', 0.0))
            return widget
        else:
            widget = QLineEdit()
            widget.setText(str(field.get('default', '')))
            return widget
    
    def update_preview(self):
        payload_bytes = self.build_payload()
        hex_str = payload_bytes.hex().upper()
        formatted_hex = ' '.join(hex_str[i:i+2] for i in range(0, len(hex_str), 2))
        self.preview_label.setText(f"Payload ({len(payload_bytes)} bytes): {formatted_hex}")
    
    def build_payload(self):
        import struct
        payload = b""
        
        for field in self.template.get('fields', []):
            widget = self.field_widgets[field['name']]
            field_type = field['type']
            
            if field_type == 'choice':
                choice_idx = widget.currentIndex()
                values = field.get('values', list(range(len(field['choices']))))
                value = values[choice_idx] if choice_idx < len(values) else 0
                payload += struct.pack('<I', value)  # uint32 little endian
            elif field_type == 'uint32':
                payload += struct.pack('<I', widget.value())
            elif field_type == 'int32':
                payload += struct.pack('<i', widget.value())
            elif field_type == 'double':
                payload += struct.pack('<d', widget.value())
        
        return payload
    
    def get_payload_hex(self):
        return self.build_payload().hex()

# ---------------------------------------------------------------------------
# Networking worker thread
# ---------------------------------------------------------------------------

# Auto-detect PKI directory and certificates based on environment
def detect_pki_environment_for_target(target_environment):
    """Detect PKI environment and return appropriate certificate paths for a specific target."""
    import os
    import platform

    # Get project root directory
    project_root = Path("/home/christophe/pato/APP_CORE")

    # Try to detect build directory
    possible_build_dirs = [
        "build/x86_64-debug",
        "build/x86_64-release",
        "build/raspi-debug",
        "build/raspi-release",
        "build/production"
    ]

    # Map target environments to certificate directories
    env_to_actor = {
        "local": "local",
        "command": "command",
        "pato-explo": "pato-explo",
        "pato-inter": "pato-inter",
        "production": "command"  # Default to command for production
    }

    target_actor = env_to_actor.get(target_environment, "local")

    for build_dir in possible_build_dirs:
        pki_path = project_root / build_dir / "pki"
        if pki_path.exists():
            print(f"🔍 PKI détectée: {pki_path}")

            # Check if target actor certificates exist
            actor_cert = pki_path / target_actor / "client-chain.pem"
            actor_key = pki_path / target_actor / "client.key"
            root_ca = pki_path / "root-ca" / "root-ca.pem"

            if actor_cert.exists() and actor_key.exists() and root_ca.exists():
                print(f"✅ Certificats {target_actor} trouvés: {pki_path}/{target_actor}/")
                return {
                    "pki_dir": pki_path,
                    "ca_cert": root_ca,
                    "client_cert": actor_cert,
                    "client_key": actor_key,
                    "environment": target_environment,
                    "actor": target_actor
                }

    # Fallback to default paths
    print(f"⚠️ Aucune PKI détectée pour {target_environment}, utilisation des chemins par défaut")
    default_pki = project_root / "build" / "x86_64-debug" / "pki"
    default_actor = env_to_actor.get(target_environment, "local")
    return {
        "pki_dir": default_pki,
        "ca_cert": default_pki / "root-ca" / "root-ca.pem",
        "client_cert": default_pki / default_actor / "client-chain.pem",
        "client_key": default_pki / default_actor / "client.key",
        "environment": "fallback",
        "actor": default_actor
    }

def detect_pki_environment():
    """Legacy function - kept for backward compatibility."""
    return detect_pki_environment_for_target("local")

# Detect PKI environment
PKI_CONFIG = detect_pki_environment()
CERT_DIR = PKI_CONFIG["pki_dir"]
CA_CERT = PKI_CONFIG["ca_cert"]
CLIENT_CERT = PKI_CONFIG["client_cert"]
CLIENT_KEY = PKI_CONFIG["client_key"]


class ClientWorker(QThread):
    """TLS client running in its own thread."""

    log = pyqtSignal(str)
    connected = pyqtSignal()
    disconnected = pyqtSignal()

    def __init__(
        self,
        host: str,
        port: int,
        ca_path: Path = CA_CERT,
        cert_path: Path = CLIENT_CERT,
        key_path: Path = CLIENT_KEY,
        cipher_suite: str = "ECDHE+CHACHA20",
        server_hostname: Optional[str] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.host = host
        self.port = port
        self.ca_path = ca_path
        self.cert_path = cert_path
        self.key_path = key_path
        self.cipher_suite = cipher_suite
        self.server_hostname = server_hostname or host  # Default to host if not specified

        self._send_queue: "queue.Queue[bytes]" = queue.Queue()
        self._stop_event = threading.Event()
        self._socket: Optional[ssl.SSLSocket] = None

    # ----------------------------- public API -----------------------------

    def send_frame(self, frame: bytes) -> None:
        """Enqueue a binary frame to be sent by the thread."""
        self._send_queue.put(frame)

    def close(self) -> None:
        """Signal the thread to close the connection and terminate."""
        self._stop_event.set()
        self._send_queue.put(b"")  # unblocks queue.get()
    
    def _decode_frame(self, data: bytes) -> str:
        """Decode received frame according to protocol format."""
        try:
            if len(data) < 3:
                return f"Trame trop courte ({len(data)} bytes) - minimum 3 bytes requis"
            
            # Lire l'en-tête du protocole (2 bytes payload size + 1 byte message type)
            payload_size = struct.unpack(">H", data[:2])[0]  # Big endian uint16
            msg_type = data[2]
            
            # Chercher le message dans les templates
            message_info = None
            for tpl in PREDEFINED_MESSAGES:
                if tpl["id"] == msg_type:
                    message_info = tpl
                    break
            
            if message_info:
                msg_label = message_info["label"]
                category = message_info.get("category", "unknown")
                category_emoji = "🤖" if category == "robot_only" else "📤" if category == "sendable" else "❓"
            else:
                msg_label = f"Message Inconnu (0x{msg_type:02X})"
                category_emoji = "❓"
            
            result = f"{category_emoji} {msg_label}"
            
            # Extraire le payload s'il existe
            if len(data) > 3:
                payload = data[3:]
                result += f" | Payload: {len(payload)} bytes"
                
                # Décodage spécifique selon le type de message
                if message_info and message_info.get("fields"):
                    decoded_payload = self._decode_payload(payload, message_info["fields"])
                    if decoded_payload:
                        result += f" | {decoded_payload}"
                else:
                    # Affichage hex du payload pour les messages sans structure connue
                    if len(payload) <= 16:
                        result += f" | Hex: {payload.hex().upper()}"
            else:
                result += " | Pas de payload"
            
            # Validation de la taille
            expected_frame_size = payload_size + 2  # payload_size + 2 bytes d'en-tête
            if len(data) != expected_frame_size:
                result += f" | ⚠️ Taille incorrecte: {len(data)}/{expected_frame_size}"
            
            return result
            
        except Exception as e:
            return f"❌ Erreur décodage: {str(e)}"
    
    def _decode_payload(self, payload: bytes, fields: list) -> str:
        """Decode structured payload based on field definitions."""
        try:
            offset = 0
            decoded_parts = []
            
            for field in fields:
                field_name = field["name"]
                field_type = field["type"]
                
                if offset >= len(payload):
                    break
                
                if field_type == "uint32":
                    if offset + 4 <= len(payload):
                        value = struct.unpack("<I", payload[offset:offset+4])[0]  # Little endian
                        decoded_parts.append(f"{field_name}={value}")
                        offset += 4
                elif field_type == "int32":
                    if offset + 4 <= len(payload):
                        value = struct.unpack("<i", payload[offset:offset+4])[0]  # Little endian
                        decoded_parts.append(f"{field_name}={value}")
                        offset += 4
                elif field_type == "double":
                    if offset + 8 <= len(payload):
                        value = struct.unpack("<d", payload[offset:offset+8])[0]  # Little endian
                        decoded_parts.append(f"{field_name}={value:.3f}")
                        offset += 8
                elif field_type == "choice":
                    if offset + 4 <= len(payload):
                        value = struct.unpack("<I", payload[offset:offset+4])[0]  # Little endian uint32
                        choices = field.get("choices", [])
                        values = field.get("values", list(range(len(choices))))
                        if value < len(values) and values.index(value) < len(choices) if value in values else False:
                            choice_name = choices[values.index(value)]
                            decoded_parts.append(f"{field_name}={choice_name}({value})")
                        else:
                            decoded_parts.append(f"{field_name}={value}")
                        offset += 4
            
            return " | ".join(decoded_parts) if decoded_parts else None
            
        except Exception as e:
            return f"Erreur payload: {str(e)}"

    # ----------------------------- internals -----------------------------

    def _establish_connection(self) -> ssl.SSLSocket:
        try:
            self.log.emit("🔧 Initialisation du contexte SSL/TLS...")
            ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            ctx.minimum_version = ssl.TLSVersion.TLSv1_3

            # Configuration de la vérification des certificats
            self.log.emit("🔍 Configuration de la vérification des certificats...")
            try:
                ctx.load_verify_locations(str(self.ca_path))
                self.log.emit(f"✅ CA chargé: {self.ca_path}")
            except Exception as e:
                self.log.emit(f"❌ Erreur chargement CA {self.ca_path}: {e}")
                raise

            # Configuration du certificat client
            self.log.emit("🔑 Configuration du certificat client...")
            try:
                ctx.load_cert_chain(certfile=str(self.cert_path), keyfile=str(self.key_path))
                self.log.emit(f"✅ Certificat client chargé: {self.cert_path}")
                self.log.emit(f"✅ Clé privée chargée: {self.key_path}")
            except Exception as e:
                self.log.emit(f"❌ Erreur chargement certificat client {self.cert_path}: {e}")
                self.log.emit(f"   Vérifiez que le fichier existe et que la clé correspond")
                raise

            # Configurer la suite de chiffrement selon le choix utilisateur
            if self.cipher_suite != "DEFAULT":
                try:
                    ctx.set_ciphers(self.cipher_suite)
                    self.log.emit(f"🔐 Suite de chiffrement configurée: {self.cipher_suite}")
                except Exception as e:
                    self.log.emit(f"⚠️ Erreur configuration cipher suite {self.cipher_suite}: {e}")
                    self.log.emit("🔧 Basculement vers négociation automatique")
                    ctx.set_ciphers(None)
            else:
                self.log.emit("🔧 Suite de chiffrement: Négociation automatique")

            # Résoudre l'adresse et établir la connexion TCP
            self.log.emit("🌐 Résolution de l'adresse et connexion TCP...")
            actual_host = "127.0.0.1" if self.host in ("localhost", "127.0.0.1") else self.host
            try:
                raw_sock = socket.create_connection((actual_host, self.port))
                self.log.emit(f"✅ Connexion TCP établie vers {actual_host}:{self.port}")
            except Exception as e:
                self.log.emit(f"❌ Erreur connexion TCP vers {actual_host}:{self.port}: {e}")
                raise

            # Établir la connexion TLS
            self.log.emit(f"🔒 Établissement de la connexion TLS avec SNI: {self.server_hostname}...")
            try:
                tls_sock = ctx.wrap_socket(raw_sock, server_hostname=self.server_hostname)
                self.log.emit("✅ Handshake TLS réussi!")
            except ssl.SSLCertVerificationError as e:
                self.log.emit(f"❌ Erreur vérification certificat serveur: {e}")
                self.log.emit("   Vérifiez que le certificat serveur est valide et signé par la CA")
                raise
            except ssl.SSLError as e:
                self.log.emit(f"❌ Erreur SSL/TLS: {e}")
                self.log.emit("   Vérifiez la configuration des certificats et la compatibilité TLS")
                raise

            # Log détaillé de la connexion avec couleurs
            tls_version = tls_sock.version()
            cipher_info = tls_sock.cipher()
            cipher_name = cipher_info[0] if cipher_info else "Inconnu"

            self.log.emit(f"🔒 Version TLS négociée: {tls_version}")
            self.log.emit(f"🔐 Suite chiffrée: {cipher_name}")

            if "CHACHA20" in cipher_name:
                self.log.emit("🚀 CHACHA20-POLY1305 sélectionné - Performance optimale!")
            elif "AES" in cipher_name:
                self.log.emit("🛡️ AES sélectionné - Sécurité éprouvée")

            # Vérifier le certificat peer si disponible
            try:
                peer_cert = tls_sock.getpeercert()
                if peer_cert:
                    subject = peer_cert.get('subject', [])
                    subject_str = ', '.join([f"{name[0][1]}" for name in subject if name])
                    self.log.emit(f"📜 Certificat serveur vérifié - Sujet: {subject_str}")
                else:
                    self.log.emit("⚠️ Aucun certificat peer reçu")
            except Exception as e:
                self.log.emit(f"⚠️ Impossible de vérifier le certificat peer: {e}")

            return tls_sock

        except Exception as e:
            self.log.emit(f"❌ Échec établissement connexion TLS: {e}")
            raise

    def run(self) -> None:
        try:
            self._socket = self._establish_connection()
            self.connected.emit()
        except Exception as exc:
            self.log.emit(f"[ERREUR] Connexion échouée : {exc}")
            self.disconnected.emit()
            return

        # Main loop
        while not self._stop_event.is_set():
            # 1 / Envoi des messages en file d'attente
            try:
                frame: bytes = self._send_queue.get(timeout=0.1)
            except queue.Empty:
                frame = b""

            if frame:
                try:
                    self._socket.sendall(frame)
                    self.log.emit(f"→ {frame.hex()} ({len(frame)} bytes)")
                except Exception as exc:
                    self.log.emit(f"[ERREUR] Envoi : {exc}")
                    self._stop_event.set()
                    break

            # 2 / Réception non-bloquante avec décodage
            try:
                self._socket.settimeout(0.0)
                data = self._socket.recv(4096)
                if data:
                    hex_data = data.hex().upper()
                    self.log.emit(f"← {hex_data} ({len(data)} bytes)")
                    
                    # Décodage de la trame si possible
                    decoded_info = self._decode_frame(data)
                    if decoded_info:
                        self.log.emit(f"📋 {decoded_info}")
                        
            except (ssl.SSLWantReadError, BlockingIOError):
                pass  # rien à lire pour l'instant
            except Exception as exc:
                self.log.emit(f"[ERREUR] Réception : {exc}")
                self._stop_event.set()
                break

        # Cleanup with proper TLS shutdown
        try:
            if self._socket is not None:
                # Proper TLS shutdown sequence
                try:
                    # Send close_notify alert
                    self._socket.unwrap()
                    self.log.emit("🔒 Shutdown TLS gracieux effectué")
                except Exception as shutdown_exc:
                    self.log.emit(f"⚠️ Shutdown TLS: {shutdown_exc}")
                finally:
                    # Force close the underlying socket
                    self._socket.close()
                    self.log.emit("📡 Socket TCP fermé")
        except Exception as cleanup_exc:
            self.log.emit(f"[ERREUR] Cleanup: {cleanup_exc}")
        finally:
            self.disconnected.emit()


# ---------------------------------------------------------------------------
# GUI widgets
# ---------------------------------------------------------------------------


class ConnectionTab(QWidget):
    """A single tab managing one TLS connection."""
    def __init__(self, target_name: str = "Local Development"):
        super().__init__()
        self.worker: Optional[ClientWorker] = None
        self.client_id = id(self)  # ID unique pour ce client
        self.is_connected = False
        self.current_target = None

        # --- UI elements ---------------------------------------------------
        # Server target selector
        self.target_combo = QComboBox()
        for target in SERVER_TARGETS:
            self.target_combo.addItem(target["name"], target)

        # Host and port (for custom target)
        self.host_edit = QLineEdit()
        self.port_spin = QSpinBox()
        self.port_spin.setRange(1, 65535)

        # Environment selector (auto-synced with target)
        self.env_combo = QComboBox()
        self.env_combo.addItem("🏠 Local (Développement)", "local")
        self.env_combo.addItem("📱 Command (Tablette)", "command")
        self.env_combo.addItem("🤖 Pato-Explo (Robot)", "pato-explo")
        self.env_combo.addItem("🔧 Pato-Inter (Robot)", "pato-inter")
        self.env_combo.addItem("🔧 Manuel (Personnalisé)", "manual")
        
        # Certificate paths (for manual mode)
        self.ca_path_edit = QLineEdit(str(CA_CERT))
        self.cert_path_edit = QLineEdit(str(CLIENT_CERT))
        self.key_path_edit = QLineEdit(str(CLIENT_KEY))
        
        # Target info label
        self.target_info_label = QLabel("Sélectionnez une cible serveur")
        self.target_info_label.setStyleSheet("color: #4fc3f7; font-weight: bold; padding: 4px;")

        # SNI field for custom targets
        self.sni_edit = QLineEdit()
        self.sni_edit.setPlaceholderText("Server Name Indication (SNI)")

        self.btn_connect = QPushButton("🔗 Connecter")
        
        self.btn_disconnect = QPushButton("❌ Déconnecter")
        self.btn_disconnect.setEnabled(False)

        self.template_combo = QComboBox()
        # Apply theme to combo box
        self.apply_combo_theme()
        
        for tpl in PREDEFINED_MESSAGES:
            self.template_combo.addItem(tpl["label"], tpl)

        # Cipher suite selector
        self.cipher_combo = QComboBox()
        self.cipher_combo.addItem("🔒 CHACHA20-POLY1305 (Recommandé)", "ECDHE+CHACHA20")
        self.cipher_combo.addItem("🛡️ AES-256-GCM", "ECDHE+AESGCM")
        self.cipher_combo.addItem("⚡ AES-128-GCM (Rapide)", "ECDHE+AES128")
        self.cipher_combo.addItem("🔧 Automatique (Défaut)", "DEFAULT")
        self.cipher_combo.setCurrentIndex(0)  # CHACHA20 par défaut

        self.msg_edit = QLineEdit("30")  # message type hex sans 0x
        self.payload_edit = QLineEdit("")  # payload hex (optionnel)
        
        self.btn_send = QPushButton("🚀 Envoyer")
        self.btn_send.setEnabled(False)
        
        self.btn_build_frame = QPushButton("🔧 Construire")
        self.btn_build_frame.clicked.connect(self._on_build_frame)

        self.log_view = QTextEdit()
        self.log_view.setReadOnly(True)
        self.log_view.setFont(QFont("Fira Code", 11))

        # Style labels
        def create_label(text, color=None):
            theme = theme_manager.get_current_theme()
            label = QLabel(text)
            label.setStyleSheet(f"""
                QLabel {{
                    color: {color or theme['text']};
                    font-weight: 500;
                    font-size: 13px;
                    padding: 4px 8px;
                }}
            """)
            return label

        # Target selection bar
        target_bar = QHBoxLayout()
        target_bar.setSpacing(12)
        target_bar.addWidget(create_label("🎯 Cible:"))
        target_bar.addWidget(self.target_combo)
        target_bar.addWidget(self.target_info_label)
        target_bar.addStretch()

        # Connection details bar (host/port for custom targets)
        conn_bar = QHBoxLayout()
        conn_bar.setSpacing(12)
        conn_bar.addWidget(create_label("🌐 Hôte:"))
        conn_bar.addWidget(self.host_edit)
        conn_bar.addWidget(create_label("🔌 Port:"))
        conn_bar.addWidget(self.port_spin)
        conn_bar.addWidget(create_label("🔐 Chiffrement:"))
        conn_bar.addWidget(self.cipher_combo)
        conn_bar.addWidget(self.btn_connect)
        conn_bar.addWidget(self.btn_disconnect)
        conn_bar.addStretch()

        # SNI and Environment bar
        sni_env_bar = QHBoxLayout()
        sni_env_bar.setSpacing(12)
        sni_env_bar.addWidget(create_label("🔒 SNI:"))
        sni_env_bar.addWidget(self.sni_edit)
        sni_env_bar.addWidget(create_label("🏗️ Environnement:"))
        sni_env_bar.addWidget(self.env_combo)
        sni_env_bar.addStretch()
        
        # Manual certificate paths (initially hidden)
        cert_bar = QHBoxLayout()
        cert_bar.setSpacing(12)
        cert_bar.addWidget(create_label("📜 CA:"))
        cert_bar.addWidget(self.ca_path_edit)
        cert_bar.addWidget(create_label("🔑 Cert:"))
        cert_bar.addWidget(self.cert_path_edit)
        cert_bar.addWidget(create_label("🗝️ Key:"))
        cert_bar.addWidget(self.key_path_edit)
        cert_bar.addStretch()

        send_bar = QHBoxLayout()
        send_bar.setSpacing(12)
        send_bar.addWidget(self.template_combo)
        send_bar.addWidget(create_label("🏷️ ID:"))
        send_bar.addWidget(self.msg_edit)
        send_bar.addWidget(create_label("📦 Payload:"))
        send_bar.addWidget(self.payload_edit)
        send_bar.addWidget(self.btn_build_frame)
        send_bar.addWidget(self.btn_send)
        send_bar.addStretch()

        root = QVBoxLayout(self)
        root.setSpacing(16)
        root.setContentsMargins(16, 16, 16, 16)
        root.addLayout(target_bar)
        root.addLayout(conn_bar)
        root.addLayout(sni_env_bar)
        root.addLayout(cert_bar)
        root.addLayout(send_bar)
        root.addWidget(self.log_view)

        # Signals
        self.btn_connect.clicked.connect(self._on_connect)
        self.btn_disconnect.clicked.connect(self._on_disconnect)
        self.btn_send.clicked.connect(self._on_send)
        # Utiliser seulement les signaux standard Qt - pas de customisation
        self.template_combo.currentIndexChanged.connect(self._on_template_changed)
        self.cipher_combo.currentIndexChanged.connect(self._on_cipher_changed)
        self.env_combo.currentIndexChanged.connect(self._on_environment_changed)
        self.target_combo.currentIndexChanged.connect(self._on_target_changed)
        
        # Appliquer les thèmes ComboBox simplifiés
        self.apply_combo_theme()
        self.apply_cipher_combo_theme()
        
        # Configuration ComboBox simplifiée
        self._setup_simple_combo_behavior()
        
        # Initialize target selection
        self._initialize_target_selection()

        # Apply theme to all elements
        self.apply_theme()
    
    def _setup_simple_combo_behavior(self):
        """Configuration ComboBox simplifiée - laisse Qt gérer le comportement"""
        # Configuration de base seulement
        self.template_combo.setMaxVisibleItems(8)
        self.cipher_combo.setMaxVisibleItems(5)
        self.env_combo.setMaxVisibleItems(5)
    
    def _initialize_target_selection(self):
        """Initialize target selection and related UI elements."""
        # Hide manual fields initially
        self._update_custom_target_visibility()

        # Set default target based on detected PKI environment
        default_target_idx = 0  # Local Development
        if PKI_CONFIG['environment'] == 'production':
            actor = PKI_CONFIG.get('actor', 'local')
            if actor == 'command':
                default_target_idx = 1  # Command Tablet
            elif actor == 'pato-explo':
                default_target_idx = 2  # Robot Pato-Explo
            elif actor == 'pato-inter':
                default_target_idx = 3  # Robot Pato-Inter

        self.target_combo.setCurrentIndex(default_target_idx)
        self._on_target_changed(default_target_idx)

    def _update_custom_target_visibility(self):
        """Show/hide custom target fields based on selected target."""
        target = self.target_combo.currentData()
        if target and target["name"] == "🔧 Custom Target":
            # Show custom fields for manual configuration
            self.host_edit.setVisible(True)
            self.port_spin.setVisible(True)
            self.sni_edit.setVisible(True)
            self.target_info_label.setText("Configuration personnalisée")
        else:
            # Hide custom fields for predefined targets
            self.host_edit.setVisible(False)
            self.port_spin.setVisible(False)
            self.sni_edit.setVisible(False)
            if target:
                self.target_info_label.setText(f"📋 {target['description']}")

    def _on_target_changed(self, index: int):
        """Handle target selection change."""
        target = self.target_combo.itemData(index)
        if not target:
            return

        self.current_target = target

        # Update host/port for custom target
        if target["name"] == "🔧 Custom Target":
            self.host_edit.setText("")
            self.port_spin.setValue(8080)
            self.sni_edit.setText("")
            self._update_custom_target_visibility()
            self._append_log(f"🎯 Cible sélectionnée: {target['name']} - Configuration personnalisée requise")
        else:
            # For predefined targets, update all fields automatically
            self.host_edit.setText(target["host"])
            self.port_spin.setValue(target["port"])
            self.sni_edit.setText(target["sni"])
            self._update_custom_target_visibility()
            self._append_log(f"🎯 Cible sélectionnée: {target['name']} ({target['host']}:{target['port']})")

        # Auto-sync environment with target
        env_mapping = {
            "local": 0,
            "command": 1,
            "pato-explo": 2,
            "pato-inter": 3,
            "production": 4,
            "manual": 4
        }
        env_index = env_mapping.get(target["environment"], 0)
        self.env_combo.setCurrentIndex(env_index)

        # Update certificate paths
        self._on_environment_changed(env_index)

    def apply_cipher_combo_theme(self):
        """Apply theme to cipher suite combo box"""
        theme = theme_manager.get_current_theme()
        # Style simplifié identique au template combo
        self.cipher_combo.setStyleSheet(f"""
            QComboBox {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                border: 1px solid {theme['border']};
                border-radius: 8px;
                padding: 8px 12px;
                font-size: 14px;
                min-width: 200px;
            }}
            QComboBox:hover {{
                border-color: {theme['accent_secondary']};
            }}
            QComboBox QAbstractItemView {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                selection-background-color: {theme['accent_secondary']};
                border: 1px solid {theme['border']};
                border-radius: 4px;
            }}
            QComboBox QAbstractItemView::item {{
                padding: 6px 12px;
                border: none;
            }}
            QComboBox QAbstractItemView::item:selected {{
                background: {theme['accent_secondary']};
                color: white;
            }}
        """)

    # ---------------------------- slots -----------------------------------

    def _on_connect(self):
        if self.worker and self.worker.isRunning():
            QMessageBox.warning(self, "Déjà connecté", "La connexion est déjà active.")
            return

        # Get target information
        target = self.current_target
        if not target:
            QMessageBox.warning(self, "Erreur", "Veuillez sélectionner une cible serveur.")
            return

        host = self.host_edit.text().strip()
        port = int(self.port_spin.value())

        if not host:
            QMessageBox.warning(self, "Erreur", "Veuillez saisir une adresse d'hôte.")
            return

        if port < 1 or port > 65535:
            QMessageBox.warning(self, "Erreur", "Le port doit être entre 1 et 65535.")
            return

        cipher_suite = self.cipher_combo.currentData()
        sni = self.sni_edit.text().strip() or host  # Use host as SNI if not specified

        # Get certificate paths based on environment
        env = self.env_combo.currentData()
        if env == "manual":
            # Use manual paths
            ca_path = Path(self.ca_path_edit.text())
            cert_path = Path(self.cert_path_edit.text())
            key_path = Path(self.key_path_edit.text())
        else:
            # Use auto-detected paths
            ca_path = Path(self.ca_path_edit.text())
            cert_path = Path(self.cert_path_edit.text())
            key_path = Path(self.key_path_edit.text())

        # Créer un worker unique pour ce client avec SNI
        self.worker = ClientWorker(host, port,
                                 ca_path=ca_path,
                                 cert_path=cert_path,
                                 key_path=key_path,
                                 cipher_suite=cipher_suite)
        self.worker.log.connect(self._append_log)
        self.worker.connected.connect(self._on_connected)
        self.worker.disconnected.connect(self._on_disconnected)

        # Override SNI in worker if custom target
        if target["name"] == "🔧 Custom Target":
            self.worker.server_hostname = sni
        else:
            self.worker.server_hostname = target["sni"]

        # Log de démarrage avec info du client et de la cible
        target_name = target["name"]
        self._append_log(f"[INFO] 🚀 Connexion vers {target_name} ({host}:{port})")
        self._append_log(f"[INFO] 🔒 SNI: {self.worker.server_hostname}, Cipher: {cipher_suite}")
        self._append_log("[INFO] Démarrage du thread de connexion…")

        self.worker.start()

    def _on_disconnect(self):
        if self.worker:
            self._append_log("[INFO] Fermeture de la connexion…")
            self.worker.close()
            self.worker = None

    def _on_connected(self):
        self.is_connected = True
        self.btn_connect.setEnabled(False)
        self.btn_disconnect.setEnabled(True)
        self.btn_send.setEnabled(True)
        host = self.host_edit.text().strip()
        port = self.port_spin.value()
        self._append_log(f"[INFO] ✅ Connecté à {host}:{port}")

    def _on_disconnected(self):
        self.is_connected = False
        self.btn_connect.setEnabled(True)
        self.btn_disconnect.setEnabled(False)
        self.btn_send.setEnabled(False)
        self._append_log("[INFO] ❌ Déconnecté")

    def _on_send(self):
        if not self.worker or not self.worker.isRunning():
            QMessageBox.warning(self, "Non connecté", "Veuillez d'abord établir la connexion.")
            return

        try:
            msg_type_hex = self.msg_edit.text().strip()
            if msg_type_hex.lower().startswith("0x"):
                msg_type_hex = msg_type_hex[2:]
            msg_type = int(msg_type_hex, 16)

            payload_hex = self.payload_edit.text().strip().replace(" ", "")
            if payload_hex.lower().startswith("0x"):
                payload_hex = payload_hex[2:]

            if len(payload_hex) % 2 != 0:
                raise ValueError("Payload hex doit contenir un nombre pair de caractères.")

            payload_bytes = bytes.fromhex(payload_hex) if payload_hex else b""

            payload_size = 1 + len(payload_bytes)  # type + payload
            frame = struct.pack(">H", payload_size) + struct.pack("B", msg_type) + payload_bytes
            self.worker.send_frame(frame)
        except ValueError:
            QMessageBox.critical(self, "Erreur", "ID de message invalide, utilisez de l'hexadécimal.")

    def _on_build_frame(self):
        tpl = self.template_combo.currentData()
        if not tpl:
            return
            
        if not tpl.get('fields'):
            QMessageBox.information(self, "Pas de champs", "Ce message ne contient pas de champs personnalisables.")
            return
            
        dialog = FrameBuilderDialog(tpl, self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            payload_hex = dialog.get_payload_hex()
            self.payload_edit.setText(payload_hex)
            self._append_log(f"🔧 Frame construite: {len(payload_hex)//2} bytes payload")

    def _on_template_changed(self, index: int):
        tpl = self.template_combo.itemData(index)
        if not tpl:
            return
        
        self.msg_edit.setText(f"{tpl['id']:02X}")
        self.payload_edit.setText(tpl['payload'])
        
        # Log du changement
        self._append_log(f"📋 Template sélectionné: {tpl['label']}")
        
        # Warn if this is a robot-only message
        if tpl.get('category') == 'robot_only':
            self._append_log("⚠️ [ATTENTION] Ce message est normalement envoyé par le robot, pas par le client!")
            QMessageBox.warning(
                self, 
                "Message Robot-Only", 
                f"Le message '{tpl['label']}' est habituellement envoyé par le robot.\n"
                "Vous pouvez l'envoyer pour tester, mais ce n'est pas l'usage normal."
            )
    
    def _on_cipher_changed(self, index: int):
        cipher_suite = self.cipher_combo.currentData()
        cipher_name = self.cipher_combo.currentText()
        if cipher_suite:
            self._append_log(f"🔐 Cipher sélectionné: {cipher_name}")
    
    def _on_environment_changed(self, index: int):
        """Handle environment selection change."""
        env = self.env_combo.currentData()
        env_name = self.env_combo.currentText()
        
        # Show/hide manual certificate paths
        if env == "manual":
            self.ca_path_edit.setVisible(True)
            self.cert_path_edit.setVisible(True)
            self.key_path_edit.setVisible(True)
            self._append_log("🔧 Mode manuel activé - configurez les chemins de certificats")
        else:
            self.ca_path_edit.setVisible(False)
            self.cert_path_edit.setVisible(False)
            self.key_path_edit.setVisible(False)
            
            # Update certificate paths based on environment
            self._update_certificate_paths(env)
            self._append_log(f"🏗️ Environnement sélectionné: {env_name}")
    
    def _update_certificate_paths(self, environment):
        """Update certificate paths based on selected environment."""
        try:
            # Use the new auto-detection function
            cert_config = detect_pki_environment_for_target(environment)

            ca_path = cert_config["ca_cert"]
            cert_path = cert_config["client_cert"]
            key_path = cert_config["client_key"]
            actor = cert_config["actor"]

            # Update the edit fields
            self.ca_path_edit.setText(str(ca_path))
            self.cert_path_edit.setText(str(cert_path))
            self.key_path_edit.setText(str(key_path))

            # Update info label
            if hasattr(self, 'cert_info_label'):
                self.cert_info_label.setText(f"📋 Environnement: {environment} ({actor})")

            # Check if files exist and provide verbose feedback
            if ca_path.exists() and cert_path.exists() and key_path.exists():
                self._append_log(f"✅ Certificats {environment} ({actor}) trouvés")
                self._append_log(f"   CA: {ca_path}")
                self._append_log(f"   Cert: {cert_path}")
                self._append_log(f"   Key: {key_path}")
            else:
                self._append_log(f"⚠️ Certificats {environment} ({actor}) manquants ou incomplets")
                # Debug: show what files are missing
                missing = []
                if not ca_path.exists():
                    missing.append(f"CA ({ca_path})")
                if not cert_path.exists():
                    missing.append(f"Cert ({cert_path})")
                if not key_path.exists():
                    missing.append(f"Key ({key_path})")

                if missing:
                    self._append_log(f"   Fichiers manquants: {', '.join(missing)}")
                else:
                    self._append_log("   ⚠️ Chemins configurés mais fichiers potentiellement corrompus")

        except Exception as e:
            self._append_log(f"❌ Erreur mise à jour certificats: {e}")
            import traceback
            self._append_log(f"   Détails: {traceback.format_exc()}")
    
    def eventFilter(self, obj, event):
        """Simplified event filter - let Qt handle ComboBox behavior naturally"""
        return super().eventFilter(obj, event)

    # ---------------------------- helpers ---------------------------------

    def _append_log(self, line: str):
        # Color-code logs depending on their prefix and content
        theme = theme_manager.get_current_theme()
        color = theme['text']
        
        if line.startswith("→"):
            color = theme['success']  # Envoi
        elif line.startswith("←"):
            color = theme['accent']  # Réception
        elif "[ERREUR]" in line or "ÉCHEC" in line.upper() or "ERROR" in line.upper():
            color = theme['danger']  # Erreurs
        elif line.startswith("[INFO]") or "Connecté" in line:
            color = theme['accent_secondary']  # Info connexion
        elif line.startswith("⚠️") or "ATTENTION" in line:
            color = theme['warning']  # Avertissements
        elif "TLS" in line and ("négociée" in line or "version" in line.lower()):
            color = theme['success']  # Utiliser la couleur success du thème
        elif "Suite chiffrée" in line or "cipher" in line.lower():
            color = theme['accent_secondary']  # Violet pour cipher suite
        elif "CHACHA20" in line.upper():
            color = theme['success']  # Vert du thème pour CHACHA20
        elif "AES" in line.upper():
            color = theme['accent']  # Bleu du thème pour AES
        elif line.startswith("🔧") or line.startswith("📋"):
            color = theme['accent_secondary']  # Construction de frame et décodage

        html = f'<span style="color:{color}">{line}</span>'
        self.log_view.append(html)
        self.log_view.moveCursor(QTextCursor.MoveOperation.End)

    def apply_theme(self):
        """Apply Apple-style liquid glass theme to this tab"""
        theme = theme_manager.get_current_theme()
        # Appliquer les thèmes simplifiés
        self.apply_combo_theme()
        self.apply_cipher_combo_theme()
        self.apply_input_themes(theme)
        self.apply_button_themes(theme)
        self.apply_log_theme(theme)
        
    def apply_combo_theme(self):
        theme = theme_manager.get_current_theme()
        # Style simplifié pour éviter les bugs d'affichage
        self.template_combo.setStyleSheet(f"""
            QComboBox {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                border: 1px solid {theme['border']};
                border-radius: 8px;
                padding: 8px 12px;
                font-size: 14px;
                min-width: 300px;
            }}
            QComboBox:hover {{
                border-color: {theme['accent']};
            }}
            QComboBox QAbstractItemView {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                selection-background-color: {theme['accent']};
                border: 1px solid {theme['border']};
                border-radius: 4px;
            }}
            QComboBox QAbstractItemView::item {{
                padding: 6px 12px;
                border: none;
            }}
            QComboBox QAbstractItemView::item:selected {{
                background: {theme['accent']};
                color: white;
            }}
        """)
    
    def apply_input_themes(self, theme):
        base_input_style = f"""
            {theme['card_effect']}
            color: {theme['text']};
            border: 1px solid {theme['border']};
            border-radius: 10px;
            padding: 12px 16px;
            font-family: 'Inter', 'Segoe UI', system-ui, sans-serif;
            font-size: 14px;
            font-weight: 400;
        """
        focus_style = f"""
            border-color: {theme['border_focus']};
            background: {theme['glass_effect']};
        """
        
        # Host input
        self.host_edit.setStyleSheet(f"""
            QLineEdit {{
                {base_input_style}
                min-width: 180px;
                max-width: 220px;
            }}
            QLineEdit:focus {{
                {focus_style}
            }}
            QLineEdit:hover {{
                border-color: {theme['border_focus']};
            }}
        """)
        
        # Port input
        self.port_spin.setStyleSheet(f"""
            QSpinBox {{
                {base_input_style}
                min-width: 80px;
                max-width: 100px;
            }}
            QSpinBox:focus {{
                {focus_style}
            }}
            QSpinBox:hover {{
                border-color: {theme['border_focus']};
            }}
        """)
        
        # Message ID input - highlighted
        self.msg_edit.setStyleSheet(f"""
            QLineEdit {{
                {base_input_style}
                color: {theme['accent']};
                font-weight: 600;
                font-family: 'Fira Code', 'SF Mono', monospace;
                min-width: 60px;
                max-width: 80px;
                text-align: center;
            }}
            QLineEdit:focus {{
                {focus_style}
                color: {theme['accent']};
            }}
            QLineEdit:hover {{
                border-color: {theme['border_focus']};
            }}
        """)
        
        # Payload input - highlighted
        self.payload_edit.setStyleSheet(f"""
            QLineEdit {{
                {base_input_style}
                color: {theme['accent_secondary']};
                font-family: 'Fira Code', 'SF Mono', monospace;
                min-width: 250px;
                font-weight: 500;
            }}
            QLineEdit:focus {{
                {focus_style}
                color: {theme['accent_secondary']};
            }}
            QLineEdit:hover {{
                border-color: {theme['border_focus']};
            }}
        """)
        
        # Target combo box - highlighted for primary selection
        self.target_combo.setStyleSheet(f"""
            QComboBox {{
                {base_input_style}
                min-width: 220px;
                color: {theme['accent']};
                font-weight: 600;
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['secondary_bg']},
                    stop:1 {theme['tertiary_bg']});
            }}
            QComboBox:focus {{
                {focus_style}
                color: {theme['accent']};
            }}
            QComboBox:hover {{
                border-color: {theme['border_focus']};
            }}
            QComboBox QAbstractItemView {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                selection-background-color: {theme['accent']};
                border: 1px solid {theme['border']};
                border-radius: 4px;
            }}
        """)

        # SNI input - security focused
        self.sni_edit.setStyleSheet(f"""
            QLineEdit {{
                {base_input_style}
                color: {theme['accent_secondary']};
                font-family: 'Fira Code', 'SF Mono', monospace;
                min-width: 200px;
                font-weight: 500;
            }}
            QLineEdit:focus {{
                {focus_style}
                color: {theme['accent_secondary']};
            }}
            QLineEdit:hover {{
                border-color: {theme['border_focus']};
            }}
        """)

        # Environment combo box
        self.env_combo.setStyleSheet(f"""
            QComboBox {{
                {base_input_style}
                min-width: 200px;
                color: {theme['accent']};
                font-weight: 600;
            }}
            QComboBox:focus {{
                {focus_style}
                color: {theme['accent']};
            }}
            QComboBox:hover {{
                border-color: {theme['border_focus']};
            }}
            QComboBox QAbstractItemView {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
                selection-background-color: {theme['accent']};
                border: 1px solid {theme['border']};
                border-radius: 4px;
            }}
        """)
        
        # Certificate path inputs (for manual mode)
        cert_input_style = f"""
            QLineEdit {{
                {base_input_style}
                color: {theme['text_secondary']};
                font-family: 'Fira Code', 'SF Mono', monospace;
                font-size: 12px;
                min-width: 200px;
            }}
            QLineEdit:focus {{
                {focus_style}
                color: {theme['text']};
            }}
            QLineEdit:hover {{
                border-color: {theme['border_focus']};
            }}
        """
        
        self.ca_path_edit.setStyleSheet(cert_input_style)
        self.cert_path_edit.setStyleSheet(cert_input_style)
        self.key_path_edit.setStyleSheet(cert_input_style)
    
    def apply_button_themes(self, theme):
        # Base button style
        base_button_style = f"""
            border: none;
            border-radius: 10px;
            padding: 12px 24px;
            font-weight: 500;
            font-size: 14px;
            min-width: 100px;
        """
        
        # Connect button - success style
        self.btn_connect.setStyleSheet(f"""
            QPushButton {{
                {base_button_style}
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['success']},
                    stop:1 rgba(16, 185, 129, 0.9));
                color: white;
            }}
            QPushButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 rgba(16, 185, 129, 0.9),
                    stop:1 rgba(16, 185, 129, 0.8));
            }}
            QPushButton:pressed {{
                background: {theme['success']};
            }}
        """)
        
        # Disconnect button - danger style
        self.btn_disconnect.setStyleSheet(f"""
            QPushButton {{
                {base_button_style}
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['danger']},
                    stop:1 rgba(239, 68, 68, 0.9));
                color: white;
            }}
            QPushButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 rgba(239, 68, 68, 0.9),
                    stop:1 rgba(239, 68, 68, 0.8));
            }}
            QPushButton:pressed {{
                background: {theme['danger']};
            }}
            QPushButton:disabled {{
                background: {theme['tertiary_bg']};
                color: {theme['text_secondary']};
            }}
        """)
        
        # Build button - secondary accent
        self.btn_build_frame.setStyleSheet(f"""
            QPushButton {{
                {base_button_style}
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['accent_secondary']},
                    stop:1 rgba(139, 92, 246, 0.9));
                color: white;
            }}
            QPushButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 rgba(139, 92, 246, 0.9),
                    stop:1 rgba(139, 92, 246, 0.8));
            }}
            QPushButton:pressed {{
                background: {theme['accent_secondary']};
            }}
        """)
        
        # Send button - warning style
        self.btn_send.setStyleSheet(f"""
            QPushButton {{
                {base_button_style}
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['warning']},
                    stop:1 rgba(245, 158, 11, 0.9));
                color: white;
            }}
            QPushButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 rgba(245, 158, 11, 0.9),
                    stop:1 rgba(245, 158, 11, 0.8));
            }}
            QPushButton:pressed {{
                background: {theme['warning']};
            }}
            QPushButton:disabled {{
                background: {theme['tertiary_bg']};
                color: {theme['text_secondary']};
            }}
        """)
    
    def apply_log_theme(self, theme):
        self.log_view.setStyleSheet(f"""
            QTextEdit {{
                {theme['glass_blur']}
                color: {theme['text']};
                border: 1px solid {theme['border']};
                border-radius: 16px;
                padding: 20px;
                selection-background-color: {theme['accent']};
                selection-color: white;
                font-family: 'Fira Code', 'SF Mono', 'Consolas', monospace;
                font-size: 13px;
                line-height: 1.6;
                font-weight: 400;
            }}
            QScrollBar:vertical {{
                background: {theme['secondary_bg']};
                width: 12px;
                border-radius: 6px;
                margin: 0px;
            }}
            QScrollBar::handle:vertical {{
                background: {theme['border']};
                border-radius: 6px;
                min-height: 20px;
            }}
            QScrollBar::handle:vertical:hover {{
                background: {theme['text_secondary']};
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("🚀 TLS Multi-Client GUI - Robot Control Center")
        self.resize(1400, 800)
        self.setMinimumSize(1200, 600)

        # Toolbar
        self.toolbar = self.addToolBar("Main")
        
        new_conn_action = self.toolbar.addAction("➕ Nouvelle Connexion", self._add_new_tab)
        self.toolbar.addSeparator()
        
        # Theme toggle button  
        self.theme_toggle = QPushButton("☀️ Mode Clair")
        self.theme_toggle.clicked.connect(self._toggle_theme)
        self.toolbar.addWidget(self.theme_toggle)
        self.toolbar.addSeparator()
        
        # Add status label to toolbar
        self.status_label = QLabel("📊 Connexions actives: 0")
        self.toolbar.addWidget(self.status_label)

        self.tabs = QTabWidget()
        self.tabs.setTabsClosable(True)
        self.tabs.tabCloseRequested.connect(self._close_tab)
        self.setCentralWidget(self.tabs)

        # Apply theme after all widgets are created
        self.apply_liquid_glass_theme()

        # Initial tab with default target
        self._add_new_tab("Local Development")
        self._update_status()

    def _add_new_tab(self, target_name: str = "Local Development"):
        tab_number = self.tabs.count() + 1
        tab = ConnectionTab(target_name)

        # Connecter les signaux pour la mise à jour du statut
        def on_tab_connected():
            self._update_status()
            # Mettre à jour le titre de l'onglet
            tab_idx = self.tabs.indexOf(tab)
            if tab_idx != -1:
                self.tabs.setTabText(tab_idx, f"✅ Client {tab_number}")

        def on_tab_disconnected():
            self._update_status()
            # Mettre à jour le titre de l'onglet
            tab_idx = self.tabs.indexOf(tab)
            if tab_idx != -1:
                self.tabs.setTabText(tab_idx, f"🔌 Client {tab_number}")

        # Connecter aux signaux du worker à travers les slots du tab
        tab._on_connected_original = tab._on_connected
        tab._on_disconnected_original = tab._on_disconnected

        def new_on_connected():
            tab._on_connected_original()
            on_tab_connected()

        def new_on_disconnected():
            tab._on_disconnected_original()
            on_tab_disconnected()

        tab._on_connected = new_on_connected
        tab._on_disconnected = new_on_disconnected

        idx = self.tabs.addTab(tab, f"🔌 Client {tab_number}")
        self.tabs.setCurrentIndex(idx)
        self._update_status()
    
    def _update_status(self):
        connected_count = 0
        total_tabs = self.tabs.count()
        
        for i in range(total_tabs):
            tab = self.tabs.widget(i)
            if hasattr(tab, 'is_connected') and tab.is_connected:
                connected_count += 1
            elif hasattr(tab, 'worker') and tab.worker and tab.worker.isRunning():
                connected_count += 1
        
        self.status_label.setText(f"📊 Connexions actives: {connected_count}/{total_tabs}")
        
        # Mettre à jour le titre de la fenêtre
        if connected_count > 0:
            self.setWindowTitle(f"🚀 TLS Multi-Client GUI - {connected_count} connexion(s) active(s)")
        else:
            self.setWindowTitle("🚀 TLS Multi-Client GUI - Robot Control Center")

    def _close_tab(self, index):
        if self.tabs.count() <= 1:
            QMessageBox.information(self, "Information", "Au moins un onglet doit rester ouvert.")
            return  # Garde au moins un onglet
            
        tab = self.tabs.widget(index)
        tab_text = self.tabs.tabText(index)
        
        # Demander confirmation si connecté
        if hasattr(tab, 'worker') and tab.worker and tab.worker.isRunning():
            reply = QMessageBox.question(
                self, 
                "Fermer l'onglet", 
                f"L'onglet '{tab_text}' est connecté. Voulez-vous vraiment le fermer ?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No
            )
            if reply == QMessageBox.StandardButton.No:
                return
            
            # Fermer proprement la connexion
            tab._append_log("[INFO] Fermeture demandée par l'utilisateur...")
            tab.worker.close()
            tab.worker.wait(2000)  # Attendre 2 secondes max
            
        # Supprimer l'onglet
        self.tabs.removeTab(index)
        self._update_status()
        
        # Log de la fermeture
        print(f"Onglet fermé: {tab_text}")

    def _toggle_theme(self):
        theme = theme_manager.toggle_theme()
        self.theme_toggle.setText("🌙 Mode Sombre" if theme_manager.current_theme == "light" else "☀️ Mode Clair")
        self.apply_liquid_glass_theme()
        
        # Update all tabs
        for i in range(self.tabs.count()):
            tab = self.tabs.widget(i)
            if hasattr(tab, 'apply_theme'):
                tab.apply_theme()

    def apply_liquid_glass_theme(self):
        theme = theme_manager.get_current_theme()
        
        # Main window style with clean glass effects
        self.setStyleSheet(f"""
            QMainWindow {{
                {theme['glass_blur']}
                color: {theme['text']};
                font-family: 'Inter', 'SF Pro Display', 'Helvetica Neue', system-ui, sans-serif;
                font-weight: 400;
            }}
        """)
        
        # Toolbar style - clean and compatible
        self.toolbar.setStyleSheet(f"""
            QToolBar {{
                {theme['glass_effect']}
                border: none;
                spacing: 16px;
                padding: 20px 24px;
                margin: 0px;
            }}
            QToolBar QToolButton {{
                {theme['card_effect']}
                color: {theme['text']};
                border: none;
                border-radius: 12px;
                padding: 12px 20px;
                font-weight: 500;
                font-size: 14px;
                min-width: 140px;
            }}
            QToolBar QToolButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['accent']},
                    stop:1 {theme['accent_secondary']});
                color: white;
                border: 1px solid {theme['border_focus']};
            }}
            QToolBar QToolButton:pressed {{
                background: {theme['accent']};
                color: white;
            }}
            QPushButton {{
                {theme['card_effect']}
                color: {theme['text']};
                border: none;
                border-radius: 12px;
                padding: 12px 20px;
                font-weight: 500;
                font-size: 14px;
            }}
            QPushButton:hover {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['accent']},
                    stop:1 {theme['accent_secondary']});
                color: white;
                border: 1px solid {theme['border_focus']};
            }}
            QPushButton:pressed {{
                background: {theme['accent']};
                color: white;
            }}
        """)
        
        # Status label - elegant badge style
        self.status_label.setStyleSheet(f"""
            QLabel {{
                color: {theme['text_secondary']};
                font-weight: 500;
                padding: 8px 16px;
                {theme['card_effect']}
                font-size: 13px;
                letter-spacing: 0.5px;
            }}
        """)
        
        # Tabs - clean professional style
        self.tabs.setStyleSheet(f"""
            QTabWidget::pane {{
                {theme['glass_blur']}
                border: 1px solid {theme['border']};
                border-radius: 20px;
                margin-top: 12px;
                padding: 4px;
            }}
            QTabWidget::tab-bar {{
                alignment: left;
            }}
            QTabBar::tab {{
                {theme['card_effect']}
                color: {theme['text_secondary']};
                border: none;
                border-radius: 12px;
                padding: 12px 24px;
                margin-right: 6px;
                margin-top: 6px;
                font-weight: 500;
                font-size: 14px;
                min-width: 100px;
            }}
            QTabBar::tab:selected {{
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1,
                    stop:0 {theme['accent']},
                    stop:1 rgba(59, 130, 246, 0.9));
                color: white;
                font-weight: 600;
            }}
            QTabBar::tab:hover:!selected {{
                background: {theme['secondary_bg']};
                color: {theme['text']};
            }}
            QTabBar::close-button {{
                image: none;
                background: rgba(71, 85, 105, 0.2);
                border-radius: 10px;
                width: 20px;
                height: 20px;
                margin: 2px;
            }}
            QTabBar::close-button:hover {{
                background: {theme['danger']};
            }}
        """)

    def keyPressEvent(self, event):
        # Ctrl+N = new connection tab
        if event.key() == Qt.Key.Key_N and event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            self._add_new_tab()
        super().keyPressEvent(event)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main():
    app = QApplication(sys.argv)

    # Apply liquid glass styling
    app.setStyle("Fusion")

    window = MainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
