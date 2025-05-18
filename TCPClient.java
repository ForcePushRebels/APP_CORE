import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

public class TCPClient extends JFrame {
    private JTextField ipField;
    private JTextField portField;
    private JTextField messageField;
    private JTextArea responseArea;
    private JButton connectButton;
    private JButton sendButton;
    private JComboBox<String> messageTypeComboBox;
    private JComboBox<String> testPayloadComboBox;
    private JCheckBox useCustomPayloadCheckbox;
    
    private Socket socket;
    private OutputStream outputStream;
    private boolean connected = false;
    
    // Message type constants - SENT BY ANDROID
    private static final byte ID_SET_MOVEMENT = 0x00;
    private static final byte ID_START = 0x01;
    private static final byte ID_STOP = 0x02;
    private static final byte ID_CHOOSE_MANU_MODE = 0x03;
    private static final byte ID_SELECTED_POINTS = 0x04;
    private static final byte ID_GET_FINAL_METRICS = 0x05;
    private static final byte ID_UPLOAD_MAP = 0x06;
    private static final byte ID_ASK_STRATS_LIST = 0x07;
    private static final byte ID_SELECT_START = 0x08;

    // Message type constants - SENT BY BOT
    private static final byte ID_INF_BATTERY = 0x10;
    private static final byte ID_INF_STATUS = 0x11;
    private static final byte ID_INF_POS = 0x12;
    private static final byte ID_INF_TIME = 0x13;

    private static final byte ID_MAP_FRAGMENT = 0x20;
    private static final byte ID_MAP_FULL = 0x21;
    private static final byte ID_METRICS_EXPLO = 0x22;
    private static final byte ID_METRICS_INTER = 0x23;
    private static final byte ID_STARTS_LIST = 0x24;

    // UDP message types
    private static final byte ID_IS_ANY_ROBOT_HERE = 0x30;
    private static final byte ID_MANIFEST = 0x31;
    
    public TCPClient() {
        super("TCP Client - API C");
        setSize(600, 500);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        // Création du panel principal avec BorderLayout
        JPanel mainPanel = new JPanel(new BorderLayout(5, 5));
        setContentPane(mainPanel);
        
        // Panneau de contrôles (en haut)
        JPanel topPanel = new JPanel(new GridLayout(3, 1, 0, 5));
        
        // 1. Panneau de connexion
        JPanel connectionPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        connectionPanel.add(new JLabel("IP:"));
        ipField = new JTextField("127.0.0.1", 10);
        connectionPanel.add(ipField);
        connectionPanel.add(new JLabel("Port:"));
        portField = new JTextField("8080", 5);
        connectionPanel.add(portField);
        
        connectButton = new JButton("Connecter");
        connectionPanel.add(connectButton);
        
        // 2. Panel for message type selection
        JPanel typePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        typePanel.add(new JLabel("Type de message:"));
        messageTypeComboBox = new JComboBox<>(new String[] {
            "ID_SET_MOVEMENT (0x00)",
            "ID_START (0x01)",
            "ID_STOP (0x02)",
            "ID_CHOOSE_MANU_MODE (0x03)",
            "ID_SELECTED_POINTS (0x04)",
            "ID_GET_FINAL_METRICS (0x05)",
            "ID_UPLOAD_MAP (0x06)",
            "ID_ASK_STRATS_LIST (0x07)",
            "ID_SELECT_START (0x08)",
            "ID_INF_BATTERY (0x10)",
            "ID_INF_STATUS (0x11)",
            "ID_INF_POS (0x12)",
            "ID_INF_TIME (0x13)",
            "ID_MAP_FRAGMENT (0x20)",
            "ID_MAP_FULL (0x21)",
            "ID_METRICS_EXPLO (0x22)",
            "ID_METRICS_INTER (0x23)",
            "ID_STARTS_LIST (0x24)",
            "ID_IS_ANY_ROBOT_HERE (0x30)",
            "ID_MANIFEST (0x31)"
        });
        typePanel.add(messageTypeComboBox);
        
        // 3. Panel for test payloads
        JPanel testPayloadPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        testPayloadPanel.add(new JLabel("Payload prédéfinie:"));
        testPayloadComboBox = new JComboBox<>(new String[] {
            "Vide (aucune payload)",
            "Test simple (\"test\")",
            "Position (\"x:100,y:200\")",
            "Points (\"p1:10,20;p2:30,40\")",
            "Commande (\"speed:50,direction:1\")",
            "JSON ({\"command\":\"move\",\"params\":{\"speed\":100}})"
        });
        testPayloadPanel.add(testPayloadComboBox);
        
        useCustomPayloadCheckbox = new JCheckBox("Payload personnalisée", true);
        testPayloadPanel.add(useCustomPayloadCheckbox);
        
        // Ajouter les trois panels au panel du haut
        topPanel.add(connectionPanel);
        topPanel.add(typePanel);
        topPanel.add(testPayloadPanel);
        
        // Panneau central (zone de texte avec scrolling)
        responseArea = new JTextArea();
        responseArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(responseArea);
        
        // Panneau inférieur (message + bouton d'envoi)
        JPanel bottomPanel = new JPanel(new BorderLayout(5, 0));
        bottomPanel.add(new JLabel("Payload:"), BorderLayout.WEST);
        messageField = new JTextField();
        bottomPanel.add(messageField, BorderLayout.CENTER);
        sendButton = new JButton("Envoyer");
        bottomPanel.add(sendButton, BorderLayout.EAST);
        
        // Ajouter tous les panels au panel principal
        mainPanel.add(topPanel, BorderLayout.NORTH);
        mainPanel.add(scrollPane, BorderLayout.CENTER);
        mainPanel.add(bottomPanel, BorderLayout.SOUTH);
        
        // Ajouter une marge autour des composants
        mainPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        
        // Désactiver le bouton d'envoi jusqu'à la connexion
        sendButton.setEnabled(false);
        
        // Gestionnaires d'événements
        connectButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (!connected) {
                    connect();
                } else {
                    disconnect();
                }
            }
        });
        
        sendButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sendMessage();
            }
        });
        
        messageField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sendMessage();
            }
        });
        
        // Update message field when test payload is selected
        testPayloadComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (!useCustomPayloadCheckbox.isSelected()) {
                    updateMessageFieldFromTestPayload();
                }
            }
        });
        
        // Toggle between custom and predefined payload
        useCustomPayloadCheckbox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (!useCustomPayloadCheckbox.isSelected()) {
                    updateMessageFieldFromTestPayload();
                    messageField.setEditable(false);
                } else {
                    messageField.setEditable(true);
                }
            }
        });
        
        // Gestionnaire de fermeture pour nettoyer les connexions
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                disconnect();
            }
        });
    }
    
    private void connect() {
        try {
            String ip = ipField.getText();
            int port = Integer.parseInt(portField.getText());
            
            responseArea.append("Connexion à " + ip + ":" + port + "...\n");
            
            socket = new Socket(ip, port);
            outputStream = socket.getOutputStream();
            
            connected = true;
            connectButton.setText("Déconnecter");
            sendButton.setEnabled(true);
            ipField.setEnabled(false);
            portField.setEnabled(false);
            
            responseArea.append("Connecté!\n");
            
            // Thread pour lire les réponses du serveur
            new Thread(new Runnable() {
                public void run() {
                    try {
                        byte[] buffer = new byte[4096]; // Buffer pour les données brutes
                        int bytesRead;
                        
                        while ((bytesRead = socket.getInputStream().read(buffer)) != -1) {
                            final byte[] data = Arrays.copyOf(buffer, bytesRead);
                            
                            SwingUtilities.invokeLater(new Runnable() {
                                public void run() {
                                    // Essayer de décoder comme un message réseau
                                    DecodedMessage decodedMessage = NetworkMessageDecoder.decode(data);
                                    
                                    if (decodedMessage != null) {
                                        String messageInfo = String.format(
                                            "Message reçu [%s - 0x%02X]\n" +
                                            "Structure détaillée:\n%s\n",
                                            decodedMessage.getMessageTypeString(),
                                            decodedMessage.getMessageType() & 0xFF,
                                            decodedMessage.getStructureDetails()
                                        );
                                        responseArea.append(messageInfo + "\n");
                                    } else {
                                        // Afficher comme données brutes si le décodage échoue
                                        responseArea.append("Données brutes reçues: " + bytesToHex(data) + "\n");
                                    }
                                }
                            });
                        }
                    } catch (IOException e) {
                        if (connected) {
                            SwingUtilities.invokeLater(new Runnable() {
                                public void run() {
                                    responseArea.append("Erreur de lecture: " + e.getMessage() + "\n");
                                    disconnect();
                                }
                            });
                        }
                    }
                }
            }).start();
            
        } catch (NumberFormatException e) {
            responseArea.append("Erreur: Port invalide\n");
        } catch (IOException e) {
            responseArea.append("Erreur de connexion: " + e.getMessage() + "\n");
        }
    }
    
    private void disconnect() {
        if (connected) {
            try {
                if (socket != null) socket.close();
                if (outputStream != null) outputStream.close();
            } catch (IOException e) {
                responseArea.append("Erreur lors de la déconnexion: " + e.getMessage() + "\n");
            } finally {
                connected = false;
                connectButton.setText("Connecter");
                sendButton.setEnabled(false);
                ipField.setEnabled(true);
                portField.setEnabled(true);
                responseArea.append("Déconnecté\n");
            }
        }
    }
    
    private void updateMessageFieldFromTestPayload() {
        int selectedIndex = testPayloadComboBox.getSelectedIndex();
        switch (selectedIndex) {
            case 0: // Empty
                messageField.setText("");
                break;
            case 1: // Test simple
                messageField.setText("test");
                break;
            case 2: // Position
                messageField.setText("x:100,y:200");
                break;
            case 3: // Points
                messageField.setText("p1:10,20;p2:30,40");
                break;
            case 4: // Command
                messageField.setText("speed:50,direction:1");
                break;
            case 5: // JSON
                messageField.setText("{\"command\":\"move\",\"params\":{\"speed\":100}}");
                break;
            default:
                messageField.setText("");
        }
    }
    
    private void sendMessage() {
        if (connected && outputStream != null) {
            String messageText;
            
            if (!useCustomPayloadCheckbox.isSelected()) {
                // Use predefined payload
                int selectedIndex = testPayloadComboBox.getSelectedIndex();
                switch (selectedIndex) {
                    case 0: // Empty
                        messageText = "";
                        break;
                    case 1: // Test simple
                        messageText = "test";
                        break;
                    case 2: // Position
                        messageText = "x:100,y:200";
                        break;
                    case 3: // Points
                        messageText = "p1:10,20;p2:30,40";
                        break;
                    case 4: // Command
                        messageText = "speed:50,direction:1";
                        break;
                    case 5: // JSON
                        messageText = "{\"command\":\"move\",\"params\":{\"speed\":100}}";
                        break;
                    default:
                        messageText = "";
                }
                messageField.setText(messageText);
            } else {
                // Use custom payload
                messageText = messageField.getText().trim();
            }
            
            if (messageText.isEmpty() && testPayloadComboBox.getSelectedIndex() != 0) {
                // If text is empty but not intentionally selected as empty payload
                responseArea.append("Erreur: Payload vide\n");
                return;
            }
            
            try {
                // Get message type based on selection
                int selectedIndex = messageTypeComboBox.getSelectedIndex();
                byte messageType;
                
                if (selectedIndex >= 0 && selectedIndex <= 8) {
                    // Android messages
                    messageType = (byte)selectedIndex;
                } else if (selectedIndex >= 9 && selectedIndex <= 12) {
                    // Bot info messages
                    messageType = (byte)(0x10 + (selectedIndex - 9));
                } else if (selectedIndex >= 13 && selectedIndex <= 17) {
                    // Bot map messages
                    messageType = (byte)(0x20 + (selectedIndex - 13));
                } else if (selectedIndex >= 18 && selectedIndex <= 19) {
                    // UDP messages
                    messageType = (byte)(0x30 + (selectedIndex - 18));
                } else {
                    messageType = ID_START;
                }
                
                // Create and send encoded network message
                NetworkMessage networkMsg = new NetworkMessage(messageType, messageText.getBytes());
                byte[] encodedMessage = networkMsg.encode();
                
                outputStream.write(encodedMessage);
                outputStream.flush();
                
                responseArea.append("Message envoyé: Type=0x" + 
                                   String.format("%02X", messageType) + 
                                   ", Payload=\"" + messageText + "\"\n");
                messageField.setText("");
            } catch (IOException e) {
                responseArea.append("Erreur d'envoi: " + e.getMessage() + "\n");
                disconnect();
            }
        }
    }
    
    // Utilitaire pour convertir un tableau d'octets en chaîne hexadécimale
    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02X ", b & 0xFF));
        }
        return sb.toString();
    }
    
    // Network message class to handle the C API format
    private static class NetworkMessage {
        private byte messageType;
        private byte[] payload;
        private int payloadSize;
        
        public NetworkMessage(byte messageType, byte[] msgPayload) {
            this.messageType = messageType;
            this.payload = msgPayload;
            this.payloadSize = (msgPayload != null) ? msgPayload.length : 0;
        }
        
        public byte[] encode() {
            int totalSize = 4 + 1 + payloadSize; // payloadSize + messageType + payload
            ByteBuffer buffer = ByteBuffer.allocate(totalSize);
            buffer.order(ByteOrder.BIG_ENDIAN); // Utiliser Big Endian (ordre réseau)
            
            // Écrire la taille du payload (uint32_t)
            buffer.putInt(payloadSize);
            
            // Écrire le type de message (1 octet)
            buffer.put(messageType);
            
            // Écrire le payload
            if (payload != null && payloadSize > 0) {
                buffer.put(payload);
            }
            
            return buffer.array();
        }
    }
    
    /**
     * Classe pour représenter la structure idCard_t du C
     * #pragma pack(push, 1)
     * typedef struct idCard_t
     * {
     *     char t_pcRobotName[32];
     *     char t_pcIpAddr[16];
     *     int t_iRole;
     * } manifest_t;
     * #pragma pack(pop)
     */
    private static class IdCard {
        private static final int ROBOT_NAME_LENGTH = 32;
        private static final int IP_ADDR_LENGTH = 16;
        
        private String robotName;
        private String ipAddr;
        private int role;
        
        public IdCard(String robotName, String ipAddr, int role) {
            this.robotName = robotName;
            this.ipAddr = ipAddr;
            this.role = role;
        }
        
        public String getRobotName() {
            return robotName;
        }
        
        public String getIpAddr() {
            return ipAddr;
        }
        
        public int getRole() {
            return role;
        }
        
        /**
         * Décode un tableau de bytes au format big endian en un objet IdCard
         */
        public static IdCard fromBytes(byte[] data) {
            if (data == null || data.length < ROBOT_NAME_LENGTH + IP_ADDR_LENGTH + 4) {
                return null;
            }
            
            ByteBuffer buffer = ByteBuffer.wrap(data);
            buffer.order(ByteOrder.BIG_ENDIAN); // Format réseau (big endian)
            
            // Extraire le nom du robot
            byte[] nameBytes = new byte[ROBOT_NAME_LENGTH];
            buffer.get(nameBytes);
            String robotName = extractNullTerminatedString(nameBytes);
            
            // Extraire l'adresse IP
            byte[] ipBytes = new byte[IP_ADDR_LENGTH];
            buffer.get(ipBytes);
            String ipAddr = extractNullTerminatedString(ipBytes);
            
            // Extraire le rôle (int)
            int role = buffer.getInt();
            
            return new IdCard(robotName, ipAddr, role);
        }
        
        private static String extractNullTerminatedString(byte[] data) {
            int nullPos = 0;
            while (nullPos < data.length && data[nullPos] != 0) {
                nullPos++;
            }
            return new String(data, 0, nullPos);
        }
        
        @Override
        public String toString() {
            return "IdCard{" +
                   "robotName='" + robotName + "', " +
                   "ipAddress='" + ipAddr + "', " +
                   "role=" + role +
                   "}";
        }
    }
    
    // Network message decoder class to handle the C API format
    private static class NetworkMessageDecoder {
        // Décode un message à partir d'un tableau d'octets
        public static DecodedMessage decode(byte[] data) {
            if (data == null || data.length < 5) { // Taille minimale: 4 (size) + 1 (header)
                return null;
            }
            
            ByteBuffer buffer = ByteBuffer.wrap(data);
            buffer.order(ByteOrder.BIG_ENDIAN); // Utiliser Big Endian (ordre réseau)
            
            // Lire la taille du payload
            int payloadSize = buffer.getInt();
            
            // Lire le type de message (1 octet de header)
            byte msgType = buffer.get();
            
            // Vérifier que la taille du message est cohérente
            if (data.length < 5 + payloadSize) {
                return null;
            }
            
            // Extraire le payload
            byte[] payload = new byte[payloadSize];
            if (payloadSize > 0) {
                buffer.get(payload, 0, payloadSize);
            }
            
            return new DecodedMessage(msgType, payload, payloadSize);
        }
    }
    
    // Classe pour représenter un message décodé
    private static class DecodedMessage {
        private byte messageType;
        private byte[] payload;
        private int payloadSize;
        
        public DecodedMessage(byte messageType, byte[] payload, int payloadSize) {
            this.messageType = messageType;
            this.payload = payload;
            this.payloadSize = payloadSize;
        }
        
        public byte getMessageType() {
            return messageType;
        }
        
        public byte[] getPayload() {
            return payload;
        }
        
        public int getPayloadSize() {
            return payloadSize;
        }
        
        // Méthode pour convertir le type de message en chaîne
        public String getMessageTypeString() {
            switch (messageType) {
                // Messages ANDROID
                case ID_SET_MOVEMENT: return "ID_SET_MOVEMENT";
                case ID_START: return "ID_START";
                case ID_STOP: return "ID_STOP";
                case ID_CHOOSE_MANU_MODE: return "ID_CHOOSE_MANU_MODE";
                case ID_SELECTED_POINTS: return "ID_SELECTED_POINTS";
                case ID_GET_FINAL_METRICS: return "ID_GET_FINAL_METRICS";
                case ID_UPLOAD_MAP: return "ID_UPLOAD_MAP";
                case ID_ASK_STRATS_LIST: return "ID_ASK_STRATS_LIST";
                case ID_SELECT_START: return "ID_SELECT_START";
                
                // Messages BOT
                case ID_INF_BATTERY: return "ID_INF_BATTERY";
                case ID_INF_STATUS: return "ID_INF_STATUS";
                case ID_INF_POS: return "ID_INF_POS";
                case ID_INF_TIME: return "ID_INF_TIME";
                case ID_MAP_FRAGMENT: return "ID_MAP_FRAGMENT";
                case ID_MAP_FULL: return "ID_MAP_FULL";
                case ID_METRICS_EXPLO: return "ID_METRICS_EXPLO";
                case ID_METRICS_INTER: return "ID_METRICS_INTER";
                case ID_STARTS_LIST: return "ID_STARTS_LIST";
                
                // UDP
                case ID_IS_ANY_ROBOT_HERE: return "ID_IS_ANY_ROBOT_HERE";
                case ID_MANIFEST: return "ID_MANIFEST";
                default: return "UNKNOWN (0x" + String.format("%02X", messageType) + ")";
            }
        }
        
        // Méthode pour afficher tous les champs de la structure
        public String getStructureDetails() {
            StringBuilder sb = new StringBuilder();
            sb.append("Structure network_message_t:\n");
            sb.append(String.format("t_iPayloadSize: %d (0x%08X)\n", payloadSize, payloadSize));
            sb.append(String.format("t_iHeader: 0x%02X (%s)\n", messageType & 0xFF, getMessageTypeString()));
            sb.append(String.format("t_ptucPayload: %d bytes\n", payload.length));
            
            // Afficher le contenu du payload en hex et en texte
            if (payload.length > 0) {
                sb.append("Payload content (hex): ");
                StringBuilder hexBuilder = new StringBuilder();
                for (byte b : payload) {
                    hexBuilder.append(String.format("%02X ", b & 0xFF));
                }
                sb.append(hexBuilder.toString().trim());
                
                sb.append("\nPayload content (text): ");
                StringBuilder textBuilder = new StringBuilder();
                for (byte b : payload) {
                    if (b >= 32 && b < 127) {
                        textBuilder.append((char)b);
                    } else {
                        textBuilder.append('.');
                    }
                }
                sb.append(textBuilder.toString());
                sb.append("\n");
                
                // Analyse additionnelle selon le type de message
                sb.append("\n");
                sb.append(getFormattedPayload());
            }
            
            return sb.toString();
        }
        
        // Méthode pour formater le payload selon son type
        public String getFormattedPayload() {
            if (payload == null || payload.length == 0) {
                return "[Pas de payload]";
            }
            
            // Selon le type de message, formater le payload différemment
            try {
                switch (messageType) {
                    case ID_INF_BATTERY:
                        if (payload.length >= 4) {
                            ByteBuffer buffer = ByteBuffer.wrap(payload);
                            buffer.order(ByteOrder.BIG_ENDIAN);
                            float batteryLevel = buffer.getFloat();
                            return String.format("Analyse détaillée:\nNiveau de batterie: %.2f%%", batteryLevel * 100);
                        }
                        break;
                        
                    case ID_INF_STATUS:
                        if (payload.length >= 1) {
                            int status = payload[0] & 0xFF;
                            return String.format("Analyse détaillée:\nStatus: %d", status);
                        }
                        break;
                        
                    case ID_INF_POS:
                        if (payload.length >= 8) {
                            ByteBuffer buffer = ByteBuffer.wrap(payload);
                            buffer.order(ByteOrder.BIG_ENDIAN);
                            float x = buffer.getFloat();
                            float y = buffer.getFloat();
                            return String.format("Analyse détaillée:\nPosition: (%.2f, %.2f)", x, y);
                        }
                        break;
                        
                    case ID_INF_TIME:
                        if (payload.length >= 4) {
                            ByteBuffer buffer = ByteBuffer.wrap(payload);
                            buffer.order(ByteOrder.BIG_ENDIAN);
                            int seconds = buffer.getInt();
                            return String.format("Analyse détaillée:\nTemps: %d secondes", seconds);
                        }
                        break;
                        
                    case ID_MANIFEST:
                        // Décoder le manifest selon la nouvelle structure idCard_t
                        // Structure: char t_pcRobotName[32], char t_pcIpAddr[16], int t_iRole
                        IdCard idCard = IdCard.fromBytes(payload);
                        
                        if (idCard != null) {
                            StringBuilder manifestBuilder = new StringBuilder("Analyse détaillée du MANIFEST:\n");
                            manifestBuilder.append(String.format("Nom du robot: %s\n", idCard.getRobotName()));
                            manifestBuilder.append(String.format("Adresse IP: %s\n", idCard.getIpAddr()));
                            manifestBuilder.append(String.format("Rôle: %d\n", idCard.getRole()));
                            
                            // Afficher les octets sous forme brute pour vérification
                            manifestBuilder.append("\nDécomposition des octets:\n");
                            
                            // Nom du robot (32 octets)
                            manifestBuilder.append("Octets 0-31 (Nom): ");
                            for (int i = 0; i < Math.min(IdCard.ROBOT_NAME_LENGTH, payload.length); i++) {
                                if (i % 16 == 0 && i > 0) {
                                    manifestBuilder.append("\n                 ");
                                }
                                manifestBuilder.append(String.format("%02X ", payload[i] & 0xFF));
                            }
                            manifestBuilder.append("\n");
                            
                            // Adresse IP (16 octets)
                            int ipOffset = IdCard.ROBOT_NAME_LENGTH;
                            if (payload.length >= ipOffset + IdCard.IP_ADDR_LENGTH) {
                                manifestBuilder.append("Octets 32-47 (IP): ");
                                for (int i = ipOffset; i < ipOffset + IdCard.IP_ADDR_LENGTH && i < payload.length; i++) {
                                    manifestBuilder.append(String.format("%02X ", payload[i] & 0xFF));
                                }
                                manifestBuilder.append("\n");
                            }
                            
                            // Rôle (4 octets)
                            int roleOffset = ipOffset + IdCard.IP_ADDR_LENGTH;
                            if (payload.length >= roleOffset + 4) {
                                manifestBuilder.append("Octets 48-51 (Rôle): ");
                                for (int i = roleOffset; i < roleOffset + 4 && i < payload.length; i++) {
                                    manifestBuilder.append(String.format("%02X ", payload[i] & 0xFF));
                                }
                                manifestBuilder.append("\n");
                            }
                            
                            return manifestBuilder.toString();
                        }
                        break;
                        
                    default:
                        // Pour les types non spécifiés, analyser le contenu de manière générique
                        StringBuilder detailBuilder = new StringBuilder("Analyse générique du payload:\n");

                        // Essayer de décoder comme texte
                        if (isTextPayload(payload)) {
                            detailBuilder.append("Type détecté: Texte\n");
                            detailBuilder.append("Contenu: \"" + new String(payload) + "\"\n");
                        } 
                        // Essayer de décoder comme valeurs numériques
                        else if (payload.length % 4 == 0 && payload.length <= 16) {
                            detailBuilder.append("Possible contenu numérique:\n");
                            ByteBuffer buffer = ByteBuffer.wrap(payload);
                            buffer.order(ByteOrder.BIG_ENDIAN);
                            
                            // Interpréter comme int
                            buffer.rewind();
                            detailBuilder.append("En tant qu'entiers: ");
                            for (int i = 0; i < payload.length / 4; i++) {
                                detailBuilder.append(buffer.getInt() + " ");
                            }
                            
                            // Interpréter comme float
                            buffer.rewind();
                            detailBuilder.append("\nEn tant que flottants: ");
                            for (int i = 0; i < payload.length / 4; i++) {
                                detailBuilder.append(buffer.getFloat() + " ");
                            }
                            detailBuilder.append("\n");
                        } else {
                            detailBuilder.append("Contenu binaire (non analysé spécifiquement)\n");
                        }
                        
                        return detailBuilder.toString();
                }
            } catch (Exception e) {
                return "Erreur lors de l'analyse du payload: " + e.getMessage();
            }
            
            return "Payload brut (" + payload.length + " octets)";
        }
        
        // Utilitaire pour extraire une chaîne terminée par null
        private String extractNullTerminatedString(byte[] data, int offset, int maxLength) {
            int nullPos = offset;
            // Trouver la position du premier octet null
            while (nullPos < offset + maxLength && nullPos < data.length) {
                if (data[nullPos] == 0) {
                    break;
                }
                nullPos++;
            }
            
            if (nullPos == offset) {
                return ""; // Chaîne vide
            }
            
            return new String(data, offset, nullPos - offset);
        }
        
        // Utilitaire pour déterminer si un payload semble être du texte
        private boolean isTextPayload(byte[] data) {
            if (data.length == 0) return false;
            
            int textChars = 0;
            for (byte b : data) {
                if ((b >= 32 && b < 127) || b == '\n' || b == '\r' || b == '\t') {
                    textChars++;
                }
            }
            
            // Si plus de 80% des caractères sont du texte, considérer comme texte
            return (textChars * 100 / data.length) > 80;
        }
    }
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                new TCPClient().setVisible(true);
            }
        });
    }
}
