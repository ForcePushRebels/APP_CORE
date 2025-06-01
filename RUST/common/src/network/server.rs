////////////////////////////////////////////////////////////
//  Server source file
//  Defines server configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

use crate::x_log::{write_log, LogConfig, XOS_LOG_OK};
use crate::x_assert::x_assert;
use crate::network::converter::ConverterConfig;
use crate::network::handle_network::handle_incoming_message;

use std::net::{TcpListener, TcpStream};
use std::thread::JoinHandle;
use std::thread;
use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};

///error code encoded in 32 bits
pub const SERVER_OK: u32 = 0xE3452100;
pub const SERVER_ERROR: u32 = 0xE3452101;
pub const SERVER_NOT_INIT: u32 = 0xE3452102;
pub const SERVER_MUTEX_ERROR: u32 = 0xE3452103;
pub const SERVER_THREAD_ERROR: u32 = 0xE3452104;
pub const SERVER_BUFFER_ERROR: u32 = 0xE3452105;

/// Configuration du serveur
#[derive(Clone)]
pub struct ServerConfig {
    pub port: u16,
    pub max_clients: u32,
    pub max_connections: u32,
    pub max_threads: u32,
    pub max_buffer_size: u32,
    pub max_packet_size: u32,
    pub ip_addr: String,
}

impl ServerConfig {
    /// Crée une nouvelle configuration de serveur avec des valeurs par défaut
    pub fn new(ip_addr: String, port: u16) -> Self {
        Self {
            port,
            max_clients: 10,
            max_connections: 100,
            max_threads: 4,
            max_buffer_size: 4096,
            max_packet_size: 1024,
            ip_addr,
        }
    }
}

pub struct Server {
    pub config: ServerConfig,
    pub listener: TcpListener,
    pub clients: Vec<TcpStream>,
    pub threads: Arc<Mutex<Vec<JoinHandle<()>>>>,
    pub server_thread: Option<JoinHandle<()>>,
    pub running: Arc<AtomicBool>,
}

impl Server {
    pub fn new(config: ServerConfig) -> Result<Self, u32> {
        x_assert(config.port > 0);
        x_assert(config.max_clients > 0);
        x_assert(config.max_connections > 0);
        x_assert(config.max_threads > 0);
        x_assert(config.max_buffer_size > 0);
        x_assert(config.max_packet_size > 0);
        x_assert(!config.ip_addr.is_empty());

        let bind_addr = format!("{}:{}", config.ip_addr, config.port);
        let listener = match TcpListener::bind(&bind_addr) {
            Ok(listener) => listener,
            Err(_) => {
                write_log("Erreur lors de la création du listener");
                return Err(SERVER_ERROR);
            }
        };

        Ok(Self {
            config,
            listener,
            clients: Vec::new(),
            threads: Arc::new(Mutex::new(Vec::new())),
            server_thread: None,
            running: Arc::new(AtomicBool::new(false)),
        })
    }

    // Start the server in its own thread
    pub fn start(&mut self) -> Result<(), u32> {
        if self.server_thread.is_some() {
            write_log("Serveur déjà démarré");
            return Ok(());
        }

        self.running.store(true, Ordering::Relaxed);
        
        // Cloner les données nécessaires pour le thread
        let config = self.config.clone();
        let threads = self.threads.clone();
        let running = self.running.clone();
        
        // Créer un nouveau listener pour le thread (car on ne peut pas déplacer self.listener)
        let bind_addr = format!("{}:{}", config.ip_addr, config.port);
        let listener = match TcpListener::bind(&bind_addr) {
            Ok(listener) => listener,
            Err(_) => {
                write_log("Erreur lors de la création du listener pour le thread");
                return Err(SERVER_ERROR);
            }
        };

        let server_thread = thread::spawn(move || {
            write_log(&format!("Serveur démarré sur {}:{}", config.ip_addr, config.port));
            
            for stream in listener.incoming() {
                if !running.load(Ordering::Relaxed) {
                    write_log("Arrêt du serveur demandé");
                    break;
                }

                match stream {
                    Ok(stream) => {
                        write_log("Nouvelle connexion établie");
                        
                        // Vérifier le nombre maximum de threads
                        let current_thread_count = {
                            let threads_guard = threads.lock().unwrap();
                            threads_guard.len()
                        };
                        
                        if current_thread_count >= config.max_threads as usize {
                            write_log("Nombre maximum de threads atteint");
                            continue;
                        }
                        
                        // Cloner la configuration pour le thread client
                        let config_clone = config.clone();
                        let threads_clone = threads.clone();
                        
                        // Gérer la connexion dans un thread séparé
                        let handle = thread::spawn(move || {
                            Server::handle_client(stream, config_clone);
                            
                            // Nettoyer le thread de la liste (optionnel)
                            // Note: Dans une vraie implémentation, vous pourriez vouloir
                            // un mécanisme plus sophistiqué pour nettoyer les threads terminés
                        });
                        
                        // Ajouter le handle à la liste des threads
                        if let Ok(mut threads_guard) = threads.lock() {
                            threads_guard.push(handle);
                        }
                    }
                    Err(e) => {
                        write_log(&format!("Erreur de connexion: {}", e));
                        // Ne pas retourner d'erreur ici, continuer à écouter
                    }
                }
            }
            
            write_log("Thread serveur principal terminé");
        });

        self.server_thread = Some(server_thread);
        write_log("Thread serveur démarré avec succès");
        
        Ok(())
    }
    
    fn handle_client(mut stream: TcpStream, config: ServerConfig) {
        use std::io::{Read, Write};
        
        let mut buffer = vec![0; config.max_buffer_size as usize];
        
        loop {
            match stream.read(&mut buffer) {
                Ok(0) => {
                    write_log("Client déconnecté");
                    break;
                }
                Ok(bytes_read) => {
                    write_log(&format!("Reçu {} bytes du client", bytes_read));
                    
                    // Traiter le message avec le gestionnaire de réseau
                    let result = handle_incoming_message(&buffer, bytes_read);
                    
                    if result != SERVER_OK {
                        write_log(&format!("Erreur traitement message: 0x{:08X}", result));
                    }
                    
                    // Créer une réponse simple
                    let response = match result {
                        SERVER_OK => "HTTP/1.1 200 OK\r\n\r\nMessage traite avec succes".as_bytes(),
                        _ => "HTTP/1.1 400 Bad Request\r\n\r\nErreur dans le traitement du message".as_bytes(),
                    };
                    
                    if let Err(e) = stream.write_all(response) {
                        write_log(&format!("Erreur d'écriture de la réponse: {}", e));
                        break;
                    }
                    
                    if let Err(e) = stream.flush() {
                        write_log(&format!("Erreur de flush: {}", e));
                        break;
                    }
                }
                Err(e) => {
                    write_log(&format!("Erreur de lecture: {}", e));
                    break;
                }
            }
        }
    }

    pub fn stop(&mut self) -> u32 {
        write_log("Arrêt du serveur...");
        
        // Signaler l'arrêt
        self.running.store(false, Ordering::Relaxed);
        
        // Attendre que le thread principal se termine
        if let Some(handle) = self.server_thread.take() {
            if let Err(_) = handle.join() {
                write_log("Erreur lors de l'arrêt du thread serveur principal");
                return SERVER_THREAD_ERROR;
            }
        }
        
        // Attendre que tous les threads clients se terminent
        if let Ok(mut threads_guard) = self.threads.lock() {
            while let Some(handle) = threads_guard.pop() {
                if let Err(_) = handle.join() {
                    write_log("Erreur lors de l'arrêt d'un thread client");
                    return SERVER_THREAD_ERROR;
                }
            }
        }
        
        write_log("Serveur arrêté");
        SERVER_OK
    }

    pub fn is_running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.stop();
    }
}
