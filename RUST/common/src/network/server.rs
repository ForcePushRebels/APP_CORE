////////////////////////////////////////////////////////////
//  Server source file
//  Defines server configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
// Updated: Added thread pool, timeouts, poison handling, proper shutdown
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::x_assert::x_assert;
use crate::network::handle_network::handle_incoming_message;

use std::net::{TcpListener, TcpStream, Shutdown};
use std::thread::JoinHandle;
use std::thread;
use std::sync::{Arc, Mutex, mpsc, PoisonError};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

///error code encoded in 32 bits
pub const SERVER_OK: u32 = 0xE3452100;
pub const SERVER_ERROR: u32 = 0xE3452101;
pub const SERVER_NOT_INIT: u32 = 0xE3452102;
pub const SERVER_MUTEX_ERROR: u32 = 0xE3452103;
pub const SERVER_THREAD_ERROR: u32 = 0xE3452104;
pub const SERVER_BUFFER_ERROR: u32 = 0xE3452105;
pub const SERVER_SHUTDOWN_ERROR: u32 = 0xE3452106;

/// Configuration du serveur
#[derive(Clone)]
pub struct ServerConfig {
    pub port: u16,
    pub max_clients: u32,
    pub max_connections: u32,
    pub thread_pool_size: u32,  // Remplace max_threads
    pub max_buffer_size: u32,
    pub max_packet_size: u32,
    pub ip_addr: String,
    pub socket_timeout_ms: u64,  // Nouveau: timeout des sockets
    pub shutdown_timeout_ms: u64, // Nouveau: timeout pour l'arrêt
}

impl ServerConfig {
    /// Crée une nouvelle configuration de serveur avec des valeurs par défaut
    pub fn new(ip_addr: String, port: u16) -> Self {
        Self {
            port,
            max_clients: 10,
            max_connections: 100,
            thread_pool_size: 4,  // Pool fixe de threads
            max_buffer_size: 4096,
            max_packet_size: 1024,
            ip_addr,
            socket_timeout_ms: 30000,  // 30 secondes
            shutdown_timeout_ms: 5000,  // 5 secondes pour l'arrêt
        }
    }
}

// Structure pour les jobs du pool de threads
struct ClientJob {
    stream: TcpStream,
    config: ServerConfig,
}

pub struct Server {
    pub config: ServerConfig,
    pub listener: Option<Arc<TcpListener>>,  // Arc pour pouvoir le partager
    pub worker_threads: Vec<JoinHandle<()>>,  // Pool fixe de workers
    pub server_thread: Option<JoinHandle<()>>,
    pub running: Arc<AtomicBool>,
    job_sender: Option<mpsc::Sender<ClientJob>>,
}

impl Server {
    pub fn new(config: ServerConfig) -> Result<Self, u32> {
        x_assert(config.port > 0);
        x_assert(config.max_clients > 0);
        x_assert(config.max_connections > 0);
        x_assert(config.thread_pool_size > 0);
        x_assert(config.max_buffer_size > 0);
        x_assert(config.max_packet_size > 0);
        x_assert(!config.ip_addr.is_empty());

        Ok(Self {
            config,
            listener: None,
            worker_threads: Vec::new(),
            job_sender: None,
            server_thread: None,
            running: Arc::new(AtomicBool::new(false)),
        })
    }

    // Gestion des mutex empoisonnés
    fn handle_poison_lock<T>(result: Result<T, PoisonError<T>>) -> Option<T> {
        match result {
            Ok(guard) => Some(guard),
            Err(poisoned) => {
                write_log("Mutex empoisonné détecté, récupération du guard");
                Some(poisoned.into_inner())
            }
        }
    }

    // Configure les timeouts sur un socket
    fn configure_socket_timeouts(stream: &TcpStream, timeout_ms: u64) -> Result<(), u32> {
        let timeout = Duration::from_millis(timeout_ms);
        
        if let Err(_) = stream.set_read_timeout(Some(timeout)) {
            write_log("Erreur lors de la configuration du timeout de lecture");
            return Err(SERVER_ERROR);
        }
        
        if let Err(_) = stream.set_write_timeout(Some(timeout)) {
            write_log("Erreur lors de la configuration du timeout d'écriture");
            return Err(SERVER_ERROR);
        }
        
        Ok(())
    }

    // Crée le pool de threads workers
    fn create_worker_pool(&mut self, job_receiver: mpsc::Receiver<ClientJob>) {
        let job_receiver = Arc::new(Mutex::new(job_receiver));
        
        for worker_id in 0..self.config.thread_pool_size {
            let receiver = job_receiver.clone();
            let running = self.running.clone();
            
            let handle = thread::spawn(move || {
                write_log(&format!("Worker {} démarré", worker_id));
                
                while running.load(Ordering::Relaxed) {
                    // Utiliser try_recv avec un petit délai pour pouvoir vérifier running
                    let job = {
                        let guard_opt = Self::handle_poison_lock(receiver.lock());
                        if let Some(guard) = guard_opt {
                            match guard.try_recv() {
                                Ok(job) => Some(job),
                                Err(mpsc::TryRecvError::Empty) => {
                                    drop(guard);
                                    thread::sleep(Duration::from_millis(10));
                                    continue;
                                }
                                Err(mpsc::TryRecvError::Disconnected) => {
                                    write_log(&format!("Worker {} : canal fermé", worker_id));
                                    break;
                                }
                            }
                        } else {
                            write_log(&format!("Worker {} : impossible de verrouiller le récepteur", worker_id));
                            thread::sleep(Duration::from_millis(100));
                            continue;
                        }
                    };
                    
                    if let Some(job) = job {
                        write_log(&format!("Worker {} traite une connexion", worker_id));
                        Server::handle_client(job.stream, job.config);
                    }
                }
                
                write_log(&format!("Worker {} terminé", worker_id));
            });
            
            self.worker_threads.push(handle);
        }
    }

    // Start the server in its own thread
    pub fn start(&mut self) -> Result<(), u32> {
        if self.server_thread.is_some() {
            write_log("Serveur déjà démarré");
            return Ok(());
        }

        // Créer le listener
        let bind_addr = format!("{}:{}", self.config.ip_addr, self.config.port);
        let listener = match TcpListener::bind(&bind_addr) {
            Ok(listener) => Arc::new(listener),
            Err(_) => {
                write_log("Erreur lors de la création du listener");
                return Err(SERVER_ERROR);
            }
        };

        // Créer le canal pour les jobs
        let (job_sender, job_receiver) = mpsc::channel();
        self.job_sender = Some(job_sender.clone());

        self.running.store(true, Ordering::Relaxed);
        
        // Créer le pool de workers
        self.create_worker_pool(job_receiver);
        
        // Cloner les données nécessaires pour le thread principal
        let config = self.config.clone();
        let running = self.running.clone();
        let listener_clone = listener.clone();
        self.listener = Some(listener);
        
        // Thread principal d'acceptation des connexions
        self.server_thread = Some(thread::spawn(move || {
            write_log(&format!("Serveur démarré sur {}:{}", config.ip_addr, config.port));
            
            // Configurer le listener pour être non-bloquant avec timeout
            if let Err(_) = listener_clone.set_nonblocking(false) {
                write_log("Erreur lors de la configuration du listener");
                return;
            }

            for stream in listener_clone.incoming() {
                if !running.load(Ordering::Relaxed) {
                    write_log("Arrêt du serveur demandé");
                    break;
                }

                match stream {
                    Ok(stream) => {
                        write_log("Nouvelle connexion établie");
                        
                        // Configurer les timeouts du socket
                        if let Err(_) = Server::configure_socket_timeouts(&stream, config.socket_timeout_ms) {
                            write_log("Erreur configuration timeouts socket");
                            continue;
                        }
                        
                        // Envoyer le job au pool de workers
                        let job = ClientJob {
                            stream,
                            config: config.clone(),
                        };
                        
                        if let Err(_) = job_sender.send(job) {
                            write_log("Erreur envoi job aux workers");
                            break;
                        }
                    }
                    Err(e) => {
                        if running.load(Ordering::Relaxed) {
                            write_log(&format!("Erreur de connexion: {}", e));
                        }
                        // Ne pas retourner d'erreur ici, continuer à écouter
                    }
                }
            }
            
            write_log("Thread serveur principal terminé");
        }));

        write_log("Serveur démarré avec succès");
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
                    
                    if result.status_code != SERVER_OK {
                        write_log(&format!("Erreur traitement message: 0x{:08X}", result.status_code));
                    }
                    
                    // Créer une réponse simple
                    let response = match result.status_code {
                        SERVER_OK => {
                            if let Some(response_msg) = result.response_message {
                                // Convertir le message de réponse en bytes et l'envoyer
                                response_msg.convert_to_bytes()
                            } else {
                                "HTTP/1.1 200 OK\r\n\r\nMessage traite avec succes".as_bytes().to_vec()
                            }
                        },
                        _ => "HTTP/1.1 400 Bad Request\r\n\r\nErreur dans le traitement du message".as_bytes().to_vec(),
                    };
                    
                    if let Err(e) = stream.write_all(&response) {
                        write_log(&format!("Erreur d'écriture de la réponse: {}", e));
                        break;
                    }
                    
                    if let Err(e) = stream.flush() {
                        write_log(&format!("Erreur de flush: {}", e));
                        break;
                    }
                }
                Err(e) => {
                    // Différencier timeout vs vraie erreur
                    match e.kind() {
                        std::io::ErrorKind::TimedOut => {
                            write_log("Timeout de lecture - fermeture connexion");
                        }
                        std::io::ErrorKind::WouldBlock => {
                            continue; // Réessayer
                        }
                        _ => {
                            write_log(&format!("Erreur de lecture: {}", e));
                        }
                    }
                    break;
                }
            }
        }
        
        // Fermeture propre du socket
        if let Err(e) = stream.shutdown(Shutdown::Both) {
            write_log(&format!("Erreur lors de la fermeture du socket: {}", e));
        }
    }

    pub fn stop(&mut self) -> u32 {
        write_log("Arrêt du serveur...");
        
        // Signaler l'arrêt
        self.running.store(false, Ordering::Relaxed);
        
        // Fermer le canal des jobs pour signaler aux workers de s'arrêter
        self.job_sender.take();
        
        // Fermer explicitement le listener pour débloquer accept()
        if let Some(_listener) = &self.listener {
            // On ne peut pas fermer directement le TcpListener, mais on peut
            // utiliser un socket factice pour débloquer accept()
            let dummy_addr = format!("{}:{}", self.config.ip_addr, self.config.port);
            if let Ok(_) = std::net::TcpStream::connect(&dummy_addr) {
                write_log("Signal d'arrêt envoyé au listener");
            }
        }
        
        // Attendre que le thread principal se termine avec timeout
        if let Some(handle) = self.server_thread.take() {
            let timeout = Duration::from_millis(self.config.shutdown_timeout_ms);
            
            // Simuler un join avec timeout en utilisant un thread auxiliaire
            let (sender, receiver) = mpsc::channel();
            thread::spawn(move || {
                let result = handle.join();
                let _ = sender.send(result);
            });
            
            match receiver.recv_timeout(timeout) {
                Ok(Ok(())) => {
                    write_log("Thread serveur principal arrêté");
                }
                Ok(Err(_)) => {
                    write_log("Erreur lors de l'arrêt du thread serveur principal");
                    return SERVER_THREAD_ERROR;
                }
                Err(_) => {
                    write_log("Timeout lors de l'arrêt du thread serveur principal");
                    return SERVER_SHUTDOWN_ERROR;
                }
            }
        }
        
        // Attendre que tous les workers se terminent
        let mut failed_workers = 0;
        for (i, handle) in self.worker_threads.drain(..).enumerate() {
            let timeout = Duration::from_millis(self.config.shutdown_timeout_ms / self.config.thread_pool_size as u64);
            
            let (sender, receiver) = mpsc::channel();
            thread::spawn(move || {
                let result = handle.join();
                let _ = sender.send(result);
            });
            
            match receiver.recv_timeout(timeout) {
                Ok(Ok(())) => {
                    write_log(&format!("Worker {} arrêté", i));
                }
                Ok(Err(_)) => {
                    write_log(&format!("Erreur lors de l'arrêt du worker {}", i));
                    failed_workers += 1;
                }
                Err(_) => {
                    write_log(&format!("Timeout lors de l'arrêt du worker {}", i));
                    failed_workers += 1;
                }
            }
        }
        
        if failed_workers > 0 {
            write_log(&format!("{} workers n'ont pas pu être arrêtés proprement", failed_workers));
            return SERVER_THREAD_ERROR;
        }
        
        write_log("Serveur arrêté avec succès");
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
