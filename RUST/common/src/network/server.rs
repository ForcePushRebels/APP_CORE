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

/// Server configuration
#[derive(Clone)]
pub struct ServerConfig {
    pub port: u16,
    pub max_clients: u32,
    pub max_connections: u32,
    pub thread_pool_size: u32,  // Replaces max_threads
    pub max_buffer_size: u32,
    pub max_packet_size: u32,
    pub ip_addr: String,
    pub socket_timeout_ms: u64,  // New: socket timeout
    pub shutdown_timeout_ms: u64, // New: shutdown timeout
}

impl ServerConfig {
    /// Creates a new server configuration with default values
    pub fn new(ip_addr: String, port: u16) -> Self {
        Self {
            port,
            max_clients: 10,
            max_connections: 100,
            thread_pool_size: 4,  // Fixed thread pool
            max_buffer_size: 4096,
            max_packet_size: 1024,
            ip_addr,
            socket_timeout_ms: 30000,  // 30 seconds
            shutdown_timeout_ms: 5000,  // 5 seconds for shutdown
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

    // Handle poisoned mutexes
    fn handle_poison_lock<T>(result: Result<T, PoisonError<T>>) -> Option<T> {
        match result {
            Ok(guard) => Some(guard),
            Err(poisoned) => {
                write_log("Poisoned mutex detected, recovering guard");
                Some(poisoned.into_inner())
            }
        }
    }

    // Configure timeouts on a socket
    fn configure_socket_timeouts(stream: &TcpStream, timeout_ms: u64) -> Result<(), u32> {
        let timeout = Duration::from_millis(timeout_ms);
        
        if let Err(_) = stream.set_read_timeout(Some(timeout)) {
            write_log("Error configuring read timeout");
            return Err(SERVER_ERROR);
        }
        
        if let Err(_) = stream.set_write_timeout(Some(timeout)) {
            write_log("Error configuring write timeout");
            return Err(SERVER_ERROR);
        }
        
        Ok(())
    }

    // Create worker thread pool
    fn create_worker_pool(&mut self, job_receiver: mpsc::Receiver<ClientJob>) {
        let job_receiver = Arc::new(Mutex::new(job_receiver));
        
        for worker_id in 0..self.config.thread_pool_size {
            let receiver = job_receiver.clone();
            let running = self.running.clone();
            
            let handle = thread::spawn(move || {
                write_log(&format!("Worker {} started", worker_id));
                
                while running.load(Ordering::Relaxed) {
                    // Use try_recv with small delay to check running status
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
                                    write_log(&format!("Worker {}: channel closed", worker_id));
                                    break;
                                }
                            }
                        } else {
                            write_log(&format!("Worker {}: unable to lock receiver", worker_id));
                            thread::sleep(Duration::from_millis(100));
                            continue;
                        }
                    };
                    
                    if let Some(job) = job {
                        write_log(&format!("Worker {} handling connection", worker_id));
                        Server::handle_client(job.stream, job.config);
                    }
                }
                
                write_log(&format!("Worker {} finished", worker_id));
            });
            
            self.worker_threads.push(handle);
        }
    }

    // Start the server in its own thread
    pub fn start(&mut self) -> Result<(), u32> {
        if self.server_thread.is_some() {
            write_log("Server already started");
            return Ok(());
        }

        // Create listener
        let bind_addr = format!("{}:{}", self.config.ip_addr, self.config.port);
        let listener = match TcpListener::bind(&bind_addr) {
            Ok(listener) => Arc::new(listener),
            Err(_) => {
                write_log("Error creating listener");
                return Err(SERVER_ERROR);
            }
        };

        // Create job channel
        let (job_sender, job_receiver) = mpsc::channel();
        self.job_sender = Some(job_sender.clone());

        self.running.store(true, Ordering::Relaxed);
        
        // Create worker pool
        self.create_worker_pool(job_receiver);
        
        // Clone necessary data for main thread
        let config = self.config.clone();
        let running = self.running.clone();
        let listener_clone = listener.clone();
        self.listener = Some(listener);
        
        // Main connection acceptance thread
        self.server_thread = Some(thread::spawn(move || {
            write_log(&format!("Server started on {}:{}", config.ip_addr, config.port));
            
            // Configure listener to be non-blocking with timeout
            if let Err(_) = listener_clone.set_nonblocking(false) {
                write_log("Error configuring listener");
                return;
            }

            for stream in listener_clone.incoming() {
                if !running.load(Ordering::Relaxed) {
                    write_log("Server shutdown requested");
                    break;
                }

                match stream {
                    Ok(stream) => {
                        write_log("New connection established");
                        
                        // Configure socket timeouts
                        if let Err(_) = Server::configure_socket_timeouts(&stream, config.socket_timeout_ms) {
                            write_log("Error configuring socket timeouts");
                            continue;
                        }
                        
                        // Send job to worker pool
                        let job = ClientJob {
                            stream,
                            config: config.clone(),
                        };
                        
                        if let Err(_) = job_sender.send(job) {
                            write_log("Error sending job to workers");
                            break;
                        }
                    }
                    Err(e) => {
                        if running.load(Ordering::Relaxed) {
                            write_log(&format!("Connection error: {}", e));
                        }
                        // Don't return error here, continue listening
                    }
                }
            }
            
            write_log("Main server thread finished");
        }));

        write_log("Server started successfully");
        Ok(())
    }
    
    fn handle_client(mut stream: TcpStream, config: ServerConfig) {
        use std::io::{Read, Write};
        
        let mut buffer = vec![0; config.max_buffer_size as usize];

        loop {
            match stream.read(&mut buffer) {
                Ok(0) => {
                    write_log("Client disconnected");
                    break;
                }
                Ok(bytes_read) => {
                    write_log(&format!("Received {} bytes from client", bytes_read));
                    
                    // Process message with network handler
                    let result = handle_incoming_message(&buffer, bytes_read);
                    
                    if result.status_code != SERVER_OK {
                        write_log(&format!("Message processing error: 0x{:08X}", result.status_code));
                    }
                    
                    // Create simple response
                    let response = match result.status_code {
                        SERVER_OK => {
                            if let Some(response_msg) = result.response_message {
                                // Convert response message to bytes and send
                                response_msg.convert_to_bytes()
                            } else {
                                "HTTP/1.1 200 OK\r\n\r\nMessage processed successfully".as_bytes().to_vec()
                            }
                        },
                        _ => "HTTP/1.1 400 Bad Request\r\n\r\nError processing message".as_bytes().to_vec(),
                    };
                    
                    if let Err(e) = stream.write_all(&response) {
                        write_log(&format!("Response write error: {}", e));
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
        write_log("Stopping server...");
        
        // Signal shutdown
        self.running.store(false, Ordering::Relaxed);
        
        // Close job channel to signal workers to stop
        self.job_sender.take();
        
        // Explicitly close listener to unblock accept()
        if let Some(_listener) = &self.listener {
            // We can't close TcpListener directly, but we can
            // use a dummy socket to unblock accept()
            let dummy_addr = format!("{}:{}", self.config.ip_addr, self.config.port);
            if let Ok(_) = std::net::TcpStream::connect(&dummy_addr) {
                write_log("Shutdown signal sent to listener");
            }
        }
        
        // Wait for main thread to finish with timeout
        if let Some(handle) = self.server_thread.take() {
            let timeout = Duration::from_millis(self.config.shutdown_timeout_ms);
            
            // Simulate join with timeout using auxiliary thread
            let (sender, receiver) = mpsc::channel();
            thread::spawn(move || {
                let result = handle.join();
                let _ = sender.send(result);
            });
            
            match receiver.recv_timeout(timeout) {
                Ok(Ok(())) => {
                    write_log("Main server thread stopped");
                }
                Ok(Err(_)) => {
                    write_log("Error stopping main server thread");
                    return SERVER_THREAD_ERROR;
                }
                Err(_) => {
                    write_log("Timeout stopping main server thread");
                    return SERVER_SHUTDOWN_ERROR;
                }
            }
        }
        
        // Wait for all workers to finish
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
                    write_log(&format!("Worker {} stopped", i));
                }
                Ok(Err(_)) => {
                    write_log(&format!("Error stopping worker {}", i));
                    failed_workers += 1;
                }
                Err(_) => {
                    write_log(&format!("Timeout stopping worker {}", i));
                    failed_workers += 1;
                }
            }
        }
        
        if failed_workers > 0 {
            write_log(&format!("{} workers could not be stopped cleanly", failed_workers));
            return SERVER_THREAD_ERROR;
        }
        
        write_log("Server stopped successfully");
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
