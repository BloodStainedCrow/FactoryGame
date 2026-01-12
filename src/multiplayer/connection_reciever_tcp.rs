use std::{
    net::{TcpListener, TcpStream, ToSocketAddrs},
    sync::{Arc, atomic::AtomicBool, mpsc::Sender},
    thread,
};

use parking_lot::Mutex;

pub type ConnectionList = Arc<Mutex<Vec<TcpStream>>>;

pub fn accept_continously(
    local_addr: impl ToSocketAddrs,
    new_connection: Sender<TcpStream>,
    cancel: Arc<AtomicBool>,
) -> Result<(), std::io::Error> {
    let listener = TcpListener::bind(local_addr)?;
    thread::spawn(move || {
        for conn in listener.incoming() {
            match conn {
                Ok(conn) => {
                    log::info!("Got new connection");
                    if cancel.load(std::sync::atomic::Ordering::SeqCst) {
                        return;
                    } else {
                        conn.set_nonblocking(true)
                            .expect("Setting connectiong to nonblocking failed");
                        new_connection
                            .send(conn)
                            .expect("Sending via channel failed");
                    }
                },
                Err(err) => match err.kind() {
                    std::io::ErrorKind::WouldBlock => todo!(),
                    _ => todo!(),
                },
            }
        }
    });

    Ok(())
}
