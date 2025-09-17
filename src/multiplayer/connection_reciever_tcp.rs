use std::{
    net::{TcpListener, TcpStream, ToSocketAddrs},
    sync::{Arc, atomic::AtomicBool},
    thread,
};

use parking_lot::Mutex;

pub type ConnectionList = Arc<Mutex<Vec<TcpStream>>>;

pub fn accept_continously(
    local_addr: impl ToSocketAddrs,
    connections: ConnectionList,
    cancel: Arc<AtomicBool>,
) -> Result<(), std::io::Error> {
    let listener = TcpListener::bind(local_addr)?;
    thread::spawn(move || {
        for conn in listener.incoming() {
            match conn {
                Ok(conn) => {
                    if cancel.load(std::sync::atomic::Ordering::SeqCst) {
                        return;
                    } else {
                        conn.set_nonblocking(true)
                            .expect("Setting connectiong to nonblocking failed");
                        connections.lock().push(conn);
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
