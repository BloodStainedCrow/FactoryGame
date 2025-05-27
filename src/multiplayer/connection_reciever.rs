use std::{
    net::{TcpListener, TcpStream},
    sync::Arc,
    thread,
};

use parking_lot::Mutex;

pub type ConnectionList = Arc<Mutex<Vec<TcpStream>>>;

pub fn accept_continously(connections: ConnectionList) -> Result<(), std::io::Error> {
    let listener = TcpListener::bind("127.0.0.1:8080")?;
    thread::spawn(move || {
        for conn in listener.incoming() {
            match conn {
                Ok(conn) => connections.lock().push(conn),
                Err(_) => todo!("Handle errors"),
            }
        }
    });
    Ok(())
}
