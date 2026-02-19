use std::net::TcpStream;

enum ClientState {
    PreConnection(TcpStream),
}

enum ServerState {
    PreConnection(TcpStream),
}
