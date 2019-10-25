use crate::LoggingEnableFlags;

use bincode::deserialize;
use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use serde_derive::Deserialize;
// use serde::Deserializer;
use serde_derive::Serialize;
// use serde::Serializer;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Write;
use std::net::Shutdown;
use std::net::TcpListener;
use std::net::TcpStream;
use std::thread::spawn;

pub fn start_socket(address: String) -> Result<(), Error> {
  let socket = TcpListener::bind(&address)?;
  spawn(move || {
    run_debug_window(socket);
  });
  Ok(())
}

fn run_debug_window(socket: TcpListener) {
  let mut errors = 0;

  for incoming in socket.incoming() {
    match incoming {
      Ok(stream) => {
        spawn(move || {
          run_client(stream);
        });
        errors = 0;
      }
      Err(_e) => {
        errors += 1; /* error!(DebugWindow, "{}", e); */
      }
    }

    if errors > 10 {
      /* log!(DebugWindow, "incoming() failed too many times."); */
      break;
    }
  }
}

#[derive(Serialize, Deserialize)]
pub enum Contents {
  Nop,
  LoggingFlags(LoggingEnableFlags),
}

#[derive(Serialize, Deserialize)]
pub struct Command {
  contents: Contents,
}

fn run_client(mut stream: TcpStream) {
  loop {
    match read_packet(&mut stream) {
      Ok(packet) => {
        let pass = process_packet(packet);

        let buffer = if pass { [0_u8; 1] } else { [1_u8; 1] };

        let result = stream.write(&buffer);

        if let Err(_e) = result {
          break;
        }
      }
      Err(_e) => {
        break;
      }
    }
  }

  let _ = stream.shutdown(Shutdown::Both);
}

fn read_packet(stream: &mut TcpStream) -> Result<Command, Error> {
  let mut buffer: [u8; 4] = [0, 0, 0, 0];
  let mut i = 0;

  while i < buffer.len() {
    let count = stream.read(&mut buffer[i..])?;
    i += count;
  }

  let mut slice = &buffer[0..];
  let size = slice.read_u32::<LittleEndian>()?;
  let mut buffer = Vec::with_capacity(size as usize);

  i = 0;

  while i < size as usize {
    let count = stream.read(&mut buffer[i..])?;
    i += count;
  }

  let result: Command = match deserialize(&buffer[..]) {
    Ok(command) => command,
    Err(e) => {
      return Err(Error::new(ErrorKind::Other, e));
    }
  };

  Ok(result)
}

fn process_packet(packet: Command) -> bool {
  match packet.contents {
    Contents::Nop => true,
    Contents::LoggingFlags(flags) => crate::set_logging(&flags),
  }
}
