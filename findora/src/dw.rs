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
  println!("run_debug_window:  active");
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
  println!("run_client:  active");
  if let Err(e) = stream.set_nonblocking(false) {
    println!("run_client:  nonblocking failed:  {}", e);
    return;
  }

  loop {
    let pass = match read_packet(&mut stream) {
      Ok(packet) => process_packet(packet),
      Err(_e) => {
        println!("run_client:  The stream failed.");
        break;
      }
    };

    let buffer = if pass { [0_u8; 1] } else { [1_u8; 1] };

    let result = stream.write(&buffer);

    if let Err(_e) = result {
      break;
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
  let mut buffer = vec![0; size as usize];
  // println!("read_packet:  got {} for the size", size);

  i = 0;

  while i < size as usize {
    let count = stream.read(&mut buffer[i..])?;
    i += count;
    // println!("read_packet:  got {} bytes for {} total", count, i);
  }

  let result: Command = match deserialize(&buffer[..]) {
    Ok(command) => command,
    Err(e) => {
      return Err(Error::new(ErrorKind::Other, e));
    }
  };

  // println!("read_packet:  got a packet");
  Ok(result)
}

fn process_packet(packet: Command) -> bool {
  match packet.contents {
    Contents::Nop => {
      // println!("process_packet:  nop");
      true
    }
    Contents::LoggingFlags(flags) => {
      let pass = crate::set_logging(&flags);
      println!("process_packet:  set_logging -> {}", pass);
      pass
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::LoggingEnableFlags;
  use byteorder::WriteBytesExt;
  use std::net::TcpStream;

  #[test]
  fn test_socket() {
    let mut i = 8192;
    let mut address;

    let found = loop {
      address = "localhost:".to_owned() + &i.to_string();

      match start_socket(address.clone()) {
        Ok(()) => {
          break true;
        }
        Err(_e) => {}
      }

      i += 1;

      if i > 16000 {
        break false;
      }
    };

    assert!(found);
    println!("The debug socket is {}", &address);

    let result = TcpStream::connect(address);

    if let Err(e) = result {
      panic!("connect failed:  {}", e);
    }

    let mut stream = result.unwrap();
    stream.set_nonblocking(false).unwrap();

    let flags = LoggingEnableFlags { name: "test".to_owned(),
                                     log: true,
                                     error: true,
                                     warning: true,
                                     debug: true,
                                     info: true,
                                     modify_log: true,
                                     modify_error: true,
                                     modify_warning: true,
                                     modify_debug: true,
                                     modify_info: true };

    let command = Command { contents: Contents::LoggingFlags(flags) };
    let command = bincode::serialize(&command).unwrap();

    let mut size = vec![];
    size.write_u32::<LittleEndian>(command.len() as u32)
        .unwrap();

    let count = stream.write(&size[..]).unwrap();
    assert!(count == 4);

    let count = stream.write(&command[..]).unwrap();
    assert!(count == command.len());

    let mut buffer: [u8; 1] = [0_u8; 1];

    let count = stream.read(&mut buffer[..]).unwrap();
    assert!(count == buffer.len());
  }
}
