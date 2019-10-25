use crate::LoggingEnableFlags;

use bincode::deserialize;
use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use serde_derive::Deserialize;
use serde_derive::Serialize;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Write;
use std::net::Shutdown;
use std::net::TcpListener;
use std::net::TcpStream;
use std::thread::spawn;

/// Start a thread to manage a socket for debug window requests.
pub fn start_socket(address: String) -> Result<(), Error> {
  let socket = TcpListener::bind(&address)?;
  spawn(move || {
    run_debug_window(socket);
  });
  Ok(())
}

// Run a given server socket.  Multiple simultaneous connections
// are allowed.
fn run_debug_window(socket: TcpListener) {
  println!("run_debug_window:  active");
  let mut errors = 0;

  // Wait for incoming connections.
  for incoming in socket.incoming() {
    match incoming {
      Ok(stream) => {
        // Spawn another thread to handle this new socket.
        spawn(move || {
          run_client(stream);
        });
        errors = 0;
      }
      Err(_e) => {
        errors += 1; /* error!(DebugWindow, "{}", e); */
      }
    }

    // Quit if the error rate seems too high.
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
  pub contents: Contents,
}

// Run a single connection to the ledger process.
fn run_client(mut stream: TcpStream) {
  println!("run_client:  active");
  if let Err(e) = stream.set_nonblocking(false) {
    println!("run_client:  nonblocking failed:  {}", e);
    return;
  }

  // Loop reading and processing commands.
  loop {
    let pass = match read_packet(&mut stream) {
      Ok(packet) => process_packet(packet),
      Err(_e) => {
        println!("run_client:  The stream failed.");
        break;
      }
    };

    // Write a one-byte response indicating whether or
    // not the command encountered an error.
    let buffer = if pass { [0_u8; 1] } else { [1_u8; 1] };

    let result = stream.write(&buffer);

    if let Err(_e) = result {
      break;
    }
  }

  let _ = stream.shutdown(Shutdown::Both);
}

// Read a single packet from the socket.  The packet proper is
// preceded by a 4-byte length.  This routine returns a
// deserialized Command structure if it is successful.
fn read_packet(stream: &mut TcpStream) -> Result<Command, Error> {
  // Read the length.
  let mut buffer: [u8; 4] = [0, 0, 0, 0];
  read_all(stream, &mut buffer[..])?;

  // Read the command packet proper.
  let mut slice = &buffer[0..];
  let size = slice.read_u32::<LittleEndian>()?;
  let mut buffer = vec![0; size as usize];
  // println!("read_packet:  got {} for the size", size);
  read_all(stream, &mut buffer)?;

  let result: Command = match deserialize(&buffer[..]) {
    Ok(command) => command,
    Err(e) => {
      return Err(Error::new(ErrorKind::Other, e));
    }
  };

  // println!("read_packet:  got a packet");
  Ok(result)
}

// Read a full buffer.  Whether stream.read will block until all the bytes
// are present is unclear.
fn read_all(stream: &mut TcpStream, slice: &mut [u8]) -> Result<(), Error> {
  let total = slice.len();
  let mut i = 0;

  while i < total {
    let count = stream.read(&mut slice[i..])?;
    i += count;
  }

  Ok(())
}

// Process a packet.
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

pub fn write_packet(stream: &mut TcpStream, command: &Command) -> Result<(), Error> {
  let command = bincode::serialize(&command).unwrap();

  let mut size = vec![];
  size.write_u32::<LittleEndian>(command.len() as u32)
      .unwrap();

  write_all(stream, &size[..])?;
  write_all(stream, &command[..])?;
  Ok(())
}

// Write all the bytes in a given slice.  Whether stream.write
// waits until all the bytes can be queued is unclear.
fn write_all(stream: &mut TcpStream, slice: &[u8]) -> Result<(), Error> {
  let total = slice.len();
  let mut i = 0;

  while i < total {
    let count = stream.write(&slice[i..])?;
    i += count;
  }

  Ok(())
}

pub fn get_test_socket() -> TcpStream {
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

  result.unwrap()
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::LoggingEnableFlags;

  #[test]
  fn test_socket() {
    let mut stream = get_test_socket();
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
    write_packet(&mut stream, &command).unwrap();

    // Read the one-byte response from the server side.  A
    // value of zero implies success, and that's what this
    // test should get.
    let mut buffer: [u8; 1] = [0_u8; 1];
    let count = stream.read(&mut buffer[..]).unwrap();

    assert!(count == buffer.len());
    assert!(buffer[0] == 0);

    // Try an invalid category name.

    let flags = LoggingEnableFlags { name: "no-test".to_owned(),
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
    write_packet(&mut stream, &command).unwrap();

    // Okay, read the response.  It should indicate failure, which
    // means it should not be zero.
    let mut buffer: [u8; 1] = [0_u8; 1];
    let count = stream.read(&mut buffer[..]).unwrap();

    assert!(count == buffer.len());
    assert!(buffer[0] != 0);
  }
}
