use std::fs::OpenOptions;
use std::io::Write;

/// save log information to a file
pub fn add_log(str: String) {
    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open("/root/debug_log")
        .unwrap();

    if let Err(e) = writeln!(file, "{}", str) {
        eprintln!("Couldn't write to file: {}", e);
    }
}
