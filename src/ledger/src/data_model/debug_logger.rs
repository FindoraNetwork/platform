use chrono::prelude::*;
use std::fs::OpenOptions;
use std::io::Write;

/// save log information to a file
pub fn add_log(str: String) {
    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open("/root/debug_log")
        .unwrap();

    let now = Utc::now();
    let date = now.format("%H:%M:%S");

    if let Err(e) = writeln!(file, "{} {}", date, str) {
        eprintln!("Couldn't write to file: {}", e);
    }
}
