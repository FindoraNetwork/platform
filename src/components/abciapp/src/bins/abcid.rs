//!
//! # binary process
//!

use {
    abciapp::abci,
    ruc::*,
    std::{
        sync::{atomic::Ordering, mpsc::channel},
        thread,
    },
};

fn main() {
    globutils::logging::init_logging(None);
    log::info!(concat!(
        "Build: ",
        env!("VERGEN_SHA"),
        " ",
        env!("VERGEN_BUILD_DATE")
    ));

    let thread = thread::spawn(|| pnk!(abci::run()));

    let (tx, rx) = channel();

    pnk!(ctrlc::set_handler(move || {
        println!("Waiting to exit.");
        abci::IS_EXITING.store(true, Ordering::Release);
        while abci::IN_SAFE_ITV.load(Ordering::SeqCst) {
            sleep_ms!(1);
        }
        pnk!(tx.send(()));
    }));

    pnk!(rx.recv());

    println!("Exiting...");
    thread.thread().unpark();
    thread.join().unwrap();
}
