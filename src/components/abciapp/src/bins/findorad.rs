use abciapp::abci;

fn main() {
    env_logger::init();
    log::info!(concat!(
        "Build: ",
        env!("VERGEN_SHA"),
        " ",
        env!("VERGEN_BUILD_DATE")
    ));

    abci::run().unwrap();
}
