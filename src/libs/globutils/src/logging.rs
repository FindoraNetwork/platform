//!
//! log system
//!

use {
    std::env, std::io, tracing_subscriber::filter::LevelFilter,
    tracing_subscriber::EnvFilter,
};

#[allow(missing_docs)]
pub fn init_logging(verbose: Option<&str>) {
    let mut env_filter =
        EnvFilter::new("actix_web=warn,actix_server=warn,actix_http=warn,rpc=warn");
    if let Some(module) = verbose {
        if module.is_empty() {
            env_filter = env_filter.add_directive(LevelFilter::DEBUG.into());
        } else {
            env_filter =
                env_filter.add_directive(format!("{module}=debug").parse().unwrap());
        }
    }

    if let Ok(rust_log) = env::var("RUST_LOG") {
        if !rust_log.is_empty() {
            for directive in rust_log.split(',').filter_map(|s| match s.parse() {
                Ok(directive) => Some(directive),
                Err(err) => {
                    eprintln!("Ignoring directive `{s}`: {err}");
                    None
                }
            }) {
                env_filter = env_filter.add_directive(directive);
            }
        }
    } else {
        env_filter = env_filter.add_directive(LevelFilter::ERROR.into());
    }

    tracing_subscriber::fmt::Subscriber::builder()
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
        .with_env_filter(env_filter)
        .with_writer(io::stderr)
        .init();
}
