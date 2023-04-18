use parking_lot::Mutex;
use std::{fs, path::Path};

const MAX_USERS: u8 = 127u8;
static PROFILER_GUARD: Mutex<Option<(pprof::ProfilerGuard<'static>, u8)>> =
    Mutex::new(None);

pub fn start_profiler() -> bool {
    let mut guard = PROFILER_GUARD.lock();
    match guard.as_mut() {
        None => *guard = pprof::ProfilerGuard::new(100).map(|p| (p, 1)).ok(),
        Some(guard) if guard.1 < MAX_USERS => guard.1 += 1,
        _ => return false,
    }
    guard.is_some()
}

pub fn gen_flame_graph<P>(path: P) -> bool
where
    P: AsRef<Path>,
{
    let guard = PROFILER_GUARD.lock();
    guard
        .as_ref()
        .and_then(|(guard, _)| match guard.report().build() {
            Ok(report) if !report.data.is_empty() => Some(report),
            _ => None,
        })
        .and_then(|report| fs::File::create(path).map(|file| (report, file)).ok())
        .and_then(|(report, file)| report.flamegraph(file).ok())
        .is_some()
}

pub fn stop_profiler() {
    let mut guard = PROFILER_GUARD.lock();
    if let Some(g) = guard.as_mut() {
        if g.1 == 1 {
            // drop the profiler
            *guard = None;
        } else if g.1 > 1 {
            g.1 -= 1;
        } else {
            panic!("impossible");
        }
    }
}
