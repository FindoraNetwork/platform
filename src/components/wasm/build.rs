fn main() {
    let mut config = vergen::Config::default();
    *config.build_mut().kind_mut() = vergen::TimestampKind::All;
    *config.build_mut().semver_mut() = false;
    *config.git_mut().sha_kind_mut() = vergen::ShaKind::Both;
    vergen::vergen(config).expect("Unable to generate the cargo keys!");
}
