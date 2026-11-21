use std::{env, fs::OpenOptions, io::Write, os::unix::fs::OpenOptionsExt};
use tempo_consensus_config::{SigningKey, SigningKeyPassphrase};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args_os().collect();
    if args.len() != 4 {
        return Err("expected encrypted-key seed-output public-key-output".into());
    }
    let key = SigningKey::read_from_file_encrypted(
        &args[1], SigningKeyPassphrase::from("tempo-localnet-signing-key-secret"),
    )?;
    let mut seed = OpenOptions::new().write(true).create_new(true).mode(0o600).open(&args[2])?;
    key.to_writer_unencrypted(&mut seed)?;
    let mut public = OpenOptions::new().write(true).create_new(true).mode(0o600).open(&args[3])?;
    write!(public, "{}", key.public_key())?;
    Ok(())
}
