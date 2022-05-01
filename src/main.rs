use std::io;

use dilaria::Config;

fn main() {
    let mut args = std::env::args();

    if let Some(filename) = args.nth(1) {
        let mut stdout = io::stdout();

        let mut cfg = Config {
            debug: false,
            step: false,
            parse_only: false,
            stdout: &mut stdout,
        };

        for arg in args {
            match &*arg {
                "--debug" => cfg.debug = true,
                "--step" => cfg.step = true,
                "--parse-only" => cfg.parse_only = true,
                _ => {}
            }
        }

        match std::fs::read_to_string(filename) {
            Ok(contents) => {
                dilaria::run_program(&contents, &mut cfg);
            }
            Err(err) => {
                eprintln!("{}", err);
            }
        }
    } else {
        eprintln!("Usage: <filename>");
    }
}
