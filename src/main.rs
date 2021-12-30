fn main() {
    if let Some(filename) = std::env::args().nth(1) {
        // match Ok("fn main() {}") {
        //     Ok(contents) => {
        dilaria::run_program("fn main() {}");
        //     }
        //     Err(err) => {
        //         eprintln!("{}", err);
        //     }
        // }
    } else {
        eprintln!("Usage: <filename>")
    }
}
