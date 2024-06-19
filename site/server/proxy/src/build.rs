/*
* This file is used to generate the Rust code from the proto file.
* *Note*: For some reason, the generator does not work when the proto file is not in the same directory as the build.rs file.
* So, we create a symlink to the proto file in the src directory.
*/

use std::io::Result;
use std::os::unix::fs::{symlink};


fn main() -> Result<()> {
    let current_dir = std::env::current_dir().unwrap();
    let proto_file_path = current_dir.parent().unwrap().join("protos").join("interpreter.proto");
    
    symlink(proto_file_path, current_dir.join("src").join("interpreter.proto"))?;

    tonic_build::configure()
        .build_server(false)
        .compile(&["src/interpreter.proto"], &["src"])?;

    std::fs::remove_file(current_dir.join("src").join("interpreter.proto"))?;
    Ok(())
}