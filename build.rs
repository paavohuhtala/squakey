// https://blog.cyplo.dev/posts/2018/12/generate-rust-tests-from-data/

use std::env;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(&destination).unwrap();

    // write test file header, put `use`, `const` etc there
    write_header(&mut test_file);

    let test_data_directories = read_dir("./tests/data/").unwrap();

    for directory in test_data_directories {
        write_test(&mut test_file, &directory.unwrap());
    }
}

const TEST_TEMPLATE: &'static str = include_str!("./tests/test_template");

fn write_test(test_file: &mut File, directory: &DirEntry) {
    let directory = directory.path().canonicalize().unwrap();
    let path = directory.display();
    let test_name = format!(
        "snapshot_{}",
        directory.file_name().unwrap().to_string_lossy()
    );

    let mut template = TEST_TEMPLATE.to_owned();
    template = template.replace("%NAME%", &test_name);
    template = template.replace("%PATH%", &path.to_string());
    write!(test_file, "{}", template).unwrap();
}

fn write_header(test_file: &mut File) {
    write!(
        test_file,
        r#"
use squakey::{{parser::parse_program, serializer::format_program}};
use pretty_assertions::{{assert_eq}};
"#
    )
    .unwrap();
}
