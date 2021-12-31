#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    dilaria::_fuzz_parse(&data);
});
