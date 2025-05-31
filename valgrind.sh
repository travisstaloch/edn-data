zig build --release=fast

valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes zig-out/bin/edn-bench /tmp/twitter.edn

kcachegrind &