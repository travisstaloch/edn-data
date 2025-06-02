zig build

afl-clang-lto -o fuzz zig-out/lib/libfuzz.a

AFL_SKIP_CPUFREQ=true AFL_AUTORESUME=1 afl-fuzz -i afl/input -o afl/output -- ./fuzz

# find -wholename "./afl/output/default/crashes/*" -exec sh -c "echo {}; zig-out/bin/edn-parse {}" \;