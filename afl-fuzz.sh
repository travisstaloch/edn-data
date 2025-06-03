set -xe

zig build -Dbuild-fuzz-exe

#
# previous compile and launch
#
# afl-clang-lto -o fuzz zig-out/lib/libfuzz.a
# AFL_SKIP_CPUFREQ=true AFL_AUTORESUME=1 afl-fuzz -i afl/input -o afl/output -- ./fuzz

AFL_AUTORESUME=1 afl-fuzz -t20 -i afl/input -o afl/output2 -- ./zig-out/bin/fuzz-afl
