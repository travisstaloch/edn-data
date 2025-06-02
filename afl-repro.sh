# find -wholename "./afl/output/default/crashes/*" -exec sh -c "echo {}; zig-out/bin/edn-parse {}" \;

set -xe

for i in $(find -wholename "./afl/output/default/crashes/*"); do
  echo $i
  zig build run -- $i
  # zig-out/bin/edn-parse $i
done