#!/bin/bash

set -xe

zig build --release=fast

# zig-out/bin/edn-parse --json-to-edn /tmp/msft.json
# zig-out/bin/edn-parse --json-to-edn /tmp/msft.json > /tmp/msft.edn
# zig-out/bin/edn-parse /tmp/msft.edn

# zig-out/bin/edn-parse --json-to-edn ../simdjzon/test/twitter.json > /tmp/twitter.edn
# zig-out/bin/edn-parse /tmp/twitter.edn > /dev/null

POOP=../poop/zig-out/bin/poop

if false; then
  curl -fsS -o /tmp/msft.json https://microsoftedge.github.io/Demos/json-dummy-data/64KB.json
  zig-out/bin/edn-parse --json-to-edn /tmp/msft.json > /tmp/msft.edn
  $POOP -d 3000 'zig-out/bin/edn-bench --json /tmp/msft.json' 'zig-out/bin/edn-bench /tmp/msft.edn'
fi

if true; then
  curl -fsS -o /tmp/twitter.json https://raw.githubusercontent.com/simdjson/simdjson/master/jsonexamples/twitter.json
  zig-out/bin/edn-parse --json-to-edn /tmp/twitter.json > /tmp/twitter.edn
  $POOP -d 3000 'zig-out/bin/edn-bench --json /tmp/twitter.json' 'zig-out/bin/edn-bench /tmp/twitter.edn'
fi
