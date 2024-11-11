# extensible data notation

# TODO

- [x] make Value.Map and Set simple slices instead of ArrayHashMaps
    - [x] get rid of set/map equality dependency on map.getContext()
    - [x] implement Value.eql()
- [ ] parse mode which doesn't allocate, only returning required lengths
- [x] parse mode which ignores whitespace and comments.
  - [x] move whitespace/comments into its own list
- [ ] maybe support parsing at comptime

# references
* https://github.com/edn-format/edn
