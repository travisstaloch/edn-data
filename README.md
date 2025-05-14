# extensible data notation
an edn parser in zig

### features
* runtime parsing
  * parse arbitrary data with `edn.parseFromSliceAlloc()` or `edn.parseFromSliceBuf()`
  * parse structured data with `edn.parseTypeFromSlice(T)`
* comptime parsing
  * parse arbitrary data with `edn.parseFromSliceComptime()`
  * parse structured data with `comptime edn.parseTypeFromSlice(T)`
* formatting
  * format parse results with `edn.fmtParseResult(result, src)`

### use
depend on edn with the package manager
```console
zig fetch --save git+https://github.com/travisstaloch/edn-format
```
```zig
// build.zig
const edn_dep = b.dependency("extensible_data_notation", .{
    .target = target,
    .optimize = optimize,
});
exe.root_module.addImport("extensible-data-notation", edn_dep.module("extensible-data-notation"));
```
```zig
// main.zig
test "readme" {
    // const edn = @import("extensible-data-notation");
    const src = "a (a b c [1 2 3] {:a 1, :b 2})";
    const result = try edn.parseFromSliceAlloc(std.testing.allocator, src, .{}, .{});
    defer result.deinit(std.testing.allocator);
    try std.testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}
```

### TODO

- [ ] built-in tagged elements
  - [ ] #inst - instant http://www.ietf.org/rfc/rfc3339.txt
  - [ ] #uuid - http://en.wikipedia.org/wiki/Universally_unique_identifier
- [ ] maybe cannonical encoding/parsing - https://en.wikipedia.org/wiki/Canonical_S-expressions
- [x] rework parser, use Tokenizer.zig
- [ ] simplify parsing by using some kind of writer interface. goal is to replace `ParseMode = enum{measure, allocate}` with some writer where one writer does measuring while another allocates.  hopefully this remove lots of duplication such as `if(mode == .measure) something() else otherthing()`
  - [ ] this would require to re-think how parse results are ordered.
- [ ] api to parse from reader
  - [ ] option for duping strings.  currently we don't dupe anything.
- [ ] fix test "unclosed containers"
- [ ] fuzz test the parser

### references
* https://github.com/edn-format/edn
* https://github.com/jorinvo/edn-data/blob/main/test/parse.test.ts