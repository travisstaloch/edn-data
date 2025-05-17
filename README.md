## EDN Data - Extensible Data Notation for Zig

A parser for [Extensible Data Notation (EDN)](https://github.com/edn-format/edn) implemented in Zig. EDN is a data format akin to JSON or [Ziggy](https://ziggy-lang.io/) but with more data types and extensibility features, commonly used in Clojure.

This library provides both runtime and compile-time parsing capabilities for EDN data in Zig applications.

#### Table of Contents
- [Features](#features)
- [Usage](#usage)

#### Features
* parse arbitrary data with `edn.parseFromSliceAlloc()` or `edn.parseFromSliceBuf()`.  `edn.measure()` is used to determine required buffer sizes for `parseFromSliceBuf()`.
  * `edn.Options.whitespace` - whether to save whitespace and comments.  `.exclude` is like minify and means that `ParseResult.wss.len` will be 0 and each merged whitespace/comment will be replaced by a single space in `fmtParseResult()`.
* parse structured data with `edn.parseTypeFromSlice(T)`
  * custom parsing when `T` provides a  `pub fn ednParse()`.  see [src/tests.zig](src/tests.zig) `test "ednParse()"` for an example.
* [tagged element](https://github.com/edn-format/edn#tagged-elements) handlers.  see [src/tests.zig](src/tests.zig) `test "tagged handler"` for an example.
* `ParseResult.find()` - access parsed data with simple queries such as `'0//1//foo'`.  see [src/tests.zig](src/tests.zig) `test "ParseResult find()"` for examples.
* comptime parsing
  * parse arbitrary data with `edn.parseFromSliceComptime()`
  * parse structured data with `comptime edn.parseTypeFromSlice(T)`
* formatting
  * format parse results with `edn.fmtParseResult(parse_result, src)`

#### Usage
fetch with the package manager
```console
# with zig 0.14.0
$ zig fetch --save git+https://github.com/travisstaloch/edn-data#0.14.0
```
```console
# with zig nightly
$ zig fetch --save git+https://github.com/travisstaloch/edn-data
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
const edn = @import("extensible-data-notation");
test "parseFromSliceAlloc demo" {
    const src = "a (a b c [1 2 3] {:a 1, :b 2})";
    const result = try edn.parseFromSliceAlloc(std.testing.allocator, src, .{}, .{});
    defer result.deinit(std.testing.allocator);
    if (!@import("builtin").is_test) {
        std.debug.print("{}\n", .{edn.fmtParseResult(result, src)});
    }
    try std.testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}
```
```zig
test "parseFromSliceComptime demo" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const result = comptime try edn.parseFromSliceComptime(src, .{}, .{});
    const src2 = std.fmt.comptimePrint("{}", .{comptime edn.fmtParseResult(result, src)});
    try std.testing.expectEqualStrings(src, src2);
}
```
```zig
test "parseFromSliceBuf demo - runtime no allocation" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const measured = comptime try edn.measure(src, .{}, .{}); // src must be comptime known here
    var values: [measured.capacity]edn.Value = undefined;
    var wss: [measured.capacity][2]u32 = undefined;
    const result = try edn.parseFromSliceBuf(src, measured, &values, &wss, .{}, .{});
    try std.testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}
```

#### Testing
run tests in [src/tests.zig](src/tests.zig), [src/Tokenizer.zig](src/Tokenizer.zig), and [src/ringbuffer.zig](src/ringbuffer.zig)
```console
$ zig build test
```

##### Fuzzing
```console
$ zig build test -Dtest-filters="fuzz parseFromSliceAlloc and fmtParseResult" --summary all --fuzz --port 38495
```
```console
$ zig build test -Dtest-filters="fuzz parseTypeFromSlice" --summary all --fuzz --port 38495
```

#### References
* https://github.com/edn-format/edn
* https://github.com/jorinvo/edn-data/blob/main/test/parse.test.ts

#### Ideas and Planned Features
- [ ] built-in tagged elements
  - [ ] #inst - instant http://www.ietf.org/rfc/rfc3339.txt
  - [ ] #uuid - http://en.wikipedia.org/wiki/Universally_unique_identifier
- [ ] maybe cannonical encoding/parsing - https://en.wikipedia.org/wiki/Canonical_S-expressions
- [x] rework parser, use Tokenizer.zig
- [ ] simplify parsing by using some kind of writer interface. goal is to replace `ParseMode = enum{measure, allocate}` with some writer where one writer does measuring while another allocates.  hopefully this remove lots of duplication such as `if(mode == .measure) something() else otherthing()`
  - [ ] this would require to re-think how parse results are ordered.
- [ ] api to parse from reader
  - [ ] option for duping strings.  currently we don't dupe anything.
- [w] fix test "unclosed containers"
  - [ ] add more "unclosed containers" test cases
- [w] fuzz test the parser
  - [x] parseFromSliceAlloc, fmtParseResult
  - [x] expand fuzzing: parseTypeFromSlice