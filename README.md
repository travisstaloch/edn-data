## EDN Data - Extensible Data Notation for Zig

A parser for [Extensible Data Notation (EDN)](https://github.com/edn-format/edn) implemented in Zig. EDN is a data format akin to JSON or [Ziggy](https://ziggy-lang.io/) but with more data types and extensibility features, commonly used in Clojure.

This library provides both runtime and compile-time parsing capabilities for EDN data in Zig applications.

#### Table of Contents
- [Features](#features)
- [Usage](#usage)

#### Features
* parse arbitrary data with `edn.parseFromSlice(edn.Result, ...)` or `edn.parseFromSliceBuf()`.  `edn.measure()` is used to determine required buffer sizes for `parseFromSliceBuf()`.
  * `edn.Options.whitespace` - whether to save whitespace and comments.  false means to minify and each whitespace/comment span will be replaced by a single space in `fmtParseResult()`.
* parse structured data with `edn.parseFromSlice(T)`
  * custom parsing when `T` provides a  `pub fn ednParse()`.  see [src/tests.zig](src/tests.zig) `test "ednParse()"` for an example.
* [tagged element](https://github.com/edn-format/edn#tagged-elements) handlers.  see [src/tests.zig](src/tests.zig) `test "tagged handler"` for an example.
* `ParseResult.find()` - access parsed data with simple queries such as `'0//1//foo'`.  see [src/tests.zig](src/tests.zig) `test "ParseResult find()"` for examples.
* comptime parsing
  * parse arbitrary data with `edn.parseFromSliceComptime()`
  * parse structured data with `comptime edn.parseFromSlice(T)`
* formatting
  * format parse results with `parse_result.formatter(src)`

#### Usage
fetch with the package manager
```console
# with zig 0.14.0
$ zig fetch --save git+https://github.com/travisstaloch/edn-data#0.14.0
```
```console
# with zig nightly - TODO
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
test "parseFromSlice demo with Diagnostic" {
    const src = "a (a b c [1 2 3] {:a 1, :b 2})";
    // on error, Diagnostic line, column, and error_message will be populated.
    var diag: edn.Diagnostic = .{ .file_path = "<test-file>" };
    const result = edn.parseFromSlice(edn.Result, src, .{
        .diagnostic = &diag,
        .allocator = std.testing.allocator,
    }, .{}) catch |e| {
        std.log.debug("{s}\n", .{diag.error_message});
        return e;
    };
    defer result.deinit(std.testing.allocator);
    if (!@import("builtin").is_test) { // use format helper
        std.debug.print("{}\n", .{result.formatter(src)});
    }
    try std.testing.expectFmt(src, "{}", .{result.formatter(src)});
}

test "parseFromSliceComptime demo" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const result = comptime try edn.parseFromSliceComptime(src, .{}, .{ .eval_branch_quota = 2000 });
    const src2 = std.fmt.comptimePrint("{}", .{comptime result.formatter(src)});
    try std.testing.expectEqualStrings(src, src2);
}

test "parseFromSliceBuf demo - runtime no allocation" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const shape = comptime try edn.measure(src, .{}, .{}); // src must be comptime known here
    var arrays: shape.Arrays() = undefined;
    const result = try edn.parseFromSliceBuf(src, shape, arrays.buffers(), .{}, .{});
    try std.testing.expectFmt(src, "{}", .{result.formatter(src)});
}
```

#### Testing
run tests in [src/tests.zig](src/tests.zig), [src/Tokenizer.zig](src/Tokenizer.zig), and [src/ringbuffer.zig](src/ringbuffer.zig)
```console
$ zig build test
```

##### Fuzzing
```console
$ zig build test -Dtest-filters="fuzz parseFromSlice and format" --summary all --fuzz --port 38495
```
```console
$ zig build test -Dtest-filters="fuzz parseFromSlice(T)" --summary all --fuzz --port 38495
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
  - [x] this would require to re-think how parse results are ordered.
    - [x] parse results are stored with the tree structure represented by first_child_ids and next_sibling_ids arrays.  
      - [ ] this is quite a wasteful since most entries are empty. would be bettter as if it was a sparse array represented by a bitset and a full array.
- [x] fix test "unclosed containers"
  - [x] add more "unclosed containers" test cases
- [w] fuzz test the parser
  - [x] parseFromSliceAlloc, fmtParseResult
  - [x] expand fuzzing: parseTypeFromSlice
  - [ ] expand fuzzing
- [x] merge Parser and root.zig as Parser.zig. rename some redundant names such as ParseResult, ParseError.
- [x] compresss parser code by reusing parseList to parse maps.
- [x] store top level items in a list and reuse parseList again.
- [x] merge ParseMode and some Parser fields into Options
- [x] make ValueId an enum
- [ ] unify parseFrom* methods with parseType* like std.json
- [ ] api to parse from reader
  - [ ] option for duping strings.  currently we don't dupe anything.
