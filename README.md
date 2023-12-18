# Zappy (Zig-Argument-Parsing-Package-You-like)

A feature-rich argument parser for Zig.


```zig
    // You can use your favorite allocator!
var arena_allocator =
    std.heap.ArenaAllocator.init(std.heap.page_allocator);

// just make sure you don't forget to deinit!
defer arena_allocator.deinit();

// Get the arguments
const args =
    try std.process.argsAlloc(arena_allocator.allocator());

// Create the argument parser
var argparser =
    zappyArgumentParser.create(arena_allocator.allocator());
defer argparser.deinit(); // it has it's own deinit() function, try it!

// For required arguments, how should the argument parser handle them?
argparser.for_required_arguments(.prompt);

// sets the program details
argparser.details("test [-fh]", "A Test Program.", "test");

// flags have a bunch of different abstractions and methods, try some!
var flag1 = try argparser.add_flag('f', "flag", .number, "this is a flag");
// flags have a bunch of different abstractions and methods, try some!
const flag2 = try argparser.add_flag('g', "flag2", .number, "this is a flag");

flag1.set_constraints(1, 15);
flag1.set_problem_handler(print_problem);
flag1.is_required();

flag2.set_mutual_flag(flag1);

// parse the arguments, note: all required arguments are handled by default
try argparser.parse_args(args[1..]);

// print the flag's value
std.debug.print("flag1 value: {s}\n", .{flag1.convert([]const u8)});

// and any positional values too
for (0..argparser.get_positionals()) |i| {
    std.debug.print("positional: {s}\n", .{argparser.get_positional(i)});
}

// free the arguments
std.process.argsFree(arena_allocator.allocator(), args);
```

## Supports

* Multiple flag types
* Conversions to real Zig types
* Help
* Positionals
* Mutually exclusive flags
* Required flags
* Different methods for handling required flags
* Flag Input
* Custom error/value change handlers
* POSIX-style flags (`-f --flag`)
* Windows-style flags (`/flag`)
* Compound flags (`-a -b -c` -> `-abc`)
* GNU-style non-compound flags (`-Wall`)
* Long flags
* Short flags
* Allocator-based

## Dependencies

There are no dependencies except for the Zig standard library, which comes with
a majority of Zig installations.

## Inspired By

This library is inspired by `argparse`.

## To use

To use, simply add the `argparse.zig` file into your project root.
