//! Zappy Argument Parser
//! 
//! Copyright (C) 2023 Fake VOLT Foundation
//! 
//! Licensed under MIT

// $Id: VJArgParse.zig

const std = @import("std");

/// The type of a flag it could be, the value it expects.
pub const zappyArgumentType = enum {
    boolean,
    string,
    number,
    no_compound,
};

/// The type of flag.
pub const zappyFlagType = enum {
    positional, // a.out
    short, // -f
    long, // --flag
};

/// Holds different issue types, for issue handlers
pub const zappyIssueType = enum {
    /// If that flag's value is out of the constraints specified
    out_of_range,

    /// If that flag's value is not a number, and a number was expected/converted
    not_a_number,

    /// Missing value for flag
    missing_value,

    /// Flag is required and value is missing
    required_flag,
};

/// How the Argument Parser handles required flags
pub const zappyHandlingMethod = enum {
    /// Prints a message saying the flags were missing and errors (unless
    /// overriden by problem handler)
    message,

    /// Asks for user input for missing flag values
    ///
    /// e.g:
    ///
    /// ```
    /// $ ./test
    /// [-f]: <...>
    /// ```
    prompt,

    /// Ignores them (not recommended)
    ignore,
};

/// Tries to assume a flag's type. Supports Windows-style flags
/// as well as POSIX
pub fn zappyAssumeFlagType(flag: []const u8) zappyFlagType {
    if (std.mem.startsWith(u8, flag, "--") or std.mem.startsWith(u8, flag, "/")) { // '/' for windows-style flags
        return .long;
    } else if (std.mem.startsWith(u8, flag, "-")) {
        return .short;
    } else {
        return .positional;
    }
}

/// (deprecated) parses a flag to get the name
pub fn zappyParseFlag(flag: []const u8) []const u8 {
    const atype: zappyFlagType = zappyAssumeFlagType(flag);

    if (atype == .long) {
        return flag[2..];
    }

    if (atype == .short) {
        return flag[1..];
    }

    return flag;
}

/// Contains all the information that a flag should have.
///
/// Also contains events for when the flag value is changed, an error occurs, etc.
pub const zappyFlag = struct {
    /// The short version of the flag (e.g. `-f`)
    short: u8,

    /// The long version of the flag (e.g. `--flag`)
    long: []const u8,

    /// The type of the flag
    type: zappyArgumentType,

    /// The description of the flag
    description: []const u8,

    /// The current value of the flag or null if it's not set
    value: ?[]const u8 = null,

    /// The constraints of the flag, a number range is primarily useful
    constraint_min: ?i32 = null,
    constraint_max: ?i32 = null,

    /// Events for flag changes and errors
    value_handler: ?*const fn (*zappyFlag, []const u8) void = null,
    problem_handler: ?*const fn (*zappyFlag, []const u8, zappyIssueType) void = null,

    /// Is this flag required?
    required: bool = false,

    /// A pointer to another flag (mutual exclusition)
    mutual_flag: ?*zappyFlag = null,

    /// Creates a new flag
    pub fn new(short: u8, long: []const u8, atype: zappyArgumentType, description: []const u8) zappyFlag {
        return zappyFlag{
            .short = short,
            .long = long,
            .type = atype,
            .description = description,
        };
    }

    /// Sets the mutual flag, to make it mutually exclusive
    pub fn set_mutual_flag(self: *zappyFlag, flag: *zappyFlag) void {
        self.mutual_flag = flag;

        self.mutual_flag.?.constraint_max = self.constraint_max;
        self.mutual_flag.?.constraint_min = self.constraint_min;

        if (self.mutual_flag.?.value != null) {
            if (self.value != null) {
                self.mutual_flag.?._set_value_str(self.value.?);
            }
        }
    }

    /// Makes the flag required
    pub fn is_required(self: *zappyFlag) void {
        self.required = true;

        if (self.mutual_flag != null) {
            self.mutual_flag.?.is_required();
        }
    }

    /// Makes the flag optional (note: this is default)
    pub fn is_optional(self: *zappyFlag) void {
        self.required = false;

        if (self.mutual_flag != null) {
            self.mutual_flag.?.is_optional();
        }
    }

    /// sets the value change handler
    pub fn set_value_handler(self: *zappyFlag, handler: *const fn (*zappyFlag, []const u8) void) void {
        self.value_handler = handler;

        if (self.mutual_flag != null) {
            self.mutual_flag.?.set_value_handler(handler);
        }
    }

    /// Sets the problem handler function
    /// 
    /// AKA when an error occurs
    /// 
    /// **NOTE:** this does silence all internal errors in place for the custom handler
    pub fn set_problem_handler(self: *zappyFlag, handler: *const fn (*zappyFlag, []const u8, zappyIssueType) void) void {
        self.problem_handler = handler;

        if (self.mutual_flag != null) {
            self.mutual_flag.?.set_problem_handler(handler);
        }
    }

    /// Sets the constraints of the flag (number range)
    pub fn set_constraints(self: *zappyFlag, min: ?i32, max: ?i32) void {
        self.constraint_min = min;
        self.constraint_max = max;

        if (self.mutual_flag != null) {
            self.mutual_flag.?.set_constraints(min, max);
        }
    }

    /// Sets the raw value of the flag
    pub fn _set_value_str(self: *zappyFlag, value: []const u8) void {
        self.value = value;

        if (self.mutual_flag != null) {
            self.mutual_flag.?._set_value_str(value);
        }
    }

    /// Returns the value of the flag
    pub fn get_value(self: *zappyFlag) []const u8 {
        return self.value.?;
    }

    /// Sets the default value (simply sets the value)
    pub fn default_value(self: *zappyFlag, value: []const u8) void {
        self.value = value;

        if (self.mutual_flag != null) {
            self.mutual_flag.?._set_value_str(value);
        }
    }

    /// tries to convert the value to the given type
    pub fn convert(self: *zappyFlag, comptime T: type) T {
        if (T == bool) {
            if (self.value == null) {
                return false;
            }
            return (std.mem.eql(u8, self.value.?, "true"));
        } else if (T == []const u8) {
            if (self.value == null) {
                return "";
            }
            return self.value.?;
        } else if (T == i32) {
            if (self.value == null) {
                return 0;
            }
            return std.fmt.parseInt(i32, self.value.?, 10) catch {
                if (self.problem_handler != null) {
                    self.problem_handler.?(self, self.value.?, .not_a_number);
                }

                std.process.exit(1);
            };
        }
        return 0;
    }
};

/// Holds a list of flags, and grows incrementally.
pub const zappyFlags = struct {
    /// The number of flags in a zappyFlags object
    length: u32 = 0,

    /// The list of flags
    flags: []zappyFlag = undefined,

    /// The allocator used to create the zappyFlags object, good for
    /// sub-allocations and re-allocations
    alloc: std.mem.Allocator,

    pub fn create(allocator: std.mem.Allocator) zappyFlags {
        return zappyFlags{
            .length = 0,
            .alloc = allocator,
            .flags = allocator.alloc(zappyFlag, 256) catch {
                std.debug.print("argparse.zig: error: out of memory\n", .{});
                std.process.exit(1);
            },
        };
    }

    /// Adds a new flag to the zappyFlags, reallocating if necessary
    pub fn add_flag(self: *zappyFlags, flag: zappyFlag) void {
        if (self.length >= self.flags.len) {
            self.flags = self.alloc.realloc(self.flags, self.flags.len * 2) catch {
                std.debug.print("argparse.zig: error: out of memory\n", .{});
                std.process.exit(1);
            };
        }
        self.flags[self.length] = flag;
        self.length += 1;
    }

    pub fn append(self: *zappyFlags, flag: zappyFlag) void {
        self.add_flag(flag);
    }

    pub fn destroy(self: *zappyFlags) void {
        self.alloc.free(self.flags);
    }
};

/// ## Zappy Argument Parser
///
/// **NOTE:** this is not a subparser-supported argument parsing structure, however,
/// it is easy enough to extend it to support subparsers, which may be a separate module.
///
/// ```zig
/// var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
/// defer arena.deinit();
///
/// var parser = zappyArgumentParser.create(arena.allocator());
/// defer parser.deinit();
///
/// parser.details("test [-fh]", "A Test Program.", "test");
/// var flag = parser.add_flag('f', "--flag", .boolean, "a flag");
/// flag.default_value("false");
/// ```
///
pub const zappyArgumentParser = struct {
    flags: zappyFlags,
    positional: std.ArrayList([]const u8),

    allocator: std.mem.Allocator,

    usage: []const u8 = "",
    desc: []const u8 = "",
    prog: []const u8 = "",

    /// How the parser should handle missing flags
    required_method: zappyHandlingMethod = .message,

    /// Creates a new argument parser relative to the arguments supplied,
    /// holding it's own flags, etc.
    pub fn create(allocator: std.mem.Allocator) zappyArgumentParser {
        return zappyArgumentParser{
            .flags = zappyFlags.create(allocator),
            .positional = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn for_required_arguments(self: *zappyArgumentParser, method: zappyHandlingMethod) void {
        self.required_method = method;
    }

    /// Returns the number of positional arguments
    pub fn get_positionals(self: *zappyArgumentParser) usize {
        return self.positional.items.len;
    }

    /// Returns the positional at the index, if the index is out of bounds it returns an empty string
    pub fn get_positional(self: *zappyArgumentParser, index: usize) []const u8 {
        if (index >= self.positional.items.len) {
            return "";
        }
        return self.positional.items[index];
    }

    /// Deinitializes the argument parser
    pub fn deinit(self: *zappyArgumentParser) void {
        self.flags.destroy();
        self.positional.deinit();
    }

    /// Add details about the program
    ///
    /// * `PROG`: name of the program
    /// * `DESC`: description of the program
    /// * `USAGE`: usage of the program
    ///
    /// You are able to implement custom help and all that but these details are still good
    /// to know.
    ///
    /// Usage:
    ///
    /// ```zig
    /// var parser = zappyArgumentParser.create(allocator);
    /// defer parser.deinit();
    ///
    /// parser.details("test [-fh]", "A Test Program.", "test");
    /// ```
    pub fn details(self: *zappyArgumentParser, usage: []const u8, desc: []const u8, prog: []const u8) void {
        self.usage = usage;
        self.desc = desc;
        self.prog = prog;
    }

    /// Adds a flag to `self.flags`
    ///
    /// * `short`: the short version of the flag (e.g. `-f`)
    /// * `long`: the long version of the flag (e.g. `--flag`)
    /// * `atype`: the type of the flag
    ///     * boolean
    ///     * string
    ///     * number
    ///     *
    /// * `description`: the description of the flag (what it does in simple terms)
    pub fn add_flag(self: *zappyArgumentParser, short: u8, long: []const u8, atype: zappyArgumentType, description: []const u8) !*zappyFlag {
        const setup = zappyFlag{ .short = short, .long = long, .type = atype, .description = description };

        self.flags.append(setup);

        return &self.flags.flags[self.flags.length - 1];
    }

    /// Searches for a flag in `self.flags`, using the long version
    ///
    /// Returns `null` if the flag name given isn't found.
    ///
    /// **Note:** This function requires the flagname with no `--` prefix,
    /// unless the arguments are designed that way.
    pub fn search_flag_long(self: *zappyArgumentParser, flag: []const u8) ?*zappyFlag {
        for (0..self.flags.length) |i| {
            if (std.mem.eql(u8, self.flags.flags[i].long, flag)) {
                return &self.flags.flags[i];
            }
        }

        return null;
    }

    /// Searches for a flag in `self.flags`, using the short version
    ///
    /// Returns `null` if the flag name given isn't found.
    pub fn search_flag_short(self: *zappyArgumentParser, flag: u8) ?*zappyFlag {
        for (0..self.flags.length) |i| {
            if (self.flags.flags[i].short == flag) {
                return &self.flags.flags[i];
            }
        }

        return null;
    }

    pub fn parse_args(self: *zappyArgumentParser, args: [][]const u8) !void {
        var state: i32 = 0;

        var last_flag: ?*zappyFlag = null;

        for (args) |item| {
            const typeof = zappyAssumeFlagType(item);

            if (typeof == .positional) {

                // If the state's 0, means that we're not supplying a flag
                if (state == 0) {
                    try self.positional.append(item);
                } else if (state == 1) {
                    if (last_flag != null) {
                        last_flag.?._set_value_str(item);

                        if (last_flag.?.constraint_min != null and last_flag.?.constraint_max != null) {
                            if (last_flag.?.convert(i32) < last_flag.?.constraint_min.? or last_flag.?.convert(i32) > last_flag.?.constraint_max.?) {
                                if (last_flag.?.problem_handler != null) {
                                    last_flag.?.problem_handler.?(last_flag.?, item, .out_of_range);
                                } else {
                                    std.debug.print("{s}: error: `{s}' must be between {d} and {d}\n", .{ self.prog, last_flag.?.long, last_flag.?.constraint_min.?, last_flag.?.constraint_max.? });
                                }
                            }
                        }

                        if (last_flag.?.value_handler != null) {
                            last_flag.?.value_handler.?(last_flag.?, item);
                        }
                    }

                    state = 0;
                    last_flag = null;
                }
            } else if (typeof == .long) {
                if (item.len < 3) {
                    std.debug.print("{s}: error: invalid long flag: `{s}'\n", .{ self.prog, item });
                    std.process.exit(1);
                }
                var stripped = item[2..];

                if (item[0] == '/') {
                    stripped = item[1..];
                }

                var key_value = false;
                var key: []const u8 = "";
                var value: []const u8 = "";

                for (0..stripped.len) |i| {
                    if (stripped[i] == '=') {
                        key_value = true;
                        key = stripped[0..i];
                        value = stripped[i + 1 ..];
                    }
                }

                if (!key_value) {
                    key = stripped;
                }

                var flag = self.search_flag_long(key);

                if (flag == null) {
                    std.debug.print("{s}: error: unknown flag: `{s}'\n", .{ self.prog, item });

                    const help_flag = self.search_flag_short('h');

                    if (help_flag != null) {
                        std.debug.print("{s}: type `{s} -h' for help\n", .{ self.prog, self.prog });
                    }

                    std.process.exit(1);
                }

                if (flag.?.type == .boolean) {
                    flag.?._set_value_str("true");
                } else {
                    if (key_value) {
                        flag.?._set_value_str(value);
                    }
                    state = 1;
                    last_flag = flag;
                }
            } else if (typeof == .short) {
                const stripped = item[1..];

                for (0..stripped.len) |i| { // Compound flags
                    const flag = self.search_flag_short(stripped[i]);

                    if (flag == null) {
                        std.debug.print("{s}: fatal: unrecognized short flag in compound: `{c}'\n", .{ self.prog, stripped[i] });
                        const help_flag = self.search_flag_short('h');

                        if (help_flag != null) {
                            std.debug.print("{s}: type `{s} -h' for help\n", .{ self.prog, self.prog });
                        }
                        std.process.exit(1);
                    }

                    if (flag.?.type == .boolean) {
                        flag.?._set_value_str("true");
                    } else if (flag.?.type == .no_compound) {
                        flag.?._set_value_str(stripped[i + 1 ..]);
                        break;
                    } else {
                        state = 1;
                        last_flag = flag;
                    }
                }
            }
        }

        // if the state is 1, means that we're still supplying a flag
        if (state == 1) {
            if (last_flag != null) {
                if (last_flag.?.problem_handler == null) {
                    std.debug.print("{s}: error: missing value for flag: `{s}'\n", .{ self.prog, last_flag.?.long });
                } else {
                    last_flag.?.problem_handler.?(last_flag.?, "", .missing_value);
                }
            }
        }

        // check for any required flags that weren't set
        if (self.required_method != .ignore) {
            for (0..self.flags.length) |i| {
                if (self.flags.flags[i].mutual_flag != null) {
                    if (self.flags.flags[i].mutual_flag.?.value != null) {
                        continue;
                    }
                }
                if (self.flags.flags[i].required and self.flags.flags[i].value == null) {
                    switch (self.required_method) {
                        .message => {
                            if (self.flags.flags[i].problem_handler != null) {
                                self.flags.flags[i].problem_handler.?(&self.flags.flags[i], "", .required_flag);
                            } else {
                                std.debug.print("{s}: error: missing required flag: `{s}'\n", .{ self.prog, self.flags.flags[i].long });
                                std.process.exit(1);
                            }
                        },
                        .ignore => {},
                        .prompt => {
                            const stdin = std.io.getStdIn();
                            const stdout = std.io.getStdOut();
                            _ = stdout;
                            const stdin_buffered = std.io.bufferedReader(stdin.reader());
                            _ = stdin_buffered;

                            var buf = self.allocator.alloc(u8, 512) catch {
                                std.debug.print("{s}: fatal: out of memory\n", .{self.prog});
                                std.process.exit(1);
                            };
                            var buflen: usize = 0;

                            std.debug.print("[-{c}]: ", .{self.flags.flags[i].short});

                            var _c: u8 = stdin.reader().readByte() catch {
                                std.debug.print("{s}: fatal: unable to read from stdin\n", .{self.prog});
                                std.process.exit(1);
                            };

                            while (_c != '\n') {
                                if (buflen >= buf.len) {
                                    std.debug.print("{s}: note: value prompt too long!\n", .{self.prog});
                                    std.process.exit(1);
                                }

                                if (!std.ascii.isWhitespace(_c)) {
                                    buf[buflen] = _c;
                                    buflen += 1;
                                }

                                _c = stdin.reader().readByte() catch {
                                    std.debug.print("{s}: fatal: unable to read from stdin\n", .{self.prog});
                                    std.process.exit(1);
                                };
                            }

                            self.flags.flags[i]._set_value_str(buf[0..buflen]);
                        },
                    }
                }
            }
        }
    }

    pub fn print_help(self: *zappyArgumentParser) void {
        std.debug.print("usage: {s}\n{s}\nOptions:\n", .{ self.usage, self.desc });

        for (0..self.flags.length) |j| {
            std.debug.print("\t-{c}\t\t{s} (also --{s})\n", .{ self.flags.flags[j].short, self.flags.flags[j].description, self.flags.flags[j].long });
        }
    }

    pub fn flag_exists(self: *zappyArgumentParser, short: u8) bool {
        for (0..self.flags.length) |i| {
            if (self.flags.flags[i].short == short) {
                return true;
            }
        }
    }
};

/// A sample helper function that prints an error.
pub fn print_problem(flag: *zappyFlag, item: []const u8, error_type: zappyIssueType) void {
    switch (error_type) {
        .out_of_range => {
            std.debug.print("test: error: `{s}' must be between {d} and {d}\n", .{ flag.long, flag.constraint_min.?, flag.constraint_max.? });
        },
        .not_a_number => {
            std.debug.print("test: error: `{s}' must be a number\n", .{item});
        },
        .missing_value => {
            std.debug.print("test: error: `{s}' requires a value\n", .{flag.long});
        },
        .required_flag => {
            std.debug.print("test: error: `{s}' is required\n", .{flag.long});
        },
    }
    std.process.exit(1);
}

pub fn main() !void {
    // You can use your favorite allocator
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
}
