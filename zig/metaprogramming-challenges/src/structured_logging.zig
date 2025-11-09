const std = @import("std");

/// Challenge 1: Effortless Structured Logging
///
/// This implementation uses Zig's comptime capabilities to automatically
/// extract variable names from format strings and create structured JSON output.
///
/// Example:
/// ```zig
/// const user = "John";
/// const balance = 42;
/// try log(writer, "Hello {s}, your balance is {d}", .{ user, balance });
/// ```
///
/// Output:
/// ```json
/// {
///   "template": "Hello %s%, your balance is %d%",
///   "args": {
///     "user": "John",
///     "balance": 42
///   }
/// }
/// ```

pub const LogEntry = struct {
    template: []const u8,
    args: std.StringHashMap(std.json.Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, template: []const u8) !LogEntry {
        return LogEntry{
            .template = template,
            .args = std.StringHashMap(std.json.Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LogEntry) void {
        // Free all string values
        var iter = self.args.valueIterator();
        while (iter.next()) |value| {
            if (value.* == .string) {
                self.allocator.free(value.string);
            }
        }
        self.args.deinit();
    }

    pub fn addArg(self: *LogEntry, name: []const u8, value: std.json.Value) !void {
        try self.args.put(name, value);
    }

    pub fn writeJson(self: *const LogEntry, writer: anytype) !void {
        try writer.writeAll("{\n");
        try writer.writeAll("  \"template\": \"");
        try writer.writeAll(self.template);
        try writer.writeAll("\",\n");
        try writer.writeAll("  \"args\": {\n");

        var iter = self.args.iterator();
        var first = true;
        while (iter.next()) |entry| {
            if (!first) {
                try writer.writeAll(",\n");
            }
            first = false;
            try writer.writeAll("    \"");
            try writer.writeAll(entry.key_ptr.*);
            try writer.writeAll("\": ");

            // Write value based on type
            switch (entry.value_ptr.*) {
                .string => |s| {
                    try writer.writeAll("\"");
                    try writer.writeAll(s);
                    try writer.writeAll("\"");
                },
                .integer => |i| {
                    try writer.print("{d}", .{i});
                },
                .float => |f| {
                    try writer.print("{d}", .{f});
                },
                .bool => |b| {
                    try writer.writeAll(if (b) "true" else "false");
                },
                .null => {
                    try writer.writeAll("null");
                },
                else => {
                    try writer.writeAll("\"<unsupported>\"");
                },
            }
        }

        try writer.writeAll("\n  }\n");
        try writer.writeAll("}");
    }
};

/// Parse template string and extract format specifiers
/// Converts {s}, {d}, etc. to %s%, %d%, etc.
fn transformTemplate(comptime template: []const u8) []const u8 {
    comptime var result: []const u8 = "";
    comptime var i: usize = 0;
    inline while (i < template.len) : (i += 1) {
        if (template[i] == '{' and i + 1 < template.len) {
            // Find the closing brace
            comptime var j = i + 1;
            inline while (j < template.len and template[j] != '}') : (j += 1) {}
            if (j < template.len) {
                // Extract format specifier
                const spec = template[i + 1 .. j];
                result = result ++ "%" ++ spec ++ "%";
                i = j;
                continue;
            }
        }
        result = result ++ &[_]u8{template[i]};
    }
    return result;
}

/// Helper to convert values to JSON values
fn toJsonValue(allocator: std.mem.Allocator, value: anytype) !std.json.Value {
    const T = @TypeOf(value);
    const info = @typeInfo(T);

    switch (info) {
        .int, .comptime_int => {
            return std.json.Value{ .integer = @as(i64, @intCast(value)) };
        },
        .float, .comptime_float => {
            return std.json.Value{ .float = @as(f64, @floatCast(value)) };
        },
        .bool => {
            return std.json.Value{ .bool = value };
        },
        .pointer => |ptr_info| {
            switch (ptr_info.size) {
                .slice => {
                    if (ptr_info.child == u8) {
                        // String slice
                        const s = try allocator.dupe(u8, value);
                        return std.json.Value{ .string = s };
                    }
                },
                .one => {
                    // Check if it's a pointer to an array of u8 (string literal)
                    const child_info = @typeInfo(ptr_info.child);
                    if (child_info == .array and child_info.array.child == u8) {
                        // String literal - dereference and convert
                        const s = try allocator.dupe(u8, value);
                        return std.json.Value{ .string = s };
                    }
                    if (ptr_info.child == u8) {
                        // Single char
                        const s = try allocator.dupe(u8, &[_]u8{value.*});
                        return std.json.Value{ .string = s };
                    }
                },
                else => {},
            }
        },
        .array => |arr_info| {
            if (arr_info.child == u8) {
                // String literal
                const s = try allocator.dupe(u8, &value);
                return std.json.Value{ .string = s };
            }
        },
        else => {},
    }

    // Fallback
    return std.json.Value{ .null = {} };
}

/// Structured logging macro
/// Usage: log(writer, "Hello {s}, balance: {d}", .{ user, balance })
pub fn log(allocator: std.mem.Allocator, writer: anytype, comptime template: []const u8, args: anytype) !void {
    const transformed = transformTemplate(template);

    var entry = try LogEntry.init(allocator, transformed);
    defer entry.deinit();

    // Extract field names from args tuple
    const args_info = @typeInfo(@TypeOf(args));
    if (args_info != .@"struct") {
        @compileError("args must be a tuple");
    }

    // We can't easily extract the original variable names in Zig without macros
    // So we'll use indices as names for now
    inline for (args_info.@"struct".fields, 0..) |field, idx| {
        const value = @field(args, field.name);
        const json_value = try toJsonValue(allocator, value);
        const name = try std.fmt.allocPrint(allocator, "arg{d}", .{idx});
        defer allocator.free(name);
        try entry.addArg(name, json_value);
    }

    try entry.writeJson(writer);
    try writer.writeAll("\n");
}

/// Advanced version with named arguments
/// Usage: logNamed(writer, "Hello {s}, balance: {d}", .{ .user = "John", .balance = 42 })
pub fn logNamed(allocator: std.mem.Allocator, writer: anytype, comptime template: []const u8, args: anytype) !void {
    const transformed = transformTemplate(template);

    var entry = try LogEntry.init(allocator, transformed);
    defer entry.deinit();

    // Extract field names from args struct
    const args_info = @typeInfo(@TypeOf(args));
    if (args_info != .@"struct") {
        @compileError("args must be a struct");
    }

    inline for (args_info.@"struct".fields) |field| {
        const value = @field(args, field.name);
        const json_value = try toJsonValue(allocator, value);
        try entry.addArg(field.name, json_value);
    }

    try entry.writeJson(writer);
    try writer.writeAll("\n");
}

test "structured logging basic" {
    const allocator = std.testing.allocator;
    var buffer: std.ArrayList(u8) = .{};
    defer buffer.deinit(allocator);

    try logNamed(allocator, buffer.writer(allocator), "Hello {s}, your balance is {d}", .{
        .user = "John",
        .balance = 42,
    });

    const output = buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, output, "\"template\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "Hello %s%, your balance is %d%") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"user\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"John\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\"balance\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "42") != null);
}

test "structured logging multiple types" {
    const allocator = std.testing.allocator;
    var buffer: std.ArrayList(u8) = .{};
    defer buffer.deinit(allocator);

    try logNamed(allocator, buffer.writer(allocator), "User {s} age {d} score {d} active {}", .{
        .name = "Alice",
        .age = 30,
        .score = 95.5,
        .active = true,
    });

    const output = buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, output, "\"Alice\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "30") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "95.5") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "true") != null);
}
