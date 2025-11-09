const std = @import("std");
const mp = @import("../src/main.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.writeAll("=== Challenge 2: Library-Based Reflection ===\n\n");

    // Example 1: TypeId creation and comparison
    try stdout.writeAll("Example 1: Type identity comparison\n");
    const id1 = mp.TypeId(i32).init();
    const id2 = mp.TypeId(i32).init();
    const id3 = mp.TypeId(u32).init();

    try stdout.print("TypeId(i32) name: {s}\n", .{mp.TypeId(i32).name()});
    try stdout.print("TypeId(i32) hash: {d}\n", .{mp.TypeId(i32).hash()});
    try stdout.print("TypeId(u32) name: {s}\n", .{mp.TypeId(u32).name()});
    try stdout.print("TypeId(u32) hash: {d}\n", .{mp.TypeId(u32).hash()});

    try stdout.print("i32 == i32: {}\n", .{mp.isSame(i32, i32, id1, id2)});
    try stdout.print("i32 == u32: {}\n", .{mp.isSame(i32, u32, id1, id3)});
    try stdout.writeAll("\n");

    // Example 2: Different types
    try stdout.writeAll("Example 2: Different types\n");
    const str_id = mp.TypeId([]const u8).init();
    const bool_id = mp.TypeId(bool).init();

    try stdout.print("TypeId([]const u8) name: {s}\n", .{mp.TypeId([]const u8).name()});
    try stdout.print("TypeId(bool) name: {s}\n", .{mp.TypeId(bool).name()});
    try stdout.print("i32 == []const u8: {}\n", .{mp.isSame(i32, []const u8, id1, str_id)});
    try stdout.print("i32 == bool: {}\n", .{mp.isSame(i32, bool, id1, bool_id)});
    try stdout.writeAll("\n");

    // Example 3: Subtype checking
    try stdout.writeAll("Example 3: Subtype checking\n");
    const i8_id = mp.TypeId(i8).init();
    const i16_id = mp.TypeId(i16).init();
    const i32_id = mp.TypeId(i32).init();

    try stdout.print("i8 <: i16: {}\n", .{mp.isSubtypeOf(i8, i16, i8_id, i16_id)});
    try stdout.print("i8 <: i32: {}\n", .{mp.isSubtypeOf(i8, i32, i8_id, i32_id)});
    try stdout.print("i16 <: i8: {}\n", .{mp.isSubtypeOf(i16, i8, i16_id, i8_id)});
    try stdout.writeAll("\n");

    // Example 4: Type metadata
    try stdout.writeAll("Example 4: Type metadata\n");
    const Point = struct {
        x: i32,
        y: i32,
    };

    const i32_meta = mp.TypeMetadata(i32);
    const point_meta = mp.TypeMetadata(Point);

    try stdout.print("i32 metadata:\n", .{});
    try stdout.print("  name: {s}\n", .{i32_meta.type_name});
    try stdout.print("  size: {d}\n", .{i32_meta.size});
    try stdout.print("  alignment: {d}\n", .{i32_meta.alignment});
    try stdout.print("  is_integer: {}\n", .{i32_meta.isInteger()});
    try stdout.print("  is_struct: {}\n", .{i32_meta.isStruct()});

    try stdout.print("\nPoint metadata:\n", .{});
    try stdout.print("  name: {s}\n", .{point_meta.type_name});
    try stdout.print("  size: {d}\n", .{point_meta.size});
    try stdout.print("  alignment: {d}\n", .{point_meta.alignment});
    try stdout.print("  is_integer: {}\n", .{point_meta.isInteger()});
    try stdout.print("  is_struct: {}\n", .{point_meta.isStruct()});
}
