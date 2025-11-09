const std = @import("std");
const mp = @import("metaprogramming");

pub fn main() !void {
    const stdout = std.fs.File.stdout().deprecatedWriter();
    const allocator = std.heap.page_allocator;

    try stdout.writeAll("=== Challenge 1: Effortless Structured Logging ===\n\n");

    // Example 1: Basic logging with named arguments
    try stdout.writeAll("Example 1: Basic logging\n");
    try mp.log(allocator, stdout, "Hello {s}, your balance is {d}", .{
        .user = "John",
        .balance = 42,
    });
    try stdout.writeAll("\n");

    // Example 2: Multiple types
    try stdout.writeAll("Example 2: Multiple types\n");
    try mp.log(allocator, stdout, "User {s} is {d} years old with score {d} (active: {})", .{
        .name = "Alice",
        .age = 30,
        .score = 95.5,
        .active = true,
    });
    try stdout.writeAll("\n");

    // Example 3: Computed values
    try stdout.writeAll("Example 3: With computed values\n");
    const x: i32 = 10;
    const y: i32 = 5;
    const sum = x + y;
    const product = x * y;
    try mp.log(allocator, stdout, "x={d}, y={d}, sum={d}, product={d}", .{
        .x = x,
        .y = y,
        .sum = sum,
        .product = product,
    });
    try stdout.writeAll("\n");

    // Example 4: String formatting
    try stdout.writeAll("Example 4: Different value types\n");
    try mp.log(allocator, stdout, "Status: {s}, Code: {d}, Success: {}", .{
        .status = "OK",
        .code = 200,
        .success = true,
    });
    try stdout.writeAll("\n");
}
