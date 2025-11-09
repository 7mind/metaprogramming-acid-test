const std = @import("std");
const mp = @import("../src/main.zig");

fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn greet(name: []const u8, age: i32) bool {
    _ = name;
    return age >= 18;
}

fn multiply(x: i32, y: i32) i32 {
    return x * y;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    try stdout.writeAll("=== Challenge 3: Functoid Concept ===\n\n");

    // Example 1: Basic functoid
    try stdout.writeAll("Example 1: Basic functoid introspection\n");
    const add_functoid = mp.Functoid(@TypeOf(add)).init(add);

    try stdout.print("Function: add\n", .{});
    try stdout.print("Arity: {d}\n", .{add_functoid.arity()});

    const add_params = try add_functoid.parameterTypes(allocator);
    defer allocator.free(add_params);

    try stdout.writeAll("Parameters:\n");
    for (add_params, 0..) |param, i| {
        try stdout.print("  [{d}] type: {s}, hash: {d}\n", .{ i, param.type_name, param.type_hash });
    }

    try stdout.print("Return type: {s}\n", .{add_functoid.returnType()});
    try stdout.print("Return type hash: {d}\n", .{add_functoid.returnTypeHash()});

    const add_result = add_functoid.invoke(.{ 10, 32 });
    try stdout.print("Invocation: add(10, 32) = {d}\n", .{add_result});
    try stdout.writeAll("\n");

    // Example 2: Functoid with different types
    try stdout.writeAll("Example 2: Functoid with mixed parameter types\n");
    const greet_functoid = mp.Functoid(@TypeOf(greet)).init(greet);

    try stdout.print("Function: greet\n", .{});
    try stdout.print("Arity: {d}\n", .{greet_functoid.arity()});

    const greet_params = try greet_functoid.parameterTypes(allocator);
    defer allocator.free(greet_params);

    try stdout.writeAll("Parameters:\n");
    for (greet_params, 0..) |param, i| {
        try stdout.print("  [{d}] type: {s}\n", .{ i, param.type_name });
    }

    try stdout.print("Return type: {s}\n", .{greet_functoid.returnType()});

    const greet_result = greet_functoid.invoke(.{ "Alice", 25 });
    try stdout.print("Invocation: greet(\"Alice\", 25) = {}\n", .{greet_result});
    try stdout.writeAll("\n");

    // Example 3: Functoid with parameter IDs
    try stdout.writeAll("Example 3: Functoid with parameter IDs\n");
    const param_ids = &[_]?[]const u8{ "user-name", "user-age" };
    const greet_with_ids = mp.Functoid(@TypeOf(greet)).initWithIds(greet, param_ids);

    try stdout.print("Function: greet (with IDs)\n", .{});

    const greet_ids_params = try greet_with_ids.parameterTypes(allocator);
    defer allocator.free(greet_ids_params);

    try stdout.writeAll("Parameters with IDs:\n");
    for (greet_ids_params, 0..) |param, i| {
        const id_str = param.id orelse "<no id>";
        try stdout.print("  [{d}] type: {s}, id: {s}\n", .{ i, param.type_name, id_str });
    }
    try stdout.writeAll("\n");

    // Example 4: FunctoidWithIds (compile-time IDs)
    try stdout.writeAll("Example 4: FunctoidWithIds with compile-time IDs\n");
    const comptime_ids = &[_][]const u8{ "multiplier", "multiplicand" };
    const MultiplyFunctoid = mp.FunctoidWithIds(@TypeOf(multiply), comptime_ids);
    const mult_functoid = MultiplyFunctoid.init(multiply);

    try stdout.print("Function: multiply (with compile-time IDs)\n", .{});
    try stdout.print("Arity: {d}\n", .{mult_functoid.arity()});

    const mult_params = try mult_functoid.parameterTypes(allocator);
    defer allocator.free(mult_params);

    try stdout.writeAll("Parameters with compile-time IDs:\n");
    for (mult_params, 0..) |param, i| {
        const id_str = param.id orelse "<no id>";
        try stdout.print("  [{d}] type: {s}, id: {s}\n", .{ i, param.type_name, id_str });
    }

    const mult_result = mult_functoid.invoke(.{ 6, 7 });
    try stdout.print("Invocation: multiply(6, 7) = {d}\n", .{mult_result});
    try stdout.writeAll("\n");

    // Example 5: Zero-parameter function
    try stdout.writeAll("Example 5: Zero-parameter functoid\n");
    const getAnswer = struct {
        fn f() i32 {
            return 42;
        }
    }.f;

    const answer_functoid = mp.Functoid(@TypeOf(getAnswer)).init(getAnswer);

    try stdout.print("Function: getAnswer\n", .{});
    try stdout.print("Arity: {d}\n", .{answer_functoid.arity()});
    try stdout.print("Return type: {s}\n", .{answer_functoid.returnType()});

    const answer = answer_functoid.invoke(.{});
    try stdout.print("Invocation: getAnswer() = {d}\n", .{answer});
}
