const std = @import("std");
const TypeId = @import("type_reflection.zig").TypeId;

/// Challenge 3: Functoid Concept
///
/// This implementation uses Zig's comptime type introspection to create
/// runtime-introspectable function wrappers.
///
/// A Functoid wraps an arbitrary function and provides:
/// - Runtime access to parameter type information
/// - Runtime access to return type information
/// - Ability to invoke the function
/// - Optional parameter identifiers via struct-based approach
///
/// Example:
/// ```zig
/// fn greet(name: []const u8, age: i32) []const u8 {
///     return "Hello!";
/// }
///
/// const functoid = Functoid(@TypeOf(greet)).init(greet);
/// const params = functoid.parameterTypes();
/// const ret = functoid.returnType();
/// ```

/// Parameter information
pub const ParamInfo = struct {
    type_name: []const u8,
    type_hash: u64,
    id: ?[]const u8,

    pub fn init(comptime T: type, id: ?[]const u8) ParamInfo {
        return ParamInfo{
            .type_name = @typeName(T),
            .type_hash = TypeId(T).hash(),
            .id = id,
        };
    }
};

/// Functoid wrapper for functions
pub fn Functoid(comptime FuncType: type) type {
    const func_info = @typeInfo(FuncType);

    if (func_info != .@"fn") {
        @compileError("Functoid requires a function type");
    }

    const fn_info = func_info.@"fn";

    return struct {
        const Self = @This();

        func: FuncType,
        param_ids: ?[]const ?[]const u8,

        /// Initialize a Functoid with a function
        pub fn init(func: FuncType) Self {
            return Self{
                .func = func,
                .param_ids = null,
            };
        }

        /// Initialize a Functoid with a function and parameter IDs
        pub fn initWithIds(func: FuncType, param_ids: []const ?[]const u8) Self {
            if (param_ids.len != fn_info.params.len) {
                @compileError("Number of param_ids must match function arity");
            }
            return Self{
                .func = func,
                .param_ids = param_ids,
            };
        }

        /// Get the number of parameters
        pub fn arity(self: Self) usize {
            _ = self;
            return fn_info.params.len;
        }

        /// Get parameter information
        pub fn parameterTypes(self: Self, allocator: std.mem.Allocator) ![]ParamInfo {
            var params = try allocator.alloc(ParamInfo, fn_info.params.len);

            inline for (fn_info.params, 0..) |param, i| {
                const param_type = param.type orelse @compileError("Generic parameters not supported");
                const id = if (self.param_ids) |ids| ids[i] else null;
                params[i] = ParamInfo.init(param_type, id);
            }

            return params;
        }

        /// Get return type information
        pub fn returnType(self: Self) []const u8 {
            _ = self;
            const ret_type = fn_info.return_type orelse @compileError("Function must have return type");
            return @typeName(ret_type);
        }

        /// Get return type hash
        pub fn returnTypeHash(self: Self) u64 {
            _ = self;
            const ret_type = fn_info.return_type orelse @compileError("Function must have return type");
            return TypeId(ret_type).hash();
        }

        /// Invoke the function
        pub fn invoke(self: Self, args: anytype) fn_info.return_type.? {
            const args_info = @typeInfo(@TypeOf(args));
            if (args_info != .@"struct") {
                @compileError("invoke requires a tuple of arguments");
            }

            if (args_info.@"struct".fields.len != fn_info.params.len) {
                @compileError("Argument count must match function arity");
            }

            return @call(.auto, self.func, args);
        }

        /// Get function pointer
        pub fn getFunction(self: Self) FuncType {
            return self.func;
        }
    };
}

/// Named parameter wrapper for Functoid
/// This allows attaching IDs to parameters using a struct-based approach
pub fn NamedParam(comptime T: type, comptime id: []const u8) type {
    return struct {
        pub const Type = T;
        pub const Id = id;
        value: T,

        pub fn init(value: T) @This() {
            return .{ .value = value };
        }
    };
}

/// Create a Functoid with named parameters from a wrapper function
/// This is an alternative approach to parameter IDs
pub fn FunctoidWithIds(comptime FuncType: type, comptime param_ids: []const []const u8) type {
    const func_info = @typeInfo(FuncType);
    if (func_info != .@"fn") {
        @compileError("FunctoidWithIds requires a function type");
    }

    const fn_info = func_info.@"fn";
    if (param_ids.len != fn_info.params.len) {
        @compileError("Number of param_ids must match function arity");
    }

    return struct {
        const Self = @This();

        func: FuncType,

        pub fn init(func: FuncType) Self {
            return Self{ .func = func };
        }

        pub fn arity(self: Self) usize {
            _ = self;
            return fn_info.params.len;
        }

        pub fn parameterTypes(self: Self, allocator: std.mem.Allocator) ![]ParamInfo {
            _ = self;
            var params = try allocator.alloc(ParamInfo, fn_info.params.len);

            inline for (fn_info.params, 0..) |param, i| {
                const param_type = param.type orelse @compileError("Generic parameters not supported");
                params[i] = ParamInfo.init(param_type, param_ids[i]);
            }

            return params;
        }

        pub fn returnType(self: Self) []const u8 {
            _ = self;
            const ret_type = fn_info.return_type orelse @compileError("Function must have return type");
            return @typeName(ret_type);
        }

        pub fn invoke(self: Self, args: anytype) fn_info.return_type.? {
            return @call(.auto, self.func, args);
        }
    };
}

test "Functoid basic" {
    const add = struct {
        fn f(a: i32, b: i32) i32 {
            return a + b;
        }
    }.f;

    const functoid = Functoid(@TypeOf(add)).init(add);

    try std.testing.expectEqual(@as(usize, 2), functoid.arity());

    const allocator = std.testing.allocator;
    const params = try functoid.parameterTypes(allocator);
    defer allocator.free(params);

    try std.testing.expect(std.mem.eql(u8, params[0].type_name, "i32"));
    try std.testing.expect(std.mem.eql(u8, params[1].type_name, "i32"));

    const ret = functoid.returnType();
    try std.testing.expect(std.mem.eql(u8, ret, "i32"));

    const result = functoid.invoke(.{ 10, 32 });
    try std.testing.expectEqual(@as(i32, 42), result);
}

test "Functoid with different types" {
    const greet = struct {
        fn f(name: []const u8, age: i32) bool {
            _ = name;
            return age >= 18;
        }
    }.f;

    const functoid = Functoid(@TypeOf(greet)).init(greet);

    try std.testing.expectEqual(@as(usize, 2), functoid.arity());

    const allocator = std.testing.allocator;
    const params = try functoid.parameterTypes(allocator);
    defer allocator.free(params);

    try std.testing.expect(std.mem.eql(u8, params[0].type_name, "[]const u8"));
    try std.testing.expect(std.mem.eql(u8, params[1].type_name, "i32"));

    const ret = functoid.returnType();
    try std.testing.expect(std.mem.eql(u8, ret, "bool"));

    const result = functoid.invoke(.{ "Alice", 25 });
    try std.testing.expect(result);
}

test "Functoid with parameter IDs" {
    const greet = struct {
        fn f(name: []const u8, age: i32) bool {
            _ = name;
            return age >= 18;
        }
    }.f;

    const param_ids = &[_]?[]const u8{ "user-name", "user-age" };
    const functoid = Functoid(@TypeOf(greet)).initWithIds(greet, param_ids);

    const allocator = std.testing.allocator;
    const params = try functoid.parameterTypes(allocator);
    defer allocator.free(params);

    try std.testing.expect(std.mem.eql(u8, params[0].id.?, "user-name"));
    try std.testing.expect(std.mem.eql(u8, params[1].id.?, "user-age"));
}

test "FunctoidWithIds compile-time IDs" {
    const multiply = struct {
        fn f(a: i32, b: i32) i32 {
            return a * b;
        }
    }.f;

    const param_ids = &[_][]const u8{ "multiplier", "multiplicand" };
    const FunctoidType = FunctoidWithIds(@TypeOf(multiply), param_ids);
    const functoid = FunctoidType.init(multiply);

    try std.testing.expectEqual(@as(usize, 2), functoid.arity());

    const allocator = std.testing.allocator;
    const params = try functoid.parameterTypes(allocator);
    defer allocator.free(params);

    try std.testing.expect(std.mem.eql(u8, params[0].id.?, "multiplier"));
    try std.testing.expect(std.mem.eql(u8, params[1].id.?, "multiplicand"));

    const result = functoid.invoke(.{ 6, 7 });
    try std.testing.expectEqual(@as(i32, 42), result);
}

test "Functoid zero parameters" {
    const getAnswer = struct {
        fn f() i32 {
            return 42;
        }
    }.f;

    const functoid = Functoid(@TypeOf(getAnswer)).init(getAnswer);

    try std.testing.expectEqual(@as(usize, 0), functoid.arity());

    const result = functoid.invoke(.{});
    try std.testing.expectEqual(@as(i32, 42), result);
}
