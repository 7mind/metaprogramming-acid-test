const std = @import("std");

/// Challenge 2: Library-Based Reflection
///
/// This implementation uses Zig's comptime type introspection to provide
/// runtime type identity comparison and subtype checking.
///
/// Zig provides @typeName() and @typeInfo() which allow us to:
/// - Get string representations of types at compile time
/// - Compare type identities at runtime
/// - Introspect type structures
///
/// Example:
/// ```zig
/// const id1 = TypeId(i32).init();
/// const id2 = TypeId(i32).init();
/// const id3 = TypeId(u32).init();
///
/// std.debug.assert(isSame(i32, i32, id1, id2));
/// std.debug.assert(!isSame(i32, u32, id1, id3));
/// ```

/// TypeId - Runtime type identifier
pub fn TypeId(comptime T: type) type {
    return struct {
        const Self = @This();

        /// The type this TypeId represents
        pub const Type = T;

        /// Get the type name as a compile-time known string
        pub fn name() []const u8 {
            return @typeName(T);
        }

        /// Get a hash of the type (computed at compile time)
        pub fn hash() u64 {
            return comptime hashString(@typeName(T));
        }

        /// Create a TypeId instance
        pub fn init() Self {
            return Self{};
        }

        /// Compare two type IDs for equality
        pub fn eql(self: Self, comptime U: type, other: TypeId(U)) bool {
            _ = self;
            _ = other;
            return T == U;
        }
    };
}

/// Hash function for strings (FNV-1a)
fn hashString(str: []const u8) u64 {
    var h: u64 = 14695981039346656037;
    for (str) |c| {
        h ^= c;
        h *%= 1099511628211;
    }
    return h;
}

/// Runtime type identity comparison
/// Returns true if both TypeIds represent the same type
pub fn isSame(comptime A: type, comptime B: type, a: TypeId(A), b: TypeId(B)) bool {
    _ = a;
    _ = b;
    return A == B;
}

/// Check if type A is a subtype of type B
/// In Zig, we approximate this using type compatibility rules
pub fn isSubtypeOf(comptime Child: type, comptime Parent: type, child: TypeId(Child), parent: TypeId(Parent)) bool {
    _ = child;
    _ = parent;

    // Exact match
    if (Child == Parent) return true;

    // Check for pointer covariance
    const child_info = @typeInfo(Child);
    const parent_info = @typeInfo(Parent);

    // If both are pointers, check if child pointer can be cast to parent pointer
    if (child_info == .pointer and parent_info == .pointer) {
        const child_ptr = child_info.pointer;
        const parent_ptr = parent_info.pointer;

        // Check if child pointer's pointee type is compatible with parent
        if (child_ptr.child == parent_ptr.child) {
            // Same pointee type - check mutability
            // const pointer can be used where any pointer is expected
            if (parent_ptr.is_const and !child_ptr.is_const) {
                return false; // Can't use mutable as const in general case
            }
            return true;
        }
    }

    // Check for integer widening
    if (child_info == .int and parent_info == .int) {
        const child_int = child_info.int;
        const parent_int = parent_info.int;

        // Same signedness and child bits <= parent bits
        if (child_int.signedness == parent_int.signedness) {
            return child_int.bits <= parent_int.bits;
        }
    }

    // Check for float widening
    if (child_info == .float and parent_info == .float) {
        return child_info.float.bits <= parent_info.float.bits;
    }

    // Check for optional wrapping
    if (parent_info == .optional) {
        return Child == parent_info.optional.child;
    }

    return false;
}

/// Type metadata - provides additional type information
pub fn TypeMetadata(comptime T: type) type {
    return struct {
        pub const type_name = @typeName(T);
        pub const type_info = @typeInfo(T);
        pub const size = @sizeOf(T);
        pub const alignment = @alignOf(T);

        pub fn isInteger() bool {
            return type_info == .int or type_info == .comptime_int;
        }

        pub fn isFloat() bool {
            return type_info == .float or type_info == .comptime_float;
        }

        pub fn isPointer() bool {
            return type_info == .pointer;
        }

        pub fn isStruct() bool {
            return type_info == .@"struct";
        }

        pub fn isEnum() bool {
            return type_info == .@"enum";
        }

        pub fn isOptional() bool {
            return type_info == .optional;
        }
    };
}

test "TypeId basic operations" {
    const id1 = TypeId(i32).init();
    const id2 = TypeId(i32).init();
    const id3 = TypeId(u32).init();

    try std.testing.expect(isSame(i32, i32, id1, id2));
    try std.testing.expect(!isSame(i32, u32, id1, id3));
}

test "TypeId name and hash" {
    const name = TypeId(i32).name();
    const h = TypeId(i32).hash();

    try std.testing.expect(std.mem.eql(u8, name, "i32"));
    try std.testing.expect(h != 0);
}

test "TypeId different types" {
    const int_id = TypeId(i32).init();
    const str_id = TypeId([]const u8).init();
    const bool_id = TypeId(bool).init();

    try std.testing.expect(!isSame(i32, []const u8, int_id, str_id));
    try std.testing.expect(!isSame(i32, bool, int_id, bool_id));
    try std.testing.expect(!isSame([]const u8, bool, str_id, bool_id));
}

test "isSubtypeOf integer widening" {
    const i8_id = TypeId(i8).init();
    const i16_id = TypeId(i16).init();
    const i32_id = TypeId(i32).init();

    // i8 is subtype of i16 (can widen)
    try std.testing.expect(isSubtypeOf(i8, i16, i8_id, i16_id));
    // i8 is subtype of i32 (can widen)
    try std.testing.expect(isSubtypeOf(i8, i32, i8_id, i32_id));
    // i16 is NOT subtype of i8 (can't narrow)
    try std.testing.expect(!isSubtypeOf(i16, i8, i16_id, i8_id));
}

test "isSubtypeOf same type" {
    const id1 = TypeId(i32).init();
    const id2 = TypeId(i32).init();

    try std.testing.expect(isSubtypeOf(i32, i32, id1, id2));
}

test "TypeMetadata" {
    const meta = TypeMetadata(i32);

    try std.testing.expect(meta.isInteger());
    try std.testing.expect(!meta.isFloat());
    try std.testing.expect(!meta.isPointer());
    try std.testing.expect(meta.size == 4);
}

test "TypeMetadata struct" {
    const Point = struct {
        x: i32,
        y: i32,
    };

    const meta = TypeMetadata(Point);
    try std.testing.expect(meta.isStruct());
    try std.testing.expect(!meta.isInteger());
    try std.testing.expect(meta.size == 8);
}
