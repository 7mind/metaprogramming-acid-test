/// Metaprogramming Acid Test - Zig Implementation
///
/// This library implements three metaprogramming challenges in Zig:
/// 1. Effortless Structured Logging
/// 2. Library-Based Reflection
/// 3. Functoid Concept
///
/// Zig's comptime capabilities enable all three challenges to be implemented
/// as pure library code without compiler plugins or runtime reflection.

pub const structured_logging = @import("structured_logging.zig");
pub const type_reflection = @import("type_reflection.zig");
pub const functoid = @import("functoid.zig");

// Re-export main types and functions for convenience
pub const log = structured_logging.log;
pub const logNamed = structured_logging.logNamed;
pub const LogEntry = structured_logging.LogEntry;

pub const TypeId = type_reflection.TypeId;
pub const isSame = type_reflection.isSame;
pub const isSubtypeOf = type_reflection.isSubtypeOf;
pub const TypeMetadata = type_reflection.TypeMetadata;

pub const Functoid = functoid.Functoid;
pub const FunctoidWithIds = functoid.FunctoidWithIds;
pub const ParamInfo = functoid.ParamInfo;
pub const NamedParam = functoid.NamedParam;

test {
    @import("std").testing.refAllDecls(@This());
}
