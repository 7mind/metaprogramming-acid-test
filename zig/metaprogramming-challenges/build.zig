const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create the root module for the library
    const metaprogramming_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Tests
    const tests = b.addTest(.{
        .root_module = metaprogramming_module,
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_tests.step);

    // Example executables - create modules for each and add metaprogramming as import
    const structured_logging_module = b.createModule(.{
        .root_source_file = b.path("examples/structured_logging_example.zig"),
        .target = target,
        .optimize = optimize,
    });
    structured_logging_module.addImport("metaprogramming", metaprogramming_module);
    const structured_logging_example = b.addExecutable(.{
        .name = "structured_logging_example",
        .root_module = structured_logging_module,
    });
    b.installArtifact(structured_logging_example);

    const type_reflection_module = b.createModule(.{
        .root_source_file = b.path("examples/type_reflection_example.zig"),
        .target = target,
        .optimize = optimize,
    });
    type_reflection_module.addImport("metaprogramming", metaprogramming_module);
    const type_reflection_example = b.addExecutable(.{
        .name = "type_reflection_example",
        .root_module = type_reflection_module,
    });
    b.installArtifact(type_reflection_example);

    const functoid_module = b.createModule(.{
        .root_source_file = b.path("examples/functoid_example.zig"),
        .target = target,
        .optimize = optimize,
    });
    functoid_module.addImport("metaprogramming", metaprogramming_module);
    const functoid_example = b.addExecutable(.{
        .name = "functoid_example",
        .root_module = functoid_module,
    });
    b.installArtifact(functoid_example);

    // Run examples
    const run_structured_logging = b.addRunArtifact(structured_logging_example);
    const run_type_reflection = b.addRunArtifact(type_reflection_example);
    const run_functoid = b.addRunArtifact(functoid_example);

    const run_examples_step = b.step("run-examples", "Run all examples");
    run_examples_step.dependOn(&run_structured_logging.step);
    run_examples_step.dependOn(&run_type_reflection.step);
    run_examples_step.dependOn(&run_functoid.step);
}
