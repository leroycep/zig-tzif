const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("tzif", "tzif.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("tzif.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const target = b.standardTargetOptions(.{});
    const dump_exe = b.addExecutable("dump", "examples/dump.zig");
    dump_exe.addPackagePath("tzif", "tzif.zig");
    dump_exe.setBuildMode(mode);
    dump_exe.setTarget(target);

    const run_dump_example = dump_exe.run();
    
    const run_dump_example_step = b.step("example-dump", "Run the `dump` example");
    run_dump_example_step.dependOn(&run_dump_example.step);
}
