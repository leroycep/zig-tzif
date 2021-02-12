const std = @import("std");
const tzif = @import("tzif");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;

    const cwd = std.fs.cwd();

    const localtime_file = try cwd.openFile("/etc/localtime", .{});
    const localtime = try tzif.parse(allocator, localtime_file.reader(), localtime_file.seekableStream());
    defer localtime.deinit();

    std.log.info("TZ string: {s}", .{localtime.string});
    std.log.info("TZif version: {s}", .{localtime.version.string()});
    std.log.info("{} transition times", .{localtime.transitionTimes.len});
    std.log.info("{} leap seconds", .{localtime.leapSeconds.len});
}
