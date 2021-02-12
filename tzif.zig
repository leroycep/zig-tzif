const std = @import("std");
const testing = std.testing;

pub const TimeZone = struct {
    allocator: *std.mem.Allocator,
    version: Version,
    transitionTimes: []i64,
    transitionTypes: []u8,
    localTimeTypes: []LocalTimeType,
    designations: []u8,
    leapSeconds: []LeapSecond,
    transitionIsStd: []bool,
    transitionIsUT: []bool,
    string: []u8,

    pub fn deinit(this: @This()) void {
        this.allocator.free(this.transitionTimes);
        this.allocator.free(this.transitionTypes);
        this.allocator.free(this.localTimeTypes);
        this.allocator.free(this.designations);
        this.allocator.free(this.leapSeconds);
        this.allocator.free(this.transitionIsStd);
        this.allocator.free(this.transitionIsUT);
        this.allocator.free(this.string);
    }

    fn findTransitionTime(this: @This(), utc: i64) ?usize {
        var left: usize = 0;
        var right: usize = this.transitionTimes.len;

        while (left < right) {
            // Avoid overflowing in the midpoint calculation
            const mid = left + (right - left) / 2;
            // Compare the key with the midpoint element
            if (this.transitionTimes[mid] == utc) {
                if (mid + 1 < this.transitionTimes.len) {
                    return mid;
                } else {
                    return null;
                }
            } else if (this.transitionTimes[mid] > utc) {
                right = mid;
            } else if (this.transitionTimes[mid] < utc) {
                left = mid + 1;
            }
        }

        if (right == this.transitionTimes.len) {
            return null;
        } else if (right > 0) {
            return right - 1;
        } else {
            return 0;
        }
    }

    pub const ConversionResult = struct {
        timestamp: i64,
        offset: i32,
        dst: bool,
        designation: [:0]const u8,
    };

    pub fn localTimeFromUTC(this: @This(), utc: i64) ConversionResult {
        if (this.findTransitionTime(utc)) |idx| {
            std.log.warn("trans[{}] = {}", .{ idx, this.transitionTimes[idx] });
            const transition_type = this.transitionTypes[idx];
            std.log.warn("trans_type[{}] = {}", .{ idx, transition_type });
            const local_time_type = this.localTimeTypes[transition_type];
            std.log.warn("localTimeTypes[{}] = {}", .{ transition_type, local_time_type });

            var designation = this.designations[local_time_type.idx .. this.designations.len - 1 :0];
            for (designation) |c, i| {
                if (c == 0) {
                    designation = designation[0..i :0];
                    break;
                }
            }

            return ConversionResult{
                .timestamp = utc + local_time_type.utoff,
                .offset = local_time_type.utoff,
                .dst = local_time_type.dst,
                .designation = designation,
            };
        } else {
            // Base offset on the TZ string
            unreachable;
        }
    }
};

pub const Version = enum(u8) {
    V1 = 0,
    V2 = '2',
    V3 = '3',

    pub fn timeSize(this: @This()) u32 {
        return switch (this) {
            .V1 => 4,
            .V2, .V3 => 8,
        };
    }

    pub fn leapSize(this: @This()) u32 {
        return this.timeSize() + 4;
    }

    pub fn string(this: @This()) []const u8 {
        return switch (this) {
            .V1 => "1",
            .V2 => "2",
            .V3 => "3",
        };
    }
};

pub const LocalTimeType = struct {
    utoff: i32,
    /// Indicates whether this local time is Daylight Saving Time
    dst: bool,
    idx: u8,
};

pub const LeapSecond = struct {
    occur: i64,
    corr: i32,
};

/// This is based on Posix definition of the TZ environment variable
pub const PosixTZ = struct {
    std: []const u8,
    std_offset: i32,
    dst: ?[]const u8 = null,
    /// This field is ignored when dst is null
    dst_offset: i32 = 0,
    start: ?Rule = null,
    end: ?Rule = null,

    pub const Rule = union(enum) {
        /// 1 <= JulianDay1 <= 365. Leap days are not counted and are impossible to refer to
        JulianDay1: u16,
        /// 0 <= JulianDay0 <= 365. Leap days are counted, and can be referred to.
        JulianDay0: u16,
        MonthWeekDay: struct {
            /// 1 <= m <= 12
            m: u8,
            /// 1 <= n <= 5
            n: u8,
            /// 0 <= n <= 6
            d: u8,
        },
    };
};

const TIME_TYPE_SIZE = 6;

pub const TZifHeader = struct {
    version: Version,
    isutcnt: u32,
    isstdcnt: u32,
    leapcnt: u32,
    timecnt: u32,
    typecnt: u32,
    charcnt: u32,

    pub fn dataSize(this: @This(), dataBlockVersion: Version) u32 {
        return this.timecnt * dataBlockVersion.timeSize() +
            this.timecnt +
            this.typecnt * TIME_TYPE_SIZE +
            this.charcnt +
            this.leapcnt * dataBlockVersion.leapSize() +
            this.isstdcnt +
            this.isutcnt;
    }
};

pub fn parseHeader(reader: anytype, seekableStream: anytype) !TZifHeader {
    var magic_buf: [4]u8 = undefined;
    try reader.readNoEof(&magic_buf);
    if (!std.mem.eql(u8, "TZif", &magic_buf)) {
        return error.InvalidFormat;
    }

    // Check verison
    const version = reader.readEnum(Version, .Little) catch |err| switch (err) {
        error.InvalidValue => return error.UnsupportedVersion,
        else => |e| return e,
    };
    if (version == .V1) {
        return error.UnsupportedVersion;
    }

    // Seek past reserved bytes
    try seekableStream.seekBy(15);

    return TZifHeader{
        .version = version,
        .isutcnt = try reader.readInt(u32, .Big),
        .isstdcnt = try reader.readInt(u32, .Big),
        .leapcnt = try reader.readInt(u32, .Big),
        .timecnt = try reader.readInt(u32, .Big),
        .typecnt = try reader.readInt(u32, .Big),
        .charcnt = try reader.readInt(u32, .Big),
    };
}

fn hhmmss_offset_to_s(_string: []const u8, idx: *usize) !i32 {
    var string = _string;
    var sign: i2 = 1;
    if (string[0] == '+') {
        sign = 1;
        string = string[1..];
        idx.* += 1;
    } else if (string[0] == '-') {
        sign = -1;
        string = string[1..];
        idx.* += 1;
    }

    for (string) |c, i| {
        if (!(std.ascii.isDigit(c) or c == ':')) {
            string = string[0..i];
            break;
        }
        idx.* += 1;
    }

    var result: i32 = 0;

    var segment_iter = std.mem.split(string, ":");
    const hour_string = segment_iter.next() orelse return error.EmptyString;
    const hours = try std.fmt.parseInt(u32, hour_string, 10);
    if (hours > 24) return error.InvalidFormat;
    result += std.time.s_per_hour * @intCast(i32, hours);

    if (segment_iter.next()) |minute_string| {
        const minutes = try std.fmt.parseInt(u32, minute_string, 10);
        if (minutes > 59) return error.InvalidFormat;
        result += std.time.s_per_min * @intCast(i32, minutes);
    }

    if (segment_iter.next()) |second_string| {
        const seconds = try std.fmt.parseInt(u8, second_string, 10);
        if (seconds > 59) return error.InvalidFormat;
        result += seconds;
    }

    return result * sign;
}

fn parsePosixTZ_rule(string: []const u8) !PosixTZ.Rule {
    if (string.len < 2) return error.InvalidFormat;
    if (string[0] == 'J') {
        const julian_day1 = try std.fmt.parseInt(u16, string[1..], 10);
        if (julian_day1 < 1 or julian_day1 > 365) return error.InvalidFormat;
        return PosixTZ.Rule{ .JulianDay1 = julian_day1 };
    } else if (std.ascii.isDigit(string[0])) {
        const julian_day0 = try std.fmt.parseInt(u16, string[0..], 10);
        if (julian_day0 > 365) return error.InvalidFormat;
        return PosixTZ.Rule{ .JulianDay0 = julian_day0 };
    } else if (string[0] == 'M') {
        var split_iter = std.mem.split(string[1..], ".");
        const m_str = split_iter.next() orelse return error.InvalidFormat;
        const n_str = split_iter.next() orelse return error.InvalidFormat;
        const d_str = split_iter.next() orelse return error.InvalidFormat;

        const m = try std.fmt.parseInt(u8, m_str, 10);
        const n = try std.fmt.parseInt(u8, n_str, 10);
        const d = try std.fmt.parseInt(u8, d_str, 10);

        if (m < 1 or m > 12) return error.InvalidFormat;
        if (n < 1 or n > 5) return error.InvalidFormat;
        if (d > 6) return error.InvalidFormat;

        return PosixTZ.Rule{ .MonthWeekDay = .{ .m = m, .n = n, .d = d } };
    } else {
        return error.InvalidFormat;
    }
}

pub fn parsePosixTZ(string: []const u8) !PosixTZ {
    var result = PosixTZ{ .std = undefined, .std_offset = undefined };
    var idx: usize = 0;

    // TODO: handle quoted designations
    while (idx < string.len) : (idx += 1) {
        if (!std.ascii.isAlpha(string[idx])) {
            result.std = string[0..idx];
            break;
        }
    }

    result.std_offset = try hhmmss_offset_to_s(string[idx..], &idx);
    if (idx >= string.len) {
        return result;
    }

    if (string[idx] != ',') {
        // TODO: handle quoted designations
        const start_dst_designation = idx;
        while (idx < string.len) : (idx += 1) {
            if (!std.ascii.isAlpha(string[idx])) {
                result.dst = string[start_dst_designation..idx];
                break;
            }
        }
        if (idx < string.len and string[idx] != ',') {
            result.dst_offset = try hhmmss_offset_to_s(string[idx..], &idx);
        } else {
            result.dst_offset = result.std_offset + std.time.s_per_hour;
        }

        if (idx >= string.len) {
            return result;
        }
    }

    std.debug.assert(string[idx] == ',');
    idx += 1;

    if (std.mem.indexOf(u8, string[idx..], ",")) |_end_of_start_rule| {
        const end_of_start_rule = idx + _end_of_start_rule;
        result.start = try parsePosixTZ_rule(string[idx..end_of_start_rule]);
        result.end = try parsePosixTZ_rule(string[end_of_start_rule + 1 ..]);
    } else {
        result.start = try parsePosixTZ_rule(string[idx..]);
    }

    return result;
}

pub fn parse(allocator: *std.mem.Allocator, reader: anytype, seekableStream: anytype) !TimeZone {
    const v1_header = try parseHeader(reader, seekableStream);
    try seekableStream.seekBy(v1_header.dataSize(.V1));

    const v2_header = try parseHeader(reader, seekableStream);

    // Parse transition times
    var transition_times = try allocator.alloc(i64, v2_header.timecnt);
    errdefer allocator.free(transition_times);
    {
        var prev: i64 = -(2 << 59); // Earliest time supported, this is earlier than the big bang
        var i: usize = 0;
        while (i < transition_times.len) : (i += 1) {
            transition_times[i] = try reader.readInt(i64, .Big);
            if (transition_times[i] <= prev) {
                return error.InvalidFormat;
            }
            prev = transition_times[i];
        }
    }

    // Parse transition types
    var transition_types = try allocator.alloc(u8, v2_header.timecnt);
    errdefer allocator.free(transition_types);
    try reader.readNoEof(transition_types);
    for (transition_types) |transition_type| {
        if (transition_type >= v2_header.typecnt) {
            return error.InvalidFormat; // a transition type index is out of bounds
        }
    }

    // Parse local time type records
    var local_time_types = try allocator.alloc(LocalTimeType, v2_header.typecnt);
    errdefer allocator.free(local_time_types);
    {
        var i: usize = 0;
        while (i < local_time_types.len) : (i += 1) {
            local_time_types[i].utoff = try reader.readInt(i32, .Big);
            local_time_types[i].dst = switch (try reader.readByte()) {
                0 => false,
                1 => true,
                else => return error.InvalidFormat,
            };

            local_time_types[i].idx = try reader.readByte();
            if (local_time_types[i].idx >= v2_header.charcnt) {
                return error.InvalidFormat;
            }
        }
    }

    // Read designations
    var time_zone_designations = try allocator.alloc(u8, v2_header.charcnt);
    errdefer allocator.free(time_zone_designations);
    try reader.readNoEof(time_zone_designations);

    // Parse leap seconds records
    var leap_seconds = try allocator.alloc(LeapSecond, v2_header.leapcnt);
    errdefer allocator.free(leap_seconds);
    {
        var i: usize = 0;
        while (i < leap_seconds.len) : (i += 1) {
            leap_seconds[i].occur = try reader.readInt(i64, .Big);
            if (i == 0 and leap_seconds[i].occur < 0) {
                return error.InvalidFormat;
            } else if (leap_seconds[i].occur - leap_seconds[i - 1].occur < 2419199) {
                return error.InvalidFormat; // There must be at least 28 days worth of seconds between leap seconds
            }

            leap_seconds[i].corr = try reader.readInt(i32, .Big);
            if (i == 0 and (leap_seconds[i].corr != 1 or leap_seconds[i].corr != -1)) {
                return error.InvalidFormat;
            } else {
                const diff = leap_seconds[i].corr - leap_seconds[i - 1].corr;
                if (diff != 1 or diff != -1) return error.InvalidFormat;
            }
        }
    }

    // Parse standard/wall indicators
    var transition_is_std = try allocator.alloc(bool, v2_header.isstdcnt);
    errdefer allocator.free(transition_is_std);
    {
        var i: usize = 0;
        while (i < transition_is_std.len) : (i += 1) {
            transition_is_std[i] = switch (try reader.readByte()) {
                1 => true,
                0 => false,
                else => return error.InvalidFormat,
            };
        }
    }

    // Parse UT/local indicators
    var transition_is_ut = try allocator.alloc(bool, v2_header.isutcnt);
    errdefer allocator.free(transition_is_ut);
    {
        var i: usize = 0;
        while (i < transition_is_ut.len) : (i += 1) {
            transition_is_ut[i] = switch (try reader.readByte()) {
                1 => true,
                0 => false,
                else => return error.InvalidFormat,
            };
        }
    }

    // Parse TZ string from footer
    // TODO: validate according to RFC 8536 section 3.3.1
    if ((try reader.readByte()) != '\n') return error.InvalidFormat;
    const tz_string = try reader.readUntilDelimiterAlloc(allocator, '\n', 60);
    errdefer allocator.free(tz_string);

    return TimeZone{
        .allocator = allocator,
        .version = v2_header.version,
        .transitionTimes = transition_times,
        .transitionTypes = transition_types,
        .localTimeTypes = local_time_types,
        .designations = time_zone_designations,
        .leapSeconds = leap_seconds,
        .transitionIsStd = transition_is_std,
        .transitionIsUT = transition_is_ut,
        .string = tz_string,
    };
}

test "parse invalid bytes" {
    var fbs = std.io.fixedBufferStream("dflkasjreklnlkvnalkfek");
    testing.expectError(error.InvalidFormat, parse(std.testing.allocator, fbs.reader(), fbs.seekableStream()));
}

test "parse UTC zoneinfo" {
    var fbs = std.io.fixedBufferStream(@embedFile("zoneinfo/UTC"));

    const res = try parse(std.testing.allocator, fbs.reader(), fbs.seekableStream());
    defer res.deinit();

    testing.expectEqual(Version.V2, res.version);
    testing.expectEqualSlices(i64, &[_]i64{}, res.transitionTimes);
    testing.expectEqualSlices(u8, &[_]u8{}, res.transitionTypes);
    testing.expectEqualSlices(LocalTimeType, &[_]LocalTimeType{.{ .utoff = 0, .dst = false, .idx = 0 }}, res.localTimeTypes);
    testing.expectEqualSlices(u8, "UTC\x00", res.designations);
}

test "parse Pacific/Honolulu zoneinfo and calculate local times" {
    const transition_times = [7]i64{ -2334101314, -1157283000, -1155436200, -880198200, -769395600, -765376200, -712150200 };
    const transition_types = [7]u8{ 1, 2, 1, 3, 4, 1, 5 };
    const local_time_types = [6]LocalTimeType{
        .{ .utoff = -37886, .dst = false, .idx = 0 },
        .{ .utoff = -37800, .dst = false, .idx = 4 },
        .{ .utoff = -34200, .dst = true, .idx = 8 },
        .{ .utoff = -34200, .dst = true, .idx = 12 },
        .{ .utoff = -34200, .dst = true, .idx = 16 },
        .{ .utoff = -36000, .dst = false, .idx = 4 },
    };
    const designations = "LMT\x00HST\x00HDT\x00HWT\x00HPT\x00";
    const is_std = &[6]bool{ false, false, false, false, true, false };
    const is_ut = &[6]bool{ false, false, false, false, true, false };
    const string = "HST10";

    var fbs = std.io.fixedBufferStream(@embedFile("zoneinfo/Pacific/Honolulu"));

    const res = try parse(std.testing.allocator, fbs.reader(), fbs.seekableStream());
    defer res.deinit();

    testing.expectEqual(Version.V2, res.version);
    testing.expectEqualSlices(i64, &transition_times, res.transitionTimes);
    testing.expectEqualSlices(u8, &transition_types, res.transitionTypes);
    testing.expectEqualSlices(LocalTimeType, &local_time_types, res.localTimeTypes);
    testing.expectEqualSlices(u8, designations, res.designations);
    testing.expectEqualSlices(bool, is_std, res.transitionIsStd);
    testing.expectEqualSlices(bool, is_ut, res.transitionIsUT);
    testing.expectEqualSlices(u8, string, res.string);

    {
        const conversion = res.localTimeFromUTC(-1156939200);
        testing.expectEqual(@as(i64, -1156973400), conversion.timestamp);
        testing.expectEqual(true, conversion.dst);
        testing.expectEqualSlices(u8, "HDT", conversion.designation);
    }
    //{
    //    const conversion = res.localTimeFromUTC(1546300800);
    //    testing.expectEqual(@as(i64, 1546264800), conversion.timestamp);
    //    testing.expectEqual(false, conversion.dst);
    //    testing.expectEqualSlices(u8, "HST", conversion.designation);
    //}
}

test "posix TZ string" {
    const result = try parsePosixTZ("MST7MDT,M3.2.0,M11.1.0");

    testing.expectEqualSlices(u8, "MST", result.std);
    testing.expectEqual(@as(i32, 25200), result.std_offset);
    testing.expectEqualSlices(u8, "MDT", result.dst.?);
    testing.expectEqual(@as(i32, 28800), result.dst_offset);
    testing.expectEqual(PosixTZ.Rule{ .MonthWeekDay = .{ .m = 3, .n = 2, .d = 0 } }, result.start.?);
    testing.expectEqual(PosixTZ.Rule{ .MonthWeekDay = .{ .m = 11, .n = 1, .d = 0 } }, result.end.?);
}
