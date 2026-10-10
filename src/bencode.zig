const std = @import("std");
const assert = std.debug.assert;
const Type = std.lang.Type;
const print = std.debug.print;
const Reader = std.Io.Reader;
const Writer = std.Io.Writer;
const Allocator = std.mem.Allocator;

pub const Error = error{
    MalformedBuffer,
    Empty,
    UnexpectedSign,
    NotSupported,
    LeadingZero,
    MissingColon,
    StringWrongLen,
    InvalidCharacter,
    OutOfBounds,
    NonExhaustiveEnum,
    InvalidEnum,
    UnknownKey,
    UnsortedKeys,
    DuplicateKey,
    MissingField,
};

/// Get the size that a value have when encoded.
fn size_of(comptime value: anytype) !usize {
    comptime return switch (@typeInfo(@TypeOf(value))) {
        inline .int => {
            var buf_len = 2 + get_num_digits(@abs(value));
            if (value < 0) {
                buf_len += 1;
            }
            return buf_len;
        },
        inline .@"enum" => size_of(@intFromEnum(value)),
        inline .@"struct" => |stru| {
            var count: usize = 2;
            for (stru.field_types) |t| {
                count += try size_of(t);
            }
            for (stru.field_names) |names| {
                for (names) |name| {
                    count += try size_of(name);
                }
            }
            return count;
        },
        inline .array => |arr| {
            // string
            if (arr.child == u8) {
                return get_num_digits(arr.len) + 1 + arr.len;
            }
            // list
            var count: usize = 2;
            for (value) |v| {
                count += try size_of(v);
            }
            return count;
        },
        inline .pointer => |p| {
            // if (p.size == .one) {
            //     return size_of(p.child);
            // }
            if (p.child == u8) return get_num_digits(value.len) + 1 + value.len;
            return switch (@typeInfo(p.child)) {
                inline .array => |arr| {
                    return get_num_digits(arr.len) + 1 + arr.len;
                },
                inline .int => {
                    // string [:0]u8 []u8
                    if (p.child == u8) {
                        return get_num_digits(value.len) + 1 + value.len;
                    }
                    // list
                    var count: usize = 2;
                    for (value) |v| {
                        count += try size_of(v);
                    }
                    return count;
                },
                else => Error.NotSupported,
            };
        },
        inline else => Error.NotSupported,
    };
}

pub fn encode(comptime value: anytype, w: *Writer) !void {
    return switch (@typeInfo(@TypeOf(value))) {
        inline .int => try encode_int(value, w),
        inline .@"enum" => try encode_int(@intFromEnum(value), w),
        inline .@"struct" => try encode_dict(value, w),
        inline .array => |arr| {
            // [32]u8 = string [:0]u8 = string
            if (arr.child == u8) return try encode_str(&value, w);
            // [12]u32 [8]Dict
            return try encode_list(&value, w);
        },
        inline .pointer => |p| {
            // []u8
            if (p.child == u8) return try encode_str(&value.*, w);

            return switch (@typeInfo(p.child)) {
                inline .array => |arr| {
                    if (arr.child == u8) return try encode_str(&value.*, w);
                    return try encode_list(&value.*, w);
                },
                inline .int => |int| {
                    if (int.bits == u8) return try encode_str(value, w);
                    return try encode_list(value, w);
                },
                else => Error.NotSupported,
            };
        },
        inline else => Error.NotSupported,
    };
}

pub fn decode(comptime T: type, r: *Reader) !T {
    return switch (@typeInfo(T)) {
        inline .int => try decode_int(T, r),
        inline .@"enum" => |en| {
            if (en.mode == .nonexhaustive) return Error.NonExhaustiveEnum;
            return std.enums.fromInt(
                T,
                try decode_int(en.tag_type, r),
            ) orelse Error.InvalidEnum;
        },
        inline .@"struct" => try decode_dict(T, r),
        inline .array => |arr| {
            if (arr.sentinel() != null) return Error.NotSupported;
            // [32]u8 = string [:0]u8 = string
            if (arr.child == u8) return try decode_str_arr(arr, r);
            // [12]u32 [8]Dict
            return decode_list_arr(arr, r);
        },
        inline .pointer => |p| {
            // []u8
            if (p.child == u8) return try decode_str(r);
            // *T
            return switch (@typeInfo(p.child)) {
                // []u8
                inline .array => |arr| {
                    if (arr.child == u8) return try decode_str(r);
                    return try decode_list_arr(arr, r);
                },
                // [*]u8
                inline .int => |int| {
                    if (int.bits == u8) return try decode_str(r);
                    return Error.NotSupported;
                },
                else => Error.NotSupported,
            };
        },
        inline else => Error.NotSupported,
    };
}

fn encode_list(comptime value: anytype, w: *Writer) !void {
    try w.writeByte('l');
    inline for (value) |f| {
        try encode(f, w);
    }
    try w.writeByte('e');
}

fn encode_dict(comptime value: anytype, w: *Writer) !void {
    const Str = @typeInfo(@TypeOf(value)).@"struct";
    try w.writeByte('d');
    inline for (Str.field_names) |name| {
        try encode_str(name, w);
        try encode(@field(value, name), w);
    }
    try w.writeByte('e');
}

fn encode_str(value: []const u8, w: *Writer) !void {
    try w.printIntAny(value.len, 10, .lower, .{});
    try w.writeByte(':');
    try w.writeAll(value);
}

fn encode_int(value: anytype, w: *Writer) !void {
    try w.writeByte('i');
    try w.printIntAny(value, 10, .lower, .{});
    try w.writeByte('e');
}

/// Dicts are encoded as `d<str><val>e`.
/// For example: `d3:fooi2ee`.
fn decode_dict(comptime T: type, r: *Reader) !T {
    if (r.seek >= r.buffer.len or r.buffer[r.seek] != 'd') return Error.MalformedBuffer;

    var dict: T = undefined;
    var seen: std.EnumSet(std.meta.FieldEnum(T)) = .empty;
    const s = @typeInfo(T).@"struct";
    var prev_key: ?[]const u8 = null;

    // cursor at d3:foo
    //           ^
    r.toss(1);
    // cursor at d3:foo
    //            ^

    while (r.buffer[r.seek] != 'e') {
        const key = try decode_str(r);
        // cursor at d3:foo
        //             ^

        // advance into the value's first byte
        // cursor at d3:foo
        //              ^
        r.toss(1);

        if (prev_key) |pk| {
            // if (std.mem.order(u8, pk, key) != .lt) return Error.UnsortedKeys;
            if (std.mem.order(u8, pk, key) == .gt) return Error.UnsortedKeys;
        }
        prev_key = key;

        found: {
            inline for (s.field_names, s.field_types) |n, ft| {
                if (std.mem.eql(u8, n, key)) {
                    const F = std.meta.FieldEnum(T);
                    if (seen.contains(@field(F, n))) return Error.DuplicateKey;
                    seen.insert(@field(F, n));
                    @field(dict, n) = try decode(ft, r);
                    break :found;
                }
            }
            return Error.UnknownKey;
        }

        if (r.seek + 1 >= r.buffer.len) return Error.MalformedBuffer;
        // advance to next key's first byte, or to the dict's 'e'
        // cursor at 3:foo
        //           ^
        r.toss(1);
    }

    if (seen.count() != s.field_names.len) return Error.MissingField;
    return dict;
}

/// Lists are encoded as `l<elements>e`.
/// For example: `l3:foo:bare`.
fn decode_list_arr(comptime Arr: Type.Array, r: *Reader) ![Arr.len]Arr.child {
    const data = r.buffer[r.seek..];

    // empty list
    if (std.mem.eql(u8, data, "le")) {
        r.toss(1);
        return std.mem.zeroes([Arr.len]Arr.child);
    }

    if (data.len < 3) {
        return Error.MalformedBuffer;
    }

    if (data[0] != 'l' or data[data.len - 1] != 'e') {
        return Error.MalformedBuffer;
    }

    var arr: [Arr.len]Arr.child = undefined;

    inline for (0..Arr.len) |i| {
        // skip the last byte of the prev structure and
        // enter the current one
        r.toss(1);
        arr[i] = try decode(Arr.child, r);
    }

    r.toss(1);
    return arr;
}

/// Zero-copy string decoding, just return a slice into `r`.
fn decode_str(r: *Reader) ![]u8 {
    const colon = std.mem.findScalar(
        u8,
        r.buffer[r.seek..],
        ':',
    ) orelse return Error.MissingColon;
    if (colon == 0) return Error.MalformedBuffer;
    const len = try std.fmt.parseInt(usize, r.buffer[r.seek..][0..colon], 10);
    const data_start = r.seek + colon + 1;
    if (data_start + len > r.buffer.len) return Error.MalformedBuffer;
    const str =
        r.buffer[r.seek + colon + 1 .. r.seek + colon + len + 1];
    r.toss(colon + len);
    return str;
}

/// Decode a byte str, encoded as `<length>:<contents>`.
/// `<length>` is specified in bytes, not characters.
/// For example: `6:italia`.
fn decode_str_arr(comptime Arr: Type.Array, r: *Reader) ![Arr.len]u8 {
    if (r.buffer[r.seek..].len < 2) {
        return Error.MalformedBuffer;
    }
    // empty string
    if (std.mem.eql(u8, r.buffer[r.seek..], "0:")) {
        return std.mem.zeroes([Arr.len]Arr.child);
    }
    const colon = std.mem.find(
        u8,
        r.buffer[r.seek..],
        ":",
    ) orelse return Error.MissingColon;
    const len_slice = r.buffer[r.seek .. r.seek + colon];
    const len = try std.fmt.parseInt(usize, len_slice, 10);
    if (len < Arr.len) {
        return Error.OutOfBounds;
    }
    const str =
        r.buffer[r.seek + colon + 1 .. r.seek + colon + len + 1];
    r.toss(colon + len);
    var arr: [Arr.len]u8 = undefined;
    @memcpy(&arr, str);
    return arr;
}

/// Decode an integer, encoded as `i<base10 integer>e`.
/// For example: `i123e`
fn decode_int(comptime T: type, r: *Reader) !T {
    if (r.buffer[r.seek..].len < 3) {
        return Error.MalformedBuffer;
    }

    const Int = @typeInfo(T).int;
    const U = @Int(.unsigned, Int.bits);

    var result: U = 0;
    var seen_digit = false;
    var leading_zero = false;
    var negate = false;

    sw: switch (r.buffer[r.seek]) {
        'i' => {
            r.toss(1);
            continue :sw r.buffer[r.seek];
        },
        '0' => {
            if (leading_zero) return Error.LeadingZero;
            if (negate and !seen_digit) return Error.LeadingZero;
            if (!seen_digit) leading_zero = true;
            seen_digit = true;
            result = try std.math.mul(U, result, 10);
            if (r.seek + 1 == r.buffer.len) return Error.OutOfBounds;
            r.toss(1);
            continue :sw r.buffer[r.seek];
        },
        '1'...'9' => |c| {
            if (leading_zero) return Error.LeadingZero;
            seen_digit = true;
            result = try std.math.mul(U, result, 10);
            result = try std.math.add(U, result, c - '0');
            if (r.seek + 1 == r.buffer.len) return Error.OutOfBounds;
            r.toss(1);
            continue :sw r.buffer[r.seek];
        },
        '-' => {
            if (Int.signedness == .unsigned) {
                return Error.UnexpectedSign;
            }
            negate = true;
            if (r.seek + 1 == r.buffer.len) return Error.OutOfBounds;
            r.toss(1);
            continue :sw r.buffer[r.seek];
        },
        'e' => break :sw,
        else => return Error.InvalidCharacter,
    }

    if (Int.signedness == .signed) {
        if (!negate) return std.math.cast(T, result) orelse error.Overflow;
        const limit: U = @as(U, 1) << (Int.bits - 1);
        if (result > limit) return error.Overflow;
        if (result == limit) return std.math.minInt(T);
        return -@as(T, @intCast(result));
    }
    return @intCast(result);
}

/// Return the number of digits of a number.
/// Example:
/// ```zig
/// const n = get_enc_size(123);
/// try std.testing.expect(n == 3);
/// ```
fn get_num_digits(comptime n: usize) usize {
    if (n == 0) return 1;
    var enc_size: usize = 0;
    var v = n;
    while (v > 0) {
        enc_size += 1;
        v = @divTrunc(v, 10);
    }
    return enc_size;
}

const expect = std.testing.expect;

// ------
// encode
// ------

test "encode_dict" {
    const MyDict = struct {
        foo: u8,
        bar: u32,
        zip: [7]u8,
    };
    const v = MyDict{
        .foo = 3,
        .bar = 321,
        .zip = .{ 'a', 'v', 'o', 'c', 'a', 'd', 'o' },
    };
    // TODO: wait for Zig's bug to be fixed
    // const len = try size_of(v);
    var buffer: [34]u8 = undefined;
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "d3:fooi3e3:bari321e3:zip7:avocadoe"));
}

test "encode_list_1" {
    const v = [2]u32{ 0, 2 };
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 8);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "li0ei2ee"));
}

test "encode_list_2" {
    const v = [2]i8{ 0, 2 };
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 8);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "li0ei2ee"));
}

test "encode_list_3" {
    const v = [4]i8{ 'w', 'i', 'l', 'l' };
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 22);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "li119ei105ei108ei108ee"));
}

test "encode_list_4" {
    const v = [4]u32{ 119, 105, 108, 108 };
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 22);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "li119ei105ei108ei108ee"));
}

test "encode_str_1" {
    const v = "avocado";
    var buffer: [9]u8 = undefined;
    try expect(buffer.len == 9);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "7:avocado"));
}

test "encode_str_2" {
    const v = "iwillhave11";
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 14);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "11:iwillhave11"));
}

test "encode_enum" {
    const MyEnum = enum(u8) {
        Core,
        ExtHandshake,
        Metainfo,
    };
    const v = MyEnum.ExtHandshake;
    var buffer: [try size_of(MyEnum.ExtHandshake)]u8 = undefined;
    try expect(buffer.len == 3);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "i1e"));
}

test "encode_int" {
    const v: u8 = 255;
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 5);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "i255e"));
}

test "encode_int_2" {
    const v: i8 = 30;
    var buffer: [try size_of(v)]u8 = undefined;
    var w = Writer.fixed(&buffer);
    try expect(buffer.len == 4);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "i30e"));
}

test "encode_int_3" {
    const v: i8 = -30;
    var buffer: [try size_of(v)]u8 = undefined;
    try expect(buffer.len == 5);
    var w = Writer.fixed(&buffer);
    try encode(v, &w);
    try expect(std.mem.eql(u8, &buffer, "i-30e"));
}

// ------
// decode
// ------

test "decode_enum_1" {
    const MyEnum = enum(u8) {
        Core,
        ExtHandshake,
        Metainfo,
    };
    const encoded = "i1e";
    var r = Reader.fixed(encoded);
    const num = try decode(MyEnum, &r);
    try expect(num == .ExtHandshake);
    try expect(r.seek == encoded.len - 1);
}

test "decode_enum_2" {
    const MyEnum = enum(u8) {
        Core,
        ExtHandshake,
        Metainfo,
    };
    const encoded = "i3e";
    var r = Reader.fixed(encoded);
    const err = decode(MyEnum, &r);
    try expect(err == Error.InvalidEnum);
}

test "decode_dict_1" {
    const MyDict = struct {
        foo: u8,
    };
    const encoded = "d3:fooi3ee";
    var r = Reader.fixed(encoded);
    const s = try decode(MyDict, &r);
    try expect(s.foo == 3);
    try expect(r.seek == encoded.len - 1);
}

test "decode_dict_2" {
    const MyDict = extern struct {
        foo: u8,
        ber: u32,
        zip: [7]u8,
    };
    const encoded = "d3:beri321e3:fooi3e3:zip7:avocadoe";
    var r = Reader.fixed(encoded);
    const s = try decode(MyDict, &r);
    try expect(s.foo == 3);
    try expect(s.ber == 321);
    try expect(std.mem.eql(u8, &s.zip, "avocado"));
    try expect(r.seek == encoded.len - 1);
}

test "decode_dict_unsorted_1" {
    const MyDict = extern struct {
        foo: u8,
        bar: u32,
        zip: [7]u8,
    };
    const encoded = "d3:fooi3e3:bari321e3:zip7:avocadoe";
    var r = Reader.fixed(encoded);
    const err = decode(MyDict, &r);
    try expect(err == Error.UnsortedKeys);
}

test "decode_dict_dupe_1" {
    const MyDict = extern struct {
        foo: u8,
        bar: u32,
        zip: [7]u8,
    };
    const encoded = "d3:bari321e3:bari321e3:fooi3e3:zip7:avocadoe";
    var r = Reader.fixed(encoded);
    const err = decode(MyDict, &r);
    try expect(err == Error.DuplicateKey);
}

test "decode_dict_missing_1" {
    const MyDict = extern struct {
        foo: u8,
        bar: u32,
        zip: [7]u8,
    };
    const encoded = "d3:bari321e3:fooi3ee";
    var r = Reader.fixed(encoded);
    const err = decode(MyDict, &r);
    try expect(err == Error.MissingField);
}

test "decode_arr_u32_1" {
    const encoded = "li123ei8888ee";
    var r = Reader.fixed(encoded);
    const arr = try decode([2]u32, &r);
    try expect(arr[0] == 123);
    try expect(arr[1] == 8888);
    try expect(arr.len == 2);
    try expect(r.seek == encoded.len - 1);
}

test "decode_list_str_1" {
    const encoded = "l3:vvv3:fooe";
    var r = Reader.fixed(encoded);
    const buff = try decode([2][]u8, &r);
    try expect(std.mem.eql(u8, buff[0], "vvv"));
    try expect(std.mem.eql(u8, buff[1], "foo"));
    try expect(r.seek == encoded.len - 1);
}

test "decode_str_1" {
    const encoded = "3:hih";
    var r = Reader.fixed(encoded);
    const view = try decode([]u8, &r);
    try expect(std.mem.eql(u8, view, "hih"));
    try expect(r.seek == encoded.len - 1);
}

test "decode_str_2" {
    const encoded = "10:hihhihhihh";
    var r = Reader.fixed(encoded);
    const view = try decode([]u8, &r);
    try expect(std.mem.eql(u8, view, "hihhihhihh"));
    try expect(r.seek == encoded.len - 1);
}

test "decode_empty_str" {
    const encoded = "0:";
    var r = Reader.fixed(encoded);
    const view = try decode([]u8, &r);
    try expect(view.len == 0);
    try expect(std.mem.eql(u8, view, ""));
}

test "decode_arr_str_1" {
    const encoded = "3:hih";
    var r = Reader.fixed(encoded);
    const arr = try decode([3]u8, &r);
    try expect(std.mem.eql(u8, &arr, "hih"));
    try expect(r.seek == encoded.len - 1);
}

test "decode_empty_arr_1" {
    const encoded = "le";
    var r = Reader.fixed(encoded);
    const re = try decode([1]u32, &r);
    try expect(re[0] == 0);
    try expect(r.seek == encoded.len - 1);
}

test "decode_empty_arr_2" {
    const encoded = "le";
    var r = Reader.fixed(encoded);
    const re = try decode([0]u32, &r);
    try expect(re.len == 0);
    try expect(r.seek == encoded.len - 1);
}

// Test numbers not close to the max
test "decode_u8" {
    const encoded = "i50e";
    var r = Reader.fixed(encoded);
    const num = try decode(u8, &r);
    try expect(num == 50);
    try expect(r.seek == encoded.len - 1);
}

test "decode_i8" {
    const encoded = "i-50e";
    var r = Reader.fixed(encoded);
    const num = try decode(i8, &r);
    try expect(num == -50);
    try expect(r.seek == encoded.len - 1);
}

test "decode_u16" {
    const encoded = "i65535e";
    var r = Reader.fixed(encoded);
    const num = try decode(u16, &r);
    try expect(num == 65_535);
    try expect(r.seek == encoded.len - 1);
}

test "decode_i16_1" {
    const encoded = "i-32768e";
    var r = Reader.fixed(encoded);
    const num = try decode(i16, &r);
    try expect(num == -32_768);
    try expect(r.seek == encoded.len - 1);
}

// Test the max value of a signed int and that the minus sign is optional.
test "decode_i16_2" {
    const encoded = "i32767e";
    var r = Reader.fixed(encoded);
    const num = try decode(i16, &r);
    try expect(num == 32_767);
    try expect(r.seek == encoded.len - 1);
}

test "decode_u32" {
    const encoded = "i4294967295e";
    var r = Reader.fixed(encoded);
    const num = try decode(u32, &r);
    try expect(num == 4_294_967_295);
    try expect(r.seek == encoded.len - 1);
}

test "decode_i32" {
    const encoded = "i-2147483648e";
    var r = Reader.fixed(encoded);
    const num = try decode(i32, &r);
    try expect(num == -2_147_483_648);
    try expect(r.seek == encoded.len - 1);
}

test "decode_zero" {
    const encoded = "i0e";
    var r = Reader.fixed(encoded);
    const num = try decode(u8, &r);
    try expect(num == 0);
    try expect(r.seek == encoded.len - 1);
}

// Test errors

test "decode_leading_zero_1" {
    const encoded = "i02e";
    var r = Reader.fixed(encoded);
    const err = decode(u8, &r);
    try expect(err == Error.LeadingZero);
}

test "decode_leading_zero_2" {
    const encoded = "i-0e";
    var r = Reader.fixed(encoded);
    const err = decode(i8, &r);
    try expect(err == Error.LeadingZero);
}

test "decode_overflow_1" {
    const encoded = "i256e";
    var r = Reader.fixed(encoded);
    const err = decode(u8, &r);
    try expect(err == error.Overflow);
}

test "decode_overflow_2" {
    const encoded = "i-296e";
    var r = Reader.fixed(encoded);
    const err = decode(i8, &r);
    try expect(err == error.Overflow);
}

test "decode_non_int" {
    const encoded = "i12#e";
    var r = Reader.fixed(encoded);
    const err = decode(i8, &r);
    try expect(err == error.InvalidCharacter);
}

test "decode_int_missing_e" {
    const encoded = "i12";
    var r = Reader.fixed(encoded);
    const err = decode(i8, &r);
    try expect(err == Error.OutOfBounds);
}

test "decode_str_with_garbage" {
    // should ignore all the rest of the string
    const encoded = "2:higarbage";
    var r = Reader.fixed(encoded);
    const hi = try decode([]u8, &r);
    try expect(std.mem.eql(u8, hi, "hi"));
    try expect(r.seek == 3);
}

test "decode_str_no_len" {
    const encoded = ":hih";
    var r = Reader.fixed(encoded);
    const err = decode([]const u8, &r);
    try expect(err == error.MalformedBuffer);
}

test "decode_str_missing_colon" {
    const encoded = "10hihhihhihh";
    var r = Reader.fixed(encoded);
    const err = decode([]u8, &r);
    try expect(err == Error.MissingColon);
}
