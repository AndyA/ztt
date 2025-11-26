const TI = @This();

src: []const u8,

pos: u32 = 0,
line_number: u32 = 1,
line_start: u32 = 0,
state: enum {
    TEXT,
    START,
    EXPR,
    SEMI,
    COMMENT,
    BLOCK_COMMENT,
} = .TEXT,

token_start: Location = .{},

pub fn init(src: []const u8) TI {
    return TI{ .src = src };
}

fn markTokenStart(self: *TI) void {
    self.token_start = .{
        .line = self.line_number,
        .column = self.pos - self.line_start,
    };
}

pub fn getTokenStart(self: *const TI) Location {
    return self.token_start;
}

fn eof(self: *const TI) bool {
    return self.pos == self.src.len;
}

fn peek(self: *const TI) u8 {
    assert(!self.eof());
    return self.src[self.pos];
}

fn available(self: *const TI) usize {
    return self.src.len - self.pos;
}

fn slice(self: *const TI, len: usize) []const u8 {
    assert(self.available() >= len);
    return self.src[self.pos .. self.pos + len];
}

fn peekNext(self: *TI, comptime want: []const u8) bool {
    return self.available() >= want.len and
        std.mem.eql(u8, want, self.slice(want.len));
}

fn isNext(self: *TI, comptime want: []const u8) bool {
    if (peekNext(self, want)) {
        self.pos += want.len;
        return true;
    }
    return false;
}

fn advance(self: *TI) u8 {
    assert(!self.eof());
    const nc = self.peek();
    self.pos += 1;
    if (nc == '\n') {
        @branchHint(.unlikely);
        self.line_number += 1;
        self.line_start = self.pos;
    }
    return nc;
}

fn skipSpace(self: *TI) void {
    while (!self.eof()) {
        if (!std.ascii.isWhitespace(self.peek())) break;
        _ = self.advance();
    }
}

fn skipDigits(self: *TI) void {
    while (!self.eof()) {
        if (!std.ascii.isDigit(self.peek())) break;
        _ = self.advance();
    }
}

fn wantDigits(self: *TI) TokerError!void {
    const start = self.pos;
    self.skipDigits();
    if (start == self.pos) return error.SyntaxError;
}

fn wantFloat(self: *TI) TokerError!void {
    if (self.isNext("."))
        try self.wantDigits();
    if (!self.eof()) {
        const nc = self.peek();
        if (nc == 'e' or nc == 'E') {
            _ = self.advance();
            if (self.eof()) return error.SyntaxError;
            const sign = self.peek();
            if (sign == '+' or sign == '-')
                _ = self.advance();
            try self.wantDigits();
        }
    }
}

fn keywordLookup(op: []const u8) ?Keyword {
    if (std.meta.stringToEnum(Keyword, op)) |kw|
        return kw.normalise();
    return null;
}

pub fn next(self: *TI) TokerError!?Token {
    if (self.eof()) {
        self.markTokenStart();
        return null;
    }
    return parse: switch (self.state) {
        .TEXT => {
            const start = self.pos;
            self.markTokenStart();

            assert(!self.eof());

            while (!self.eof()) {
                if (self.peekNext("[%")) {
                    self.state = .START;
                    break;
                }
                _ = self.advance();
            }

            const text = self.src[start..self.pos];
            if (text.len == 0) continue :parse self.state;
            break :parse .{ .literal = text };
        },
        .START => {
            self.state = .EXPR;
            self.markTokenStart();
            assert(self.available() >= 2);
            self.pos += 2;
            if (!self.eof()) {
                const nc = self.peek();
                if (nc == '-' or nc == '+') {
                    _ = self.advance();
                    break :parse .{ .start = .{ .swallow = true } };
                } else if (nc == '#') {
                    self.state = .BLOCK_COMMENT;
                    continue :parse self.state;
                }
            }
            break :parse .{ .start = .{} };
        },
        .EXPR => {
            self.skipSpace();
            self.markTokenStart();
            if (self.eof()) break :parse error.UnexpectedEOF;

            const start = self.pos;
            switch (self.advance()) {
                'a'...'z', 'A'...'Z', '_' => {
                    while (!self.eof() and ctype.isSymbol(self.peek()))
                        _ = self.advance();
                    const sym = self.src[start..self.pos];
                    break :parse if (keywordLookup(sym)) |op|
                        .{ .keyword = op }
                    else
                        .{ .symbol = sym };
                },
                '0'...'9' => {
                    self.skipDigits();
                    if (!self.eof()) {
                        const nc = self.peek();
                        if (nc == '.' or nc == 'e' or nc == 'E') {
                            try self.wantFloat();
                            const float = try std.fmt.parseFloat(f64, self.src[start..self.pos]);
                            break :parse .{ .float = float };
                        }
                    }
                    const int = try std.fmt.parseInt(i64, self.src[start..self.pos], 10);
                    break :parse .{ .int = int };
                },
                '"', '\'' => |quote| {
                    while (!self.eof()) {
                        const nc = self.advance();
                        if (nc == quote) break;
                        if (nc == '\\') {
                            if (self.eof())
                                break :parse error.MissingQuote;
                            _ = self.advance();
                        }
                    } else {
                        break :parse error.MissingQuote;
                    }
                    const body = self.src[start + 1 .. self.pos - 1];
                    break :parse if (quote == '"')
                        .{ .dq_string = body }
                    else
                        .{ .sq_string = body };
                },
                '+', '-' => |sign| {
                    if (self.isNext("%]")) {
                        self.state = .TEXT;
                        break :parse .{ .end = .{ .swallow = true } };
                    }
                    break :parse .{ .keyword = if (sign == '+') .@"+" else .@"-" };
                },
                ';' => {
                    // Desugar ';'' as '%]', '[%'
                    self.state = .SEMI;
                    break :parse .{ .end = .{} };
                },
                '%' => {
                    if (self.isNext("]")) {
                        self.state = .TEXT;
                        break :parse .{ .end = .{} };
                    }

                    break :parse .{ .keyword = .@"%" };
                },
                '#' => {
                    self.state = .COMMENT;
                    continue :parse self.state;
                },
                '<', '=', '>', '!', '&', '|' => {
                    if (!self.eof()) {
                        switch (self.peek()) {
                            '>', '=', '&', '|' => {
                                const op = self.src[self.pos - 1 .. self.pos + 1];
                                if (keywordLookup(op)) |kw| {
                                    _ = self.advance();
                                    break :parse .{ .keyword = kw };
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
            // Last resort: look for a single character keyword
            const op = self.src[self.pos - 1 .. self.pos];
            if (keywordLookup(op)) |kw|
                break :parse .{ .keyword = kw };
            break :parse error.SyntaxError;
        },
        .SEMI => {
            self.state = .EXPR;
            break :parse .{ .start = .{} };
        },
        .COMMENT => {
            while (true) {
                if (self.eof()) break :parse error.UnexpectedEOF;
                if (self.advance() == '\n') {
                    self.state = .EXPR;
                    continue :parse self.state;
                }
            }
        },
        .BLOCK_COMMENT => {
            while (true) {
                if (self.eof()) break :parse error.UnexpectedEOF;
                const nc = self.advance();
                if (nc == '%' and self.isNext("]")) {
                    self.state = .TEXT;
                    continue :parse self.state;
                }
            }
        },
    };
}

test "TokenIter" {
    const gpa = testing.allocator;
    const T = Token;
    const cases = &[_]struct { src: []const u8, want: []const T }{
        .{ .src = "", .want = &[_]T{} },
        .{ .src = "hello", .want = &[_]T{.{ .literal = "hello" }} },
        .{ .src = "[% %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .end = .{} },
        } },
        .{ .src = "[%- %]", .want = &[_]T{
            .{ .start = .{ .swallow = true } },
            .{ .end = .{} },
        } },
        .{ .src = "[% -%]", .want = &[_]T{
            .{ .start = .{} },
            .{ .end = .{ .swallow = true } },
        } },
        .{ .src = "[%- -%]", .want = &[_]T{
            .{ .start = .{ .swallow = true } },
            .{ .end = .{ .swallow = true } },
        } },
        .{ .src = "[%---%]", .want = &[_]T{
            .{ .start = .{ .swallow = true } },
            .{ .keyword = .@"-" },
            .{ .end = .{ .swallow = true } },
        } },
        .{ .src = "[%+ %]", .want = &[_]T{
            .{ .start = .{ .swallow = true } },
            .{ .end = .{} },
        } },
        .{ .src = "[% + %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .keyword = .@"+" },
            .{ .end = .{} },
        } },
        .{ .src = "[% _ %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .keyword = ._ },
            .{ .end = .{} },
        } },
        .{ .src = "[% +%]", .want = &[_]T{
            .{ .start = .{} },
            .{ .end = .{ .swallow = true } },
        } },
        .{ .src = "[% '[%' %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .sq_string = "[%" },
            .{ .end = .{} },
        } },
        .{ .src = "[% ! && || FOR %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .keyword = .NOT },
            .{ .keyword = .AND },
            .{ .keyword = .OR },
            .{ .keyword = .FOREACH },
            .{ .end = .{} },
        } },
        .{ .src = "hello [% %] world", .want = &[_]T{
            .{ .literal = "hello " },
            .{ .start = .{} },
            .{ .end = .{} },
            .{ .literal = " world" },
        } },
        .{ .src = "hello [% foo %] world", .want = &[_]T{
            .{ .literal = "hello " },
            .{ .start = .{} },
            .{ .symbol = "foo" },
            .{ .end = .{} },
            .{ .literal = " world" },
        } },
        .{ .src = "hello [% foo.bar %] world", .want = &[_]T{
            .{ .literal = "hello " },
            .{ .start = .{} },
            .{ .symbol = "foo" },
            .{ .keyword = .@"." },
            .{ .symbol = "bar" },
            .{ .end = .{} },
            .{ .literal = " world" },
        } },
        .{ .src = "[% foo = 'Hello' %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .symbol = "foo" },
            .{ .keyword = .@"=" },
            .{ .sq_string = "Hello" },
            .{ .end = .{} },
        } },
        .{ .src = "[% foo = \"Hello\" %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .symbol = "foo" },
            .{ .keyword = .@"=" },
            .{ .dq_string = "Hello" },
            .{ .end = .{} },
        } },
        .{ .src = "[% INCLUDE foo %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .keyword = .INCLUDE },
            .{ .symbol = "foo" },
            .{ .end = .{} },
        } },
        .{ .src = "[% 1 -1 1e3 1.0 %]", .want = &[_]T{
            .{ .start = .{} },
            .{ .int = 1 },
            .{ .keyword = .@"-" },
            .{ .int = 1 },
            .{ .float = 1e3 },
            .{ .float = 1.0 },
            .{ .end = .{} },
        } },
        .{
            .src = "[% < > <= >= <> = == != => $ %]",
            .want = &[_]T{
                .{ .start = .{} },
                .{ .keyword = .@"<" },
                .{ .keyword = .@">" },
                .{ .keyword = .@"<=" },
                .{ .keyword = .@">=" },
                .{ .keyword = .@"!=" },
                .{ .keyword = .@"=" },
                .{ .keyword = .@"==" },
                .{ .keyword = .@"!=" },
                .{ .keyword = .@"=>" },
                .{ .keyword = .@"$" },
                .{ .end = .{} },
            },
        },
        .{
            .src = "[% << >> %]",
            .want = &[_]T{
                .{ .start = .{} },
                .{ .keyword = .@"<" },
                .{ .keyword = .@"<" },
                .{ .keyword = .@">" },
                .{ .keyword = .@">" },
                .{ .end = .{} },
            },
        },
        .{ .src = "hello [%# INCLUDE foo %] world", .want = &[_]T{
            .{ .literal = "hello " },
            .{ .literal = " world" },
        } },
        .{ .src = "hello [% INCLUDE #foo\n %] world", .want = &[_]T{
            .{ .literal = "hello " },
            .{ .start = .{} },
            .{ .keyword = .INCLUDE },
            .{ .end = .{} },
            .{ .literal = " world" },
        } },
        .{
            .src = "[% a; b %]",
            .want = &[_]T{
                .{ .start = .{} },
                .{ .symbol = "a" },
                .{ .end = .{} },
                .{ .start = .{} },
                .{ .symbol = "b" },
                .{ .end = .{} },
            },
        },
    };

    for (cases) |case| {
        var iter = TI.init(case.src);
        var tokens: std.ArrayList(T) = .empty;
        defer tokens.deinit(gpa);
        while (try iter.next()) |t| {
            try tokens.append(gpa, t);
        }
        const got = try tokens.toOwnedSlice(gpa);
        defer gpa.free(got);
        try testing.expectEqualDeep(case.want, got);
    }
}

const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

const ctype = @import("./ctype.zig");

const types = @import("./types.zig");
const Token = types.Token;
const TokerError = types.TokerError;
const Keyword = types.Keyword;
const Location = types.Location;
