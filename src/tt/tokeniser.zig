fn isSymbol(chr: u8) bool {
    return std.ascii.isAlphanumeric(chr) or chr == '_';
}

pub const TokerError = error{
    MissingQuote,
    UnexpectedEOF,
    SyntaxError,
} || std.fmt.ParseFloatError;

pub const Keyword = enum {
    pub fn normalise(kw: Keyword) Keyword {
        return switch (kw) {
            .@"!" => .NOT,
            .@"&&" => .AND,
            .@"<>" => .@"!=",
            .@"|" => .FILTER,
            .@"||" => .OR,
            .FOR => .FOREACH,
            else => kw,
        };
    }

    @"!",
    @"!=",
    @"$",
    @"%",
    @"&",
    @"&&",
    @"(",
    @")",
    @"*",
    @"+",
    @",",
    @"-",
    @".",
    @"/",
    @":",
    @";",
    @"<",
    @"<=",
    @"<>",
    @"=",
    @"==",
    @"=>",
    @">",
    @">=",
    @"?",
    @"[",
    @"]",
    @"_",
    @"{",
    @"|",
    @"||",
    @"}",
    AND,
    BLOCK,
    BREAK,
    CALL,
    CASE,
    CATCH,
    CLEAR,
    DEFAULT,
    DIV,
    ELSE,
    ELSIF,
    END,
    FILTER,
    FINAL,
    FOR,
    FOREACH,
    GET,
    IF,
    INCLUDE,
    INSERT,
    LAST,
    MACRO,
    META,
    MOD,
    NEXT,
    NOT,
    OR,
    PERL,
    PLUGIN,
    PROCESS,
    RAWPERL,
    RETURN,
    SET,
    STEP,
    STOP,
    SWITCH,
    THROW,
    TO,
    TRY,
    UNLESS,
    USE,
    WHILE,
    WRAPPER,
};

const ExprFrame = struct { swallow: bool = false };

pub const Token = union(enum) {
    literal: []const u8,
    start: ExprFrame,
    end: ExprFrame,
    symbol: []const u8,
    number: f64,
    sq_string: []const u8,
    dq_string: []const u8,
    keyword: Keyword,
};

pub const Location = struct {
    name: []const u8,
    line: u32, // 1 based
    column: u32, // 0 based
};

// TODO handle bareword parsing for INCLUDE / PROCESS

pub const TokenIter = struct {
    const Self = @This();

    name: []const u8,
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

    pub fn init(name: []const u8, src: []const u8) Self {
        return Self{ .name = name, .src = src };
    }

    pub fn getLocation(self: *const Self) Location {
        return .{
            .name = self.name,
            .line = self.line_number,
            .column = self.pos - self.line_start,
        };
    }

    fn eof(self: *const Self) bool {
        return self.pos == self.src.len;
    }

    fn peek(self: *const Self) u8 {
        assert(!self.eof());
        return self.src[self.pos];
    }

    fn available(self: *const Self) usize {
        return self.src.len - self.pos;
    }

    fn slice(self: *const Self, len: usize) []const u8 {
        assert(self.available() >= len);
        return self.src[self.pos .. self.pos + len];
    }

    fn isNext(self: *Self, comptime want: []const u8) bool {
        if (self.available() < want.len)
            return false;
        if (std.mem.eql(u8, want, self.slice(want.len))) {
            self.pos += want.len; // assumes no newlines in want
            return true;
        }
        return false;
    }

    fn advance(self: *Self) u8 {
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

    fn skipSpace(self: *Self) void {
        while (!self.eof()) {
            if (!std.ascii.isWhitespace(self.peek())) break;
            _ = self.advance();
        }
    }

    fn skipDigits(self: *Self) void {
        while (!self.eof()) {
            if (!std.ascii.isDigit(self.peek())) break;
            _ = self.advance();
        }
    }

    fn wantDigits(self: *Self) TokerError!void {
        const start = self.pos;
        self.skipDigits();
        if (start == self.pos) return error.SyntaxError;
    }

    fn wantNumber(self: *Self) TokerError!void {
        self.skipDigits();
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

    pub fn next(self: *Self) TokerError!?Token {
        if (self.eof()) return null;
        return parse: switch (self.state) {
            .TEXT => {
                const start = self.pos;

                assert(!self.eof());

                const text = text: while (!self.eof()) {
                    const nc = self.advance();
                    if (nc == '[' and !self.eof() and self.advance() == '%') {
                        self.state = .START;
                        break :text self.src[start .. self.pos - 2];
                    }
                } else {
                    break :text self.src[start..self.pos];
                };

                if (text.len == 0) continue :parse self.state;
                break :parse .{ .literal = text };
            },
            .START => {
                self.state = .EXPR;
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
                if (self.eof()) break :parse error.UnexpectedEOF;

                const start = self.pos;
                switch (self.advance()) {
                    'a'...'z', 'A'...'Z', '_' => {
                        while (!self.eof() and isSymbol(self.peek()))
                            _ = self.advance();
                        const sym = self.src[start..self.pos];
                        break :parse if (keywordLookup(sym)) |op|
                            .{ .keyword = op }
                        else
                            .{ .symbol = sym };
                    },
                    '0'...'9' => {
                        try self.wantNumber();
                        const number = try std.fmt.parseFloat(f64, self.src[start..self.pos]);
                        break :parse .{ .number = number };
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
};

test TokenIter {
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
            .{ .number = 1 },
            .{ .keyword = .@"-" },
            .{ .number = 1 },
            .{ .number = 1e3 },
            .{ .number = 1.0 },
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
        var iter = TokenIter.init("test", case.src);
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

pub const LocationToken = struct {
    tok: Token,
    loc: Location,
};

pub const LocationTokenIter = struct {
    const Self = @This();

    iter: TokenIter,

    pub fn init(name: []const u8, src: []const u8) Self {
        return Self{ .iter = TokenIter.init(name, src) };
    }

    pub fn next(self: *Self) TokerError!?LocationToken {
        const loc = self.iter.getLocation();
        const tok = try self.iter.next();
        if (tok) |t|
            return LocationToken{ .tok = t, .loc = loc };
        return null;
    }
};

test LocationTokenIter {
    const gpa = testing.allocator;
    const T = LocationToken;
    const cases = &[_]struct { src: []const u8, want: []const T }{
        .{ .src = "", .want = &[_]T{} },
        .{ .src = "hello [% %] world", .want = &[_]T{
            .{
                .tok = .{ .literal = "hello " },
                .loc = .{ .name = "test", .line = 1, .column = 0 },
            },
            .{
                .tok = .{ .start = .{} },
                .loc = .{ .name = "test", .line = 1, .column = 8 },
            },
            .{
                .tok = .{ .end = .{} },
                .loc = .{ .name = "test", .line = 1, .column = 8 },
            },
            .{
                .tok = .{ .literal = " world" },
                .loc = .{ .name = "test", .line = 1, .column = 11 },
            },
        } },
    };

    for (cases) |case| {
        var iter = LocationTokenIter.init("test", case.src);
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
