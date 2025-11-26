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

    pub fn lookup(op: []const u8) ?Keyword {
        if (std.meta.stringToEnum(Keyword, op)) |kw|
            return kw.normalise();
        return null;
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
    // bareword: []const u8,
    int: i64,
    float: f64,
    sq_string: []const u8,
    dq_string: []const u8,
    string: []const u8,
    keyword: Keyword,
};

pub const TokerError = error{
    MissingQuote,
    BadSymbol,
    UnexpectedEOF,
    SyntaxError,
} || std.fmt.ParseFloatError || std.fmt.ParseIntError;

pub const Location = struct {
    line: u32 = 1, // 1 based
    column: u32 = 0, // 0 based
};

const std = @import("std");
