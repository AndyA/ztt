const ASTParser = @This();
const Keyword = types.Keyword;
const EltRef = *const ASTElement;

pub const Error = types.TokerError || Allocator.Error || error{
    Overflow,
    MissingParen,
    MissingTerminal,
    MissingComma,
    MissingColon,
    MissingBrace,
    MissingFatArrow,
    BadString,
};

const ParserState = struct {
    tok: ?types.Token = null,
    loc: types.Location = .{},

    fn eof(p: *const ParserState) bool {
        return p.tok == null;
    }
};

gpa: Allocator,
iter: TokenIter,
state: ParserState = .{},

pub fn init(gpa: Allocator, iter: TokenIter) Error!ASTParser {
    var p = ASTParser{ .gpa = gpa, .iter = iter };
    try p.nextState();
    return p;
}

pub fn nextState(p: *ASTParser) Error!void {
    const tok = p.iter.next();
    p.state.loc = p.iter.getTokenStart();
    p.state.tok = try tok;
}

pub fn eof(p: *const ASTParser) bool {
    return p.state.eof();
}

pub fn advance(p: *ASTParser) Error!void {
    if (p.eof())
        return Error.UnexpectedEOF;
    try p.nextState();
}

pub fn newNode(p: *const ASTParser, node: ASTNode, loc: types.Location) Allocator.Error!*ASTElement {
    return try ASTElement.create(p.gpa, node, loc);
}

pub fn nextKeywordIs(p: *ASTParser, want: Keyword) bool {
    if (p.state.tok) |tok| {
        return switch (tok) {
            .keyword => |kw| kw == want,
            else => false,
        };
    }
    return false;
}

const std = @import("std");
const Allocator = std.mem.Allocator;

const types = @import("./types.zig");
const expr = @import("./ASTParser/expr.zig");

const TokenIter = @import("./TokenIter.zig");

const ASTNode = @import("./node.zig").ASTNode;
const ASTElement = @import("./node.zig").ASTElement;
