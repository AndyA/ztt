# Template::Toolkit in Zig

## TODO

- how to handle barewords (PROCESS / INCUDE / INSERT)
- make the toker parse string escapes
- make the toker desugar double quoted strings

## Syntax

```
# Over list
FOR a IN [1, 2, 3]; a; END
# Also over list
FOR a = [1, 2, 3]; a; END
# BASIC style
FOR a = 1 TO 10 STEP 2; a; END
# Step backwards
FOR a = 1000 TO 900 STEP -10; a; END
# Step through list
FOR a IN list STEP 3; a; END
# Step backwards through list
FOR a IN ["c", "b", "a"] STEP -1; a; END
```

## Other languages to experiment with

- shell
- SQL
