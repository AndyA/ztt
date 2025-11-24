# Template::Toolkit in Zig

## TODO

- fix toker location reporting

## Syntax

- desugar `;` to `%][%`?

```
FOR a IN [1, 2, 3]; a; END
FOR a = 1 TO 10 STEP 2; a; END
FOR a = 1000 TO 900 STEP -10; a; END
FOR a IN list STEP 3; a; END
FOR a IN ["c", "b", "a"] STEP -1; a; END
```

## Other languages to experiment with

- shell
- SQL
