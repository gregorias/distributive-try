# distributive-try

This repo is my experiment into creating a distributive try in Megaparsec.

Normally

```haskell
try a >> try b >> c /= try (a >> b) >> c
```

In Megaparsec, once the first `try` succeeds, it is consumed. This repo creates
a combinator such that

```haskell
prependDistributeTry a (try b >> c) == try (a >> b) >> c
```

This is useful in a `sepBy`-like combinator, when we want to parse something
like `sep >> token` but avoid parsing `sep` when `token` doesn't succeed.
