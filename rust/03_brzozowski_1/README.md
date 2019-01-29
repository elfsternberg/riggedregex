# Brzozowski Regular Expressions, in Rust

This is a regex recognizer implementing Brzozowski's Algorithm, in Rust.
It has two standard optimizations (null branches are automatically
pruned), and with those it works fine.

This version implements regular expressions as they appear in the Racket
version, without nullability optimizations (the so-called "rerp"
implementation).

