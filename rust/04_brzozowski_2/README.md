# Brzozowski Regular Expressions, in Rust

This is a regex recognizer implementing Brzozowski's Algorithm, in Rust.
It has two standard optimizations (null branches are automatically
pruned), and with those it works fine.

This version implements regular expressions as they appear in my own
Haskell version, version, without nullability optimizations (the
so-called "rerp" implementation).

The difference between the two baseline implementations is that this one
attempts to "object orient" the code, creating an implementation that
can be modified without having to touch many portions of the code, by
isolating the 'derive' and 'nullability' tests into their own
implementation of the BrzNode.  I consider the experiment something of a
failure, in that to "work around" Rust's lack of inheritance I had to do
some fairly wacky things to teach Rust how to look stuff up.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
