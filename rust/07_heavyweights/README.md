# Rigged Generic Glushkov Regular Expressions in Rust: The Heavyweight Experiments

This implementation is significantly different than previous ones,
although it does build off that work, and it does proceed directly from
the [Haskell implementation](../../haskell/08_Heavyweights/) and the
[previous Rust experiment](../../rust/06_riggedglushkov).

I strongly recommend reading the [Haskell implementation
README](../../haskell/08_Heavyweights/README.md) to get a sense of the
changes to the algorithm.  They are fairly heavyweight and interesting,
in that the definition of the Semiring has been further abstracted to
handle position information, and as a consequence the input type of the
operation has likewise been abstracted to handle arbitrary input types.
The last was done to enable us to pass both the character being analyzed
and the position in the stream, such that we could record information
about the position under certain circumstances.

What makes the *Rust* version of this implementation noteworthy is the
ease with which the inbound data type was changed to handle just about
any kind of data.  It adds a bit of genericizing noise to the source
file, some ceremony that makes me wonder how I could abstract or derive
it automatically.

On the other hand, since the `Recognizer` and `Parser` implementations
concretize that their input type is `char` by usage, they work *completely
unchanged* from the previous Rust experiment.  That's remarkable.

The implementation of the `Leftlong` Semiring, which reports the
location and length of the first, longest substring match of a capture
group (yes!) is fairly extensive and went through a number of thrashes
before I recalled that I could match on a tuple, at which point the
implementations of `add` and `mul` became straightforward.

Putting the entirety of the semiring in a single trait makes more sense,
to me at least, than abstracting it further over `num_traits`, as I had
it in earlier versions.  While using `num_traits` is *clever*, it also
forces us to work with the `::Mul` and `::Add`, which do not take
references, and for larger and more complex semirings, working with
references made a lot of sense.

The implementation of `submatch`, a function that allows us to search
for arbitrary substrings without having to root or ceiling the string is
interesting; by using the `One()` value, I'm able to preserve the fact
that the search hasn't failed while also enforcing the notion that we're
skipping over things that match `any` but don't match the concrete
sample, which is nifty.

All in all, this is highly satisfying work, and it's a pleasure to see
it working so well.

## License

As this is entirely my work, it is copyright (c) 2019, and licensed
under the Mozilla Public License v. 2.0.  See the
[LICENSE.md](../../LICENSE.md) in the root directory.
