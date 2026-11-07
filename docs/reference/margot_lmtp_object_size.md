# Measure an object's size in memory and serialised

Returns the in-memory size reported by
[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html) and
the length of the object's serialised form,
`length(serialize(x, NULL, version = 3L))`, which is the form
[`margot_lmtp_checkpoint_compute()`](https://go-bayes.github.io/margot/reference/margot_lmtp_checkpoints.md)
writes. Use it on objects built from generated fixtures at the study's
number of participants and variables, so that
[`margot_lmtp_memory_preflight()`](https://go-bayes.github.io/margot/reference/margot_lmtp_memory_preflight.md)
receives measured sizes rather than guesses.
[`object.size()`](https://rdrr.io/r/utils/object.size.html) does not
count the contents of environments and counts shared memory once for
each reference, so it can report almost nothing for an object whose data
sit in an environment (a fitted model's closure or an R6 object, for
example); the serialised length includes environment contents, and the
memory estimate rests mainly on it.

## Usage

``` r
margot_lmtp_object_size(x)
```

## Arguments

- x:

  Object to measure.

## Value

A named numeric vector with `memory_bytes` and `serialised_bytes`.

## See also

[`margot_lmtp_memory_preflight()`](https://go-bayes.github.io/margot/reference/margot_lmtp_memory_preflight.md)

## Examples

``` r
margot_lmtp_object_size(matrix(stats::rnorm(1000), ncol = 10))
#>     memory_bytes serialised_bytes 
#>             8216             8070 
```
