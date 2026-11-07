# Canonical form of a checkpoint identity

Returns \`identity\` with provenance attributes removed, recursively:
the absolute \`file\` path that RcppTOML attaches to a parsed TOML file,
and source references (\`srcref\`, \`srcfile\`, \`wholeSrcref\`). The
same inputs read from two locations therefore give the same identity.
Every other attribute is kept, because attributes such as units,
time-series parameters and contrasts carry content. An identity with
nothing to remove is returned unchanged, so identities written by
earlier versions still match. Environments, closures, external pointers,
other reference objects and S4 objects are refused, because their
serialised form depends on the R process or holds content in slots;
identify a function by a fingerprint of its source instead. Environments
held in attributes, such as a formula's \`.Environment\`, are not
refused; they make an identity depend on where the object was created,
so pass a formula as deparsed text.

## Usage

``` r
margot_lmtp_checkpoint_identity(identity)
```

## Arguments

- identity:

  A list identifying a checkpoint task.

## Value

The canonical identity.

## Examples

``` r
study <- structure(list(seed = 42L), class = c("toml", "list"), file = "/a/study.toml")
identical(margot_lmtp_checkpoint_identity(list(study = study)),
  margot_lmtp_checkpoint_identity(list(study = structure(study, file = "/b/study.toml"))))
#> [1] TRUE
```
