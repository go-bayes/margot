# Transform a variable name into a human-readable label, preserving acronyms

This function applies explicit mappings, strips numeric-range suffixes,
removes time-prefixes and z-suffixes, replaces underscores, and converts
to title case while preserving NZ, SDO, and RWA acronyms.

## Usage

``` r
transform_var_name(
  var_name,
  label_mapping = NULL,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  expand_acronyms = FALSE
)
```

## Arguments

- var_name:

  Character; the original variable name

- label_mapping:

  Optional named list for explicit mappings

- remove_tx_prefix:

  Logical; remove leading 't0\_' etc.

- remove_z_suffix:

  Logical; remove trailing '\_z'

- use_title_case:

  Logical; convert to title case

- remove_underscores:

  Logical; replace underscores with spaces

- expand_acronyms:

  Logical; expand common acronyms (RWA, SDO, PWI, NZSEI) to their full
  names while retaining the acronym in parentheses. Defaults to FALSE.

## Value

A character scalar of the transformed label, or NA if input missing

## Examples

``` r
# Basic usage with mapping
transform_var_name("t2_rwa_z", label_mapping = list(rwa = "Right-Wing Authoritarianism"))
#> Error in transform_var_name("t2_rwa_z", label_mapping = list(rwa = "Right-Wing Authoritarianism")): could not find function "transform_var_name"

# Expand common acronyms without an explicit mapping
transform_var_name("baseline RWA", expand_acronyms = TRUE)
#> Error in transform_var_name("baseline RWA", expand_acronyms = TRUE): could not find function "transform_var_name"
# => "Baseline Right-Wing Authoritarianism (RWA)"

# Mapping takes precedence; expansion still applies to remaining acronyms
transform_var_name("PWI overall", label_mapping = list(PWI = "Personal Well-Being Index"), expand_acronyms = TRUE)
#> Error in transform_var_name("PWI overall", label_mapping = list(PWI = "Personal Well-Being Index"),     expand_acronyms = TRUE): could not find function "transform_var_name"
```
