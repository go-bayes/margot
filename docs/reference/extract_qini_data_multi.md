# Extract Qini Data for Multi-Arm Treatments

Extracts and formats Qini curve data from a maq object for multi-arm
treatments.

## Usage

``` r
extract_qini_data_multi(qini_obj, name, max_index, verbose = TRUE)
```

## Arguments

- qini_obj:

  A Qini object from maq.

- name:

  Name of the treatment comparison.

- max_index:

  Maximum index to extend the curve to.

- verbose:

  Logical indicating whether to display detailed messages.

## Value

A data frame with proportion, gain, and curve columns.
