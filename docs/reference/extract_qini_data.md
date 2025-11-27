# Extract Qini Data for Plotting

Extracts Qini curve data from a Qini object and prepares it for
plotting. This function handles cases where Qini objects may be NULL or
missing required components, extending data as needed.

## Usage

``` r
extract_qini_data(qini_obj, name, max_index, verbose = TRUE)
```

## Arguments

- qini_obj:

  A Qini object.

- name:

  Name of the treatment arm.

- max_index:

  Maximum index to extend the curve to.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

## Value

A data frame with extracted Qini data containing:

- proportion:

  Proportion of population ranked by predicted treatment effect

- gain:

  Gain at each proportion

- curve:

  Name of the curve (e.g., "cate" or "ate")
