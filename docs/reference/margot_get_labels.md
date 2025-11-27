# Get display labels for multiple variable names

Helper to map a vector of variable names to human-readable labels. Falls
back on \`transform_var_name()\` when no explicit mapping is found.

## Usage

``` r
margot_get_labels(vars, label_map)
```

## Arguments

- vars:

  Character vector of variable names to convert.

- label_map:

  Named list mapping variable names to labels (e.g.,
  \`label_mapping_all\`).

## Value

Character vector of display labels, in the same order as \`vars\`.

## Details

If an entry of \`vars\` is not present in \`label_map\`, this function
calls \`transform_var_name()\` to auto-generate a label based on naming
conventions.
