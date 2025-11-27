# Format Transition Table

Helper function to format a single transition data frame into a markdown
table.

## Usage

``` r
transition_table(
  trans_df,
  state_names = NULL,
  wave_info = NULL,
  table_name = "transition_table"
)
```

## Arguments

- trans_df:

  A data frame with columns 'from', 'to', and 'Freq'

- state_names:

  Optional vector of state names

- wave_info:

  Optional wave information string

- table_name:

  Name for the table (default "transition_table")

## Value

A list with formatted table and explanation
