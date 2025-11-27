# Read Data Frame or Object from RDS File in a Specified Directory

Reads an RDS file specified by \`name\` from a directory defined by
\`dir_path\` or \`push_mods\`, returning the data frame or object stored
within. If no .rds file is found, it searches for the file without the
.rds extension. This function uses the \`here\` package to resolve the
path, ensuring that file paths are built in a consistent and
platform-independent manner.

## Usage

``` r
here_read(name, dir_path = NULL, quiet = FALSE)
```

## Arguments

- name:

  Character string specifying the name of the file to be read (with or
  without the ".rds" extension).

- dir_path:

  Character string specifying the directory path from which the file
  will be read. If NULL (default), uses \`push_mods\`.

- quiet:

  Logical. If TRUE, suppresses console output. Default is FALSE.

## Value

The data frame or R object stored in the file.

## Details

If \`dir_path\` is NULL, the \`push_mods\` variable must be defined in
the user's environment or within the package, pointing to the directory
from where files are to be read. This function will first try to read an
.rds file. If not found, it will attempt to read the file without the
.rds extension.

## Examples

``` r
# Assuming `push_mods` is set in your environment to "~/mydata"
# and you have previously saved an RDS file named "my_df.rds" in that directory
my_df <- here_read("my_df")
#> Error in here_read("my_df"): object 'push_mods' not found

# Reading from a custom directory
my_df <- here_read("my_df", dir_path = "~/custom_dir")
#> Error in here_read("my_df", dir_path = "~/custom_dir"): File not found: ~/custom_dir/my_df.rds or ~/custom_dir/my_df
```
