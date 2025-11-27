# Save Data Frame as RDS File in a Specified Directory

Saves the provided data frame as an RDS file using the specified name,
within a directory defined by \`push_mods\` This function uses the
\`here\` package to construct the path, ensuring that file paths are
built in a consistent and platform-independent manner.

## Usage

``` r
here_save(df, name, dir_path = NULL, compress = TRUE, quiet = FALSE)
```

## Arguments

- df:

  Data frame or object to be saved. This is the object you want to
  persist on disk.

- name:

  Character string specifying the base name of the file. The ".rds"
  extension will be automatically appended to this name.

- dir_path:

  Character string specifying the directory path where the file will be
  saved. If NULL (default), uses \`push_mods\`.

- compress:

  Logical or character string specifying the type of compression to use.
  See \`?saveRDS\` for details. Default is TRUE.

- quiet:

  Logical. If TRUE, suppresses console output. Default is FALSE.

## Value

Invisibly returns the full path to the saved file.

## Details

If \`dir_path\` is NULL, the \`push_mods\` variable should be defined in
the user's environment or within the package and should point to the
directory where files will be saved. It is assumed that the specified
directory exists. This function does not create directories.

## Examples

``` r
# assuming `push_mods` is set in your environment to "~/mydata"
my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save(my_df, "my_df")
#> Error in here_save(my_df, "my_df"): object 'push_mods' not found

# specifying a custom directory
here_save(my_df, "my_df", dir_path = "~/custom_dir", compress = "xz")
#> Warning: cannot open compressed file '/Users/joseph/custom_dir/my_df.rds', probable reason 'No such file or directory'
#> Error in xzfile(file, mode): cannot open the connection
```
