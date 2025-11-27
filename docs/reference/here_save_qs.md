# Save Data Frame or Object to qs File in a Specified Directory with Enhanced Compression

Saves the provided data frame or object as a \`.qs\` file using the
specified name, within a directory defined by \`dir_path\`. This
function leverages the \`qs\` package to write data to \`.qs\` format
with enhanced compression for efficient storage and quick access in R.

## Usage

``` r
here_save_qs(obj, name, dir_path, preset = "high", nthreads = 1)
```

## Arguments

- obj:

  Data frame or object to be saved. This is the object you want to
  persist to disk in \`.qs\` format.

- name:

  Character string specifying the base name of the file (without the
  ".qs" extension).

- dir_path:

  Character string specifying the directory path where the file will be
  saved.

- preset:

  Character string specifying the compression preset. Default is "high"
  for better compression.

- nthreads:

  Integer specifying the number of threads to use for compression.
  Default is 1.

## Value

Invisible NULL. The primary effect of this function is the side effect
of writing a \`.qs\` file to disk.

## Details

The \`dir_path\` argument must point to an existing directory. The
function does not create directories; it assumes that the specified
directory already exists. The function uses enhanced compression
settings by default to minimize file size.

## Examples

``` r
my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save_qs(my_df, "my_saved_dataframe", "~/mydata")
#> Error in qs::qsave(obj, file_path, preset = preset, nthreads = nthreads): For file ~/mydata/my_saved_dataframe.qs: Failed to open for writing. Does the directory exist? Do you have file permissions? Is the file name long? (>255 chars)
```
