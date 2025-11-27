# Read Multiple Images from a Folder

This function reads multiple images from a specified folder and returns
them as a list of magick image objects. It uses the \`here\` package for
path handling, \`magick\` for image reading, and \`stringr\` for string
manipulation.

## Usage

``` r
read_multiple_images(path_multi_plots, pattern = "policy_tree_plot\\.png$")
```

## Arguments

- path_multi_plots:

  Character string specifying the path to the folder containing the
  images, relative to the project root.

- pattern:

  Character string specifying the pattern to match in the filenames.
  Default is "policy_tree_plot\\png\$".

## Value

A named list of magick image objects.

## Examples

``` r
if (FALSE) { # \dontrun{
path_multi_plots <- "plots"
policy_tree_plots <- read_multiple_images(path_multi_plots, "policy_tree_plot\\.png$")
qini_plots <- read_multiple_images(path_multi_plots, "qini_plot\\.png$")
} # }
```
