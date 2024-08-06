# # margot 0.2.0.2
## Deprecations

* `coloured_histogram()` and `coloured_histogram_quantile()` are deprecated.
  Now use the new `margot_plot_hist()` instead.
* `create_ordered_variable_custom` is deprecated. Now use `created_ordered_variable`function with `custom_breaks = c(..)` to obtain custom breaks. 

## New
* `margot_plot_hist()` for plotting distributions of the exposure variable


# margot 0.2.0.1
* new table and plot functions for `grf` outputs

# margot 0.2.0

* improved subgroup comparison function 
* deprecated `compare_group_means` function, with `compare_group`allowing for contrasts of on both the causal difference and relative risk scales.
* new wrapper functions functions for `grf` plus visualising results.


# margot 0.1.2.1
* group_tab now works under the hood of margot_plot, so no need to specify explicitly
* group_tab now allows custom order for plot, not just by decreasing effect size
* numerous small enhancements to older parametric model options
* new logo

# margot 0.1.2

* minor fixes


# margot 0.1.1

* functions to interpret outputs of `lmtp` models
* functions to create tables
* functions to create graphs

# margot 0.1.0.1

* Minor bug fixes and performance improvements.


# margot 0.1.0

* Initial release: includes data exploration, model preparation, utility functions.
