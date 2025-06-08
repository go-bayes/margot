pkgname <- "margot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "margot-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('margot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("back_transform_log_z")
### * back_transform_log_z

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: back_transform_log_z
### Title: Back Transform Z-Score to Original Log-Transformed Scale
### Aliases: back_transform_log_z

### ** Examples

# Given log-transformed data with log_mean = 1.5 and log_sd = 0.5
original_value <- back_transform_log_z(z_scores = 1.2, log_mean = 1.5, log_sd = 0.5)
print(original_value)

# Multiple z-scores can be transformed at once
z_scores <- c(-1, 0, 1, 2)
original_values <- back_transform_log_z(z_scores = z_scores, log_mean = 1.5, log_sd = 0.5)
print(original_values)

# Real-world example: back-transforming household income z-scores
# Get mean and sd from original log-transformed data
log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)

# Back-transform all z-scores in the dataset
original_data_scale <- back_transform_log_z(
  df_grf$t0_log_household_inc_z,
  log_mean = log_mean_inc,
  log_sd = log_sd_inc
)
head(original_data_scale)

# Interpret key points on the distribution (-1 SD, mean, +1 SD)
z_scores <- c(-1, 0, 1)
scale_values <- back_transform_log_z(
  z_scores,
  log_mean = log_mean_inc,
  log_sd = log_sd_inc
)

# Create a data frame to display the relationship between z-scores and original values
results_df <- data.frame(
  z_score = z_scores,
  data_scale = scale_values
)
print(results_df) # Shows what values on the original scale correspond to each z-score




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("back_transform_log_z", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("back_transform_logmean")
### * back_transform_logmean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: back_transform_logmean
### Title: Back-transform Log-transformed Mean
### Aliases: back_transform_logmean

### ** Examples

log_mean <- 1.098612 # true mean is 2. We add + 1 to the log to handle zero: log(2+1) = log(3)
back_transformed_result <- back_transform_logmean(log_mean)
print(back_transformed_result)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("back_transform_logmean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("back_transform_zscore")
### * back_transform_zscore

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: back_transform_zscore
### Title: Back Transform Z-Score to Original Scale
### Aliases: back_transform_zscore

### ** Examples

# Given a dataset with mean = 100 and sd = 15
original_value <- back_transform_zscore(z = 1.5, mean = 100, sd = 15)
print(original_value)

# Multiple z-scores can be transformed at once
z_scores <- c(-1, 0, 1, 2)
original_values <- back_transform_zscore(z = z_scores, mean = 50, sd = 10)
print(original_values)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("back_transform_zscore", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("causal_contrast_marginal")
### * causal_contrast_marginal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: causal_contrast_marginal
### Title: Causal Contrast Marginal Effects Estimation
### Aliases: causal_contrast_marginal
### Keywords: internal

### ** Examples

# Assume that df is your dataset with variables 'outcome', 'treatment', 'age', and 'gender'
result <- causal_contrast_marginal(df = df, Y = "outcome", X = "treatment",
                                   baseline_vars = c("age", "gender"),
                                   treat_0 = "control", treat_1 = "exposed",
                                   estimand = "ATE", type = "RD", nsims = 100,
                                   cores = 2, family = "gaussian", weights = "weight_var",
                                   continuous_X = FALSE, splines = FALSE,
                                   vcov = "HC3", verbose = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("causal_contrast_marginal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coloured_histogram")
### * coloured_histogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coloured_histogram
### Title: Create a Coloured Histogram Highlighting Specific Ranges
###   (DEPRECATED)
### Aliases: coloured_histogram
### Keywords: internal

### ** Examples

 ## Not run: 
##D # assuming df_19 is your dataframe and contains the column 'forgiveness'
##D graph <- coloured_histogram(
##D   df = df_19,
##D   col_name = "forgiveness",
##D   scale_min = 1,
##D   scale_max = 7,
##D   highlight_range = "highest",
##D   binwidth = .1, # adjust binwidth as needed
##D   unit_of_change = 1 # specify the unit of change
##D )
##D print(graph)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coloured_histogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coloured_histogram_sd")
### * coloured_histogram_sd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coloured_histogram_sd
### Title: Visualize Distribution with Mean and Standard Deviation
###   Highlights
### Aliases: coloured_histogram_sd
### Keywords: internal

### ** Examples

 ## Not run: 
##D # Assuming `df_nz` is a dataframe with a numeric column 'forgiveness'
##D # and a factor or integer column 'wave' for subsetting:
##D df_19 <- dplyr::filter(df_nz, wave == 2019)
##D 
##D graph_density_of_exposure <- coloured_histogram_sd(
##D   df = df_19,
##D   col_name = "forgiveness",
##D   binwidth = 0.5 # Adjust binwidth as needed
##D )
## End(Not run)
print(graph_density_of_exposure)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coloured_histogram_sd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("coloured_histogram_shift")
### * coloured_histogram_shift

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: coloured_histogram_shift
### Title: Visualise Shifts in Data Distributions with Highlighted Ranges
###   (DEPRECATED)
### Aliases: coloured_histogram_shift
### Keywords: internal

### ** Examples

## Not run: 
##D # Assuming df_nz is your dataframe and it includes a numeric variable 'forgiveness'
##D # Filter to a specific subset, for example, wave 2019
##D df_19 <- dplyr::filter(df_nz, wave == 2019)
##D 
##D # Create and print the histogram
##D graph_density_of_exposure <- coloured_histogram_shift(
##D   df = df_19,
##D   shift = "down",
##D   col_name = "forgiveness",
##D   binwidth = .5, # Adjust binwidth for your data
##D   range_highlight = c(3.9, 10)
##D )
##D print(graph_density_of_exposure)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("coloured_histogram_shift", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_transition_matrix")
### * create_transition_matrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_transition_matrix
### Title: Create transition matrix for state transitions
### Aliases: create_transition_matrix
### Keywords: internal

### ** Examples

df <- read.table(header=TRUE, text="
id wave year_measured religion_believe_god
3 0 1 0
3 1 1 1
4 0 1 0
4 1 1 1
5 0 1 1
5 1 1 0")

transition_matrix <- create_transition_matrix(df, "religion_believe_god", "id")
print(transition_matrix)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_transition_matrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("df_nz")
### * df_nz

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: df_nz
### Title: df_nz: Example Data Frame
### Aliases: df_nz
### Keywords: datasets

### ** Examples

data(df_nz)
head(df_nz)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("df_nz", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("double_robust_marginal")
### * double_robust_marginal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: double_robust_marginal
### Title: Double Robust Marginal Estimation and Tabulation
### Aliases: double_robust_marginal
### Keywords: internal

### ** Examples

# Assuming you have a dataset `df_ate` and necessary variables defined
results <- double_robust_marginal(
  df = df_ate,
  Y = "t2_kessler_latent_anxiety_z",
  X = "treatment_var",
  baseline_vars = c("age", "gender"),
  treat_1 = "intervention",
  treat_0 = "control",
  nsims = 200,
  cores = 4,
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type_causal = "RD",
  type_tab = "RD",
  vcov = "HC2",
  new_name = "Test Model Effect",
  delta = 1,
  sd = 1
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("double_robust_marginal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("group_tab")
### * group_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: group_tab
### Title: Group and Annotate Treatment Effect Estimates
### Aliases: group_tab
### Keywords: internal

### ** Examples

# descending magnitude (default for 'default')
result_df <- group_tab(df = analysis_df, order = 'default')

# ascending magnitude
result_df <- group_tab(df = analysis_df, order = 'magnitude_asc')

# strongest E-value bound first
result_df <- group_tab(df = analysis_df, order = 'evaluebound_desc')

# alphabetical
result_df <- group_tab(df = analysis_df, order = 'alphabetical')

# custom ordering
custom_order <- c('Outcome3','Outcome1','Outcome2')
result_df <- group_tab(df = analysis_df, order = 'custom', custom_order = custom_order)

# descending magnitude (default for 'default')
result_df <- group_tab(df = analysis_df, order = 'default')

# ascending magnitude
result_df <- group_tab(df = analysis_df, order = 'magnitude_asc')

# strongest E-value bound first
result_df <- group_tab(df = analysis_df, order = 'evaluebound_desc')

# alphabetical
result_df <- group_tab(df = analysis_df, order = 'alphabetical')

# custom ordering
custom_order <- c('Outcome3','Outcome1','Outcome2')
result_df <- group_tab(df = analysis_df, order = 'custom', custom_order = custom_order)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("group_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_read")
### * here_read

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_read
### Title: Read Data Frame or Object from RDS File in a Specified Directory
### Aliases: here_read

### ** Examples

# Assuming `push_mods` is set in your environment to "~/mydata"
# and you have previously saved an RDS file named "my_df.rds" in that directory
my_df <- here_read("my_df")

# Reading from a custom directory
my_df <- here_read("my_df", dir_path = "~/custom_dir")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_read", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_read_arrow")
### * here_read_arrow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_read_arrow
### Title: Read Data Frame from Parquet File in a Specified Directory
###   (Deprecated)
### Aliases: here_read_arrow
### Keywords: internal

### ** Examples

## Not run: 
##D my_df <- here_read_arrow("my_dataset")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_read_arrow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_read_qs")
### * here_read_qs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_read_qs
### Title: Read Data Frame or Object from qs File in a Specified Directory
### Aliases: here_read_qs

### ** Examples

# Assuming `push_mods` is set in your environment to "~/mydata"
# and you have previously saved a `.qs` file named "my_dataset.qs" in that directory
my_df <- here_read_qs("my_dataset")

# Reading from a custom directory with multiple threads
my_df <- here_read_qs("my_dataset", dir_path = "~/custom_dir", nthreads = 4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_read_qs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_save")
### * here_save

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_save
### Title: Save Data Frame as RDS File in a Specified Directory
### Aliases: here_save

### ** Examples

# assuming `push_mods` is set in your environment to "~/mydata"
my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save(my_df, "my_df")

# specifying a custom directory
here_save(my_df, "my_df", dir_path = "~/custom_dir", compress = "xz")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_save", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_save_arrow")
### * here_save_arrow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_save_arrow
### Title: Save Data Frame to Parquet File in a Specified Directory
###   (Deprecated)
### Aliases: here_save_arrow
### Keywords: internal

### ** Examples

## Not run: 
##D my_df <- data.frame(x = 1:5, y = letters[1:5])
##D here_save_arrow(my_df, "my_saved_dataframe")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_save_arrow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here_save_qs")
### * here_save_qs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here_save_qs
### Title: Save Data Frame or Object to qs File in a Specified Directory
###   with Enhanced Compression
### Aliases: here_save_qs

### ** Examples

my_df <- data.frame(x = 1:5, y = letters[1:5])
here_save_qs(my_df, "my_saved_dataframe", "~/mydata")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here_save_qs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("impute_and_combine")
### * impute_and_combine

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: impute_and_combine
### Title: Perform multiple imputation on a list of data frames and combine
###   the results
### Aliases: impute_and_combine

### ** Examples

## Not run: 
##D   # Assuming list_df is a list of data frames with missing values
##D   imputed_data <- impute_and_combine(list_df, m = 5)
##D   print(imputed_data)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("impute_and_combine", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lmtp_evalue_tab")
### * lmtp_evalue_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lmtp_evalue_tab
### Title: Calculate E-values for LMTP Output
### Aliases: lmtp_evalue_tab

### ** Examples

## Not run: 
##D # Assuming 'tab_contrast_hours_charity_z_null' is a data frame output from `margot_lmtp_tab()`
##D lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RD")
##D lmtp_evalue_tab(tab_contrast_hours_charity_z_null, scale = "RR")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lmtp_evalue_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_amelia_to_mice")
### * margot_amelia_to_mice

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_amelia_to_mice
### Title: convert an amelia object to a mice object
### Aliases: margot_amelia_to_mice
### Keywords: internal

### ** Examples

# load Amelia package and perform imputation
library(Amelia)
data(africa) # example dataset from Amelia package
amelia_output <- amelia(x = africa, m = 5, idvars = "country") # impute data

# convert amelia object to mice object
mids_obj <- margot_amelia_to_mice(amelia_output)

# verify mids object
print(mids_obj)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_amelia_to_mice", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_back_transform_log_z")
### * margot_back_transform_log_z

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_back_transform_log_z
### Title: Create Z-score to Original Scale Mapping for Log-Transformed
###   Data
### Aliases: margot_back_transform_log_z

### ** Examples

# Get mean and sd from original log-transformed income data
log_mean_inc <- mean(original_df$t0_log_household_inc, na.rm = TRUE)
log_sd_inc <- sd(original_df$t0_log_household_inc, na.rm = TRUE)

# Create mapping table with default z-scores
income_mapping <- margot_back_transform_log_z(
  log_mean = log_mean_inc,
  log_sd = log_sd_inc,
  label = "household_income"
)
print(income_mapping)

# Create mapping with custom z-scores
custom_mapping <- margot_back_transform_log_z(
  log_mean = log_mean_inc,
  log_sd = log_sd_inc,
  z_scores = c(-1, 0, 1),
  label = "household_income"
)
print(custom_mapping)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_back_transform_log_z", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_combine_results")
### * margot_combine_results

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_combine_results
### Title: Combine Multiple Results Tables from margot_plot into a Single
###   Formatted Table
### Aliases: margot_combine_results
### Keywords: internal

### ** Examples

## Not run: 
##D # Suppose we have domain-based results:
##D results_list <- list(
##D   Health = list(
##D     transformed_table = health_religious_vs_secular$transformed_table,
##D     interpretation = health_religious_vs_secular$interpretation
##D   ),
##D   Psychological = list(
##D     transformed_table = psych_religious_vs_secular$transformed_table,
##D     interpretation = psych_religious_vs_secular$interpretation
##D   )
##D )
##D 
##D # And corresponding options:
##D options_list <- list(
##D   Health = margot_plot_create_options(
##D     subtitle = "Health: Religious vs Secular (baseline)",
##D   ),
##D   Psychological = margot_plot_create_options(
##D     subtitle = "Psychological: Religious vs Secular (baseline)",
##D   )
##D )
##D 
##D # Combine the results and print:
##D combined_table <- margot_combine_results(
##D   results = results_list,
##D   options = options_list,
##D   format = "latex",
##D   booktabs = TRUE,
##D   longtable = TRUE,
##D   digits = 2
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_combine_results", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_compute_gender_weights_by_wave")
### * margot_compute_gender_weights_by_wave

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_compute_gender_weights_by_wave
### Title: Compute Gender-Based Sample Weights Using Baseline Wave
###   Proportions
### Aliases: margot_compute_gender_weights_by_wave

### ** Examples

dat <- data.frame(
  id = 1:100,
  male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
  wave = rep(1:2, each = 50)
)
weights <- margot_compute_gender_weights_by_wave(dat, male_col = "male",
                                                 wave_col = "wave",
                                                 target_wave = 1,
                                                 target_male_prop = 0.52)
head(weights)

dat <- data.frame(
  id = 1:100,
  male = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
  wave = rep(1:2, each = 50)
)
weights <- margot_compute_gender_weights_by_wave(dat, male_col = "male",
                                                 wave_col = "wave",
                                                 target_wave = 1,
                                                 target_male_prop = 0.52)
head(weights)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_compute_gender_weights_by_wave", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_interpret_table")
### * margot_interpret_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_interpret_table
### Title: Interpret and Describe Causal Effect Estimates Using E-values
###   (Deprecated)
### Aliases: margot_interpret_table
### Keywords: internal

### ** Examples

## Not run: 
##D # Assuming `group_tab_output` is the result from a causal analysis
##D result <- margot_interpret_table(group_tab_output, "causal_difference", "ATE")
##D cat(result$estimand_description)
##D cat(result$interpretation)
##D 
##D # Without specifying an estimand
##D result <- margot_interpret_table(group_tab_output, "risk_ratio")
##D cat(result$interpretation)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_interpret_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_lmtp")
### * margot_lmtp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_lmtp
### Title: Batch Process LMTP Models
### Aliases: margot_lmtp

### ** Examples

## Not run: 
##D # Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders
##D 
##D # Define shift functions
##D gain_function <- function(data, trt) {
##D   data[[trt]] + 1
##D }
##D 
##D loss_function <- function(data, trt) {
##D   pmax(data[[trt]] - 1, 0)
##D }
##D 
##D # Run LMTP analysis
##D result <- margot_lmtp(
##D   data = my_data,
##D   outcome_vars = c("outcome1", "outcome2"),
##D   trt = "treatment",
##D   shift_functions = list(gain = gain_function, loss = loss_function),
##D   lmtp_defaults = list(baseline = c("confounder1", "confounder2"),
##D                        time_vary = c("time_var1", "time_var2"),
##D                        outcome_type = "continuous"),
##D   save_output = TRUE,
##D   save_path = here::here("output", "lmtp_results"),
##D   prefix = "my_study"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_lmtp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_lmtp_evalue")
### * margot_lmtp_evalue

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_lmtp_evalue
### Title: Combine LMTP Summary and E-Value Calculation
### Aliases: margot_lmtp_evalue

### ** Examples

## Not run: 
##D # assuming `contrast_output` is the result from `lmtp::lmtp_contrast()`
##D summary_evalues <- margot_lmtp_evalue(
##D   lmtp_output = contrast_output,
##D   scale = "RD",
##D   new_name = "Treatment Effect"
##D )
##D print(summary_evalues)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_lmtp_evalue", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_make_tables")
### * margot_make_tables

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_make_tables
### Title: Create Summary Tables Using table1 with Custom Formatting
### Aliases: margot_make_tables

### ** Examples

## Not run: 
##D   # Flextable output for Word
##D   flex_table <- margot_make_tables(
##D     data = mydata,
##D     vars = c("age", "gender", "income"),
##D     by = "group",
##D     labels = c("age" = "Age", "gender" = "Gender", "income" = "Income"),
##D     factor_vars = "gender",
##D     table1_opts = list(overall = FALSE, transpose = TRUE),
##D     format = "flextable",
##D     flex_opts = list(font_size = 8)
##D   )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_make_tables", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_model_evalue")
### * margot_model_evalue

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_model_evalue
### Title: Combine Model Summary and E-Value Calculation for Various Causal
###   Models
### Aliases: margot_model_evalue

### ** Examples

## Not run: 
##D # For lmtp_contrast output
##D summary_evalues <- margot_model_evalue(
##D   model_output = lmtp_contrast_output,
##D   scale = "RD",
##D   new_name = "Treatment Effect"
##D )
##D 
##D # For causal_forest output
##D cf_summary <- margot_model_evalue(
##D   model_output = causal_forest_output,
##D   new_name = "Causal Forest Effect"
##D )
##D 
##D # For multi_arm_causal_forest output
##D macf_summary <- margot_model_evalue(
##D   model_output = multi_arm_cf_output,
##D   new_name = "Multi-Arm Effect"
##D )
##D 
##D # For direct input of estimate and standard error
##D direct_summary <- margot_model_evalue(
##D   model_output = data.frame(estimate = 0.5, std.err = 0.1),
##D   new_name = "Direct Effect"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_model_evalue", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_model_tab")
### * margot_model_tab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_model_tab
### Title: Summarise LMTP or Causal Forest Output into a Data Frame
### Aliases: margot_model_tab
### Keywords: internal

### ** Examples

## Not run: 
##D # Assuming `contrast_hours_charity_z_null` is output from `lmtp::lmtp_contrast()`
##D tab_contrast_hours_charity_z_null <- margot_model_tab(
##D   contrast_hours_charity_z_null,
##D   scale = "RD",
##D   new_name = "relig service: hours volunteer"
##D )
##D print(tab_contrast_hours_charity_z_null)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_model_tab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_boxplot")
### * margot_plot_boxplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_boxplot
### Title: Create panel data Boxplots using ggplot2
### Aliases: margot_plot_boxplot

### ** Examples

## Not run: 
##D # define outcome variables
##D outcome_vars <- c(
##D   "env_climate_chg_concern",
##D   "env_climate_chg_cause",
##D   "env_climate_chg_real",
##D   "env_sat_nz_environment",
##D   "envefficacy"
##D )
##D 
##D # basic usage with all waves
##D p1 <- margot_plot_boxplot(
##D   data = your_data,
##D   y_vars = outcome_vars,
##D   id_col = "id"
##D )
##D 
##D # plotting specific waves with points shown and coordinates flipped
##D p2 <- margot_plot_boxplot(
##D   data = your_data,
##D   y_vars = outcome_vars,
##D   waves = c(2021, 2022),
##D   show_points = TRUE,
##D   coord_flip = TRUE,
##D   id_col = "id"
##D )
##D 
##D # saving the plot with a custom prefix
##D margot_plot_boxplot(
##D   data = your_data,
##D   y_vars = outcome_vars,
##D   waves = c(2021, 2022, 2023),
##D   save_path = "path/to/save",
##D   prefix = "climate_change",
##D   include_timestamp = TRUE,
##D   id_col = "id"
##D )
##D 
##D # customizing the plot appearance with flipped coordinates
##D p3 <- margot_plot_boxplot(
##D   data = your_data,
##D   y_vars = c("env_climate_chg_concern", "envefficacy"),
##D   waves = c(2021, 2022),
##D   title = "Climate Change Concern and Efficacy",
##D   y_label = "Score",
##D   legend_position = "right",
##D   facet_scales = "free",
##D   coord_flip = TRUE,
##D   id_col = "id"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_boxplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_boxplot_covariate")
### * margot_plot_boxplot_covariate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_boxplot_covariate
### Title: create boxplots with covariates using ggplot2
### Aliases: margot_plot_boxplot_covariate

### ** Examples

## Not run: 
##D # example 1: basic usage with all waves
##D p1 <- margot_plot_boxplot_covariate(
##D   data = your_data,
##D   outcome = "env_climate_chg_concern",
##D   covariate = "education",
##D   id_col = "id"
##D )
##D 
##D # example 2: plotting specific waves with custom labels
##D p2 <- margot_plot_boxplot_covariate(
##D   data = your_data,
##D   outcome = "political_orientation",
##D   covariate = "age_group",
##D   waves = c(2021, 2022, 2023),
##D   y_label = "Political Orientation",
##D   color_label = "Age Group",
##D   id_col = "id"
##D )
##D 
##D # example 3: showing individual points and flipping coordinates
##D p3 <- margot_plot_boxplot_covariate(
##D   data = your_data,
##D   outcome = "env_sat_nz_environment",
##D   covariate = "income_bracket",
##D   show_points = TRUE,
##D   coord_flip = TRUE,
##D   y_label = "Satisfaction with NZ Environment",
##D   color_label = "Income Bracket",
##D   id_col = "id"
##D )
##D 
##D # example 4: customizing plot appearance and saving
##D p4 <- margot_plot_boxplot_covariate(
##D   data = your_data,
##D   outcome = "envefficacy",
##D   covariate = "gender",
##D   y_label = "Environmental Efficacy",
##D   color_label = "Gender",
##D   legend_position = "bottom",
##D   y_limits = c(1, 7),
##D   save_path = "path/to/save",
##D   prefix = "env_efficacy",
##D   width = 10,
##D   height = 6,
##D   id_col = "id"
##D )
##D 
##D # example 5: using with categorical outcome and including timestamp
##D p5 <- margot_plot_boxplot_covariate(
##D   data = your_data,
##D   outcome = "env_climate_chg_cause",
##D   covariate = "political_party",
##D   y_label = "Perceived Cause of Climate Change",
##D   color_label = "Political Party",
##D   include_timestamp = TRUE,
##D   id_col = "id"
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_boxplot_covariate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_create_options")
### * margot_plot_create_options

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_create_options
### Title: Create Plot Options for Margot Plot
### Aliases: margot_plot_create_options

### ** Examples

## Not run: 
##D # Basic usage
##D health_options <- margot_plot_create_options("Health Outcomes")
##D 
##D # Custom title, filename prefix, and output saving
##D education_options <- margot_plot_create_options(
##D   "Education Outcomes",
##D   title = "Custom Title",
##D   filename_prefix = "edu_outcomes",
##D   save_output = TRUE,
##D   use_timestamp = TRUE,
##D   base_filename = "education_analysis",
##D   save_path = "path/to/save"
##D )
##D 
##D # Using label_mapping for custom outcome labels
##D trust_science_options <- margot_plot_create_options(
##D   subtitle = "Trust in Science Outcomes",
##D   title = "Science Trust Analysis",
##D   filename_prefix = "science_trust",
##D   label_mapping = list(
##D     "t2_trust_science_our_society_places_too_much_emphasis_reversed_z" = "Science Overemphasis"
##D   ),
##D   colors = c(
##D     "positive" = "#4CAF50",
##D     "not reliable" = "#FFC107",
##D     "negative" = "#F44336"
##D   ),
##D   base_size = 16,
##D   point_size = 4,
##D   save_output = TRUE
##D )
##D 
##D # Use the created options in margot_plot
##D result <- margot_plot(your_data, options = trust_science_options)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_create_options", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_discontinuity")
### * margot_plot_discontinuity

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_discontinuity
### Title: Create a Discontinuity Plot for Multiple Events
### Aliases: margot_plot_discontinuity

### ** Examples

## Not run: 
##D library(dplyr)
##D library(ggplot2)
##D library(margot)
##D 
##D # Assume that 'dat' is your dataset and that 'path_talk' is defined
##D muslim_discontinuity_warmth_plot <- margot_plot_discontinuity(
##D   data = dat,
##D   y_var = "warm_muslims",
##D   event_dates = c("2019-03-15", "2020-03-26"),
##D   event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
##D   start_date = "2012-06-06",
##D   title = "Discontinuity at multiple events (GAM)",
##D   y_label = "Muslim Warmth",
##D   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
##D   point_alpha = 0.05,
##D   smoothing_method = "gam",
##D   gam_k = 4,
##D   data_fraction = .1,
##D   seed = 123,
##D   save_path = here::here(path_talk)
##D )
##D 
##D # Display the plot
##D print(muslim_discontinuity_warmth_plot)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_discontinuity", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_hist")
### * margot_plot_hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_hist
### Title: Create a Coloured Histogram with Quantile or Custom Breaks
###   (DEPRECATED)
### Aliases: margot_plot_hist
### Keywords: internal

### ** Examples

df <- data.frame(value = rnorm(1000))
coloured_histogram_quantiles(df, "value", n_divisions = 4)

# With custom breaks
coloured_histogram_quantiles(df, "value", custom_breaks = c(-2, -1, 0, 1, 2))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_histogram")
### * margot_plot_histogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_histogram
### Title: Create a Histogram with Mean and Standard Deviation Highlights
###   for Each Wave and Variable
### Aliases: margot_plot_histogram

### ** Examples

# basic usage with default settings
margot_plot_histogram(
  data = your_data,
  col_names = c("variable1", "variable2"),
  id_col = "participant_id",
  wave_col = "survey_wave"
)

# specify waves and custom binwidth
margot_plot_histogram(
  data = your_data,
  col_names = c("score1", "score2"),
  waves = c(2018, 2020),
  binwidth = 1
)

# use custom labels and saving the plot with timestamp and prefix
margot_plot_histogram(
  data = your_data,
  col_names = c("attitude_measure"),
  title = "Distribution of Attitudes Over Time",
  x_label = "Attitude Score",
  save_path = "path/to/save/plot",
  add_timestamp = TRUE,
  file_prefix = "study1"
)

# use a custom color palette and custom line colors
custom_colors <- c("#FF9999", "#66B2FF")
margot_plot_histogram(
  data = your_data,
  col_names = c("var1", "var2"),
  color_palette = custom_colors,
  mean_line_color = "red",
  sd_line_color = "blue"
)

# use vertical faceting
margot_plot_histogram(
  data = your_data,
  col_names = c("var1", "var2"),
  vertical_facets = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_histogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_individual_responses")
### * margot_plot_individual_responses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_individual_responses
### Title: Create Individual Longitudinal Response Plots
### Aliases: margot_plot_individual_responses

### ** Examples

## Not run: 
##D # Example 1: Basic usage with default settings
##D plot1 <- margot_plot_individual_responses(
##D   data = your_data,
##D   y_vars = c("variable1", "variable2"),
##D   id_col = "participant_id",
##D   wave_col = "year"
##D )
##D 
##D # Example 2: Plotting specific waves and using random draws
##D plot2 <- margot_plot_individual_responses(
##D   data = your_data,
##D   y_vars = c("score1", "score2", "score3"),
##D   waves = c(2020, 2021, 2022),
##D   random_draws = 50,
##D   title = "Individual Scores Over Time",
##D   y_label = "Score",
##D   x_label = "Year",
##D   seed = 123
##D )
##D 
##D # Example 3: Customizing plot appearance and saving
##D plot3 <- margot_plot_individual_responses(
##D   data = your_data,
##D   y_vars = c("measure1", "measure2"),
##D   full_response_scale = TRUE,
##D   scale_range = c(0, 10),
##D   theme = theme_minimal(),
##D   wave_label_angle = 90,
##D   jitter_amount = 0.03,
##D   legend_position = "bottom",
##D   save_path = "path/to/save",
##D   prefix = "custom_plot"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_individual_responses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_multi_arm")
### * margot_plot_multi_arm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_multi_arm
### Title: Create a Multi-arm Margot Plot with User-specified Contrast
### Aliases: margot_plot_multi_arm

### ** Examples

## Not run: 
##D # Example usage with multi-arm models
##D multi_results <- margot_plot_multi_arm(
##D   models_multi$combined_tables,
##D   contrast = "(5.0,7.0] - [1.0,3.0]",
##D   original_df = df_raw_outcomes,
##D   options = multi_options,
##D   label_mapping = label_mapping,
##D   save_output = TRUE,
##D   save_path = here::here("output", "margot_plots"),
##D   base_filename = "margot_plot_output",
##D   prefix = "test"
##D )
##D print(multi_results$plot)
##D cat(multi_results$interpretation)
##D print(multi_results$transformed_table)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_multi_arm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_qini")
### * margot_plot_qini

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_qini
### Title: Plot Qini Curves from margot_multi_arm_causal_forest Results
### Aliases: margot_plot_qini

### ** Examples

## Not run: 
##D # Assuming mc.test is the result of margot_multi_arm_causal_forest()
##D plot_qini_curves(mc.test, "model_t2_belong_z")
##D 
##D # Using custom label mapping
##D label_mapping <- list(
##D   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
##D   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
##D )
##D plot_qini_curves(mc.test, "model_t2_env_not_env_efficacy_z", label_mapping = label_mapping)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_qini", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_rate")
### * margot_plot_rate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_rate
### Title: Plot Rank Average Treatment Effect
### Aliases: margot_plot_rate

### ** Examples

## Not run: 
##D # Assuming rate_eval is your rank_average_treatment_effect object
##D p <- margot_plot_rate(rate_eval, "model_t2_belong_z")
##D print(p)
##D 
##D # Using custom label mapping
##D label_mapping <- list(
##D   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
##D   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
##D )
##D p <- margot_plot_rate(rate_eval, "model_t2_env_not_env_efficacy_z",
##D                       label_mapping = label_mapping)
##D print(p)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_rate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_response_timeline")
### * margot_plot_response_timeline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_response_timeline
### Title: Plot Panel Study Response Timeline
### Aliases: margot_plot_response_timeline

### ** Examples

## Not run: 
##D # Load required libraries
##D library(dplyr)
##D library(lubridate)
##D library(ggplot2)
##D library(here)
##D 
##D # Assume we have a data frame 'nzavs_data' with columns: id, wave, tscore
##D 
##D # Step 1: Define NZAVS-specific wave breaks
##D nzavs_wave_breaks <- list(
##D   "time 1" = c(as.Date("2009-08-30"), as.Date("2010-10-15")),
##D   "time 2" = c(as.Date("2010-10-15"), as.Date("2011-10-15")),
##D   "time 3" = c(as.Date("2011-10-15"), as.Date("2012-10-15")),
##D   "time 4" = c(as.Date("2012-10-15"), as.Date("2013-10-15")),
##D   "time 5" = c(as.Date("2013-10-15"), as.Date("2014-10-15")),
##D   "time 6" = c(as.Date("2014-10-15"), as.Date("2015-10-15")),
##D   "time 7" = c(as.Date("2015-10-15"), as.Date("2016-10-15")),
##D   "time 8" = c(as.Date("2016-10-15"), as.Date("2017-10-15")),
##D   "time 9" = c(as.Date("2017-10-15"), as.Date("2018-10-15")),
##D   "time 10" = c(as.Date("2018-10-15"), as.Date("2019-10-15")),
##D   "time 11" = c(as.Date("2019-10-15"), as.Date("2020-10-15")),
##D   "time 12" = c(as.Date("2020-10-15"), as.Date("2021-10-15")),
##D   "time 13" = c(as.Date("2021-10-15"), as.Date("2022-10-15")),
##D   "time 14" = c(as.Date("2022-10-15"), as.Date("2023-10-15"))
##D )
##D 
##D # Step 2: Prepare the NZAVS data
##D prepared_data <- prepare_panel_data(
##D   dat = nzavs_data,
##D   wave_col = "wave",
##D   tscore_col = "tscore",
##D   id_col = "id",
##D   base_date = as.Date("2009-06-30"),
##D   wave_breaks = nzavs_wave_breaks
##D )
##D 
##D # Step 3: Create the NZAVS timeline plot
##D nzavs_timeline <- margot_plot_response_timeline(
##D   df_timeline = prepared_data$df_timeline,
##D   n_total_participants = prepared_data$n_total_participants,
##D   save = TRUE,
##D   save_png = TRUE,
##D   use_timestamp = TRUE,
##D   save_path = here::here("output", "plots"),
##D   title = "New Zealand Attitudes and Values Study (panel)",
##D   x_label = paste("NZAVS years",  min(prepared_data$df_timeline$day, na.rm = TRUE),
##D                   "-", max(prepared_data$df_timeline$day, na.rm = TRUE),
##D                   "cohort: daily counts by condition"),
##D   y_label = "Count of Responses"
##D )
##D 
##D # Display the plot
##D print(nzavs_timeline)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_response_timeline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_slope")
### * margot_plot_slope

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_slope
### Title: Create a Slope Plot for Multiple Variables
### Aliases: margot_plot_slope

### ** Examples

## Not run: 
##D library(dplyr)
##D library(ggplot2)
##D library(tidyr)
##D library(here)
##D 
##D # Basic usage with a single variable
##D single_var_plot <- margot_plot_slope(
##D   data = dat,
##D   y_vars = "warm_muslims",
##D   start_date = "2012-06-06",
##D   y_label = "Warmth",
##D   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)"
##D )
##D 
##D # Multiple variables with events and custom y-axis limits
##D multi_var_plot <- margot_plot_slope(
##D   data = dat,
##D   y_vars = list("warm_muslims", "warm_immigrants"),
##D   event_dates = c("2019-03-15", "2021-01-01"),
##D   event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
##D   start_date = "2012-06-06",
##D   y_label = "Warmth",
##D   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
##D   y_limits = c(1, 7),
##D   use_facets = TRUE
##D )
##D 
##D # Plot with points, using a subset of data and custom facet layout
##D point_plot <- margot_plot_slope(
##D   data = dat,
##D   y_vars = list("warm_asians", "warm_pacific", "warm_immigrants"),
##D   plot_points = TRUE,
##D   point_alpha = 0.05,
##D   data_fraction = 0.1,
##D   seed = 123,
##D   y_label = "Warmth",
##D   use_facets = TRUE,
##D   facet_ncol = 2
##D )
##D 
##D # Save the plot
##D saved_plot <- margot_plot_slope(
##D   data = dat,
##D   y_vars = list("political_orientation", "social_dominance_orientation"),
##D   save_path = here::here("outputs", "plots"),
##D   width = 10,
##D   height = 6,
##D   use_facets = TRUE
##D )
##D 
##D # Custom styling and color palette
##D custom_plot <- margot_plot_slope(
##D   data = dat,
##D   y_vars = list("sat_government", "sat_nz_econ_conditions"),
##D   event_dates = "2017-10-26",
##D   event_names = "2017 Election",
##D   y_label = "Satisfaction Level (0-10)",
##D   y_limits = c(0, 10),
##D   event_line_color = "blue",
##D   event_label_color = "blue",
##D   legend_position = "top",
##D   color_palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
##D   use_facets = TRUE
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_slope", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_slope_covariate")
### * margot_plot_slope_covariate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_slope_covariate
### Title: Create a Slope Plot using ggeffects
### Aliases: margot_plot_slope_covariate

### ** Examples

## Not run: 
##D # Example usage remains the same
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_slope_covariate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_slope_covariate_batch")
### * margot_plot_slope_covariate_batch

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_slope_covariate_batch
### Title: Create a Combined Slope Plot using ggeffects and patchwork
### Aliases: margot_plot_slope_covariate_batch

### ** Examples

## Not run: 
##D # Define outcome variables and label mapping
##D outcome_vars <- c("var1", "var2", "var3")
##D label_mapping <- list("var1" = "Variable 1", "var2" = "Variable 2", "var3" = "Variable 3")
##D 
##D # Create combined plot
##D combined_plot <- margot_plot_slope_covariate_batch(
##D   data = dat,
##D   outcome_vars = outcome_vars,
##D   exposure_formula = "~ wave:covariate",
##D   terms = c("wave", "covariate"),
##D   label_mapping = label_mapping,
##D   x_label = "Time",
##D   color_label = "Covariate",
##D   ncol = 2,
##D   plot_annotation_params = list(
##D     title = "Combined Slope Plots",
##D     subtitle = "Subtitle for the combined plot"
##D   ),
##D   save_path = "path/to/save/directory",
##D   width = 14,
##D   height = 10
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_slope_covariate_batch", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_slope_covariate_combo")
### * margot_plot_slope_covariate_combo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_slope_covariate_combo
### Title: Create a Combined Slope Plot using ggeffects and patchwork
### Aliases: margot_plot_slope_covariate_combo

### ** Examples

## Not run: 
##D # Define outcome variables and label mapping
##D outcome_vars <- c("var1", "var2", "var3")
##D label_mapping <- list("var1" = "Variable 1", "var2" = "Variable 2", "var3" = "Variable 3")
##D 
##D # Create combined plot
##D combined_plot <- margot_plot_slope_covariate_combo(
##D   data = dat,
##D   outcome_vars = outcome_vars,
##D   exposure_formula = "~ wave:covariate",
##D   terms = c("wave", "covariate"),
##D   label_mapping = label_mapping,
##D   x_label = "Time",
##D   color_label = "Covariate",
##D   ncol = 2,
##D   plot_annotation_params = list(
##D     title = "Combined Slope Plots",
##D     subtitle = "Subtitle for the combined plot"
##D   ),
##D   save_path = "path/to/save/directory",
##D   width = 14,
##D   height = 10
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_slope_covariate_combo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_plot_tau")
### * margot_plot_tau

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_plot_tau
### Title: Create Faceted Tau Hat Distribution Plots
### Aliases: margot_plot_tau

### ** Examples

## Not run: 
##D # with label mapping - pass models_binary directly
##D label_map <- list(
##D   "model_t2_belong_z" = "Social Belonging",
##D   "model_t2_trust_z" = "Trust in Others",
##D   "model_t2_log_charity_donate_z" = "Charitable Donations",
##D   "model_t2_log_hours_charity_z" = "Volunteer Hours"
##D )
##D 
##D # method 1: pass the parent object (auto-extracts $results)
##D tau_plot <- margot_plot_tau(
##D   models_binary,
##D   label_mapping = label_map,
##D   title = "Individual Treatment Effects"
##D )
##D 
##D # method 2: pass $results directly (also works)
##D tau_plot <- margot_plot_tau(
##D   models_binary$results,
##D   label_mapping = label_map,
##D   title = "Individual Treatment Effects"
##D )
##D 
##D # with different theme
##D tau_plot <- margot_plot_tau(
##D   models_binary,
##D   label_mapping = label_map,
##D   title = "Individual Treatment Effects",
##D   theme = "minimal"
##D )
##D 
##D # without conditional colouring
##D tau_plot <- margot_plot_tau(
##D   models_binary,
##D   label_mapping = label_map,
##D   title = "Individual Treatment Effects",
##D   colour_by_sign = FALSE,
##D   fill_colour = "lightblue"
##D )
##D 
##D # with custom colours for above/below zero
##D tau_plot <- margot_plot_tau(
##D   models_binary,
##D   label_mapping = label_map,
##D   title = "Individual Treatment Effects",
##D   colour_below = "darkred",
##D   colour_above = "darkgreen"
##D )
##D 
##D # add borders if desired
##D tau_plot <- margot_plot_tau(
##D   models_binary,
##D   label_mapping = label_map,
##D   border_colour = "grey50"
##D )
##D 
##D # without label mapping (auto transform)
##D tau_plot <- margot_plot_tau(models_binary)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_plot_tau", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_process_binary_vars")
### * margot_process_binary_vars

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_process_binary_vars
### Title: Process Binary Variables in a Data Frame
### Aliases: margot_process_binary_vars

### ** Examples

df <- data.frame(
  a = factor(c("yes", "no", "yes")),
  b = c(1, 0, 1),
  c = c("apple", "banana", "apple"),
  d = factor(c("true", "false", "true")),
  e_binary = c(0, 1, 0)
)
processed_df <- margot_process_binary_vars(df, exceptions = "c")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_process_binary_vars", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_process_longitudinal_data")
### * margot_process_longitudinal_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_process_longitudinal_data
### Title: process longitudinal data for three waves
### Aliases: margot_process_longitudinal_data

### ** Examples

# assuming df_wide is your wide-format dataframe with three waves
processed_data <- margot_process_longitudinal_data(
  df_wide,
  ordinal_columns = c("education", "income_category"),
  continuous_columns_keep = c("age", "bmi")
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_process_longitudinal_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_prop_missing")
### * margot_prop_missing

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_prop_missing
### Title: Proportion of missing data at baseline
### Aliases: margot_prop_missing

### ** Examples

# Example using a dataset with a wave column
# assume dat_long_amelia_log has a column called wave
margot_prop_missing(dat_long_amelia_log)

# Example using a dataset without a wave column
# assume some_data is a dataset with no wave column
margot_prop_missing(some_data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_prop_missing", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_propensity_model_and_plots")
### * margot_propensity_model_and_plots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_propensity_model_and_plots
### Title: Create Propensity Score Model and Associated Plots
### Aliases: margot_propensity_model_and_plots

### ** Examples

## Not run: 
##D # Assuming df_propensity is your dataset with appropriate variables
##D results <- margot_propensity_model_and_plots(
##D   df_propensity = df_propensity,
##D   exposure_variable = "treatment",
##D   baseline_vars = c("age", "sex", "bmi"),
##D   weights_var_name = "sample_weights",
##D   love_plot_options = list(
##D     thresholds = c(m = .05),
##D     size = 4
##D   ),
##D   bal_tab_options = list(
##D     thresholds = c(m = .1, v = 2.5)
##D   ),
##D   verbose = TRUE
##D )
##D 
##D # Access the results
##D print(results$summary)
##D plot(results$love_plot)
##D print(results$balance_table)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_propensity_model_and_plots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_reversed_labels")
### * margot_reversed_labels

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_reversed_labels
### Title: Update label map by marking reversed outcomes
### Aliases: margot_reversed_labels

### ** Examples

label_mapping_all <- list(
  t2_log_hours_exercise_z        = "Hours of Exercise (log)",
  t2_hlth_fatigue_z              = "Fatigue",
  t2_kessler_latent_anxiety_z    = "Anxiety",
  t2_kessler_latent_depression_z = "Depression",
  t2_rumination_z                = "Rumination",
  t2_foregiveness_z              = "Forgiveness",
  t2_perfectionism_z             = "Perfectionism",
  t2_self_esteem_z               = "Self Esteem",
  t2_gratitude_z                 = "Gratitude",
  t2_lifesat_z                   = "Life Satisfaction",
  t2_meaning_purpose_z           = "Meaning: Purpose",
  t2_meaning_sense_z             = "Meaning: Sense",
  t2_pwi_z                       = "Personal Well-being Index",
  t2_belong_z                    = "Social Belonging",
  t2_neighbourhood_community_z   = "Neighbourhood Community",
  t2_support_z                   = "Social Support"
)

flip_outcomes <- c(
  "t2_kessler_latent_anxiety_z",
  "t2_kessler_latent_depression_z",
  "t2_rumination_z"
)

# update mapping
label_mapping_all <- mark_reversed_labels(label_mapping_all, flip_outcomes)

print(label_mapping_all)
#> $t2_log_hours_exercise_z
#> [1] "Hours of Exercise (log)"
#> ...
#> $t2_kessler_latent_anxiety_z
#> [1] "Anxiety (reversed)"
#> ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_reversed_labels", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_save_png")
### * margot_save_png

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_save_png
### Title: Save Margot Plot as PNG
### Aliases: margot_save_png

### ** Examples

## Not run: 
##D # Assuming you have already run margot_plot() or margot_plot_multi_arm()
##D plot_result <- margot_plot(your_data, your_options)
##D 
##D # Save the plot as PNG
##D margot_save_png(
##D   plot_result,
##D   prefix = "study1",
##D   base_filename = "treatment_effects",
##D   save_path = here::here("output", "plots")
##D )
## End(Not run)

## Not run: 
##D # For a ggplot object
##D plot_result <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
##D margot_save_png(plot_result, prefix = "study1", base_filename = "scatter")
##D 
##D # For a base R plot
##D plot_result <- plot(mtcars$mpg, mtcars$wt)
##D margot_save_png(plot_result, prefix = "study1", base_filename = "scatter")
##D 
##D # For a list containing a plot
##D plot_list <- list(plot = ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point())
##D margot_save_png(plot_list, prefix = "study1", base_filename = "scatter")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_save_png", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_size")
### * margot_size

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_size
### Title: Calculate the size of an R object in megabytes
### Aliases: margot_size

### ** Examples

big_matrix <- matrix(rnorm(1e6), nrow = 1000)
margot_size(big_matrix, "Big Matrix")

summary_tables <- list(table1 = data.frame(a = 1:1000, b = rnorm(1000)))
margot_size(summary_tables, "Summary Tables")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_size", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_subset_model")
### * margot_subset_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_subset_model
### Title: Subset Model Results for Binary and Categorical Exposures
### Aliases: margot_subset_model

### ** Examples

## Not run: 
##D # Example 1: Basic subsetting with a pre-computed condition
##D subset_condition_conservative <- X[, "t0_political_conservative_z"] > 1
##D model_subset_conservative <- margot_subset_model(
##D   model_results = models_binary,
##D   subset_condition = subset_condition_conservative,
##D   debug = FALSE
##D )
##D 
##D # Example 2: Using the built-in subsetting functionality
##D model_subset_religious <- margot_subset_model(
##D   model_results = models_binary,
##D   X = X,
##D   subset_var = "t0_religion_bigger_denominations_not_rel_binary",
##D   subset_value = 1,
##D   subset_description = "Effects among religious participants",
##D   debug = TRUE
##D )
##D 
##D # Example 3: For categorical exposures with specific contrast
##D model_subset_gen_z <- margot_subset_model(
##D   model_results = models_cat,
##D   X = X,
##D   subset_var = "t0_gen_cohort_gen_Z_binary",
##D   subset_value = 1,
##D   contrast = "[6.0,7.0] - [1.0,5.0)",
##D   scale = "RD",
##D   debug = FALSE
##D )
##D 
##D # Example 4: Multiple outcome variables
##D model_subset_boomers <- margot_subset_model(
##D   model_results = models_binary,
##D   outcome_vars = c("t2_wellbeing_z", "t2_depression_z"),
##D   X = X,
##D   subset_var = "t0_gen_cohort_gen_Boomers_binary",
##D   subset_value = 1,
##D   subset_operator = "==",
##D   subset_description = "Effects among Baby Boomer participants",
##D   debug = TRUE
##D )
##D 
##D # Example 5: More complex subsetting (multiple conditions)
##D # Define a complex condition directly
##D complex_condition <- X[, "t0_political_conservative_z"] > 1 & X[, "t0_age_z"] > -2
##D model_subset_complex <- margot_subset_model(
##D   model_results = models_binary,
##D   subset_condition = complex_condition,
##D   subset_description = "Conservative (>1 SD) and not very young (>-2 SD in age)",
##D   debug = FALSE
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_subset_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_summary_cate_difference_gain")
### * margot_summary_cate_difference_gain

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_summary_cate_difference_gain
### Title: Compute Difference in Gains and Integrated Difference Between
###   Reference and Comparison Curves
### Aliases: margot_summary_cate_difference_gain

### ** Examples

## Not run: 
##D # Assuming mc_result is the result of margot_multi_arm_causal_forest()
##D result <- margot_summary_cate_difference_gain(mc_result,
##D                                  outcome_var = "model_Y",
##D                                  reference_curve = "baseline",
##D                                  comparison_curve = "arm2",
##D                                  spend = 0.3)
##D # Use in text
##D glue::glue("The difference in gains is {result$diff_gain}. {result$summary}")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_summary_cate_difference_gain", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_summary_panel")
### * margot_summary_panel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_summary_panel
### Title: Generate Summary Panel for Margot Study
### Aliases: margot_summary_panel

### ** Examples

## Not run: 
##D # Assuming 'dat' is your dataset
##D results <- margot_summary_panel(dat)
##D 
##D # Custom settings
##D custom_results <- margot_summary_panel(
##D   data = dat,
##D   output_format = "markdown",
##D   group_waves_at = 4,
##D   id_col = "participant_id",
##D   wave_col = "survey_wave",
##D   year_measured_col = "measurement_year"
##D )
##D 
##D # View results
##D results$unique_ids_by_wave
##D results$participant_wave_summary
##D results$participant_wave_summary_grouped
##D 
##D # For markdown output
##D cat(custom_results$unique_ids_by_wave)
##D cat(custom_results$participant_wave_summary)
##D cat(custom_results$participant_wave_summary_grouped)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_summary_panel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_trim_sample_weights")
### * margot_trim_sample_weights

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_trim_sample_weights
### Title: Standardise and (optionally) trim sample weights at both ends
### Aliases: margot_trim_sample_weights

### ** Examples

set.seed(42)
w <- c(rlnorm(90, 0, 0.5), runif(5, 5, 20), runif(5, 0, 0.01), NA)
summary(w)

# trim both lower 1% and upper 99%, then standardise
w_both <- margot_trim_sample_weights(
  w,
  lower_quantile = 0.01,
  upper_quantile = 0.99
)
summary(w_both)

# only upper trim at 95th percentile
w_up95 <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = 0.95)
summary(w_up95)

# only lower trim at 5th percentile
w_low5 <- margot_trim_sample_weights(w, lower_quantile = 0.05, upper_quantile = NULL)
summary(w_low5)

# no trimming (both NULL), only standardise
w_std <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = NULL)
summary(w_std)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_trim_sample_weights", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margot_wide_impute_baseline")
### * margot_wide_impute_baseline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margot_wide_impute_baseline
### Title: Transform longitudinal data to wide format and impute baseline
###   (soft-deprecated)
### Aliases: margot_wide_impute_baseline
### Keywords: internal

### ** Examples

# Preferred: use margot_wide_machine with mice imputation
# wide_df <- margot_wide_machine(
#   df,
#   baseline_vars = c("age", "male"),
#   exposure_var = "forgiveness",
#   outcome_vars = "kessler_latent_anxiety",
#   imputation_method = "mice"
# )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margot_wide_impute_baseline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare_panel_data")
### * prepare_panel_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare_panel_data
### Title: Prepare Panel Data for Timeline Visualization
### Aliases: prepare_panel_data

### ** Examples

## Not run: 
##D dat <- read.csv("panel_data.csv")
##D wave_breaks <- list(
##D   "wave 1" = c(as.Date("2010-01-01"), as.Date("2010-12-31")),
##D   "wave 2" = c(as.Date("2011-01-01"), as.Date("2011-12-31"))
##D )
##D prepared_data <- prepare_panel_data(dat, wave_col = "Wave", tscore_col = "TimeScore",
##D                                     id_col = "ParticipantID", base_date = "2010-01-01",
##D                                     wave_breaks = wave_breaks)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare_panel_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pretty_number")
### * pretty_number

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pretty_number
### Title: Format Numbers with Commas
### Aliases: pretty_number

### ** Examples

numbers <- c(1000, 50000, 1234567)
pretty_number(numbers)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pretty_number", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("regress_with_covariates")
### * regress_with_covariates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: regress_with_covariates
### Title: Generalized Linear Regression with Covariates
### Aliases: regress_with_covariates

### ** Examples

## Not run: 
##D # using `df_nz` is your data frame with "income" as the continuous outcome variable,
##D # "age" as an exposure variable, and other covariates
##D outcome_var <- "income"
##D exposure_var <- "age"
##D baseline_vars <- c("age", "education", "partner")
##D model <- regress_with_covariates(df_nz, outcome_var, exposure_var, baseline_vars, family =  gaussian())
##D summary(model)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("regress_with_covariates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("remove_numeric_attributes")
### * remove_numeric_attributes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: remove_numeric_attributes
### Title: Remove Attributes from Numeric Columns in a Data Frame
### Aliases: remove_numeric_attributes

### ** Examples

df <- data.frame(a = I(1:3), b = c("x", "y", "z"), c = I(rnorm(3)))
cleaned_df <- remove_numeric_attributes(df)
str(cleaned_df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("remove_numeric_attributes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("select_and_rename_cols")
### * select_and_rename_cols

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: select_and_rename_cols
### Title: Select and Rename Columns Based on Criteria
### Aliases: select_and_rename_cols

### ** Examples

names_base <- c("t0_age", "t0_weight", "t0_height", "t0_outcome")
baseline_vars <- c("age", "weight")
outcome_var <- "t2_outcome"

final_columns <- select_and_rename_cols(names_base, baseline_vars, outcome_var, "t2", "t0")
print(final_columns)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("select_and_rename_cols", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tab_engine_marginal")
### * tab_engine_marginal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tab_engine_marginal
### Title: Tabulate Marginal Effects with E-Values
### Aliases: tab_engine_marginal
### Keywords: internal

### ** Examples

# Assuming you have results from a simulation or model in `results_df`
tabulated_results <- tab_engine_marginal(x = results_df,
                                         new_name = "Treatment Effect",
                                         delta = 1,
                                         sd = 0.2,
                                         type = "RD")  # Corrected 'scale' to 'type'




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tab_engine_marginal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("transition_table")
### * transition_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: transition_table
### Title: Transition Table
### Aliases: transition_table
### Keywords: internal

### ** Examples

## Not run: 
##D df <- read.table(header=TRUE, text="
##D id wave year_measured religion_believe_god
##D 3 0 1 0
##D 3 1 1 1
##D 4 0 1 0
##D 4 1 1 1
##D 5 0 1 1
##D 5 1 1 0")
##D 
##D transition_matrix <- create_transition_matrix(df, "religion_believe_god", "id")
##D # Assuming `transition_matrix` is a table with the transition counts between states
##D # First, convert `transition_matrix` to a dataframe suitable for `transition_table`
##D df_transition <- as.data.frame.matrix(transition_matrix)
##D df_transition$from <- rownames(df_transition)
##D long_df_transition <- tidyr::pivot_longer(df_transition, cols = -from, names_to = "to", values_to = "Freq")
##D 
##D transition_table_data <- transition_table(long_df_transition)
##D cat(transition_table_data$explanation)
##D cat("\n")not
##D print(transition_table_data$table)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("transition_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
