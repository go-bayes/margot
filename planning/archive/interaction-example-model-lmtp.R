
# restart -----------------------------------------------------------------
# new session
# rstudioapi::restartSession()

# library ------------------------------------------------------------------
# set key library
# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
library(margot)

# version
packageVersion(pkg = 'margot')

# reproducibility
set.seed(123)

# set data paths
push_mods <- here::here("/Users/joseph/v-project\ Dropbox/data/25-god-flourish-long")



# customisations ----------------------------------------------------------
# get name
name_exposure <- margot::here_read("name_exposure")

# check
name_exposure

# import data
df_lmtp_psych <- margot::here_read_qs('df_lmtp_psych', push_mods)
df_lmtp_present <- margot::here_read_qs('df_lmtp_present', push_mods)
df_lmtp_life <- margot::here_read_qs('df_lmtp_life', push_mods)


# import untransformed data
original_df <- margot::here_read_qs('df_wide_impute', push_mods)

# checks
head(original_df)


# label mappings ----------------------------------------------------------

# define psych outcomes 
label_mapping_psych <- list(
  "t5_hlth_fatigue_z" = "Fatigue", 
  "t5_kessler_latent_anxiety_z" = "Anxiety", 
  "t5_kessler_latent_depression_z" = "Depression",  
  "t5_rumination_z" = "Rumination"
)

# label mappings for present reflective outcomes
label_mapping_present <- list(
  "t5_bodysat_z" = "Body Satisfaction",
  "t5_foregiveness_z" = "Forgiveness",  
  "t5_perfectionism_z" = "Perfectionism",  
  "t5_self_control_z" = "Self Control",  
  "t5_sexual_satisfaction_z" = "Sexual Satisfaction"
)

# define life outcomes
label_mapping_life <- list(
  "t5_gratitude_z" = "Gratitude", 
  "t5_lifesat_z" = "Life Satisfaction", 
  "t5_lifemeaning_z" = "Meaning and Purpose", 
  "t5_pwi_z" = "Personal Well-being Index"
)

# options -----------------------------------------------------------------
# set folds (should in most cases be "5")
number_folds = 5

# set trim for weights (consistent weighting)
set_trim = .99 # no lower than .95

# set null shift value (should the average be computed)
set_null_shift = TRUE

# set shift functions -----------------------------------------------------
# define shifts
# treatment_top <- function(data, trt) {
#   ifelse(data[[trt]] !=1, 1, data[[trt]])
# }
# 
# treatment_bottom <- function(data, trt) {
#   ifelse(data[[trt]] != 0, 0, data[[trt]])
# }


# define vectorised shift function.  
believe_both <- function(data, trt_list) {
  shifted_treatments <- list()
  
  # flatten list of treatment variables
  all_trts <- unlist(trt_list)
  
  # apply shift logic with error handling
  shifted_treatments <- lapply(all_trts, function(trt_var) {
    if (!trt_var %in% names(data)) {
      stop(paste("Treatment variable", trt_var, "not found in the data."))
    }
    
    if (grepl("god", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=1, 1, data[[trt_var]]))
    } else if (grepl("spirit", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=1, 1, data[[trt_var]]))
    } else {
      stop(paste("unexpected treatment variable:", trt_var))
    }
  })
  
  # name list elements 
  names(shifted_treatments) <- all_trts
  
  return(shifted_treatments)
}


believe_neither <- function(data, trt_list) {
  shifted_treatments <- list()
  
  # flatten list of treatment variables
  all_trts <- unlist(trt_list)
  
  # apply shift logic
  shifted_treatments <- lapply(all_trts, function(trt_var) {
    if (!trt_var %in% names(data)) {
      stop(paste("Treatment variable", trt_var, "not found in the data."))
    }
    
    if (grepl("god", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=0, 0, data[[trt_var]]))
    } else if (grepl("spirit", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=0, 0, data[[trt_var]]))
    } else {
      stop(paste("Unexpected treatment variable:", trt_var))
    }
  })
  
  # list elements 
  names(shifted_treatments) <- all_trts
  
  return(shifted_treatments)
}


believe_only_spirit <- function(data, trt_list) {
  shifted_treatments <- list()d
  
  # flatten
  all_trts <- unlist(trt_list)
  
  # shift logic
  shifted_treatments <- lapply(all_trts, function(trt_var) {
    if (!trt_var %in% names(data)) {
      stop(paste("Treatment variable", trt_var, "not found in the data."))
    }
    
    if (grepl("god", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=0, 0, data[[trt_var]]))
    } else if (grepl("spirit", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=1, 1, data[[trt_var]]))
    } else {
      stop(paste("Unexpected treatment variable:", trt_var))
    }
  })
  
  # names
  names(shifted_treatments) <- all_trts
  
  return(shifted_treatments)
}

believe_only_god <- function(data, trt_list) {
  shifted_treatments <- list()
  
  # flatten
  all_trts <- unlist(trt_list)
  
  # shift logic
  shifted_treatments <- lapply(all_trts, function(trt_var) {
    if (!trt_var %in% names(data)) {
      stop(paste("Treatment variable", trt_var, "not found in the data."))
    }
    
    if (grepl("god", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=1, 1, data[[trt_var]]))
    } else if (grepl("spirit", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] !=0, 0, data[[trt_var]]))
    } else {
      stop(paste("Unexpected treatment variable:", trt_var))
    }
  })
  
  # name list
  names(shifted_treatments) <- all_trts
  
  return(shifted_treatments)
}



# load all libraries ------------------------------------------------------
pacman::p_load(
  clarify,      # sensitivity analysis for causal inference
  DiagrammeR,   # graph and network visualization
  doParallel,   # parallel processing with foreach
  future,       # multi-core
  fastDummies,  # fast creation of dummy variables
  fs,           # cross-platform file system operations
  # ggbeeswarm,   # data visualisation   
  ggplot2,      # data visualisation
  glmnet,       # lasso and elastic-net regularized models
  glue, # text
  grf,          # generalized random forests
  gt,           # html tables for data frames
  gtsummary,    # summary tables for regression models
  here,         # simple and robust file referencing
  janitor,      # data cleaning and validation
  kableExtra,   # advanced table formatting
  lmtp,         # longitudinal targeted maximum likelihood estimation
  margot,       # functions for casual inference
  naniar,       # handling and visualization of missing data
  parameters,   # parameters and performance metrics
  policytree,   # causal inference with policy trees
  progressr,    # progress reporting for R
  ranger,       # fast implementation of random forests
  skimr,        # summary statistics for data frames
  SuperLearner, # ensemble learning
  tidyverse,    # collection of R packages for data science
  WeightIt,     # weighting methods for covariate balancing
  xgboost,      # extreme gradient boosting
  EValue,       # compute Evalues
  maq,          # qini curves
  purrr,        # data wrangling
  patchwork,     # multiple plots
  labelled,
  purrr, 
  glmnet,       # learner
  xgboost,      # learner
  nnls,         # learner
  ranger,        # learner
  cli,
  crayon,
  rlang, 
  margot, 
  boilerplate,
  future,       # multi-core
  zoo
)

# set learners
sl_lib = c("SL.ranger", "SL.xgboost", "SL.glmnet")

# run 5 cores for this instance
# multisession
future::plan(future::multisession, workers = 5)

# progress bars
progressr::handlers(global = TRUE)

# list learners
listWrappers()


# set names ---------------------------------------------------------------
# get name
name_exposure_1 <- margot::here_read("name_exposure_1")
name_exposure_2 <- margot::here_read("name_exposure_2")


# exposure var 1
t0_name_exposure_1 <- paste0("t0_", name_exposure_1)
t1_name_exposure_1 <- paste0("t1_", name_exposure_1)
t2_name_exposure_1 <- paste0("t2_", name_exposure_1)
t3_name_exposure_1 <- paste0("t3_", name_exposure_1)
t4_name_exposure_1 <- paste0("t4_", name_exposure_1)


# exposure var 2
t0_name_exposure_2 <- paste0("t0_", name_exposure_2)
t1_name_exposure_2 <- paste0("t1_", name_exposure_2)
t2_name_exposure_2 <- paste0("t2_", name_exposure_2)
t3_name_exposure_2 <- paste0("t3_", name_exposure_2)
t4_name_exposure_2 <- paste0("t4_", name_exposure_2)


# read raw outcomes 
raw_outcomes_psych <- here_read("raw_outcomes_psych")
t5_outcome_psych_z <- paste0("t5_", raw_outcomes_psych, "_z")
t5_outcome_psych_z <- sort(t5_outcome_psych_z)
t5_outcome_psych_z

# read raw outcomes
raw_outcomes_present <- here_read("raw_outcomes_present")
t5_outcome_present_z <- paste0("t5_", raw_outcomes_present, "_z")
t5_outcome_present <- sort(t5_outcome_present_z)
t5_outcome_present_z

# read raw outcomes
raw_outcomes_life <- here_read("raw_outcomes_life")
t5_outcome_life_z <- paste0("t5_", raw_outcomes_life, "_z")
t5_outcome_life_z <- sort(t5_outcome_life_z)
t5_outcome_life_z

# start analysis here ----------------------------------------------------
# import data
# new data has more efficient processing
# df_lmtp_health <- margot::here_read_qs('df_lmtp_health', push_mods)

# df_lmtp_social <- margot::here_read_qs('df_lmtp_social', push_mods)

# lagged time-varying confoundres -----------------------------------------
# recommended approach (see appendix for alternatives)
# We use a strategy where we ensure disability in the present wave is not a confounder (unlikely to be an effect of religion)

# time-varying covariates # think about these. Note Outcomes are included
# so this helps address reverse causation.
confounder_vars <- here_read("confounder_vars")

# check
confounder_vars

# check
time_vary_confounder_vars <- setdiff(confounder_vars, "hlth_disability_binary")
time_vary_confounder_vars


# time-varying confounders t1 
L1 <- df_lmtp_psych |>
  select(matches(paste0("^t1_", "hlth_disability_binary"))) |>
  colnames() 

# view
L1

# time-varying confounders t2
L2 <- df_lmtp_psych |>
  select(matches(paste0("^t1_", time_vary_confounder_vars)), matches(paste0("^t2_", "hlth_disability_binary"))) |>
  colnames() 

# view
L2

# time-varying confounders t3
L3 <- df_lmtp_psych |>
  select(matches(paste0("^t2_", time_vary_confounder_vars)), matches(paste0("^t3_", "hlth_disability_binary"))) |>
  colnames() 

# view
L3

# time-varying confounders t4 
L4 <- df_lmtp_psych |>
  select(matches(paste0("^t3_", time_vary_confounder_vars)), matches(paste0("^t4_", "hlth_disability_binary"))) |>
  colnames() 

# append time-vary confounders
L <- list(c(L1), c(L2), c(L3), c(L4))

# check
L

# import names of baseline covariates
B <- margot::here_read('E')

# check
B

#  model

# variables # INTERACTION SPEC
A_list <- list(
  c("t1_believe_god_binary", "t1_believe_spirit_binary"),  # time point 1
  c("t2_believe_god_binary", "t2_believe_spirit_binary"),  # time point 2
  c("t3_believe_god_binary", "t3_believe_spirit_binary"),  # time point 3
  c("t4_believe_god_binary", "t4_believe_spirit_binary")   # time point 4
)

# check
A

# append censoring
C <- c(
  "t1_not_lost_following_wave",
  "t2_not_lost_following_wave",
  "t3_not_lost_following_wave",
  "t4_not_lost_following_wave"
)

# check
C

# import names of baseline covariates
B <- margot::here_read('E')

# check
print(B)


#t0_adjusted_combo_gender_weights # which weights depnds on the questions
weights <- df_lmtp_psych$t0_adjusted_weights # weights

# view weights
hist(weights)


# shift functions ---------------------------------------------------------


# test --------------------------------------------------------------------
df_slice <- df_lmtp_psych |>
  slice_head(n = 500) |>
  as.data.frame()

# needs to be nrows of df_slice
weights_test <- df_slice$t0_adjusted_weights

# checks
all_trts <- unlist(A_list)
missing_trts <- setdiff(all_trts, names(df_slice))
if(length(missing_trts) > 0){
  stop(paste("Missing treatment variables in data:", paste(missing_trts, collapse = ", ")))
}

missing_cens <- setdiff(C, names(df_slice))
if(length(missing_cens) > 0){
  stop(paste("Missing censoring variables in data:", paste(missing_cens, collapse = ", ")))
}

# test model with rr outcome ----------------------------------------------
# # test functions
time_taken_speed <- system.time({
  fit_test_speed <- margot_lmtp(
    data = df_slice,
    outcome_vars = "t5_rumination_z",
    trt = A_list,
    shift_functions = list(believe_both = believe_both,
                           believe_only_god = believe_only_god,
                           believe_only_spirit = believe_only_spirit, 
                           believe_neither = believe_neither), 
    include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr,
    contrast_type = 'pairwise',
    contrast_scale = 'additive',
    save_output = TRUE,
    save_path = here::here(push_mods),
    prefix = "test_",
    lmtp_defaults = list(
      baseline = B,
      mtp = TRUE,
      folds = 5,
      outcome_type = "continuous",
      cens = C,
      time_vary = L,
      weights = weights_test,
      learners_trt = "SL.ranger",
      learners_outcome = "SL.ranger",
      control = lmtp_control(.trim = set_trim)
    )
  )
})

# view results
fit_test_speed$combined_tables$combined_outcomes_believe_both_vs_believe_only_god
fit_test_speed$combined_tables$combined_outcomes_believe_both_vs_believe_only_spirit
fit_test_speed$combined_tables$combined_outcomes_believe_both_vs_believe_neither
fit_test_speed$combined_tables$combined_outcomes_believe_only_god_vs_believe_only_spirit
fit_test_speed$combined_tables$combined_outcomes_believe_only_god_vs_believe_neither
fit_test_speed$combined_tables$combined_outcomes_believe_only_spirit_vs_believe_neither



plot_religious_vs_secular <- margot_plot(fit_test_speed$combined_tables$combined_outcomes_believe_all_believe_neither,
                                         # options = options_base, 
                                         save_output = FALSE, 
                                         save_path = here::here(push_mods), 
                                         base_filename = "plot_done_vs_religious", 
                                         original_df = original_df)

plot_religious_vs_secular$plot



# lmtp models -------------------------------------------------------------

# MODELS ------------------------------------------------------------------

# checks
sl_lib
A_list
B
L

# uncomment to run
# health_lmtp_output <- margot_lmtp(
#   data = df_lmtp_health,
#   outcome_vars = t5_outcome_health_z,
#   trt = A_list,
#   shift_functions = list(believe_both = believe_both,
#                          believe_only_god = believe_only_god,
#                          believe_only_spirit = believe_only_spirit, 
#                          believe_neither = believe_neither), 
#   include_null_shift = TRUE,
#   lmtp_model_type = lmtp::lmtp_sdr,
#   contrast_type = 'pairwise',
#   contrast_scale = 'additive',
#   save_output = TRUE,
#   save_path = here::here(push_mods),
#   prefix = "health",
#   lmtp_defaults = list(
#     baseline = B,
#     mtp = TRUE,
#     folds = number_folds,
#     outcome_type = "continuous",
#     cens = C,
#     weights = weights,
#     time_vary = L,
#     learners_trt = sl_lib,
#     learners_outcome = sl_lib,
#     control = lmtp_control(.trim = set_trim)
#   )
# )


# psych model --------------------------------------------------------------


# uncomment to run
psych_lmtp_output <- margot_lmtp(
  data = df_lmtp_psych,
  outcome_vars = t5_outcome_psych_z,
  trt = A_list,
  shift_functions = list(believe_both = believe_both,
                         believe_only_god = believe_only_god,
                         believe_only_spirit = believe_only_spirit, 
                         believe_neither = believe_neither), 
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = 'pairwise',
  contrast_scale = 'additive',
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "psych",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = number_folds,
    outcome_type = "continuous",
    cens = C,
    time_vary = L,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)


# present model -----------------------------------------------------------
# uncomment to run
present_lmtp_output <- margot_lmtp(
  data = df_lmtp_present,
  outcome_vars = t5_outcome_present_z,
  trt = A_list,
  shift_functions = list(believe_both = believe_both,
                         believe_only_god = believe_only_god,
                         believe_only_spirit = believe_only_spirit, 
                         believe_neither = believe_neither), 
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = 'pairwise',
  contrast_scale = 'additive',
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "present",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = number_folds,
    outcome_type = "continuous",
    cens = C,
    time_vary = L,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)



# life model --------------------------------------------------------------
# uncomment to run
life_lmtp_output <- margot_lmtp(
  data = df_lmtp_life,
  outcome_vars = t5_outcome_life_z,
  trt = A_list,
  shift_functions = list(believe_both = believe_both,
                         believe_only_god = believe_only_god,
                         believe_only_spirit = believe_only_spirit, 
                         believe_neither = believe_neither), 
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = 'pairwise',
  contrast_scale = 'additive',
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "life",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = number_folds,
    outcome_type = "continuous",
    cens = C,
    time_vary = L,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# uncomment to run NOT RUN
# social_lmtp_output <- margot_lmtp(
#   data = df_lmtp_social,
#   outcome_vars = t5_outcome_social_z,
#   trt = A_list,
#   shift_functions = list(believe_both = believe_both,
#                          believe_only_god = believe_only_god,
#                          believe_only_spirit = believe_only_spirit, 
#                          believe_neither = believe_neither), 
#   include_null_shift = TRUE,
#   lmtp_model_type = lmtp::lmtp_sdr,
#   contrast_type = 'pairwise',
#   contrast_scale = 'additive',
#   save_output = TRUE,
#   save_path = here::here(push_mods),
#   prefix = "social",
#   lmtp_defaults = list(
#     baseline = B,
#     mtp = TRUE,
#     folds = number_folds,
#     outcome_type = "continuous",
#     cens = C,
#     time_vary = L,
#     weights = weights,
#     learners_trt = sl_lib,
#     learners_outcome = sl_lib,
#     control = lmtp_control(.trim = set_trim)
#   )
# )

