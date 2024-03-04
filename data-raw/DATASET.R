## code to prepare `DATASET` dataset goes here
# if (!requireNamespace("haven", quietly = TRUE)) {
#   install.packages("haven")
# }
#
# local path (for jb)
# push_mods <-
#   fs::path_expand(
#     "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data"
#   )
#
#
# library(haven)
# # read arrow
# df_nz <- arrow::read_parquet( here::here(push_mods, "df_nz_synth_data_t14"))
#
# # remove haven labels
# df_nz <- df_nz %>%
#   mutate(across(where(is.labelled), ~as.numeric(zap_labels(.))))
#
# # check
# colnames(df_nz)
#
# # save data
# usethis::use_data(df_nz, overwrite = TRUE)
#
# # make minimal example for data examples
#
# df_nz_light <- df_nz |>
#   dplyr::select(id, wave, year_measured, age, gen_cohort, male, kessler_latent_depression, kessler_latent_anxiety, religion_believe_god, hlth_height, hlth_weight)
#
# usethis::use_data(df_nz_light, overwrite = TRUE)
