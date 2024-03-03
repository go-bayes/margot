## code to prepare `DATASET` dataset goes here
# if (!requireNamespace("haven", quietly = TRUE)) {
#   install.packages("haven")
# }

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
# str(df_nz)
#
# # save data
# usethis::use_data(df_nz, overwrite = TRUE)
