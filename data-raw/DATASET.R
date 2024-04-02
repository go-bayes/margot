# code to prepare `DATASET` dataset goes here
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

# local path (for jb)
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data"
  )

library(dplyr)
library(tidyverse)
# # read arrow
df_nz <-
  arrow::read_parquet(here::here(push_mods, "df_nz_synth_data_t14"))

str(df_nz)
str( df_nz$male)
# # check
colnames(df_nz)
str(df_nz$male)
#
# # save data
usethis::use_data(df_nz, overwrite = TRUE)
