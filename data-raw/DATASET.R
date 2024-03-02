## code to prepare `DATASET` dataset goes here

# local path (for jb)
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data"
  )



# read arrow
df_nz <- arrow::read_parquet( here::here(push_mods, "df_nz_synth_data_t14"))

df_nz

# save data
usethis::use_data(df_nz, overwrite = TRUE)

