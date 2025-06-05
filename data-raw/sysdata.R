base_url <- "https://data.nhm.ac.uk"
download_url <- "https://data.nhm.ac.uk/api/3/action/vds_download_queue"

usethis::use_data(
  base_url,
  download_url,
  overwrite = TRUE,
  internal = TRUE
)
