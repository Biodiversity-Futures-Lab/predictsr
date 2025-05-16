base_url <- "https://data.nhm.ac.uk"
download_url <- "https://data.nhm.ac.uk/api/3/action/vds_download_queue"

package_id_2016 <- "4e3a9108-3e25-43d8-9f58-31b40fe438d6"
package_id_2022 <- "d2bf57f3-ea67-469d-afd4-0b6416be9af2"

usethis::use_data(
  base_url,
  download_url,
  package_id_2016,
  package_id_2022,
  overwrite = TRUE,
  internal = TRUE
)
