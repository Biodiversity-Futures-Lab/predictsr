# use sysdata.rda to store these
base_url <- "https://data.nhm.ac.uk"
package_id <- "4e3a9108-3e25-43d8-9f58-31b40fe438d6"

get_url_string <- function(resource_id) {
  return(
    glue::glue(
      "{base_url}/dataset/{package_id}/resource/{resource_id}/download"
    )
  )
}

#' Open URL connection and read RDS
read_rds_url <- function(url_string, ...) {
  url_con <- url(url_string, "rb")
  on.exit(close(url_con))
  return(readRDS(file = url_con, ...))
}

# download the url
resource_id <- "6fa1dedf-c546-41e0-a470-17c4863686b8"
url_string <- get_url_string(resource_id)
predicts <- read_rds_url(url_string)
