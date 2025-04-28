#' Format a resource URL as a string
#'
#' @param resource_id A string for the resource ID from the NHM data portal.
#'
#' @returns A string with the url to the data to be downloaded.
.GetURLString <- function(resource_id) {
  return(
    glue::glue(
      "{base_url}/dataset/{package_id}/resource/{resource_id}/download"
    )
  )
}

#' Open URL connection and read RDS
#'
#' @param url_string A string (URL) to read the data from.
#' @param ... Extra arguments passed into `readRDS`.
#'
#' @returns An R object read in from the RDS file at `url_string`.
.ReadRDSURL <- function(url_string, ...) {
  url_con <- url(url_string, "rb")
  on.exit(close(url_con))
  return(readRDS(file = url_con, ...))
}
