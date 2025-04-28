#' Read the PREDICTS database into either a data frame or a tibble
#'
#' This returns the complete PREDICTS database extract from the latest release
#' (v1.1, 2016). It comprises of 3,278,056 measurements, from 26,194 sampling
#' locations in 94 countries and representing 47,089 species. The data were
#' collected as part of the PREDICTS project - Projecting Responses of
#' Ecological Diversity In Changing Terrestrial Systems.
#'
#' @param fmt string, the format to return the data as. Options are 'data.frame'
#'   or 'tibble'.
#' @returns A data.frame or tibble containing the full v1.1 PREDICTS database.
#'
#' @export
GetPredictsData <- function(fmt = "data.frame") {
  resource_id <- "6fa1dedf-c546-41e0-a470-17c4863686b8"
  return(.GetResourceAsData(fmt = fmt, resource_id = resource_id))
}

#' Get the site level summaries from the RDS file.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @returns The site-level summary data in the format specified by 'fmt'.
#' @export
GetSitelevelSummaries <- function(fmt = "data.frame") {
  resource_id <- "12f66228-4e23-4f0d-8435-18467e283512"
  return(.GetResourceAsData(fmt = fmt, resource_id = resource_id))
}

#' From a given resource ID, retrieve the data at the location as a data.frame
#' or tibble.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @returns The site-level summary data in the format specified by 'fmt'.
.GetResourceAsData <- function(fmt, resource_id) {
  # should be a character of length 1
  if (!(is.character(fmt) && length(fmt) == 1)) {
    stop("Input fmt is not a length-1 character")
  }

  # get the URL
  url_string <- .GetURLString(resource_id)

  if (fmt == "data.frame") {
    sls <- .ReadRDSURL(url_string)
  } else if (fmt == "tibble") {
    sls <- .ReadRDSURL(url_string) |> tibble::as_tibble()
  } else {
    stop(
      paste(
        "Argument fmt not recognised - please supply either 'data.frame'",
        "or 'tibble'"
      )
    )
  }

  return(sls)
}

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
