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
GetPredictsData <- function(fmt = "data.frame", extract = c(2016, 2022)) {
  if (!all(extract %in% c(2016, 2022))) {
    stop("Incorrect 'extract' argument, should be 2016 and/or 2022")
  }

  inputs <- data.frame(
    year = c(2016, 2022), # char for easy comparison
    packages = c(package_id_2016, package_id_2022),
    resources = c(
      "6fa1dedf-c546-41e0-a470-17c4863686b8",
      "5b91276b-9051-4f48-9a5b-b3106730e4ae"
    )
  ) |>
    subset(year %in% extract) |>
    dplyr::mutate(url_strings = .GetURLString(packages, resources))

  extract <- lapply(inputs$url_strings, .GetResourceAsData, fmt = fmt)
  return(do.call(rbind, extract))
}

#' Get the site level summaries from the RDS file.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @returns The site-level summary data in the format specified by 'fmt'.
#' @export
GetSitelevelSummaries <- function(fmt = "data.frame", extract = 2016) {
  if (!all(extract %in% c(2016, 2022))) {
    stop("Incorrect 'extract' argument, should be 2016 and/or 2022")
  }

  inputs <- data.frame(
    year = c(2016, 2022), # char for easy comparison
    packages = c(package_id_2016, package_id_2022),
    resources = c(
      "12f66228-4e23-4f0d-8435-18467e283512",
      "83e40a70-bf91-4d85-b8af-adff624baab1"
    )
  ) |>
    subset(year %in% extract) |>
    dplyr::mutate(url_strings = .GetURLString(packages, resources))

  extract <- lapply(inputs$url_strings, .GetResourceAsData, fmt = fmt)
  return(do.call(rbind, extract))
}

#' From a given resource ID, retrieve the data at the location as a data.frame
#' or tibble.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @returns The site-level summary data in the format specified by 'fmt'.
.GetResourceAsData <- function(fmt, url_string) {
  # should be a character of length 1
  if (!(is.character(fmt) && length(fmt) == 1)) {
    stop("Input fmt is not a length-1 character")
  }

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
.GetURLString <- function(package_id, resource_id) {
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
