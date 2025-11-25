#' Get the PREDICTS database site level summaries.
#'
#' @description
#' This acesses summary data for the relevant PREDICTS database extract.
#'
#' @details
#' The PREDICTS database contains site-level summaries of the data collected
#'   as part of the PREDICTS project - Projecting Responses of Ecological
#'   Diversity In Changing Terrestrial Systems.
#'
#' The site-level summaries are provided as a dataframe, with each row
#'   corresponding to a site-level observation, and each column corresponding to
#'   a variable describing the site or the observation. The data are provided in
#'   a standardised format, with column names that are consistent across the
#'   database.
#'
#' There are two releases of the PREDICTS database, an initial release in 2016,
#'   and an additional release in 2022. The user chooses whether to pull summary
#'   data for the 2016 and/or 2022 release.
#'
#' The data are provided under a CC NC (non-commercial) license, which means
#'   that they cannot be used for commercial purposes. The 2016 release is
#'   available under a CC BY-NC-SA 4.0 license, and the 2022 release is
#'   available under a CC NC (any) license.
#'
#' @param extract Numeric, year/s corresponding to PREDICTS database releases to
#'   download. Options are 2016 or 2022. Defaults to `c(2016, 2022)` - the whole
#'   dataset.
#' @returns The site-level summary data as a dataframe.
#'
#' @examples
#' \donttest{
#'   summaries <- GetSitelevelSummaries()
#'   summaries_2016 <- GetSitelevelSummaries(extract = 2016)
#' }
#'
#' @export
GetSitelevelSummaries <- function(extract = c(2016, 2022)) {
  if (!all(extract %in% c(2016, 2022)) || is.null(extract)) {
    stop("Incorrect 'extract' argument, should be 2016 and/or 2022")
  }

  logger::log_debug(
    "Checking that order is correct; combining and building request from JSON"
  )
  extract <- sort(extract)
  year_string <- paste(extract, collapse = "_")
  site_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", paste0("sitelevel_request_", year_string, ".json")),
      package = "predictsr"
    )
  )

  logger::log_debug(
    "Request PREDICTS summary data from the data portal and pull into R"
  )
  status_json <- .RequestDataPortal(site_req) |>
    .CheckDownloadResponse()
  predicts <- .RequestRDSDataFrame(status_json)
  return(predicts)
}

#' Get a dataframe describing the columns in the PREDICTS database extract.
#'
#' @description
#' This function returns a dataframe containing the column descriptions for
#'   the PREDICTS database extract.
#'
#' @details
#' The PREDICTS - Predicting Responses of Ecological Diversity In Changing
#'   Terrestrial Systems - database contains a large number of columns, each
#'   corresponding to a variable describing the site or the observation. This
#'   function accesses the column descriptions for the PREDICTS database
#'   extract.
#'
#' The column descriptions are provided as a dataframe, with each row
#'   corresponding to a column in the PREDICTS database extract.
#'
#' There are two releases of the PREDICTS database, an initial release in 2016,
#'   and an additional release in 2022. The user chooses whether to pull summary
#'   data for the 2016 and/or 2022 release.
#'
#' The data are provided under a CC NC (non-commercial) license, which means
#'   that they cannot be used for commercial purposes. The 2016 release is
#'   available under a CC BY-NC-SA 4.0 license, and the 2022 release is
#'   available under a CC NC (any) license.
#'
#' @param ... extra arguments passed to read.csv.
#' @returns The column descriptions in the format as a dataframe.
#'
#' @examples
#' \donttest{
#'   descriptions <- GetColumnDescriptions()
#' }
#'
#' @export
GetColumnDescriptions <- function(...) {
  # HACK(connor): currently we only use the data from the year 2022 as this
  # appears to be a "cleaned-up" version of the data. The 2016 appears to be
  # similar but lacks the structural improvements gotten in the 2022 release
  # should be a character of length 1

  # column request is year-agnostic
  logger::log_debug(
    "Load in column descriptions JSON request, and make the first request",
    " (this will fetch a status endpoint which we will check)"
  )
  column_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", "column_request.json"),
      package = "predictsr"
    )
  )
  status_json <- .RequestDataPortal(column_req) |>
    .CheckDownloadResponse()

  # check that the status is "complete" --- if it isn't, log an error and
  # return an empty data frame to indicate failure
  if (status_json$status != "complete") {
    logger::log_error(
      "Download unsuccessful: check your connection and try again"
    )
    return(data.frame())
  }

  logger::log_debug("Pluck the download URL, and download to ZIP")
  data_zip_url <- status_json$urls$direct

  temp_zip <- tempfile()
  download.file(data_zip_url, temp_zip)

  logger::log_debug(
    "Unzip the download and get the column description csv"
  )
  outputs <- unzip(temp_zip, exdir = tempdir())
  output_zip <- outputs[basename(outputs) != "manifest.json"]

  # should just be the remaining ZIP of the column descriptions
  stopifnot(length(output_zip) == 1)
  output <- read.csv(output_zip)

  logger::log_debug("Delete the temporary files from the system")
  unlink(temp_zip)
  unlink(outputs)

  logger::log_debug("Fix up column names before returning as appropriate type")
  names(output) <- gsub("\\.", "_", names(output)) |>
    (\(n) sub("_+$", "", n))()

  return(output)
}
