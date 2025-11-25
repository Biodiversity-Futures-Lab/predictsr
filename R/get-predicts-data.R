#' Read the PREDICTS database into either a dataframe.
#'
#' @description
#' This returns the latest complete PREDICTS database extract as a dataframe.
#'
#' @details
#' The data were collected as part of the PREDICTS project - Projecting
#'   Responses of Ecological Diversity In Changing Terrestrial Systems, and
#'   comprise of two releases. The first was in 2016, and the second in 2022.
#'   This function accesses the 2016 and/or 2022 release.
#'
#' The database is provided as a dataframe, with each row corresponding to a
#'   site-level observation, and each column corresponding to a variable
#'   describing the site or the observation. The data are provided in a
#'   standardised format, with column names that are consistent across the
#'   database.
#'
#' The data are provided under a CC NC (non-commercial) license, which means
#'   that they cannot be used for commercial purposes. The 2016 release is
#'   available under a CC BY-NC-SA 4.0 license, and the 2022 release is available
#'   under a CC NC (any) license.
#'
#' @param extract numeric, year/s corresponding to PREDICTS database releases to
#'   download. Options are 2016 or 2022. Defaults to `c(2016, 2022)` - the whole
#'   dataset.
#' @returns A dataframe containing the v1.1 PREDICTS database extract/s.
#'
#' @examples
#' \donttest{
#'   predicts <- GetPredictsData()
#'   predicts_2016 <- GetPredictsData(extract = 2016)
#' }
#'
#' @export
GetPredictsData <- function(extract = c(2016, 2022)) {
  # check that the extract is OK
  if (!all(extract %in% c(2016, 2022)) || is.null(extract)) {
    stop("'extract' should be 2016 and/or 2022")
  }

  logger::log_debug(
    "Checking that order is correct; combining and building request from JSON"
  )
  extract <- sort(extract)
  year_string <- paste(extract, collapse = "_")
  predicts_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", paste0("predicts_request_", year_string, ".json")),
      package = "predictsr"
    )
  )

  logger::log_debug(
    "Request PREDICTS data and pull in data as dataframe, into R"
  )
  status_json <- .RequestDataPortal(predicts_req) |>
    .CheckDownloadResponse()
  predicts <- .RequestRDSDataFrame(status_json)
  return(predicts)
}
