#' Read the PREDICTS database into either a dataframe.
#'
#' This returns the complete PREDICTS database extract from the latest
#' release/s. The data were collected as part of the PREDICTS project -
#' Projecting Responses of Ecological Diversity In Changing Terrestrial Systems,
#' and comprise of two releases. The first was in 2016, and the second in 2022.
#' This function accesses the 2016 and/or 2022 release.
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
  status_json <- .RequestDataPortal(predicts_req)
  predicts <- .RequestRDSDataFrame(status_json)
  return(predicts)
}

#' Get the PREDICTS database site level summaries.
#'
#' This acesses summary data for the relevant PREDICTS database extract. There
#' are two releases of the PREDICTS database, an initial release in 2016, and an
#' additional release in 2022. The user chooses whether to pull summary data for
#' the 2016 and/or 2022 release.
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
  status_json <- .RequestDataPortal(site_req)
  predicts <- .RequestRDSDataFrame(status_json)
  return(predicts)
}

#' Get a dataframe describing the columns in the PREDICTS database extract.
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
  status_json <- .RequestDataPortal(column_req)

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

#' Request data from the NHM data portal.
#'
#' @param request_body_json A named list giving the body of the request.
#' @param timeout Integer giving the time (in seconds) to wait for the request.
#'   Requests in this package usually take < 5s to be processed, so don't set
#'   this to something really high. Default is 600s (10 minutes).
#' @returns A named `status_json` list giving the status of the request.
#'   Contains `status_json$status` which gives if the request was successful or
#'   not. If it was, data can be downloaded from the `status_json$urls$direct`
#'   entry.
#' @import httr2
#'
#' @noRd
.RequestDataPortal <- function(request_body_json, timeout = 600) {
  if (!is.list(request_body_json)) {
    stop("Request_body_json should be a list (use e.g. jsonlite::fromJSON)")
  }

  logger::log_debug(
    "Set the download request: retry for common error codes, or if curl fails"
  )
  dl_request <- request(download_url) |>
    req_body_json(request_body_json) |>
    req_user_agent(
      "predictsr resource download request <connor.duffin@nhm.ac.uk>"
    ) |>
    req_retry(
      is_transient = \(resp) resp_status(resp) %in% c(409, 429, 500, 503),
      max_tries = 10,
      retry_on_failure = TRUE
    )

  logger::log_debug("Fetch the download request")
  dl_response <- req_perform(dl_request) |>
    resp_body_json()

  logger::log_debug("Check on the status of the download (every 0.5 s)")
  status_json_request <- request(dl_response$result$status_json) |>
    req_throttle(120) |>  # 120 requests every 60s
    req_user_agent("predictsr status request <connor.duffin@nhm.ac.uk>")

  logger::log_debug("Make initial status request to check it is working")
  status_json <- status_json_request |>
    req_perform() |>
    resp_body_json()

  # we make 2 requests per second, so we will finish up after `timeout` seconds
  ticks <- 2 * timeout
  for (i in 1:ticks) {
    logger::log_debug("Download request in progress")
    # break out once it has worked
    if (status_json$status == "complete") {
      break
    }

    # otherwise make the request (again)
    status_json <- status_json_request |>
      req_perform() |>
      resp_body_json()
  }

  if (status_json$status != "complete") {
    warning("Request to download did not complete successfully!")
  }

  # return the status list
  return(status_json)
}

#' Request RDS data from the NHM data portal, returning as a dataframe-like.
#'
#' @param status_json A named list containing the request that was made. This
#'   should be marked as "complete" via its 'status' field.
#' @return A dataframe assembled from one or more RDS files returned
#'   by the API.
#' @import httr2
#'
#' @noRd
.RequestRDSDataFrame <- function(status_json) {
  if (status_json$status != "complete") {
    stop(
      "Request didn't complete (no data to download): it may have timed out"
    )
  }

  logger::log_debug("Get the location of where the data is saved to")
  data_zip_url <- status_json$urls$direct

  logger::log_debug("Download data into a tempfile")
  temp_zip <- tempfile()
  download.file(data_zip_url, temp_zip, quiet = TRUE)

  logger::log_debug("Write to a tempfile then read into an RDS")
  outputs <- unzip(temp_zip, exdir = tempdir())
  outputs <- outputs[basename(outputs) != "manifest.json"]

  logger::log_debug("Row-bind the unnamed list of read-in dataframes")
  df <- do.call(
    rbind,
    unname(
      lapply(outputs, \(output) readRDS(output))
    )
  )

  logger::log_debug("Delete temporary files from the system")
  unlink(temp_zip)
  unlink(outputs)

  return(df)
}
