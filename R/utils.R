#' Request data from the NHM data portal.
#'
#' @description
#' This function makes a request to the NHM data portal to download data.
#' It will retry the request if it fails due to common errors such as
#' concurrent requests or server errors.
#'
#' @param request_body_json A named list giving the body of the request.
#' @returns A response object from the initial download request. If the request
#'  fails, a list with `status` and `message` entries is returned.
#'
#' @import httr2
#'
#' @noRd
.RequestDataPortal <- function(request_body_json, max_tries = 10) {
  if (!is.list(request_body_json)) {
    stop("Request_body_json should be a list (use e.g. jsonlite::fromJSON)")
  }

  logger::log_debug("Set download request: retry for common errors")
  dl_request <- request(download_url) |>
    req_body_json(request_body_json) |>
    req_user_agent(
      "predictsr resource download request <connor.duffin@nhm.ac.uk>"
    ) |>
    req_retry(
      is_transient = \(resp) resp_status(resp) %in% c(409, 429, 500, 503),
      max_tries = max_tries,
      retry_on_failure = TRUE,
      backoff = \(tries) return(1) # 1 second as NHM doesn't have rate limits
    )

  logger::log_debug("Fetch the download request and return default list o/w")
  dl_response <- tryCatch(
    req_perform(dl_request) |>
      resp_body_json(),
    error = function(e) {
      return(e)
    }
  )

  # if the request failed, we just return out now
  if (inherits(dl_response, "error")) {
    logger::log_error("Download request failed: returning empty list")
    return(
      list(
        status = "failed",
        message = dl_response$message,
        result = NULL
      )
    )
  } else {
    logger::log_debug("Download request successful")
    return(dl_response)
  }
}

#' Monitor the download status response from the NHM data portal.
#'
#' @description
#' This function checks the status of a download request made to the NHM data
#' portal. It will poll the status endpoint until the request is complete or
#' timed out.
#'
#' @param dl_response A list giving the response object from the initial
#'   download request.
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
.CheckDownloadResponse <- function(dl_response, timeout = 600) {
  if (is.null(dl_response$result)) {
    return(
      list(
        status = "failed",
        message = "Provided 'dl_response' has NULL result",
        result = NULL
      )
    )
  }

  logger::log_debug("Check on the status of the download (every 0.5 s)")
  status_json_request <- request(dl_response$result$status_json) |>
    req_throttle(120) |>  # 120 requests every 60s
    req_user_agent("predictsr status request <connor.duffin@nhm.ac.uk>")

  # we make 2 requests per second, so we will finish up after `timeout` seconds
  ticks <- 2 * timeout
  for (i in 1:ticks) {
    logger::log_debug("Download request in progress")

    # make the status request
    status_json <- tryCatch(
      status_json_request |>
        req_perform() |>
        resp_body_json(),
      error = function(e) return(e)
    )

    # if the request failed, we just return out now
    if (inherits(status_json, "error")) {
      logger::log_error("Status request failed: returning empty list")
      status_json <- list(
        status = "failed",
        message = status_json$message,
        result = NULL
      )
      break
    } else if (status_json$status == "complete") {
      logger::log_debug("Download request complete")
      break
    }
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
  # check that the status_json is a list with a 'status' entry
  if (!is.list(status_json) || !("status" %in% names(status_json))) {
    stop("status_json should be a list with a 'status' entry")
  }

  # check that the status is "complete"
  if (status_json$status != "complete") {
    logger::log_error(
      "Download unsuccessful: check your connection and try again"
    )
    return(data.frame())
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
