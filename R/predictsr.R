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
#' @param extract numeric, year/s corresponding to PREDICTS database releases to
#'   download. Options are 2016 or 2022. Defaults to `c(2016, 2022)` - the whole
#'   dataset.
#' @returns A data.frame or tibble containing the full v1.1 PREDICTS database.
#'
#' @export
GetPredictsData <- function(fmt = "data.frame", extract = c(2016, 2022)) {
  # should be a character of length 1
  if (!(is.character(fmt) && length(fmt) == 1)) {
    stop("Input fmt is not a length-1 character")
  }

  # should be a dataframe or a tibble, otherwise skip
  if (fmt != "data.frame" && fmt != "tibble") {
    stop(
      paste(
        "Argument fmt not recognised - please supply either 'data.frame'",
        "or 'tibble'"
      )
    )
  }

  # check that the extract is OK
  if (!all(extract %in% c(2016, 2022)) || is.null(extract)) {
    stop("'extract' should be 2016 and/or 2022")
  }

  # make sure order is 2016, 2022, then combine and read in request
  extract <- sort(extract)
  year_string <- paste(extract, collapse = "_")
  predicts_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", paste0("predicts_request_", year_string, ".json")),
      package = "predictsr"
    )
  )

  # make the request for the final dataframe
  status_json <- .RequestDataPortal(predicts_req)
  predicts <- .RequestRDSDataFrame(status_json, fmt = fmt)
  return(predicts)
}

#' Get the site level summaries from the RDS file.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @param extract Numeric, year/s corresponding to PREDICTS database releases to
#'   download. Options are 2016 or 2022. Defaults to `c(2016, 2022)` - the whole
#'   dataset.
#' @returns The site-level summary data in the format specified by 'fmt'.
#' @export
GetSitelevelSummaries <- function(fmt = "data.frame", extract = 2016) {
  # should be a character of length 1
  if (!(is.character(fmt) && length(fmt) == 1)) {
    stop("Input fmt is not a length-1 character")
  }

  # should be a dataframe or a tibble, otherwise skip
  if (fmt != "data.frame" && fmt != "tibble") {
    stop(
      paste(
        "Argument fmt not recognised - please supply either 'data.frame'",
        "or 'tibble'"
      )
    )
  }

  if (!all(extract %in% c(2016, 2022)) || is.null(extract)) {
    stop("Incorrect 'extract' argument, should be 2016 and/or 2022")
  }

  # make sure order is 2016, 2022, then combine and read in request
  extract <- sort(extract)
  year_string <- paste(extract, collapse = "_")
  site_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", paste0("sitelevel_request_", year_string, ".json")),
      package = "predictsr"
    )
  )

  # make the request for the final dataframe
  status_json <- .RequestDataPortal(site_req)
  predicts <- .RequestRDSDataFrame(status_json, fmt = fmt)
  return(predicts)
}

#' Get a dataframe describing the columns in the PREDICTS database extract.
#'
#' @param fmt string, the format to return the data as. Options are 'data.frame'
#'   or 'tibble'.
#' @param ... extra arguments passed to read.csv.
#' @returns The column descriptions in the format as specified by 'fmt'.
GetColumnDescriptions <- function(fmt = "data.frame", ...) {
  # HACK(connor): currently we only use the data from the year 2022 as this
  # appears to be a "cleaned-up" version of the data. The 2016 appears to be
  # similar but lacks the structural improvements gotten in the 2022 release
  # should be a character of length 1
  if (!(is.character(fmt) && length(fmt) == 1)) {
    stop("Input fmt is not a length-1 character")
  }

  # should be a dataframe or a tibble, otherwise skip
  if (fmt != "data.frame" && fmt != "tibble") {
    stop(
      paste(
        "Argument fmt not recognised - please supply either 'data.frame'",
        "or 'tibble'"
      )
    )
  }

  # column request is year-agnostic
  column_req <- jsonlite::fromJSON(
    system.file(
      file.path("extdata", "column_request.json"),
      package = "predictsr"
    )
  )
  status_json <- .RequestDataPortal(column_req)

  # get the location of where the data is saved to
  data_zip_url <- status_json$urls$direct

  # from stackoverflow
  temp_zip <- tempfile()
  download.file(data_zip_url, temp_zip)

  # write to a tempfile then read into an RDS
  outputs <- unzip(temp_zip, exdir = tempdir())
  output_zip <- outputs[basename(outputs) != "manifest.json"]

  # should just be the remaining ZIP of the column descriptions
  stopifnot(length(output_zip) == 1)
  output <- read.csv(output_zip)

  # delete temporary files from the system
  unlink(temp_zip)
  unlink(outputs)

  # Replace all names with underscores and get rid of all trailing underscores
  names(output) <- gsub("\\.", "_", names(output)) |>
    (\(n) sub("_+$", "", n))()
  if (fmt == "tibble") {
    return(tibble::as_tibble(output))
  } else {
    return(output)
  }
}

#' Request data from the NHM data portal
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
.RequestDataPortal <- function(request_body_json, timeout = 600) {
  if (!is.list(request_body_json)) {
    stop("request_body_json should be a list (use e.g. jsonlite::fromJSON)")
  }

  print("setting up request")
  # set the download request: retry for common error codes, or if curl fails
  dl_request <- request(download_url) |>
    req_body_json(request_body_json) |>
    req_retry(
      is_transient = \(resp) resp_status(resp) %in% c(409, 429, 500, 503),
      max_tries = 10,
      retry_on_failure = TRUE
    )

  print("fetch DL request")
  # fetch the download request
  dl_response <- req_perform(dl_request) |>
    resp_body_json()

  # check on the status of the download
  status_json_request <- request(dl_response$result$status_json) |>
    req_throttle(120) # 120 requests every 60s

  print("make initial status request")
  # make initial request to get status
  status_json <- status_json_request |>
    req_perform() |>
    resp_body_json()

  # we make 2 requests per second, so we will finish up after `timeout` seconds
  ticks <- 2 * timeout
  for (i in 1:ticks) {
    print("DL request in progress")
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
#' @param status_json A named list containing the request that was made.
#' @param fmt Output format. Either a "data.frame" or "tibble".
#' @return A dataframe or tibble assembled from one or more RDS files returned
#'   by the API.
#' @import httr2
.RequestRDSDataFrame <- function(status_json, fmt = "data.frame") {
  if (status_json$status != "complete") {
    stop(
      "Request did not complete successfully (no data to download!) - ",
      "it may have timed out"
    )
  }

  # get the location of where the data is saved to
  data_zip_url <- status_json$urls$direct

  # from stackoverflow
  temp_zip <- tempfile()
  download.file(data_zip_url, temp_zip)

  # write to a tempfile then read into an RDS
  outputs <- unzip(temp_zip, exdir = tempdir())
  outputs <- outputs[basename(outputs) != "manifest.json"]

  df <- do.call(
    rbind,
    unname(
      lapply(
        outputs,
        \(output) {
          return(readRDS(output))
        }
      )
    )
  )

  # delete temporary files from the system
  unlink(temp_zip)
  unlink(outputs)

  # coerce to tibble if needed
  if (fmt == "data.frame") {
    return(df)
  } else if (fmt == "tibble") {
    return(tibble::as_tibble(df))
  }
}
