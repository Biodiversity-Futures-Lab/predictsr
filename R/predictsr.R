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
  predicts <- .RequestRDSDataFrame(predicts_req, fmt = fmt)
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
  predicts <- .RequestRDSDataFrame(site_req, fmt = fmt)
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
  resource_id <- "bae22f1a-b968-496d-8a61-b5d52659440b"
  url_string <- .GetURLString(package_id_2022, resource_id)

  # Set up the URL connection
  url_con <- url(
    url_string,
    headers = c("User-Agent" = "predictsr user - Lead developer: connor.duffin@nhm.ac.uk")
  )
  output <- read.csv(url_con, ...)

  # Replace all names with underscores and get rid of all trailing underscores
  names(output) <- gsub("\\.", "_", names(output)) |>
    (\(n) sub("_+$", "", n))()
  if (fmt == "tibble") {
    return(tibble::as_tibble(output))
  } else {
    return(output)
  }
}

#' From a given resource ID, retrieve the data at the location as a data.frame
#' or tibble.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @param url_string A string giving the URL to download the resource from.
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
#' @param package_id A string for the package ID from the NHM data portal.
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
  url_con <- url(
    url_string,
    "rb",
    headers = c("User-Agent" = "predictsr user - Lead developer: connor.duffin@nhm.ac.uk")
  )
  on.exit(close(url_con))
  return(readRDS(file = url_con, ...))
}

#' Request RDS data from the NHM data portal, returning as a dataframe-like.
#' Download and assemble data from a JSON-based RDS API
#'
#' Sends a JSON-formatted request to the NHM Data Portal, monitors the status of
#' the request, downloads the resulting ZIP archive containing RDS files, and
#' combines them into a single data frame.
#'
#' @param request_body_json A named list or JSON-compatible object to be sent as
#'   the request body.
#' @param fmt Output format. Currently only `"data.frame"` is supported.
#'   Reserved for future extension.
#' @return A dataframe or tibble assembled from one or more RDS files returned
#'   by the API.
#'
#' @import httr2
.RequestRDSDataFrame <- function(request_body_json, fmt = "data.frame") {
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

  # set the download request: retry for common error codes, or if curl fails
  dl_request <- request(download_url) |>
    req_body_json(request_body_json) |>
    req_retry(
      is_transient = \(resp) resp_status(resp) %in% c(409, 429, 500, 503),
      max_tries = 10,
      retry_on_failure = TRUE
    )

  # fetch the download request
  dl_response <- req_perform(dl_request) |>
    resp_body_json()

  # check on the status of the download
  status_json_request <- request(dl_response$result$status_json) |>
    req_throttle(120) # 120 requests every 60s

  # make initial request to get status
  status_json <- status_json_request |>
    req_perform() |>
    resp_body_json()

  # try running for 10 minutes at most
  for (i in 1:1200) {
    # break out once it has worked
    if (status_json$status == "complete") {
      break
    }

    # otherwise make the request (again)
    status_json <- status_json_request |>
      req_perform() |>
      resp_body_json()
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
