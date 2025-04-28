#' Get the site level summaries from the RDS file.
#'
#' @param fmt A string to give the output format, either 'data.frame' or
#'   'tibble'. Defaults to a data frame.
#' @returns The site-level summary data in the format specified by 'fmt'.
#' @export
GetSitelevelSummaries <- function(fmt = "data.frame") {
  stopifnot(is.character(fmt))

  resource_id <- "12f66228-4e23-4f0d-8435-18467e283512"
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
