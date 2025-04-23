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
get_predicts_data <- function(fmt = "data.frame") {
  resource_id <- "6fa1dedf-c546-41e0-a470-17c4863686b8"
  url_string <- .get_url_string(resource_id)

  if (fmt == "data.frame") {
    predicts <- .read_rds_url(url_string)
  } else if (fmt == "tibble") {
    predicts <- .read_rds_url(url_string) |> tibble::as_tibble()
  } else {
    stop(
      paste(
        "Argument fmt not recognised - please supply either 'data.frame'",
        "or 'tibble'"
      )
    )
  }

  return(predicts)
}
