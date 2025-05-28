.onLoad <- function(libname, pkgname) {
  logger::log_formatter(formatter = glue::glue)

  return(invisible())
}
