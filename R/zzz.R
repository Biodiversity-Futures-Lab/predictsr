.onLoad <- function(libname, pkgname) {
  logger::log_formatter(formatter = logger::formatter_glue)

  return(invisible())
}
