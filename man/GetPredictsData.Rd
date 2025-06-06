% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictsr.R
\name{GetPredictsData}
\alias{GetPredictsData}
\title{Read the PREDICTS database into either a dataframe.}
\usage{
GetPredictsData(extract = c(2016, 2022))
}
\arguments{
\item{extract}{numeric, year/s corresponding to PREDICTS database releases to
download. Options are 2016 or 2022. Defaults to \code{c(2016, 2022)} - the whole
dataset.}
}
\value{
A dataframe containing the v1.1 PREDICTS database extract/s.
}
\description{
This returns the complete PREDICTS database extract from the latest
release/s. The data were collected as part of the PREDICTS project -
Projecting Responses of Ecological Diversity In Changing Terrestrial Systems,
and comprise of two releases. The first was in 2016, and the second in 2022.
This function accesses the 2016 and/or 2022 release.
}
\examples{
\dontrun{
  predicts <- GetPredictsData()
  predicts_2016 <- GetPredictsData(extract = 2016)
}

}
