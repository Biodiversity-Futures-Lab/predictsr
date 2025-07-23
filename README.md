
# predictsr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/predictsr)](https://cran.r-project.org/package=predictsr)
[![R-CMD-check](https://github.com/Biodiversity-Futures-Lab/predictsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/predictsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

predictsr fetches the latest version of the open-access
[PREDICTS](https://www.nhm.ac.uk/our-science/research/projects/predicts.html)
database extract from the [Natural History Museum Data
Portal](https://data.nhm.ac.uk/).

The publicly-available PREDICTS (Projecting Responses of Ecological
Diversity In Changing Terrestrial Systems) dataset comprises 4,318,808
measurements, from 35,736 sampling locations in 101 countries and 54,863
species, taken across two releases: one in
[2016](https://data.nhm.ac.uk/dataset/the-2016-release-of-the-predicts-database-v1-1)
and another in
[2022](https://data.nhm.ac.uk/dataset/release-of-data-added-to-the-predicts-database-november-2022).
The database is under constant development and is continuously growing,
with later releases planned.

The data were collated from spatial comparisons of local-scale
biodiversity exposed to different intensities and types of anthropogenic
pressures, from terrestrial sites around the world, and are described in
Hudson et. al. (2016) <doi:10.1002/ece3.2579>.

This package accesses the latest version of the open-access database as
a dataframe, and is under active development.

## Installation

Installation from CRAN proceeds as usual:

    install.packages("predictsr")

You can also install the development version of predictsr from
[GitHub](https://github.com/) with:

    # install.packages("pak")
    pak::pak("Biodiversity-Futures-Lab/predictsr")

Then as usual just load the package:

    library(predictsr)

## Usage

To pull in the database and get the data, use the `GetPredictsData`
function:

    predicts <- GetPredictsData()

which by default will read in the 2016 and 2022 data, as a dataframe.

To read in the site-level summaries:

    summaries <- GetSitelevelSummaries()

which will also, by default, read in the 2016 and 2022 data, as a
dataframe.

To read in the descriptions of the columns of the database extract:

    columns <- GetColumnDescriptions()

which will give you a dataframe on the information on the columns that
are in the database extract.

## Notes

The Natural History Museum cannot warrant the quality or accuracy of the
data.

This package builds upon the [NHM data portal
API](https://data.nhm.ac.uk/about/download), which by default has no API
rate limits. Please respect this and be responsible with your access.

## Copyright

This data is provided subject to terms on the [Natural History Museum
Data Portal](https://data.nhm.ac.uk/terms-conditions). The 2016 release
is licensed under a [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/) license, and the
2022 release is licensed under a [CC
NC](https://creativecommons.org/licenses/by-nc/4.0/) license.

Note that this stipulates that **you may not** use the data for
commercial purposes, and that you must provide *attribution* to the
original source of the data.
