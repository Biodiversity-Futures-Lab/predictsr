
# predictsr

<!-- badges: start -->
[![R-CMD-check](https://github.com/Biodiversity-Futures-Lab/predictsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/predictsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

predictsr fetches the latest version of the open-access PREDICTS database extract from the [Natural History Museum Data
Portal](https://data.nhm.ac.uk/dataset/the-2016-release-of-the-predicts-database-v1-1).

The PREDICTS (Projecting Responses of Ecological Diversity In Changing Terrestrial Systems) dataset comprises 4,318,808 measurements, from 35,736 sampling locations in 101 countries and 47,089 species.

The data were collated from spatial comparisons of local-scale biodiversity exposed to different intensities and types of anthropogenic pressures, from terrestrial sites around the world.

This package accesses the latest version of the open-access database as a data.frame or a [tibble](https://tibble.tidyverse.org/). The available data is both the 2016 and 2022 releases of the database.

The package is very much in-development so there may be changes to the API as we refine the codebase.

## Installation

You can install the development version of predictsr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Biodiversity-Futures-Lab/predictsr")
```

## Usage

To pull in the database and get the data, use the `GetPredictsData` function:

```r
predicts <- GetPredictsData()
```

which by default will read in the 2016 and 2022 data, as a dataframe.

To read in the site-level summaries:

```r
summaries <- GetSitelevelSummaries()
```

which will also, by default, read in the 2016 and 2022 data, as a dataframe.

To read in the descriptions of the columns of the database extract:

```r
columns <- GetColumnDescriptions()
```

which will give you a dataframe on the information on the columns that are in the database extract.

## Notes

The Natural History Museum cannot warrant the quality or accuracy of the data.

This package builds upon the [NHM data portal API](https://data.nhm.ac.uk/terms-conditions), which by default has no API rate
limits. Please respect this and be responsible with your access.
