
# predictsr

<!-- badges: start -->
<!-- badges: end -->

predictsr fetches the latest version of the open-access PREDICTS database
extract from the [Natural History Museum Data
Portal](https://data.nhm.ac.uk/dataset/the-2016-release-of-the-predicts-database-v1-1).

The PREDICTS (Projecting Responses of Ecological Diversity In Changing
Terrestrial Systems) dataset comprises 3,278,056 measurement, collated from
26,194 sampling locations in 94 countries and 47,089 species. The
data were collated from 480 existing spatial comparisons of local-scale
biodiversity exposed to different intensities and types of anthropogenic
pressures, from terrestrial sites around the world.

This package accesses the latest version of the open-access database as a data
frame or a tibble, easily allowing R users to download and perform data analysis
with these data.

The package is very much in-development so there may be changes to the API as we
refine the codebase.

## Installation

You can install the development version of predictsr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Biodiversity-Futures-Lab/predictsr")
```
