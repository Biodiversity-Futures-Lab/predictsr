# Load (or download) PREDICTS data to a user-specified RDS file.

Implements a simple file-based cache. You supply a target filename (e.g.
"data/predicts_2016_2022.rds"). The function will:

1.  Look for that RDS file and the companion metadata file
    "filename.aux.json" (e.g. "data/predicts_2016_2022.rds.aux.json").

2.  If both exist, verify the file hash, minimal structure, and
    requested years.

3.  If validation passes return the loaded object.

4.  Otherwise download fresh data via `GetPredictsData(extract)`,
    overwrite the RDS, write a new `.aux.json`, and return the
    dataframe.

The data are provided under a CC NC (non-commercial) license, which
means that they cannot be used for commercial purposes. The 2016 release
is available under a CC BY-NC-SA 4.0 license, and the 2022 release is
available under a CC NC (any) license.

## Usage

``` r
LoadPredictsData(file_predicts, extract = c(2016, 2022), force_refresh = FALSE)
```

## Arguments

- file_predicts:

  Character path to the desired PREDICTS database RDS file (must end
  with ".rds").

- extract:

  Integer vector of release years to fetch. Defaults to `c(2016, 2022)`.

- force_refresh:

  Logical; if TRUE always re-download and overwrite existing files.

## Value

A dataframe containing the requested PREDICTS extract.

## Examples

``` r
# \donttest{
  file_predicts <- file.path(tempdir(), "predicts.rds")
  df_predicts <- LoadPredictsData(file.path(tempdir(), "predicts.rds"))
#> INFO [2025-12-01 09:41:26] Pulling in extracts for 2016
#> INFO [2025-12-01 09:41:26] Pulling in extracts for 2022
#> INFO [2025-12-01 09:41:26] No existing valid cache at /tmp/RtmpSHHzDH/predicts.rds; will download
#> INFO [2025-12-01 09:41:26] Downloading fresh PREDICTS data (extract=2016,2022)
#> INFO [2025-12-01 09:42:14] Wrote data file 'predicts.rds' and aux metadata 'predicts.rds.aux.json'
# }
```
