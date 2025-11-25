# Read the PREDICTS file cache.

This internal helper function returns a list with 3 elements: 'valid', a
boolean indicating if the cache is valid; 'data', a dataframe containing
the cached PREDICTS data; and 'aux', the auxiliary data loaded from the
cache.

## Usage

``` r
.ReadPredictsFileCache(file_predicts, aux_file_predicts, requested_years)
```

## Arguments

- file_predicts:

  Character, the path to the saved PREDICTS database extract (as an RDS
  file).

- aux_file_predicts:

  Character, the path to the saved PREDICTS database auxiliary metadata,
  saved as a JSON file.

- requested_years:

  Numeric vector, the extract years to be saved.

## Value

List, a named list of three elements: 'valid', a boolean indicating if
the cache is valid; 'data', a dataframe containing the cached PREDICTS
data; and 'aux', the auxiliary data loaded from the cache.
