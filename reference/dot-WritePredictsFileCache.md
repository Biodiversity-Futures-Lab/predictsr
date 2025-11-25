# Writes the PREDICTS dataframe to disk.

Given a PREDICTS database extract, loaded as an R dataframe, save it to
disk and write the aux JSON file (\*.aux.json), which stores metadata
for the object. This includes the release years, the timestamp of when
it was saved, som dimensions, and the SHA-256 hash of the dataframe
(computed with 'digest').

## Usage

``` r
.WritePredictsFileCache(df, file_predicts, aux_file_predicts, extract)
```

## Arguments

- df:

  Dataframe to be written to disk

- file_predicts:

  Character, path to the desired file - should be an RDS file.

- aux_file_predicts:

  Character, path to where the auxiliary file should be saved to disk.

- extract:

  Numeric vector of release years to be saved.

## Value

TRUE invisibly.
