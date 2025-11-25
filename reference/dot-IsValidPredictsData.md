# Check if a PREDICTS extract is valid.

A small set of basic checks to ensure that a PREDICTS extract is valid.
These include checking the object is a dataframe, checking all the
columns are valid, and checking that we have a nonzero row count.

## Usage

``` r
.IsValidPredictsData(df)
```

## Arguments

- df:

  Dataframe, containing the PREDICTS extract.

## Value

Boolean, `TRUE` if the dataframe is valid, `FALSE` if not.
