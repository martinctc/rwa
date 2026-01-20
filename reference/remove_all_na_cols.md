# Remove any columns where all the values are missing

Pass a data frame and returns a version where all columns made up of
entirely missing values are removed.

## Usage

``` r
remove_all_na_cols(df)
```

## Arguments

- df:

  Data frame to be passed through.

## Details

This is used within
[`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md).
