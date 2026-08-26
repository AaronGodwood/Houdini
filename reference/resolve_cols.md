# Resolve column specification to integer indices

Handles NULL (all), integer vector, or character names matched against
headers.

## Usage

``` r
resolve_cols(cols, n_cols_total, header)
```

## Arguments

- cols:

  Column spec (NULL, integer vector, or character names)

- n_cols_total:

  Total number of columns

- header:

  Header block (for name matching)

## Value

Integer vector of valid 1-based column indices
