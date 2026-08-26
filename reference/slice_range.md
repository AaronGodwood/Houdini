# Resolve a start/end row range to integer indices

Resolve a start/end row range to integer indices

## Usage

``` r
slice_range(n, row_start = NULL, row_end = NULL)
```

## Arguments

- n:

  Number of rows available

- row_start:

  1-based start (NULL = 1)

- row_end:

  1-based end (NULL = last)

## Value

Integer vector of row indices (possibly empty)
