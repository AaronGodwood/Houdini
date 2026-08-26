# Similarity score between two names in the range 0 to 1

A shared numeric key (e.g. both contain "14.1") is treated as a strong
signal and floors the score near 1; otherwise the score blends token
Jaccard and normalised edit similarity.

## Usage

``` r
match_score(a, b)
```

## Arguments

- a, b:

  Names to compare

## Value

Numeric score between 0 and 1
