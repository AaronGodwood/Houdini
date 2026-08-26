# Best-matching candidate for a name

Best-matching candidate for a name

## Usage

``` r
best_match(name, candidates)
```

## Arguments

- name:

  The name to match (e.g. a bookmark)

- candidates:

  Character vector of candidate names (e.g. table names)

## Value

list(match = best candidate or NA, score = its score from 0 to 1). Ties
break toward a shared numeric key, then alphabetically, for determinism.
