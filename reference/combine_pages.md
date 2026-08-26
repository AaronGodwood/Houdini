# Combine multiple pages into a single flat table

Header rows are taken from the first page only. Data rows are
concatenated across all pages.

## Usage

``` r
combine_pages(pages)
```

## Arguments

- pages:

  Filtered list of page objects

## Value

list(header, data, col_widths_twips) where header/data are blocks
